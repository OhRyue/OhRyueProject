package com.OhRyue.certpilot.versus.service;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.OhRyue.certpilot.versus.dto.MatchingDtos;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.MatchParticipantRepository;
import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.extern.slf4j.Slf4j;

/**
 * 실시간 매칭 큐 서비스
 * - 1:1 배틀: 2명 모이면 자동 시작
 * - 토너먼트: 8명 모이면 자동 시작
 */
@Slf4j
@Service
public class MatchingQueueService {

    private final VersusService versusService;
    private final ObjectMapper objectMapper;
    private final MatchParticipantRepository participantRepository;
    
    // 매칭 풀: mode -> matchingKey -> 대기자 리스트
    private final Map<String, Map<String, Queue<WaitingPlayer>>> matchingPools = new ConcurrentHashMap<>();
    
    // 사용자별 매칭 정보: userId -> MatchingInfo
    private final Map<String, MatchingInfo> userMatchingInfo = new ConcurrentHashMap<>();
    
    // 매칭 풀별 락
    private final Map<String, ReentrantLock> poolLocks = new ConcurrentHashMap<>();
    
    public MatchingQueueService(VersusService versusService, ObjectMapper objectMapper, MatchParticipantRepository participantRepository) {
        this.versusService = versusService;
        this.objectMapper = objectMapper;
        this.participantRepository = participantRepository;
    }

    /**
     * 매칭 요청
     */
    public MatchingDtos.MatchStatusResp requestMatch(String userId, MatchingDtos.MatchRequest request) {
        // 동일 모드의 활성 방이 이미 있는지 확인 (봇전 제외)
        List<Long> existingRoomIds = participantRepository.findActiveNonBotRoomIdsByUserIdAndMode(
            userId, MatchStatus.ONGOING, request.mode()
        );
        if (!existingRoomIds.isEmpty()) {
            Long roomId = existingRoomIds.get(0);
            log.info("이미 활성화된 방이 있습니다: userId={}, mode={}, roomId={}", 
                userId, request.mode(), roomId);
            return new MatchingDtos.MatchStatusResp(
                false, // 매칭 완료 (이미 활성 방 있음)
                roomId,
                0,
                null
            );
        }
        
        String matchingKey = buildMatchingKey(request);
        String poolKey = request.mode().name();
        
        // 기존 매칭 취소
        cancelMatch(userId);
        
        // 매칭 풀 가져오기
        Map<String, Queue<WaitingPlayer>> pool = matchingPools.computeIfAbsent(
            poolKey, k -> new ConcurrentHashMap<>()
        );
        
        Queue<WaitingPlayer> queue = pool.computeIfAbsent(
            matchingKey, k -> new LinkedList<>()
        );
        
        // 락 가져오기
        ReentrantLock lock = poolLocks.computeIfAbsent(
            poolKey + ":" + matchingKey, k -> new ReentrantLock()
        );
        
        lock.lock();
        try {
            // 대기자 추가
            WaitingPlayer player = new WaitingPlayer(
                userId,
                request,
                Instant.now()
            );
            queue.offer(player);
            
            // 사용자 매칭 정보 저장
            userMatchingInfo.put(userId, new MatchingInfo(
                poolKey,
                matchingKey,
                request.mode()
            ));
            
            log.info("매칭 요청: userId={}, mode={}, matchingKey={}, queueSize={}", 
                userId, request.mode(), matchingKey, queue.size());
            
            // 즉시 매칭 시도 (동기 처리)
            int requiredCount = request.mode() == MatchMode.DUEL ? 2 : 8;
            Long roomId = null;
            
            if (queue.size() >= requiredCount) {
                // 매칭 성공: 필요한 인원만큼 추출
                List<WaitingPlayer> matched = new ArrayList<>();
                for (int i = 0; i < requiredCount && !queue.isEmpty(); i++) {
                    WaitingPlayer p = queue.poll();
                    if (p != null) {
                        matched.add(p);
                        userMatchingInfo.remove(p.userId());
                    }
                }
                
                if (matched.size() == requiredCount) {
                    List<String> matchedPlayerIds = matched.stream()
                        .map(WaitingPlayer::userId)
                        .collect(Collectors.toList());
                    log.info("즉시 매칭 성공: mode={}, players={}, queueSize={}", 
                        request.mode(), matchedPlayerIds, queue.size());
                    
                    // 방 생성 및 시작 (동기 처리)
                    try {
                        roomId = createAndStartRoomSync(matched, request.mode());
                        if (roomId != null) {
                            log.info("즉시 매칭 완료: roomId={}, mode={}, players={}", 
                                roomId, request.mode(), matchedPlayerIds);
                            // 매칭 완료: roomId 반환
                            return new MatchingDtos.MatchStatusResp(
                                false, // 매칭 완료
                                roomId,
                                0,
                                player.joinedAt()
                            );
                        } else {
                            log.warn("방 생성 실패 (null 반환), 플레이어들을 다시 큐에 추가: mode={}, players={}", 
                                request.mode(), matchedPlayerIds);
                        }
                    } catch (Exception e) {
                        log.error("방 생성 실패 (예외 발생), 플레이어들을 다시 큐에 추가: mode={}, players={}, error={}, errorClass={}", 
                            request.mode(), matchedPlayerIds, e.getMessage(), e.getClass().getName(), e);
                    }
                    
                    // 방 생성 실패 시 플레이어들을 다시 큐에 넣기
                    if (roomId == null) {
                        matched.forEach(queue::offer);
                        // 매칭 정보 복구
                        matched.forEach(p -> userMatchingInfo.put(p.userId(), 
                            new MatchingInfo(poolKey, matchingKey, request.mode())));
                        log.info("플레이어 복구 완료 (동기): mode={}, players={}, 복구 후 큐 크기={}", 
                            request.mode(), matchedPlayerIds, queue.size());
                    }
                } else {
                    // 인원 부족: 다시 큐에 넣기
                    matched.forEach(queue::offer);
                    matched.forEach(p -> userMatchingInfo.put(p.userId(), 
                        new MatchingInfo(poolKey, matchingKey, request.mode())));
                }
            }
            
            // 매칭 대기 중 또는 즉시 매칭 실패
            // 비동기로 추가 매칭 시도 (다른 사용자가 들어올 수 있음)
            tryMatchAsync(poolKey, matchingKey, request.mode());
            
            return new MatchingDtos.MatchStatusResp(
                true, // 매칭 대기 중
                null,
                queue.size(),
                player.joinedAt()
            );
        } finally {
            lock.unlock();
        }
    }

    /**
     * 매칭 취소
     */
    public void cancelMatch(String userId) {
        MatchingInfo info = userMatchingInfo.remove(userId);
        if (info == null) {
            return;
        }
        
        Map<String, Queue<WaitingPlayer>> pool = matchingPools.get(info.poolKey());
        if (pool == null) {
            return;
        }
        
        Queue<WaitingPlayer> queue = pool.get(info.matchingKey());
        if (queue == null) {
            return;
        }
        
        ReentrantLock lock = poolLocks.get(info.poolKey() + ":" + info.matchingKey());
        if (lock != null) {
            lock.lock();
            try {
                queue.removeIf(p -> p.userId().equals(userId));
                log.info("매칭 취소: userId={}, mode={}", userId, info.mode());
            } finally {
                lock.unlock();
            }
        }
    }

    /**
     * 매칭 상태 조회
     */
    public MatchingDtos.MatchStatusResp getMatchStatus(String userId) {
        // 1. 매칭 대기 중인지 먼저 확인
        MatchingInfo info = userMatchingInfo.get(userId);
        if (info != null) {
            // 매칭 중인 모드의 활성 방만 확인 (봇전 제외)
            List<Long> activeRoomIds = participantRepository.findActiveNonBotRoomIdsByUserIdAndMode(
                userId, MatchStatus.ONGOING, info.mode()
            );
            if (!activeRoomIds.isEmpty()) {
                Long roomId = activeRoomIds.get(0); // 가장 최근 방
                // 활성 방이 있으면 매칭 정보 정리 (이미 매칭 완료된 상태)
                userMatchingInfo.remove(userId);
                return new MatchingDtos.MatchStatusResp(
                    false, // 매칭 완료
                    roomId,
                    0,
                    null
                );
            }
            
            // 아직 활성 방이 없으면 큐에서 대기 중인지 확인
            Map<String, Queue<WaitingPlayer>> pool = matchingPools.get(info.poolKey());
            if (pool != null) {
                Queue<WaitingPlayer> queue = pool.get(info.matchingKey());
                if (queue != null) {
                    int waitingCount = queue.size();
                    boolean isWaiting = queue.stream().anyMatch(p -> p.userId().equals(userId));
                    if (isWaiting) {
                        return new MatchingDtos.MatchStatusResp(
                            true, // 매칭 대기 중
                            null,
                            waitingCount,
                            null
                        );
                    } else {
                        // 큐에 없는데 매칭 정보가 남아있으면 정리
                        userMatchingInfo.remove(userId);
                    }
                }
            }
        }
        
        // 2. 매칭 정보가 없으면 기존 로직: 모든 모드의 활성 방 확인 (봇전 제외)
        List<Long> activeRoomIds = participantRepository.findActiveNonBotRoomIdsByUserId(userId, MatchStatus.ONGOING);
        if (!activeRoomIds.isEmpty()) {
            Long roomId = activeRoomIds.get(0); // 가장 최근 방
            return new MatchingDtos.MatchStatusResp(
                false, // 매칭 완료
                roomId,
                0,
                null
            );
        }
        
        // 3. 매칭 중이 아니고 활성 방도 없음
        return new MatchingDtos.MatchStatusResp(false, null, 0, null);
    }

    /**
     * 방 생성 및 자동 시작 (동기)
     */
    private Long createAndStartRoomSync(List<WaitingPlayer> players, MatchMode mode) {
        List<String> playerIds = players.stream().map(WaitingPlayer::userId).collect(Collectors.toList());
        log.info("방 생성 시작 (동기): mode={}, players={}", mode, playerIds);
        
        try {
            WaitingPlayer firstPlayer = players.get(0);
            MatchingDtos.MatchRequest request = firstPlayer.request();
            
            // scopeJson 생성
            String scopeJson = buildScopeJson(request);
            log.debug("scopeJson 생성 완료: mode={}, scopeJson={}", mode, scopeJson);
            
            // participants 리스트 생성 (첫 번째 플레이어 제외)
            List<String> participants = players.stream()
                .skip(1)
                .map(WaitingPlayer::userId)
                .collect(Collectors.toList());
            
            log.debug("참가자 리스트 생성: creator={}, participants={}", firstPlayer.userId(), participants);
            
            // 방 생성 요청
            VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
                mode,
                scopeJson,
                participants,
                null,  // questions는 자동 생성
                null,  // tournamentBracketJson
                null,  // tournamentBracketRound
                null,  // goldenbellRuleJson
                null,  // scheduledAt
                false  // skipCreatorJoin (기본값: false)
            );
            
            // 방 생성 (첫 번째 플레이어가 생성자)
            log.info("방 생성 요청: mode={}, creator={}, participants={}", mode, firstPlayer.userId(), participants);
            VersusDtos.RoomDetailResp room = versusService.createRoom(
                createReq, 
                firstPlayer.userId()
            );
            
            Long roomId = room.room().roomId();
            log.info("방 생성 성공: roomId={}, mode={}", roomId, mode);
            
            // 자동 시작
            log.info("방 시작 요청: roomId={}, mode={}", roomId, mode);
            versusService.startRoom(roomId);
            log.info("방 시작 성공: roomId={}, mode={}", roomId, mode);
            
            log.info("매칭 완료 및 방 시작: roomId={}, mode={}, players={}", 
                roomId, mode, playerIds);
            
            return roomId;
        } catch (Exception e) {
            log.error("방 생성 및 시작 실패 (동기): mode={}, players={}, error={}, errorClass={}", 
                mode, playerIds, e.getMessage(), e.getClass().getName(), e);
            return null;
        }
    }

    /**
     * 매칭 시도 (비동기)
     */
    @Async
    public void tryMatchAsync(String poolKey, String matchingKey, MatchMode mode) {
        Map<String, Queue<WaitingPlayer>> pool = matchingPools.get(poolKey);
        if (pool == null) {
            return;
        }
        
        Queue<WaitingPlayer> queue = pool.get(matchingKey);
        if (queue == null) {
            return;
        }
        
        ReentrantLock lock = poolLocks.get(poolKey + ":" + matchingKey);
        if (lock == null) {
            return;
        }
        
        lock.lock();
        try {
            int requiredCount = mode == MatchMode.DUEL ? 2 : 8;
            
            if (queue.size() < requiredCount) {
                log.debug("매칭 대기 중: mode={}, current={}, required={}", 
                    mode, queue.size(), requiredCount);
                return;
            }
            
            // 매칭 성공: 필요한 인원만큼 추출
            List<WaitingPlayer> matched = new ArrayList<>();
            for (int i = 0; i < requiredCount && !queue.isEmpty(); i++) {
                WaitingPlayer player = queue.poll();
                if (player != null) {
                    matched.add(player);
                    userMatchingInfo.remove(player.userId());
                }
            }
            
            if (matched.size() < requiredCount) {
                // 인원 부족: 다시 큐에 넣기
                List<String> matchedPlayerIds = matched.stream()
                    .map(WaitingPlayer::userId)
                    .collect(Collectors.toList());
                log.warn("인원 부족으로 매칭 취소: mode={}, required={}, matched={}, players={}", 
                    mode, requiredCount, matched.size(), matchedPlayerIds);
                matched.forEach(queue::offer);
                // 매칭 정보 복구
                matched.forEach(p -> userMatchingInfo.put(p.userId(), 
                    new MatchingInfo(poolKey, matchingKey, mode)));
                return;
            }
            
            List<String> matchedPlayerIds = matched.stream()
                .map(WaitingPlayer::userId)
                .collect(Collectors.toList());
            log.info("비동기 매칭 성공: mode={}, players={}, queueSize={}", 
                mode, matchedPlayerIds, queue.size());
            
            // 방 생성 및 시작 (비동기)
            createAndStartRoomAsync(matched, mode);
            
        } finally {
            lock.unlock();
        }
    }

    /**
     * 방 생성 및 자동 시작 (비동기)
     */
    private void createAndStartRoomAsync(List<WaitingPlayer> players, MatchMode mode) {
        List<String> playerIds = players.stream().map(WaitingPlayer::userId).collect(Collectors.toList());
        log.info("방 생성 시작 (비동기): mode={}, players={}", mode, playerIds);
        
        try {
            WaitingPlayer firstPlayer = players.get(0);
            MatchingDtos.MatchRequest request = firstPlayer.request();
            
            // scopeJson 생성
            String scopeJson = buildScopeJson(request);
            log.debug("scopeJson 생성 완료 (비동기): mode={}, scopeJson={}", mode, scopeJson);
            
            // participants 리스트 생성 (첫 번째 플레이어 제외)
            List<String> participants = players.stream()
                .skip(1)
                .map(WaitingPlayer::userId)
                .collect(Collectors.toList());
            
            log.debug("참가자 리스트 생성 (비동기): creator={}, participants={}", firstPlayer.userId(), participants);
            
            // 방 생성 요청
            VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
                mode,
                scopeJson,
                participants,
                null,  // questions는 자동 생성
                null,  // tournamentBracketJson
                null,  // tournamentBracketRound
                null,  // goldenbellRuleJson
                null,  // scheduledAt
                false  // skipCreatorJoin (기본값: false)
            );
            
            // 방 생성 (첫 번째 플레이어가 생성자)
            log.info("방 생성 요청 (비동기): mode={}, creator={}, participants={}", mode, firstPlayer.userId(), participants);
            VersusDtos.RoomDetailResp room = versusService.createRoom(
                createReq, 
                firstPlayer.userId()
            );
            
            Long roomId = room.room().roomId();
            log.info("방 생성 성공 (비동기): roomId={}, mode={}", roomId, mode);
            
            // 자동 시작
            log.info("방 시작 요청 (비동기): roomId={}, mode={}", roomId, mode);
            versusService.startRoom(roomId);
            log.info("방 시작 성공 (비동기): roomId={}, mode={}", roomId, mode);
            
            log.info("매칭 완료 및 방 시작 (비동기): roomId={}, mode={}, players={}", 
                roomId, mode, playerIds);
            
        } catch (Exception e) {
            log.error("방 생성 및 시작 실패 (비동기): mode={}, players={}, error={}, errorClass={}", 
                mode, playerIds, e.getMessage(), e.getClass().getName(), e);
            
            // 실패 시 플레이어들을 다시 큐에 넣기 (복구 로직)
            restorePlayersToQueue(players, mode);
        }
    }
    
    /**
     * 플레이어들을 큐에 복구 (방 생성 실패 시)
     */
    private void restorePlayersToQueue(List<WaitingPlayer> players, MatchMode mode) {
        if (players.isEmpty()) {
            log.warn("복구할 플레이어가 없음: mode={}", mode);
            return;
        }
        
        WaitingPlayer firstPlayer = players.get(0);
        MatchingDtos.MatchRequest request = firstPlayer.request();
        String matchingKey = buildMatchingKey(request);
        String poolKey = mode.name();
        
        Map<String, Queue<WaitingPlayer>> pool = matchingPools.get(poolKey);
        if (pool == null) {
            log.error("매칭 풀을 찾을 수 없음: poolKey={}, mode={}", poolKey, mode);
            return;
        }
        
        Queue<WaitingPlayer> queue = pool.get(matchingKey);
        if (queue == null) {
            log.error("매칭 큐를 찾을 수 없음: matchingKey={}, mode={}", matchingKey, mode);
            return;
        }
        
        ReentrantLock lock = poolLocks.get(poolKey + ":" + matchingKey);
        if (lock == null) {
            log.error("락을 찾을 수 없음: poolKey={}, matchingKey={}, mode={}", poolKey, matchingKey, mode);
            return;
        }
        
        lock.lock();
        try {
            List<String> playerIds = players.stream().map(WaitingPlayer::userId).collect(Collectors.toList());
            log.info("플레이어 복구 시작: mode={}, matchingKey={}, players={}, 현재 큐 크기={}", 
                mode, matchingKey, playerIds, queue.size());
            
            // 플레이어들을 다시 큐에 넣기
            int restoredCount = 0;
            for (WaitingPlayer player : players) {
                queue.offer(player);
                restoredCount++;
                
                // 매칭 정보 복구
                userMatchingInfo.put(player.userId(), 
                    new MatchingInfo(poolKey, matchingKey, mode));
            }
            
            log.info("플레이어 복구 완료: mode={}, matchingKey={}, 복구된 인원={}, 복구 후 큐 크기={}, players={}", 
                mode, matchingKey, restoredCount, queue.size(), playerIds);
            
        } catch (Exception e) {
            log.error("플레이어 복구 중 오류 발생: mode={}, matchingKey={}, error={}", 
                mode, matchingKey, e.getMessage(), e);
        } finally {
            lock.unlock();
        }
    }

    /**
     * 매칭 키 생성
     */
    private String buildMatchingKey(MatchingDtos.MatchRequest request) {
        if ("CATEGORY".equals(request.matchingMode())) {
            return String.format("%s:%s:CATEGORY:%s:%s", 
                request.certId(), 
                request.examMode(),
                request.topicId(),
                request.mode().name()
            );
        } else {
            return String.format("%s:%s:DIFFICULTY:%s:%s", 
                request.certId(), 
                request.examMode(),
                request.difficulty(),
                request.mode().name()
            );
        }
    }

    /**
     * scopeJson 생성
     */
    private String buildScopeJson(MatchingDtos.MatchRequest request) {
        try {
            Map<String, Object> scope = new HashMap<>();
            scope.put("examMode", request.examMode());
            
            if ("CATEGORY".equals(request.matchingMode())) {
                scope.put("topicScope", "SPECIFIC");
                scope.put("topicId", request.topicId());
                scope.put("difficulty", "NORMAL");  // 기본값
            } else {
                scope.put("topicScope", "ALL");
                scope.put("difficulty", request.difficulty());
            }
            
            return objectMapper.writeValueAsString(scope);
        } catch (Exception e) {
            log.error("scopeJson 생성 실패: {}", e.getMessage(), e);
            return "{\"examMode\":\"" + request.examMode() + "\"}";
        }
    }

    /**
     * 대기 중인 플레이어
     */
    private record WaitingPlayer(
        String userId,
        MatchingDtos.MatchRequest request,
        Instant joinedAt
    ) {}

    /**
     * 사용자 매칭 정보
     */
    private record MatchingInfo(
        String poolKey,
        String matchingKey,
        MatchMode mode
    ) {}
}

