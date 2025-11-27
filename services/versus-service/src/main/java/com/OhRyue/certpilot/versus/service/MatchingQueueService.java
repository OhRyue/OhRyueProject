package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.dto.MatchingDtos;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

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
    
    // 매칭 풀: mode -> matchingKey -> 대기자 리스트
    private final Map<String, Map<String, Queue<WaitingPlayer>>> matchingPools = new ConcurrentHashMap<>();
    
    // 사용자별 매칭 정보: userId -> MatchingInfo
    private final Map<String, MatchingInfo> userMatchingInfo = new ConcurrentHashMap<>();
    
    // 매칭 풀별 락
    private final Map<String, ReentrantLock> poolLocks = new ConcurrentHashMap<>();
    
    public MatchingQueueService(VersusService versusService, ObjectMapper objectMapper) {
        this.versusService = versusService;
        this.objectMapper = objectMapper;
    }

    /**
     * 매칭 요청
     */
    public MatchingDtos.MatchStatusResp requestMatch(String userId, MatchingDtos.MatchRequest request) {
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
            
            // 매칭 시도
            tryMatch(poolKey, matchingKey, request.mode());
            
            return new MatchingDtos.MatchStatusResp(
                true,
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
        MatchingInfo info = userMatchingInfo.get(userId);
        if (info == null) {
            return new MatchingDtos.MatchStatusResp(false, null, 0, null);
        }
        
        Map<String, Queue<WaitingPlayer>> pool = matchingPools.get(info.poolKey());
        if (pool == null) {
            return new MatchingDtos.MatchStatusResp(false, null, 0, null);
        }
        
        Queue<WaitingPlayer> queue = pool.get(info.matchingKey());
        if (queue == null) {
            return new MatchingDtos.MatchStatusResp(false, null, 0, null);
        }
        
        int waitingCount = queue.size();
        boolean isWaiting = queue.stream().anyMatch(p -> p.userId().equals(userId));
        
        return new MatchingDtos.MatchStatusResp(
            isWaiting,
            null,
            waitingCount,
            null
        );
    }

    /**
     * 매칭 시도 (비동기)
     */
    @Async
    public void tryMatch(String poolKey, String matchingKey, MatchMode mode) {
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
                matched.forEach(queue::offer);
                return;
            }
            
            log.info("매칭 성공: mode={}, players={}", mode, 
                matched.stream().map(WaitingPlayer::userId).collect(Collectors.toList()));
            
            // 방 생성 및 시작
            createAndStartRoom(matched, mode);
            
        } finally {
            lock.unlock();
        }
    }

    /**
     * 방 생성 및 자동 시작
     */
    private void createAndStartRoom(List<WaitingPlayer> players, MatchMode mode) {
        try {
            WaitingPlayer firstPlayer = players.get(0);
            MatchingDtos.MatchRequest request = firstPlayer.request();
            
            // scopeJson 생성
            String scopeJson = buildScopeJson(request);
            
            // participants 리스트 생성 (첫 번째 플레이어 제외)
            List<String> participants = players.stream()
                .skip(1)
                .map(WaitingPlayer::userId)
                .collect(Collectors.toList());
            
            // 방 생성 요청
            VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
                mode,
                scopeJson,
                participants,
                null,  // questions는 자동 생성
                null,  // tournamentBracketJson
                null,  // tournamentBracketRound
                null,  // goldenbellRuleJson
                null   // scheduledAt
            );
            
            // 방 생성 (첫 번째 플레이어가 생성자)
            VersusDtos.RoomDetailResp room = versusService.createRoom(
                createReq, 
                firstPlayer.userId()
            );
            
            Long roomId = room.room().roomId();
            
            // 나머지 플레이어들 참가
            for (int i = 1; i < players.size(); i++) {
                try {
                    versusService.joinRoom(roomId, players.get(i).userId());
                } catch (Exception e) {
                    log.error("플레이어 참가 실패: userId={}, roomId={}, error={}", 
                        players.get(i).userId(), roomId, e.getMessage());
                }
            }
            
            // 자동 시작
            versusService.startRoom(roomId);
            
            log.info("매칭 완료 및 방 시작: roomId={}, mode={}, players={}", 
                roomId, mode, 
                players.stream().map(WaitingPlayer::userId).collect(Collectors.toList()));
            
        } catch (Exception e) {
            log.error("방 생성 및 시작 실패: mode={}, error={}", mode, e.getMessage(), e);
            // 실패 시 플레이어들을 다시 큐에 넣기
            // (선택적: 재시도 로직)
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

