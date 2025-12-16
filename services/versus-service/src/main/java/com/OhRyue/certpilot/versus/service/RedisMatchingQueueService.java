package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.OhRyue.certpilot.versus.dto.MatchingDtos;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.dto.WebSocketDtos;
import com.OhRyue.certpilot.versus.repository.MatchParticipantRepository;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Redis 기반 매칭 큐 서비스 (리팩터링 버전)
 * 
 * 멀티 인스턴스 환경에서도 동일한 매칭 큐를 공유
 * 
 * 🔥 핵심 설계 원칙:
 * - Redis key는 "큐 식별자" 역할만 수행 (파싱 불필요)
 * - 실제 매칭 요청 정보는 Hash에 JSON으로 저장 (source of truth)
 * 
 * 큐 구조:
 * - List: queue:{mode}:cert={certId}:mode={matchingMode}:{condition}
 *   - Member: userId (큐 식별만을 위한 값)
 *   - 예시: queue:DUEL:cert=1:mode=DIFFICULTY:difficulty=NORMAL
 * 
 * 매칭 요청 저장:
 * - Hash: match:req:{userId}
 *   - payload: JSON 문자열 (MatchRequest의 모든 정보 포함)
 *   - 예시: {"userId":"ohryue","mode":"DUEL","certId":"1","matchingMode":"DIFFICULTY","difficulty":"NORMAL",...}
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RedisMatchingQueueService {

    private static final String QUEUE_PREFIX = "queue:";
    private static final String MATCH_REQ_PREFIX = "match:req:";
    private static final int MATCHING_TIMEOUT_SECONDS = 300; // 5분 타임아웃
    private static final int MAX_CONSECUTIVE_FAILURES = 5; // 연속 실패 시 중단
    
    // 큐별 연속 실패 횟수 추적 (무한 루프 방지)
    private final Map<String, Integer> queueFailureCounts = new ConcurrentHashMap<>();

    private final StringRedisTemplate redisTemplate;
    private final ObjectMapper objectMapper;
    private final MatchParticipantRepository participantRepository;
    private final VersusService versusService;
    private final RedisLockService redisLockService;
    private final SimpMessagingTemplate messagingTemplate;

    /**
     * 매칭 요청
     * 
     * 핵심 변경사항:
     * 1. 큐 key는 식별자 역할만 (파싱 불필요)
     * 2. 실제 요청 정보는 Hash에 JSON으로 저장
     */
    @Transactional
    public MatchingDtos.MatchStatusResp requestMatch(String userId, MatchingDtos.MatchRequest request) {
        // 동일 모드의 활성 방이 이미 있는지 확인
        List<Long> existingRoomIds = participantRepository.findActiveNonBotRoomIdsByUserIdAndMode(
                userId, MatchStatus.ONGOING, request.mode()
        );
        if (!existingRoomIds.isEmpty()) {
            Long roomId = existingRoomIds.get(0);
            log.info("이미 활성화된 방이 있습니다: userId={}, mode={}, roomId={}", 
                    userId, request.mode(), roomId);
            return new MatchingDtos.MatchStatusResp(false, roomId, 0, null);
        }

        // 큐 key 생성 (식별자 역할만)
        String queueKey = buildQueueKey(request);
        String matchReqKey = getMatchReqKey(userId);

        // 기존 매칭 취소
        cancelMatch(userId);

        // 분산락으로 큐 접근 보호
        return redisLockService.executeWithLock(
                Long.parseLong(request.certId()),
                30,
                () -> {
                    // 대기 시작 시간
                    Instant requestedAt = Instant.now();

                    // 1. 큐에 userId 추가 (중복 체크 후)
                    // 중복 방지: 이미 큐에 있는지 확인
                    Long existingCount = redisTemplate.opsForList().size(queueKey);
                    if (existingCount != null && existingCount > 0) {
                        List<String> existingUsers = redisTemplate.opsForList().range(queueKey, 0, -1);
                        if (existingUsers != null && existingUsers.contains(userId)) {
                            log.warn("매칭 요청: userId가 이미 큐에 있습니다 - userId={}, queueKey={}", userId, queueKey);
                            // 이미 있으면 기존 매칭 요청 정보만 업데이트
                        } else {
                            redisTemplate.opsForList().leftPush(queueKey, userId);
                        }
                    } else {
                        redisTemplate.opsForList().leftPush(queueKey, userId);
                    }

                    // 2. 매칭 요청 정보를 Hash에 JSON으로 저장 (source of truth)
                    Map<String, String> payload = buildMatchRequestPayload(userId, request, requestedAt);
                    String payloadJson = serializePayload(payload);
                    redisTemplate.opsForHash().put(matchReqKey, "payload", payloadJson);
                    redisTemplate.expire(matchReqKey, java.time.Duration.ofSeconds(MATCHING_TIMEOUT_SECONDS));
                    redisTemplate.expire(queueKey, java.time.Duration.ofSeconds(MATCHING_TIMEOUT_SECONDS));

                    // 큐 크기 조회
                    Long queueSize = redisTemplate.opsForList().size(queueKey);
                    if (queueSize == null) {
                        queueSize = 0L;
                    }

                    log.info("매칭 요청: userId={}, mode={}, queueKey={}, queueSize={}", 
                            userId, request.mode(), queueKey, queueSize);

                    // 즉시 매칭 시도
                    int requiredCount = request.mode() == MatchMode.DUEL ? 2 : 8;
                    if (queueSize >= requiredCount) {
                        // 매칭 성공: 필요한 인원만큼 추출
                        List<String> matchedUserIds = new ArrayList<>();
                        for (int i = 0; i < requiredCount; i++) {
                            String matchedUserId = redisTemplate.opsForList().rightPop(queueKey);
                            if (matchedUserId != null) {
                                matchedUserIds.add(matchedUserId);
                            }
                        }

                        if (matchedUserIds.size() == requiredCount) {
                            // 중복 플레이어 체크
                            Set<String> uniquePlayers = Set.copyOf(matchedUserIds);
                            if (uniquePlayers.size() < requiredCount) {
                                log.error("즉시 매칭 실패: 중복 플레이어 감지 - mode={}, players={}, uniqueCount={}", 
                                        request.mode(), matchedUserIds, uniquePlayers.size());
                                // 중복된 플레이어들을 다시 큐에 추가하지 않고 매칭 요청 정보만 삭제
                                matchedUserIds.forEach(uid -> redisTemplate.delete(getMatchReqKey(uid)));
                                incrementFailureCount(queueKey);
                                // 중복 감지 시 다음 매칭 시도로 넘어감
                            } else {

                            log.info("즉시 매칭 성공: mode={}, players={}", request.mode(), matchedUserIds);

                            // 방 생성 및 시작
                            try {
                                // payload에서 MatchRequest 재구성 (첫 번째 유저 기준)
                                MatchingDtos.MatchRequest matchRequest = restoreMatchRequestFromPayload(matchedUserIds.get(0));
                                if (matchRequest == null) {
                                    throw new IllegalStateException("매칭 요청 정보를 찾을 수 없습니다: " + matchedUserIds.get(0));
                                }

                                Long roomId = createAndStartRoom(uniquePlayers, matchRequest);
                                if (roomId != null) {
                                    log.info("즉시 매칭 완료: roomId={}, mode={}, players={}", 
                                            roomId, request.mode(), matchedUserIds);
                                    
                                    // 성공 시 매칭 요청 정보 삭제
                                    matchedUserIds.forEach(uid -> redisTemplate.delete(getMatchReqKey(uid)));
                                    
                                    // 실패 카운터 리셋
                                    queueFailureCounts.remove(queueKey);
                                    
                                    // 매칭된 모든 사용자에게 roomId 전달
                                    notifyMatchedUsers(Set.copyOf(matchedUserIds), roomId);
                                    
                                    return new MatchingDtos.MatchStatusResp(false, roomId, 0, requestedAt);
                                }
                            } catch (Exception e) {
                                log.error("방 생성 실패: mode={}, players={}, error={}, errorType={}", 
                                        request.mode(), matchedUserIds, e.getMessage(), e.getClass().getSimpleName(), e);
                                
                                // 실패 원인 분석
                                boolean isStructuralError = isStructuralError(e);
                                
                                if (isStructuralError) {
                                    // 구조적 문제(403, 401 등): 즉시 재시도해도 의미 없음
                                    log.error("방 생성 실패: 구조적 문제로 재시도하지 않음 - mode={}, players={}, error={}", 
                                            request.mode(), matchedUserIds, e.getMessage());
                                    // 매칭 요청 정보 삭제 (재시도하지 않음)
                                    matchedUserIds.forEach(uid -> redisTemplate.delete(getMatchReqKey(uid)));
                                    // 실패 카운터 증가 (임계치 초과 시 해당 큐 스킵)
                                    incrementFailureCount(queueKey);
                                } else {
                                    // 일시적 문제: 다시 큐에 추가 (backoff 적용)
                                    Integer failureCount = queueFailureCounts.get(queueKey);
                                    if (failureCount == null || failureCount < MAX_CONSECUTIVE_FAILURES) {
                                        log.warn("방 생성 실패: 일시적 문제로 재시도 - mode={}, players={}, failureCount={}", 
                                                request.mode(), matchedUserIds, failureCount);
                                        matchedUserIds.forEach(uid -> {
                                            redisTemplate.opsForList().leftPush(queueKey, uid);
                                        });
                                        incrementFailureCount(queueKey);
                                    } else {
                                        // 실패 횟수 초과: 재시도하지 않음
                                        log.error("방 생성 실패: 실패 횟수 초과로 재시도하지 않음 - mode={}, players={}, failureCount={}", 
                                                request.mode(), matchedUserIds, failureCount);
                                        matchedUserIds.forEach(uid -> redisTemplate.delete(getMatchReqKey(uid)));
                                    }
                                }
                            }
                            }
                        } else {
                            // 추출 실패 시 다시 추가
                            matchedUserIds.forEach(uid -> {
                                redisTemplate.opsForList().leftPush(queueKey, uid);
                            });
                        }
                    }

                    // 매칭 대기 중
                    return new MatchingDtos.MatchStatusResp(true, null, queueSize.intValue(), requestedAt);
                }
        );
    }

    /**
     * 매칭 취소
     */
    public void cancelMatch(String userId) {
        String matchReqKey = getMatchReqKey(userId);
        String payloadJson = (String) redisTemplate.opsForHash().get(matchReqKey, "payload");

        if (payloadJson == null || payloadJson.isEmpty()) {
            return; // 매칭 정보 없음
        }

        try {
            // payload에서 queueKey 추출
            Map<String, String> payload = deserializePayload(payloadJson);
            MatchingDtos.MatchRequest request = payloadToMatchRequest(payload);
            if (request == null || request.certId() == null) {
                log.warn("매칭 취소: 유효하지 않은 요청 정보: userId={}", userId);
                redisTemplate.delete(matchReqKey);
                return;
            }
            
            String queueKey = buildQueueKey(request);

            // 분산락으로 큐 접근 보호
            redisLockService.executeWithLock(
                    Long.parseLong(request.certId()),
                    30,
                    () -> {
                        // 큐에서 제거 (모든 발생 위치에서 제거)
                        redisTemplate.opsForList().remove(queueKey, 0, userId);
                        // 매칭 요청 정보 삭제
                        redisTemplate.delete(matchReqKey);
                        log.info("매칭 취소: userId={}, queueKey={}", userId, queueKey);
                        return null;
                    }
            );
        } catch (Exception e) {
            log.warn("매칭 취소 중 오류: userId={}, error={}", userId, e.getMessage());
            // 오류 발생 시에도 최소한 매칭 요청 정보는 삭제
            redisTemplate.delete(matchReqKey);
        }
    }

    /**
     * 매칭 상태 조회
     */
    public MatchingDtos.MatchStatusResp getMatchStatus(String userId) {
        // 활성 방 확인
        List<Long> activeRoomIds = participantRepository.findActiveNonBotRoomIdsByUserIdAndMode(
                userId, MatchStatus.ONGOING, null // 모든 모드
        );
        if (!activeRoomIds.isEmpty()) {
            Long roomId = activeRoomIds.get(0);
            // 매칭 요청 정보 정리
            redisTemplate.delete(getMatchReqKey(userId));
            return new MatchingDtos.MatchStatusResp(false, roomId, 0, null);
        }

        // 매칭 큐 확인
        String matchReqKey = getMatchReqKey(userId);
        String payloadJson = (String) redisTemplate.opsForHash().get(matchReqKey, "payload");

        if (payloadJson == null) {
            return new MatchingDtos.MatchStatusResp(false, null, 0, null);
        }

        try {
            Map<String, String> payload = deserializePayload(payloadJson);
            MatchingDtos.MatchRequest request = payloadToMatchRequest(payload);
            String queueKey = buildQueueKey(request);
            Long queueSize = redisTemplate.opsForList().size(queueKey);
            if (queueSize == null) {
                queueSize = 0L;
            }

            return new MatchingDtos.MatchStatusResp(true, null, queueSize.intValue(), null);
        } catch (Exception e) {
            log.warn("매칭 상태 조회 중 오류: userId={}, error={}", userId, e.getMessage());
            return new MatchingDtos.MatchStatusResp(false, null, 0, null);
        }
    }

    /**
     * 주기적으로 매칭 시도 (비동기 매칭)
     * 
     * 핵심 변경사항:
     * 1. payload에서 MatchRequest 재구성 (key 파싱 제거)
     * 2. 무한 루프 방지 (연속 실패 시 중단)
     */
    @Scheduled(fixedRate = 5000) // 5초마다
    public void tryMatch() {
        // 모든 모드에 대해 매칭 시도
        for (MatchMode mode : MatchMode.values()) {
            if (mode == MatchMode.GOLDENBELL) {
                continue; // 골든벨은 매칭 없음
            }

            try {
                // 큐 패턴으로 찾기
                String pattern = QUEUE_PREFIX + mode.name() + ":*";
                Set<String> queueKeys = redisTemplate.keys(pattern);

                if (queueKeys == null || queueKeys.isEmpty()) {
                    continue;
                }

                for (String queueKey : queueKeys) {
                    // 무한 루프 방지: 연속 실패 횟수 확인
                    Integer failureCount = queueFailureCounts.get(queueKey);
                    if (failureCount != null && failureCount >= MAX_CONSECUTIVE_FAILURES) {
                        log.warn("[versus] Matching aborted after {} failures for queueKey={}", 
                                failureCount, queueKey);
                        continue;
                    }

                    int requiredCount = mode == MatchMode.DUEL ? 2 : 8;
                    Long queueSize = redisTemplate.opsForList().size(queueKey);
                    if (queueSize == null) {
                        queueSize = 0L;
                    }

                    if (queueSize >= requiredCount) {
                        // 분산락으로 매칭 보호
                        final String finalQueueKey = queueKey;
                        redisLockService.executeWithLock(
                                Long.parseLong("0"), // 임시 roomId
                                30,
                                () -> {
                                    // 다시 확인 (락 획득 후)
                                    Long currentQueueSize = redisTemplate.opsForList().size(finalQueueKey);
                                    if (currentQueueSize == null) {
                                        currentQueueSize = 0L;
                                    }

                                    if (currentQueueSize >= requiredCount) {
                                        // 매칭 성공: 필요한 인원만큼 추출
                                        List<String> matchedUserIds = new ArrayList<>();
                                        for (int i = 0; i < requiredCount; i++) {
                                            String matchedUserId = redisTemplate.opsForList().rightPop(finalQueueKey);
                                            if (matchedUserId != null) {
                                                matchedUserIds.add(matchedUserId);
                                            }
                                        }

                                        if (matchedUserIds.size() == requiredCount) {
                                            // 중복 플레이어 체크
                                            Set<String> uniquePlayers = Set.copyOf(matchedUserIds);
                                            if (uniquePlayers.size() < requiredCount) {
                                                log.error("비동기 매칭 실패: 중복 플레이어 감지 - mode={}, players={}, uniqueCount={}", 
                                                        mode, matchedUserIds, uniquePlayers.size());
                                                // 중복된 플레이어들을 다시 큐에 추가하지 않고 매칭 요청 정보만 삭제
                                                matchedUserIds.forEach(uid -> redisTemplate.delete(getMatchReqKey(uid)));
                                                incrementFailureCount(finalQueueKey);
                                                // 중복 감지 시 다음 매칭 시도로 넘어감
                                            } else {

                                            log.info("비동기 매칭 성공: mode={}, players={}", mode, matchedUserIds);

                                            // 방 생성 및 시작
                                            try {
                                                // payload에서 MatchRequest 재구성 (첫 번째 유저 기준)
                                                MatchingDtos.MatchRequest request = restoreMatchRequestFromPayload(matchedUserIds.get(0));
                                                if (request == null) {
                                                    throw new IllegalStateException("매칭 요청 정보를 찾을 수 없습니다: " + matchedUserIds.get(0));
                                                }

                                                Long roomId = createAndStartRoom(uniquePlayers, request);
                                                if (roomId != null) {
                                                    log.info("비동기 매칭 완료: roomId={}, mode={}, players={}", 
                                                            roomId, mode, matchedUserIds);
                                                    
                                                    // 성공 시 매칭 요청 정보 삭제
                                                    matchedUserIds.forEach(uid -> redisTemplate.delete(getMatchReqKey(uid)));
                                                    
                                                    // 실패 카운터 리셋
                                                    queueFailureCounts.remove(finalQueueKey);
                                                    
                                                    // 매칭된 모든 사용자에게 roomId 전달
                                                    notifyMatchedUsers(Set.copyOf(matchedUserIds), roomId);
                                                }
                                            } catch (Exception e) {
                                                log.error("방 생성 실패: mode={}, players={}, error={}, errorType={}", 
                                                        mode, matchedUserIds, e.getMessage(), e.getClass().getSimpleName(), e);
                                                
                                                // 실패 원인 분석
                                                boolean isStructuralError = isStructuralError(e);
                                                
                                                if (isStructuralError) {
                                                    // 구조적 문제: 즉시 재시도해도 의미 없음
                                                    log.error("방 생성 실패: 구조적 문제로 재시도하지 않음 - mode={}, players={}, error={}", 
                                                            mode, matchedUserIds, e.getMessage());
                                                    matchedUserIds.forEach(uid -> redisTemplate.delete(getMatchReqKey(uid)));
                                                    incrementFailureCount(finalQueueKey);
                                                } else {
                                                    // 일시적 문제: 다시 큐에 추가
                                                    Integer currentFailureCount = queueFailureCounts.get(finalQueueKey);
                                                    if (currentFailureCount == null || currentFailureCount < MAX_CONSECUTIVE_FAILURES) {
                                                        log.warn("방 생성 실패: 일시적 문제로 재시도 - mode={}, players={}, failureCount={}", 
                                                                mode, matchedUserIds, currentFailureCount);
                                                        matchedUserIds.forEach(uid -> {
                                                            redisTemplate.opsForList().leftPush(finalQueueKey, uid);
                                                        });
                                                        incrementFailureCount(finalQueueKey);
                                                    } else {
                                                        // 실패 횟수 초과: 재시도하지 않음
                                                        log.error("방 생성 실패: 실패 횟수 초과로 재시도하지 않음 - mode={}, players={}, failureCount={}", 
                                                                mode, matchedUserIds, currentFailureCount);
                                                        matchedUserIds.forEach(uid -> redisTemplate.delete(getMatchReqKey(uid)));
                                                    }
                                                }
                                            }
                                            }
                                        } else {
                                            // 추출 실패 시 다시 추가
                                            matchedUserIds.forEach(uid -> {
                                                redisTemplate.opsForList().leftPush(finalQueueKey, uid);
                                            });
                                            incrementFailureCount(finalQueueKey);
                                        }
                                    }
                                    return null;
                                }
                        );
                    }
                }
            } catch (Exception e) {
                log.error("매칭 시도 중 오류: mode={}, error={}", mode, e.getMessage(), e);
            }
        }
    }

    /**
     * 방 생성 및 시작
     */
    private Long createAndStartRoom(Set<String> playerIds, MatchingDtos.MatchRequest request) {
        List<String> players = new ArrayList<>(playerIds);
        String creatorId = players.get(0);
        List<String> participants = players.stream().skip(1).collect(Collectors.toList());

        String scopeJson = buildScopeJson(request);

        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
                request.mode(),
                scopeJson,
                participants,
                null,
                null,
                null,
                null,
                null,
                false
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, creatorId);
        Long roomId = room.room().roomId();

        // 자동 시작
        versusService.startRoom(roomId);

        return roomId;
    }

    /**
     * 큐 key 생성 (식별자 역할만, 파싱 불필요)
     * 
     * 형식: queue:{mode}:cert={certId}:mode={matchingMode}:{condition}
     * 예시:
     * - queue:DUEL:cert=1:mode=DIFFICULTY:difficulty=NORMAL
     * - queue:DUEL:cert=1:mode=CATEGORY:rootTopicId=31101
     */
    private String buildQueueKey(MatchingDtos.MatchRequest request) {
        if ("CATEGORY".equals(request.matchingMode())) {
            // 카테고리 모드: rootTopicId 사용
            return String.format("%s%s:cert=%s:mode=%s:rootTopicId=%d",
                    QUEUE_PREFIX, request.mode().name(), request.certId(), 
                    request.matchingMode(), request.topicId());
        } else {
            // 난이도 모드: difficulty 사용
            return String.format("%s%s:cert=%s:mode=%s:difficulty=%s",
                    QUEUE_PREFIX, request.mode().name(), request.certId(), 
                    request.matchingMode(), request.difficulty());
        }
    }

    /**
     * scopeJson 생성
     * 
     * 모드별 필드 규칙:
     * - DIFFICULTY: difficulty만 사용, topicId 무시
     * - CATEGORY: rootTopicId만 사용, difficulty 무시
     */
    private String buildScopeJson(MatchingDtos.MatchRequest request) {
        try {
            if ("CATEGORY".equals(request.matchingMode())) {
                // 카테고리 모드: topicId만 사용
                return objectMapper.writeValueAsString(Map.of(
                        "certId", request.certId(),
                        "topicId", request.topicId(),
                        "examMode", request.examMode()
                ));
            } else {
                // 난이도 모드: difficulty만 사용
                return objectMapper.writeValueAsString(Map.of(
                        "certId", request.certId(),
                        "difficulty", request.difficulty(),
                        "examMode", request.examMode()
                ));
            }
        } catch (Exception e) {
            log.error("scopeJson 생성 실패: error={}", e.getMessage());
            return "{}";
        }
    }

    /**
     * 매칭 요청 키 생성
     */
    private String getMatchReqKey(String userId) {
        return MATCH_REQ_PREFIX + userId;
    }

    /**
     * 매칭 요청 payload 생성
     */
    private Map<String, String> buildMatchRequestPayload(String userId, MatchingDtos.MatchRequest request, Instant requestedAt) {
        Map<String, String> payload = new HashMap<>();
        payload.put("userId", userId);
        payload.put("mode", request.mode().name());
        payload.put("certId", request.certId());
        payload.put("matchingMode", request.matchingMode());
        payload.put("examMode", request.examMode());
        
        // 모드별 필드 규칙 적용
        if ("CATEGORY".equals(request.matchingMode())) {
            payload.put("rootTopicId", request.topicId() != null ? String.valueOf(request.topicId()) : null);
            payload.put("difficulty", null);
        } else {
            payload.put("difficulty", request.difficulty());
            payload.put("rootTopicId", null);
        }
        
        payload.put("requestedAt", requestedAt.toString());
        return payload;
    }

    /**
     * payload를 JSON 문자열로 직렬화
     */
    private String serializePayload(Map<String, String> payload) {
        try {
            return objectMapper.writeValueAsString(payload);
        } catch (Exception e) {
            log.error("payload 직렬화 실패: error={}", e.getMessage());
            throw new RuntimeException("payload 직렬화 실패", e);
        }
    }

    /**
     * JSON 문자열을 payload로 역직렬화
     */
    private Map<String, String> deserializePayload(String payloadJson) {
        try {
            return objectMapper.readValue(payloadJson, new TypeReference<Map<String, String>>() {});
        } catch (Exception e) {
            log.error("payload 역직렬화 실패: payloadJson={}, error={}", payloadJson, e.getMessage());
            throw new RuntimeException("payload 역직렬화 실패", e);
        }
    }

    /**
     * payload에서 MatchRequest 재구성
     * 
     * 핵심: payload가 source of truth이므로 여기서만 MatchRequest를 생성
     */
    private MatchingDtos.MatchRequest payloadToMatchRequest(Map<String, String> payload) {
        if (payload == null) {
            return null;
        }
        
        String modeStr = payload.get("mode");
        String certId = payload.get("certId");
        String matchingMode = payload.get("matchingMode");
        String examMode = payload.get("examMode");
        
        if (modeStr == null || certId == null || matchingMode == null || examMode == null) {
            log.warn("payload에 필수 필드가 없습니다: payload={}", payload);
            return null;
        }
        
        MatchMode mode;
        try {
            mode = MatchMode.valueOf(modeStr);
        } catch (IllegalArgumentException e) {
            log.warn("유효하지 않은 MatchMode: modeStr={}", modeStr);
            return null;
        }
        
        // 모드별 필드 규칙 적용
        Long topicId = null;
        String difficulty = null;
        
        if ("CATEGORY".equals(matchingMode)) {
            String rootTopicIdStr = payload.get("rootTopicId");
            if (rootTopicIdStr != null && !rootTopicIdStr.isEmpty() && !"null".equals(rootTopicIdStr)) {
                try {
                    topicId = Long.parseLong(rootTopicIdStr);
                } catch (NumberFormatException e) {
                    log.warn("유효하지 않은 rootTopicId: rootTopicIdStr={}", rootTopicIdStr);
                }
            }
            // difficulty는 무시
        } else {
            difficulty = payload.get("difficulty");
            // topicId는 무시
        }
        
        return new MatchingDtos.MatchRequest(
                mode,
                certId,
                matchingMode,
                topicId,
                difficulty,
                examMode
        );
    }

    /**
     * userId로부터 payload를 가져와 MatchRequest 재구성
     */
    private MatchingDtos.MatchRequest restoreMatchRequestFromPayload(String userId) {
        String matchReqKey = getMatchReqKey(userId);
        String payloadJson = (String) redisTemplate.opsForHash().get(matchReqKey, "payload");
        
        if (payloadJson == null) {
            log.warn("매칭 요청 정보를 찾을 수 없습니다: userId={}", userId);
            return null;
        }
        
        try {
            Map<String, String> payload = deserializePayload(payloadJson);
            return payloadToMatchRequest(payload);
        } catch (Exception e) {
            log.error("MatchRequest 재구성 실패: userId={}, error={}", userId, e.getMessage());
            return null;
        }
    }

    /**
     * 큐별 실패 카운터 증가
     */
    private void incrementFailureCount(String queueKey) {
        queueFailureCounts.merge(queueKey, 1, Integer::sum);
        log.warn("[versus] Matching failure count for queueKey={}: {}", 
                queueKey, queueFailureCounts.get(queueKey));
    }

    /**
     * 예외가 구조적 문제인지 판단
     * 
     * 구조적 문제: 즉시 재시도해도 해결되지 않는 문제
     * - 403 Forbidden (권한 문제)
     * - 401 Unauthorized (인증 문제)
     * - IllegalArgumentException (잘못된 파라미터)
     * 
     * 일시적 문제: 재시도하면 해결될 수 있는 문제
     * - IOException
     * - TimeoutException
     * - 기타 네트워크/일시적 오류
     */
    private boolean isStructuralError(Exception e) {
        String errorMessage = e.getMessage() != null ? e.getMessage().toLowerCase() : "";
        String exceptionType = e.getClass().getSimpleName();
        
        // HTTP 상태 코드 체크
        if (errorMessage.contains("403") || errorMessage.contains("forbidden")) {
            return true;
        }
        if (errorMessage.contains("401") || errorMessage.contains("unauthorized")) {
            return true;
        }
        
        // 예외 타입 체크
        if (e instanceof IllegalArgumentException) {
            return true;
        }
        if (e instanceof IllegalStateException) {
            return true;
        }
        
        // 기타 구조적 문제는 일시적 문제로 간주
        return false;
    }

    /**
     * 매칭된 사용자들에게 roomId 전달
     */
    private void notifyMatchedUsers(Set<String> userIds, Long roomId) {
        for (String userId : userIds) {
            try {
                messagingTemplate.convertAndSendToUser(
                        userId,
                        "/queue/versus/match",
                        WebSocketDtos.MatchResponse.success(false, roomId, null)
                );
                log.debug("매칭 성공 알림 전송: userId={}, roomId={}", userId, roomId);
            } catch (Exception e) {
                log.warn("매칭 성공 알림 전송 실패: userId={}, roomId={}, error={}", 
                        userId, roomId, e.getMessage());
            }
        }
    }
}

