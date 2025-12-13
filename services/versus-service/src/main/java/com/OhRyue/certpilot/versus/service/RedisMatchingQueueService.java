package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.OhRyue.certpilot.versus.dto.MatchingDtos;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.dto.WebSocketDtos;
import com.OhRyue.certpilot.versus.repository.MatchParticipantRepository;
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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Redis 기반 매칭 큐 서비스
 * 
 * 멀티 인스턴스 환경에서도 동일한 매칭 큐를 공유
 * 
 * 큐 구조:
 * - ZSet: matching:queue:{mode}:{matchingKey}
 *   - Member: userId
 *   - Score: 대기 시작 시간 (밀리초, Instant.toEpochMilli())
 * 
 * 사용자 매칭 정보:
 * - Hash: matching:user:{userId}
 *   - poolKey: mode
 *   - matchingKey: mode:certId:matchingMode:topicId/difficulty:examMode
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RedisMatchingQueueService {

    private static final String QUEUE_PREFIX = "matching:queue:";
    private static final String USER_INFO_PREFIX = "matching:user:";
    private static final int MATCHING_TIMEOUT_SECONDS = 300; // 5분 타임아웃

    private final StringRedisTemplate redisTemplate;
    private final ObjectMapper objectMapper;
    private final MatchParticipantRepository participantRepository;
    private final VersusService versusService;
    private final RedisLockService redisLockService;
    private final SimpMessagingTemplate messagingTemplate;

    /**
     * 매칭 요청
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

        String matchingKey = buildMatchingKey(request);
        String queueKey = getQueueKey(request.mode(), matchingKey);
        String userInfoKey = getUserInfoKey(userId);

        // 기존 매칭 취소
        cancelMatch(userId);

        // 분산락으로 큐 접근 보호
        return redisLockService.executeWithLock(
                Long.parseLong(request.certId()), // certId를 roomId 대신 사용
                30,
                () -> {
                    // 대기 시작 시간
                    long score = Instant.now().toEpochMilli();

                    // ZSet에 추가
                    redisTemplate.opsForZSet().add(queueKey, userId, score);

                    // 사용자 매칭 정보 저장
                    redisTemplate.opsForHash().put(userInfoKey, "poolKey", request.mode().name());
                    redisTemplate.opsForHash().put(userInfoKey, "matchingKey", matchingKey);
                    redisTemplate.expire(userInfoKey, java.time.Duration.ofSeconds(MATCHING_TIMEOUT_SECONDS));

                    // 큐 크기 조회
                    Long queueSize = redisTemplate.opsForZSet().count(queueKey, Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY);

                    log.info("매칭 요청: userId={}, mode={}, matchingKey={}, queueSize={}", 
                            userId, request.mode(), matchingKey, queueSize);

                    // 즉시 매칭 시도
                    int requiredCount = request.mode() == MatchMode.DUEL ? 2 : 8;
                    if (queueSize >= requiredCount) {
                        // 매칭 성공: 필요한 인원만큼 추출
                        Set<String> matchedUserIds = redisTemplate.opsForZSet().range(queueKey, 0, requiredCount - 1);
                        if (matchedUserIds != null && matchedUserIds.size() == requiredCount) {
                            // 큐에서 제거
                            redisTemplate.opsForZSet().remove(queueKey, matchedUserIds.toArray());
                            matchedUserIds.forEach(uid -> redisTemplate.delete(getUserInfoKey(uid)));

                            log.info("즉시 매칭 성공: mode={}, players={}", request.mode(), matchedUserIds);

                            // 방 생성 및 시작
                            try {
                                Long roomId = createAndStartRoom(matchedUserIds, request);
                                if (roomId != null) {
                                    log.info("즉시 매칭 완료: roomId={}, mode={}, players={}", 
                                            roomId, request.mode(), matchedUserIds);
                                    
                                    // 매칭된 모든 사용자에게 roomId 전달
                                    notifyMatchedUsers(matchedUserIds, roomId);
                                    
                                    return new MatchingDtos.MatchStatusResp(false, roomId, 0, Instant.ofEpochMilli(score));
                                }
                            } catch (Exception e) {
                                log.error("방 생성 실패, 플레이어들을 다시 큐에 추가: mode={}, players={}, error={}", 
                                        request.mode(), matchedUserIds, e.getMessage(), e);
                                // 실패 시 다시 큐에 추가
                                matchedUserIds.forEach(uid -> {
                                    redisTemplate.opsForZSet().add(queueKey, uid, Instant.now().toEpochMilli());
                                    redisTemplate.opsForHash().put(getUserInfoKey(uid), "poolKey", request.mode().name());
                                    redisTemplate.opsForHash().put(getUserInfoKey(uid), "matchingKey", matchingKey);
                                });
                            }
                        }
                    }

                    // 매칭 대기 중
                    return new MatchingDtos.MatchStatusResp(true, null, queueSize.intValue(), Instant.ofEpochMilli(score));
                }
        );
    }

    /**
     * 매칭 취소
     */
    public void cancelMatch(String userId) {
        String userInfoKey = getUserInfoKey(userId);
        String poolKey = (String) redisTemplate.opsForHash().get(userInfoKey, "poolKey");
        String matchingKey = (String) redisTemplate.opsForHash().get(userInfoKey, "matchingKey");

        if (poolKey == null || matchingKey == null) {
            return; // 매칭 정보 없음
        }

        MatchMode mode = MatchMode.valueOf(poolKey);
        String queueKey = getQueueKey(mode, matchingKey);

        // 분산락으로 큐 접근 보호
        redisLockService.executeWithLock(
                Long.parseLong("0"), // 임시 roomId
                30,
                () -> {
                    // 큐에서 제거
                    redisTemplate.opsForZSet().remove(queueKey, userId);
                    // 사용자 정보 삭제
                    redisTemplate.delete(userInfoKey);
                    log.info("매칭 취소: userId={}, mode={}", userId, mode);
                    return null;
                }
        );
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
            // 사용자 정보 정리
            redisTemplate.delete(getUserInfoKey(userId));
            return new MatchingDtos.MatchStatusResp(false, roomId, 0, null);
        }

        // 매칭 큐 확인
        String userInfoKey = getUserInfoKey(userId);
        String poolKey = (String) redisTemplate.opsForHash().get(userInfoKey, "poolKey");
        String matchingKey = (String) redisTemplate.opsForHash().get(userInfoKey, "matchingKey");

        if (poolKey == null || matchingKey == null) {
            return new MatchingDtos.MatchStatusResp(false, null, 0, null);
        }

        MatchMode mode = MatchMode.valueOf(poolKey);
        String queueKey = getQueueKey(mode, matchingKey);
        Long queueSize = redisTemplate.opsForZSet().count(queueKey, Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY);

        return new MatchingDtos.MatchStatusResp(true, null, queueSize.intValue(), null);
    }

    /**
     * 주기적으로 매칭 시도 (비동기 매칭)
     */
    @Scheduled(fixedRate = 5000) // 5초마다
    public void tryMatch() {
        // 모든 모드에 대해 매칭 시도
        for (MatchMode mode : MatchMode.values()) {
            if (mode == MatchMode.GOLDENBELL) {
                continue; // 골든벨은 매칭 없음
            }

            try {
                // 매칭 키 패턴으로 큐 찾기
                String pattern = QUEUE_PREFIX + mode.name() + ":*";
                Set<String> queueKeys = redisTemplate.keys(pattern);

                if (queueKeys == null || queueKeys.isEmpty()) {
                    continue;
                }

                for (String queueKey : queueKeys) {
                    int requiredCount = mode == MatchMode.DUEL ? 2 : 8;
                    Long queueSize = redisTemplate.opsForZSet().count(queueKey, Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY);

                    if (queueSize >= requiredCount) {
                        // 분산락으로 매칭 보호
                        String matchingKey = queueKey.substring(QUEUE_PREFIX.length() + mode.name().length() + 1);
                        final String finalQueueKey = queueKey; // final 변수로 복사
                        redisLockService.executeWithLock(
                                Long.parseLong("0"), // 임시 roomId
                                30,
                                () -> {
                                    // 다시 확인 (락 획득 후)
                                    Long currentQueueSize = redisTemplate.opsForZSet().count(finalQueueKey, Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY);
                                    if (currentQueueSize != null && currentQueueSize >= requiredCount) {
                                        // 매칭 성공
                                        Set<String> matchedUserIds = redisTemplate.opsForZSet().range(finalQueueKey, 0, requiredCount - 1);
                                        if (matchedUserIds != null && matchedUserIds.size() == requiredCount) {
                                            // 큐에서 제거
                                            redisTemplate.opsForZSet().remove(finalQueueKey, matchedUserIds.toArray());
                                            matchedUserIds.forEach(uid -> redisTemplate.delete(getUserInfoKey(uid)));

                                            log.info("비동기 매칭 성공: mode={}, players={}", mode, matchedUserIds);

                                            // 방 생성 및 시작 (비동기)
                                            try {
                                                MatchingDtos.MatchRequest request = buildMatchRequestFromKey(mode, matchingKey);
                                                Long roomId = createAndStartRoom(matchedUserIds, request);
                                                if (roomId != null) {
                                                    log.info("비동기 매칭 완료: roomId={}, mode={}, players={}", 
                                                            roomId, mode, matchedUserIds);
                                                    
                                                    // 매칭된 모든 사용자에게 roomId 전달
                                                    notifyMatchedUsers(matchedUserIds, roomId);
                                                }
                                            } catch (Exception e) {
                                                log.error("방 생성 실패: mode={}, players={}, error={}", 
                                                        mode, matchedUserIds, e.getMessage(), e);
                                                // 실패 시 다시 큐에 추가
                                                matchedUserIds.forEach(uid -> {
                                                    redisTemplate.opsForZSet().add(finalQueueKey, uid, Instant.now().toEpochMilli());
                                                });
                                            }
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
     * 매칭 키 생성
     */
    private String buildMatchingKey(MatchingDtos.MatchRequest request) {
        if ("CATEGORY".equals(request.matchingMode())) {
            return String.format("%s:%s:%s:%d:%s", 
                    request.mode().name(), request.certId(), request.matchingMode(), 
                    request.topicId(), request.examMode());
        } else {
            return String.format("%s:%s:%s:%s:%s", 
                    request.mode().name(), request.certId(), request.matchingMode(), 
                    request.difficulty(), request.examMode());
        }
    }

    /**
     * 매칭 키에서 MatchRequest 복원
     */
    private MatchingDtos.MatchRequest buildMatchRequestFromKey(MatchMode mode, String matchingKey) {
        String[] parts = matchingKey.split(":");
        // 간단한 구현 (실제로는 더 정교하게 처리 필요)
        return new MatchingDtos.MatchRequest(
                mode,
                parts[1], // certId
                parts[2], // matchingMode
                parts.length > 3 && !parts[3].isEmpty() ? Long.parseLong(parts[3]) : null, // topicId
                parts.length > 3 && parts[2].equals("DIFFICULTY") ? parts[3] : null, // difficulty
                parts[parts.length - 1] // examMode
        );
    }

    /**
     * scopeJson 생성
     */
    private String buildScopeJson(MatchingDtos.MatchRequest request) {
        try {
            if ("CATEGORY".equals(request.matchingMode())) {
                return objectMapper.writeValueAsString(Map.of(
                        "certId", request.certId(),
                        "topicId", request.topicId(),
                        "examMode", request.examMode()
                ));
            } else {
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
     * 큐 키 생성
     */
    private String getQueueKey(MatchMode mode, String matchingKey) {
        return QUEUE_PREFIX + mode.name() + ":" + matchingKey;
    }

    /**
     * 사용자 정보 키 생성
     */
    private String getUserInfoKey(String userId) {
        return USER_INFO_PREFIX + userId;
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

