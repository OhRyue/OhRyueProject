package com.OhRyue.certpilot.versus.service.battle.impl;

import com.OhRyue.certpilot.versus.domain.*;
import com.OhRyue.certpilot.versus.repository.*;
import com.OhRyue.certpilot.versus.service.RedisLockService;
import com.OhRyue.certpilot.versus.service.RealtimeEventService;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

/**
 * GOLDENBELL 패자부활 서비스
 * 
 * 라운드 1~2 종료 후 생존자 ≤5명일 때 REVIVAL 문제를 통해
 * "정답 + 가장 빠른 1명"을 선발하여 부활 처리합니다.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class GoldenBellRevivalService {

    private static final String EVENT_REVIVAL_STARTED = "GB_REVIVAL_STARTED";
    private static final String EVENT_REVIVAL_RESULT = "GB_REVIVAL_RESULT";
    private static final int REVIVAL_THRESHOLD = 5;
    private static final long LOCK_TTL_MS = 15000; // 15초

    private final MatchRoomRepository roomRepository;
    private final MatchQuestionRepository questionRepository;
    private final MatchAnswerRepository answerRepository;
    private final MatchEventRepository eventRepository;
    private final GoldenbellStateRepository goldenbellStateRepository;
    private final RedisLockService redisLockService;
    private final RealtimeEventService realtimeEventService;
    private final ObjectMapper objectMapper;

    /**
     * 패자부활 처리
     * 
     * @param roomId 방 ID
     * @param round 라운드 번호 (REVIVAL이 진행될 라운드, 보통 3)
     * @return 부활한 사용자 ID (없으면 null)
     */
    @Transactional
    public String processRevival(Long roomId, int round) {
        String lockKey = String.format("versus:lock:GOLDENBELL:revival:%d:%d", roomId, round);
        String requestId = UUID.randomUUID().toString();
        Duration ttl = Duration.ofMillis(LOCK_TTL_MS);

        // 1. 락 획득 시도
        boolean lockAcquired = redisLockService.tryLock(lockKey, requestId, ttl);
        if (!lockAcquired) {
            log.debug("GB_REVIVAL_LOCK_SKIPPED roomId={} round={}", roomId, round);
            return null;
        }

        try {
            log.info("GB_REVIVAL_STARTED roomId={} round={}", roomId, round);

            // 2. REVIVAL 문제 조회
            List<MatchQuestion> revivalQuestions = questionRepository.findByRoomIdAndRoundNo(roomId, round).stream()
                    .filter(q -> q.getPhase() == MatchPhase.REVIVAL)
                    .collect(Collectors.toList());

            if (revivalQuestions.isEmpty()) {
                log.warn("REVIVAL question not found: roomId={}, round={}", roomId, round);
                return null;
            }

            MatchQuestion revivalQuestion = revivalQuestions.get(0);

            // 3. 탈락자 조회 (alive=false)
            List<GoldenbellState> eliminatedStates = goldenbellStateRepository.findByRoomId(roomId).stream()
                    .filter(state -> !state.isAlive())
                    .collect(Collectors.toList());

            if (eliminatedStates.isEmpty()) {
                log.info("No eliminated participants for revival: roomId={}, round={}", roomId, round);
                return null;
            }

            // 4. REVIVAL 문제의 답안 조회 (탈락자만)
            List<String> eliminatedUserIds = eliminatedStates.stream()
                    .map(GoldenbellState::getUserId)
                    .collect(Collectors.toList());

            List<com.OhRyue.certpilot.versus.domain.MatchAnswer> revivalAnswers = 
                    answerRepository.findByRoomIdAndQuestionId(roomId, revivalQuestion.getQuestionId())
                            .stream()
                            .filter(answer -> eliminatedUserIds.contains(answer.getUserId()))
                            .collect(Collectors.toList());

            // 5. 정답자만 필터링
            List<com.OhRyue.certpilot.versus.domain.MatchAnswer> correctAnswers = revivalAnswers.stream()
                    .filter(com.OhRyue.certpilot.versus.domain.MatchAnswer::isCorrect)
                    .collect(Collectors.toList());

            if (correctAnswers.isEmpty()) {
                log.info("No correct answers in REVIVAL: roomId={}, round={}", roomId, round);
                recordRevivalResult(roomId, round, null);
                return null;
            }

            // 6. 제출시간 가장 빠른 1명 선택
            com.OhRyue.certpilot.versus.domain.MatchAnswer fastestAnswer = correctAnswers.stream()
                    .min(Comparator.comparingInt(com.OhRyue.certpilot.versus.domain.MatchAnswer::getTimeMs))
                    .orElse(null);

            if (fastestAnswer == null) {
                log.warn("Failed to find fastest answer in REVIVAL: roomId={}, round={}", roomId, round);
                return null;
            }

            String revivedUserId = fastestAnswer.getUserId();

            // 7. 부활 처리
            GoldenbellState revivedState = eliminatedStates.stream()
                    .filter(state -> state.getUserId().equals(revivedUserId))
                    .findFirst()
                    .orElse(null);

            if (revivedState != null) {
                revivedState.setAlive(true);
                revivedState.setRevived(true);
                goldenbellStateRepository.save(revivedState);
                log.info("GB_REVIVAL_WINNER roomId={} round={} userId={} timeMs={}", 
                        roomId, round, revivedUserId, fastestAnswer.getTimeMs());
            }

            // 8. 부활 결과 이벤트 기록
            recordRevivalResult(roomId, round, revivedUserId);

            return revivedUserId;

        } catch (Exception e) {
            log.error("GB_REVIVAL_ERROR roomId={} round={} ex={}", 
                    roomId, round, e.getMessage(), e);
            throw e;
        } finally {
            // 9. 락 해제
            redisLockService.unlock(lockKey, requestId);
        }
    }

    /**
     * 부활 결과 이벤트 기록
     */
    private void recordRevivalResult(Long roomId, int round, String revivedUserId) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("round", round);
        payload.put("revivedUserId", revivedUserId != null ? revivedUserId : "NONE");
        payload.put("revivedAt", Instant.now().toString());

        try {
            String payloadJson = objectMapper.writeValueAsString(payload);

            MatchEvent event = MatchEvent.builder()
                    .roomId(roomId)
                    .eventType(EVENT_REVIVAL_RESULT)
                    .payloadJson(payloadJson)
                    .build();

            MatchEvent savedEvent = eventRepository.save(event);
            realtimeEventService.broadcastEvent(savedEvent);
        } catch (Exception e) {
            log.warn("Failed to record revival result: roomId={}, round={}, error={}", 
                    roomId, round, e.getMessage());
        }
    }
}








