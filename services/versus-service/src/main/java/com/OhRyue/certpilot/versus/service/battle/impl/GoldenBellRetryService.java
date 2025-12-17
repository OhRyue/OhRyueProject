package com.OhRyue.certpilot.versus.service.battle.impl;

import com.OhRyue.certpilot.versus.domain.*;
import com.OhRyue.certpilot.versus.repository.*;
import com.OhRyue.certpilot.versus.service.RedisLockService;
import com.OhRyue.certpilot.versus.service.RealtimeEventService;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.util.*;

/**
 * GOLDENBELL 전원 탈락 방지 서비스
 * 
 * 생존자 0명 감지 시 문제 무효 처리 및 재출제를 담당합니다.
 * Redis 락으로 중복 실행 방지.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class GoldenBellRetryService {

    private static final String EVENT_QUESTION_RETRY = "GB_QUESTION_RETRY_ON_FULL_ELIMINATION";
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
     * 전원 탈락 시 문제 무효 처리 및 재출제
     * 
     * @param roomId 방 ID
     * @param stepKey stepKey (예: "1:1:MAIN")
     * @return QuestionFinishResult (재출제된 문제의 stepKey 포함)
     */
    @Transactional
    public GoldenBellQuestionFinishService.QuestionFinishResult retryQuestionOnFullElimination(
            Long roomId, String stepKey) {
        String lockKey = String.format("versus:lock:GOLDENBELL:retry:%d:%s", roomId, stepKey);
        String requestId = UUID.randomUUID().toString();
        Duration ttl = Duration.ofMillis(LOCK_TTL_MS);

        // 1. 락 획득 시도
        boolean lockAcquired = redisLockService.tryLock(lockKey, requestId, ttl);
        if (!lockAcquired) {
            log.debug("GB_RETRY_LOCK_SKIPPED roomId={} stepKey={}", roomId, stepKey);
            return GoldenBellQuestionFinishService.QuestionFinishResult.skipped();
        }

        try {
            log.info("GB_RETRY_LOCK_ACQUIRED roomId={} stepKey={}", roomId, stepKey);

            // 2. stepKey 파싱
            String[] parts = stepKey.split(":");
            if (parts.length != 3) {
                throw new IllegalArgumentException("Invalid stepKey format: " + stepKey);
            }
            int roundNo = Integer.parseInt(parts[0]);
            int orderNo = Integer.parseInt(parts[1]);
            String phaseStr = parts[2];

            // 3. 해당 문제 조회
            MatchQuestion question = questionRepository.findByRoomIdAndRoundNo(roomId, roundNo).stream()
                    .filter(q -> q.getOrderNo().equals(orderNo) && 
                            (q.getPhase() != null && q.getPhase().name().equals(phaseStr)))
                    .findFirst()
                    .orElseThrow(() -> new IllegalStateException("Question not found: " + stepKey));

            // 4. 해당 문제의 모든 답안 삭제
            List<com.OhRyue.certpilot.versus.domain.MatchAnswer> answers = 
                    answerRepository.findByRoomIdAndQuestionId(roomId, question.getQuestionId());
            answerRepository.deleteAll(answers);
            log.info("GB_RETRY_DELETED_ANSWERS roomId={} stepKey={} deletedCount={}", 
                    roomId, stepKey, answers.size());

            // 5. 모든 참가자 alive=true로 복구
            List<GoldenbellState> states = goldenbellStateRepository.findByRoomId(roomId);
            states.forEach(state -> state.setAlive(true));
            goldenbellStateRepository.saveAll(states);
            log.info("GB_RETRY_REVIVED_ALL roomId={} stepKey={} revivedCount={}", 
                    roomId, stepKey, states.size());

            // 6. 문제 무효 처리 (soft delete 또는 invalidate flag)
            // TODO: MatchQuestion에 invalidated 플래그가 있다면 설정
            // 현재는 이벤트로만 기록

            // 7. 재출제 이벤트 기록
            Map<String, Object> retryPayload = new HashMap<>();
            retryPayload.put("stepKey", stepKey);
            retryPayload.put("reason", "FULL_ELIMINATION");
            retryPayload.put("round", roundNo);
            retryPayload.put("order", orderNo);
            retryPayload.put("phase", phaseStr);
            retryPayload.put("retriedAt", Instant.now().toString());

            recordEvent(roomId, EVENT_QUESTION_RETRY, retryPayload);

            // 8. 새 문제 재출제 (같은 round/order/phase로)
            // TODO: 새 문제 생성 로직 (기존 VersusService의 문제 생성 로직 활용)
            // 현재는 stepKey를 그대로 반환하여 재시도 처리

            log.info("GB_RETRY_DONE roomId={} stepKey={} newStepKey={}", roomId, stepKey, stepKey);
            return GoldenBellQuestionFinishService.QuestionFinishResult.completed(stepKey, false);

        } catch (Exception e) {
            log.error("GB_RETRY_ERROR roomId={} stepKey={} ex={}", 
                    roomId, stepKey, e.getMessage(), e);
            throw e;
        } finally {
            // 9. 락 해제
            redisLockService.unlock(lockKey, requestId);
        }
    }

    /**
     * 이벤트 기록 및 실시간 브로드캐스트
     */
    private void recordEvent(Long roomId, String type, Map<String, Object> payload) {
        try {
            String payloadJson = payload == null || payload.isEmpty()
                    ? null
                    : objectMapper.writeValueAsString(payload);

            MatchEvent event = MatchEvent.builder()
                    .roomId(roomId)
                    .eventType(type)
                    .payloadJson(payloadJson)
                    .build();

            MatchEvent savedEvent = eventRepository.save(event);
            realtimeEventService.broadcastEvent(savedEvent);
        } catch (Exception e) {
            log.warn("Failed to record event: roomId={}, type={}, error={}", 
                    roomId, type, e.getMessage());
        }
    }
}




