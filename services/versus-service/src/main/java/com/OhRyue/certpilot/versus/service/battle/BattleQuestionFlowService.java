package com.OhRyue.certpilot.versus.service.battle;

import com.OhRyue.certpilot.versus.domain.MatchEvent;
import com.OhRyue.certpilot.versus.domain.MatchQuestion;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.repository.MatchEventRepository;
import com.OhRyue.certpilot.versus.repository.MatchQuestionRepository;
import com.OhRyue.certpilot.versus.service.RealtimeEventService;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * 배틀 질문 진행 공통 서비스
 * 
 * 모든 모드에서 공통으로 사용하는 질문 시작 로직을 제공합니다.
 * - QUESTION_STARTED 이벤트 기록 및 브로드캐스트
 * - Redis deadline 저장
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class BattleQuestionFlowService {

    private static final int QUESTION_INTERMISSION_SEC = 5; // 쉬는 시간 5초

    private final MatchQuestionRepository questionRepository;
    private final MatchEventRepository eventRepository;
    private final RealtimeEventService realtimeEventService;
    private final StringRedisTemplate redisTemplate;
    private final ObjectMapper objectMapper;

    /**
     * 다음 문제 시작
     * 
     * @param room 방 정보
     * @param stepKey stepKey (모드별 형식 다름)
     * @param question 문제 정보 (null이면 stepKey로 조회)
     */
    @Transactional
    public void startNextQuestion(MatchRoom room, String stepKey, MatchQuestion question) {
        Long roomId = room.getId();
        
        // stepKey로 문제 조회 (question이 null인 경우)
        if (question == null) {
            question = findQuestionByStepKey(room, stepKey);
        }

        if (question == null) {
            log.warn("Question not found for stepKey: roomId={}, stepKey={}", roomId, stepKey);
            return;
        }

        // 1. 쉬는 시간 시작 이벤트 기록 (선택적)
        Instant intermissionStart = Instant.now();
        recordEvent(roomId, "INTERMISSION_STARTED", Map.of(
                "nextQuestionId", question.getQuestionId(),
                "nextRoundNo", question.getRoundNo(),
                "nextPhase", question.getPhase() != null ? question.getPhase().name() : "MAIN",
                "durationSec", QUESTION_INTERMISSION_SEC,
                "startedAt", intermissionStart.toString(),
                "questionStartAt", intermissionStart.plusSeconds(QUESTION_INTERMISSION_SEC).toString()
        ));

        // 2. 다음 문제 시작 시간 계산
        Instant questionStartTime = intermissionStart.plusSeconds(QUESTION_INTERMISSION_SEC);

        // 3. QUESTION_STARTED 이벤트 기록
        Map<String, Object> payload = new HashMap<>();
        payload.put("questionId", question.getQuestionId());
        payload.put("roundNo", question.getRoundNo());
        payload.put("orderNo", question.getOrderNo());
        payload.put("phase", question.getPhase() != null ? question.getPhase().name() : "MAIN");
        payload.put("stepKey", stepKey);
        payload.put("timeLimitSec", question.getTimeLimitSec());
        payload.put("startedAt", questionStartTime.toString());
        payload.put("allParticipants", true);

        recordEvent(roomId, "QUESTION_STARTED", payload);

        // 4. Redis deadline 저장
        saveDeadline(roomId, stepKey, questionStartTime, question.getTimeLimitSec());

        log.info("ENGINE_STEP_START roomId={} mode={} stepKey={} questionId={} timeLimitSec={}",
                roomId, room.getMode(), stepKey, question.getQuestionId(), question.getTimeLimitSec());
    }

    /**
     * stepKey로 문제 조회
     */
    private MatchQuestion findQuestionByStepKey(MatchRoom room, String stepKey) {
        // 모드별 stepKey 파싱은 Strategy에서 처리하므로, 여기서는 기본 로직만 제공
        // 실제로는 Strategy를 통해 조회해야 함
        return null; // Strategy에서 호출 시 question을 전달하도록 함
    }

    /**
     * Redis deadline 저장
     */
    private void saveDeadline(Long roomId, String stepKey, Instant startTime, int timeLimitSec) {
        try {
            Instant deadline = startTime.plusSeconds(timeLimitSec);
            String deadlineKey = String.format("versus:deadline:%d:%s", roomId, stepKey);
            String deadlineValue = String.valueOf(deadline.toEpochMilli());
            
            // TTL은 timeLimitSec + 10초 여유
            Duration ttl = Duration.ofSeconds(timeLimitSec + 10);
            
            redisTemplate.opsForValue().set(deadlineKey, deadlineValue, ttl);
            log.debug("Deadline saved: key={}, deadline={}, ttl={}s", deadlineKey, deadline, ttl.getSeconds());
        } catch (Exception e) {
            log.warn("Failed to save deadline: roomId={}, stepKey={}, error={}", 
                    roomId, stepKey, e.getMessage());
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





