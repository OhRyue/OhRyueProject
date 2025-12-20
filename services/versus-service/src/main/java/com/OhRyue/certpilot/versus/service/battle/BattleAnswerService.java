package com.OhRyue.certpilot.versus.service.battle;

import com.OhRyue.certpilot.versus.domain.MatchAnswer;
import com.OhRyue.certpilot.versus.domain.MatchQuestion;
import com.OhRyue.certpilot.versus.repository.MatchAnswerRepository;
import com.OhRyue.certpilot.versus.repository.MatchParticipantRepository;
import com.OhRyue.certpilot.versus.repository.MatchEventRepository;
import com.OhRyue.certpilot.versus.service.RealtimeEventService;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 배틀 답안 처리 공통 서비스
 * 
 * 모든 모드에서 공통으로 사용하는 답안 처리 로직을 제공합니다.
 * - 미제출 유저 자동 오답 처리
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class BattleAnswerService {

    private final MatchAnswerRepository answerRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchEventRepository eventRepository;
    private final RealtimeEventService realtimeEventService;
    private final ObjectMapper objectMapper;

    /**
     * 미제출 유저 자동 오답 처리
     * 
     * @param roomId 방 ID
     * @param question 문제 정보
     * @param activeUserIds 활성 사용자 ID 목록 (null이면 모든 참가자)
     */
    @Transactional
    public void processUnansweredUsers(Long roomId, MatchQuestion question, List<String> activeUserIds) {
        // 활성 사용자 목록 결정
        Set<String> targetUserIds;
        if (activeUserIds != null && !activeUserIds.isEmpty()) {
            targetUserIds = new HashSet<>(activeUserIds);
        } else {
            // 모든 참가자
            targetUserIds = participantRepository.findByRoomId(roomId).stream()
                    .map(p -> p.getUserId())
                    .collect(Collectors.toSet());
        }

        // 해당 문제의 답안 제출 여부 확인
        Set<String> answeredUserIds = answerRepository.findByRoomIdAndQuestionId(roomId, question.getQuestionId())
                .stream()
                .map(MatchAnswer::getUserId)
                .collect(Collectors.toSet());

        // 미제출 유저 찾기
        Set<String> unansweredUserIds = targetUserIds.stream()
                .filter(userId -> !answeredUserIds.contains(userId))
                .collect(Collectors.toSet());

        // 미제출 유저에게 자동 오답 저장
        for (String userId : unansweredUserIds) {
            // 이미 답안이 있으면 skip
            if (answerRepository.findByRoomIdAndQuestionIdAndUserId(roomId, question.getQuestionId(), userId).isPresent()) {
                continue;
            }

            // 타임아웃 오답 저장
            MatchAnswer timeoutAnswer = MatchAnswer.builder()
                    .roomId(roomId)
                    .questionId(question.getQuestionId())
                    .userId(userId)
                    .roundNo(question.getRoundNo())
                    .phase(question.getPhase())
                    .correct(false)
                    .timeMs(question.getTimeLimitSec() * 1000) // 타임아웃 시간
                    .scoreDelta(0)
                    .userAnswer("")
                    .build();
            answerRepository.save(timeoutAnswer);

            // ANSWER_TIMEOUT 이벤트 기록
            try {
                Map<String, Object> payload = new HashMap<>();
                payload.put("userId", userId);
                payload.put("questionId", question.getQuestionId());
                payload.put("round", question.getRoundNo());
                payload.put("phase", question.getPhase() != null ? question.getPhase().name() : "MAIN");
                payload.put("timeLimitSec", question.getTimeLimitSec());

                String payloadJson = objectMapper.writeValueAsString(payload);
                com.OhRyue.certpilot.versus.domain.MatchEvent event = com.OhRyue.certpilot.versus.domain.MatchEvent.builder()
                        .roomId(roomId)
                        .eventType("ANSWER_TIMEOUT")
                        .payloadJson(payloadJson)
                        .build();

                com.OhRyue.certpilot.versus.domain.MatchEvent savedEvent = eventRepository.save(event);
                realtimeEventService.broadcastEvent(savedEvent);
            } catch (Exception e) {
                log.warn("Failed to record ANSWER_TIMEOUT event: roomId={}, userId={}, error={}", 
                        roomId, userId, e.getMessage());
            }
        }

        if (!unansweredUserIds.isEmpty()) {
            log.info("ENGINE_UNANSWERED_PROCESSED roomId={} questionId={} unansweredCount={}", 
                    roomId, question.getQuestionId(), unansweredUserIds.size());
        }
    }
}







