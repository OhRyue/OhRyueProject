package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.*;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.dto.WebSocketDtos;
import com.OhRyue.certpilot.versus.repository.*;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;

import java.time.Instant;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * 답안 제출 서비스
 * 
 * HTTP와 WebSocket 양쪽에서 호출 가능한 답안 제출 로직
 * - 중복 제출 방어
 * - 타임아웃 이후 제출 방어
 * - 이미 종료된 문제 제출 방어
 * - 제출 성공 시 실시간 이벤트 발행
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class AnswerSubmissionService {

    private final MatchRoomRepository roomRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchQuestionRepository questionRepository;
    private final MatchAnswerRepository answerRepository;
    private final MatchEventRepository eventRepository;
    private final GoldenbellStateRepository goldenbellStateRepository;
    private final VersusService versusService;
    private final RealtimeEventService realtimeEventService;
    private final ObjectMapper objectMapper;

    /**
     * 답안 제출 처리 (WebSocket Command 사용)
     * 
     * @param roomId 방 ID
     * @param userId 사용자 ID
     * @param command SUBMIT_ANSWER 명령
     * @return 업데이트된 스코어보드
     */
    @Transactional
    public VersusDtos.ScoreBoardResp submitAnswer(
            Long roomId,
            String userId,
            WebSocketDtos.SubmitAnswerCommand command) {
        
        // SubmitAnswerCommand를 SubmitAnswerReq로 변환
        VersusDtos.SubmitAnswerReq req = new VersusDtos.SubmitAnswerReq(
                command.questionId(),
                command.userAnswer(),
                command.correct() != null ? command.correct() : false,
                command.timeMs(),
                null, // scoreDelta는 서버에서 계산
                command.roundNo(),
                command.phase() != null ? MatchPhase.valueOf(command.phase()) : null
        );

        return submitAnswer(roomId, userId, req);
    }

    /**
     * 답안 제출 처리 (기존 SubmitAnswerReq 사용)
     * 
     * @param roomId 방 ID
     * @param userId 사용자 ID
     * @param req 답안 제출 요청
     * @return 업데이트된 스코어보드
     */
    @Transactional
    public VersusDtos.ScoreBoardResp submitAnswer(
            Long roomId,
            String userId,
            VersusDtos.SubmitAnswerReq req) {

        // 1. 방 및 참가자 검증
        MatchRoom room = roomRepository.findById(roomId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Room not found: " + roomId));

        MatchParticipant participant = participantRepository.findByRoomIdAndUserId(roomId, userId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.BAD_REQUEST, "Participant not joined"));

        // 2. 문제 검증
        MatchQuestion question = questionRepository.findByRoomIdAndQuestionId(roomId, req.questionId())
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.BAD_REQUEST, "Question not found: " + req.questionId()));

        // 3. 방어 로직: 탈락한 참가자 검증
        validateParticipantEligibility(room, participant, question);

        // 4. 방어 로직: 중복 제출 검증
        validateNoDuplicateSubmission(roomId, req.questionId(), userId, question);

        // 5. 방어 로직: 문제 종료 여부 검증
        validateQuestionNotExpired(roomId, question);

        // 6. 방어 로직: 방 상태 검증
        validateRoomStatus(room);

        // 7. 기존 VersusService의 submitAnswer 로직 호출
        // (기존 로직을 그대로 재사용)
        VersusDtos.ScoreBoardResp scoreboard = versusService.submitAnswer(roomId, userId, req);

        // 8. 실시간 이벤트 발행
        // ANSWER_SUBMITTED는 VersusService.submitAnswer 내부에서 이미 발행됨
        // SCOREBOARD_UPDATED 이벤트 추가 발행
        publishScoreboardUpdatedEvent(roomId, scoreboard);

        return scoreboard;
    }

    /**
     * 참가자 자격 검증
     */
    private void validateParticipantEligibility(MatchRoom room, MatchParticipant participant, MatchQuestion question) {
        if (room.getMode() == MatchMode.TOURNAMENT && participant.isEliminated()) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Eliminated participant cannot submit answers");
        }

        if (room.getMode() == MatchMode.GOLDENBELL) {
            GoldenbellState state = goldenbellStateRepository.findByRoomIdAndUserId(room.getId(), participant.getUserId())
                    .orElse(null);
            if (state != null && !state.isAlive()) {
                // REVIVAL phase 문제에서는 부활한 사용자(revived=true)도 참여 가능
                if (question.getPhase() != MatchPhase.REVIVAL || !state.isRevived()) {
                    throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Eliminated participant cannot submit answers");
                }
            }
        }
    }

    /**
     * 중복 제출 검증
     */
    private void validateNoDuplicateSubmission(Long roomId, Long questionId, String userId, MatchQuestion question) {
        Optional<MatchAnswer> existingAnswer = answerRepository.findByRoomIdAndQuestionIdAndUserId(roomId, questionId, userId);
        
        if (existingAnswer.isPresent()) {
            MatchAnswer answer = existingAnswer.get();
            
            // REVIVAL phase에서는 중복 제출 허용 (부활 기회)
            if (question.getPhase() == MatchPhase.REVIVAL) {
                log.debug("REVIVAL phase allows duplicate submission: roomId={}, questionId={}, userId={}",
                        roomId, questionId, userId);
                return;
            }
            
            // 이미 제출한 답안이 있으면 중복 제출 오류
            log.warn("Duplicate answer submission detected: roomId={}, questionId={}, userId={}, existingAnswerId={}",
                    roomId, questionId, userId, answer.getId());
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, 
                    "Answer already submitted for this question");
        }
    }

    /**
     * 문제 종료 여부 검증 (타임아웃 이후 제출 방어)
     */
    private void validateQuestionNotExpired(Long roomId, MatchQuestion question) {
        if (roomId == null || question == null) {
            return;
        }

        try {
            // 가장 최근 QUESTION_STARTED 이벤트 찾기
            List<MatchEvent> startEvents = eventRepository.findByRoomIdAndEventTypeContaining(
                    roomId, "QUESTION_STARTED");

            Optional<MatchEvent> currentQuestionEvent = startEvents.stream()
                    .filter(e -> {
                        try {
                            if (e.getPayloadJson() == null) return false;
                            Map<String, Object> payload = objectMapper.readValue(
                                    e.getPayloadJson(), new TypeReference<Map<String, Object>>() {});
                            Object qId = payload.get("questionId");
                            return qId != null && question.getQuestionId().equals(Long.valueOf(qId.toString()));
                        } catch (Exception ex) {
                            return false;
                        }
                    })
                    .max(Comparator.comparing(MatchEvent::getCreatedAt));

            if (currentQuestionEvent.isPresent()) {
                try {
                    Map<String, Object> payload = objectMapper.readValue(
                            currentQuestionEvent.get().getPayloadJson(), new TypeReference<Map<String, Object>>() {});
                    String startedAtStr = (String) payload.get("startedAt");
                    Instant startTime = startedAtStr != null
                            ? Instant.parse(startedAtStr)
                            : currentQuestionEvent.get().getCreatedAt();
                    Instant endTime = startTime.plusSeconds(question.getTimeLimitSec());
                    Instant now = Instant.now();

                    if (now.isAfter(endTime)) {
                        log.warn("Question expired: roomId={}, questionId={}, endTime={}, now={}",
                                roomId, question.getQuestionId(), endTime, now);
                        throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
                                "Question time limit has expired");
                    }
                } catch (ResponseStatusException e) {
                    throw e; // 재전파
                } catch (Exception e) {
                    log.debug("Failed to check question expiration: {}", e.getMessage());
                    // 검증 실패 시 경고만 남기고 계속 진행 (하위 호환성)
                }
            }
        } catch (ResponseStatusException e) {
            throw e; // 재전파
        } catch (Exception e) {
            log.debug("Failed to validate question expiration: {}", e.getMessage());
            // 검증 실패 시 경고만 남기고 계속 진행 (하위 호환성)
        }
    }

    /**
     * 방 상태 검증
     */
    private void validateRoomStatus(MatchRoom room) {
        if (room.getStatus() == MatchStatus.DONE) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Match has already finished");
        }
    }

    /**
     * SCOREBOARD_UPDATED 이벤트 발행
     */
    private void publishScoreboardUpdatedEvent(Long roomId, VersusDtos.ScoreBoardResp scoreboard) {
        try {
            Map<String, Object> payload = Map.of(
                    "roomId", roomId,
                    "status", scoreboard.status().name(),
                    "itemCount", scoreboard.items().size(),
                    "updatedAt", Instant.now().toString()
            );

            // MatchEvent를 직접 저장하고 브로드캐스트
            String payloadJson = objectMapper.writeValueAsString(payload);
            MatchEvent event = MatchEvent.builder()
                    .roomId(roomId)
                    .eventType("SCOREBOARD_UPDATED")
                    .payloadJson(payloadJson)
                    .build();
            MatchEvent savedEvent = eventRepository.save(event);

            // RealtimeEventService를 통해 브로드캐스트
            realtimeEventService.broadcastEvent(savedEvent);
            
            log.debug("SCOREBOARD_UPDATED event published: roomId={}, itemCount={}, eventId={}",
                    roomId, scoreboard.items().size(), savedEvent.getId());
        } catch (Exception e) {
            log.warn("Failed to publish SCOREBOARD_UPDATED event: roomId={}, error={}",
                    roomId, e.getMessage());
            // 이벤트 발행 실패는 로그만 남기고 예외 전파하지 않음
        }
    }
}

