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

@Service
@RequiredArgsConstructor
@Slf4j
public class AnswerSubmissionService {

  private static final String EVENT_QUESTION_STARTED = "QUESTION_STARTED";
  private static final String EVENT_TIMEOUT_HANDLED = "QUESTION_TIMEOUT_HANDLED";

  private final MatchRoomRepository roomRepository;
  private final MatchParticipantRepository participantRepository;
  private final MatchQuestionRepository questionRepository;
  private final MatchAnswerRepository answerRepository;
  private final MatchEventRepository eventRepository;
  private final GoldenbellStateRepository goldenbellStateRepository;
  private final VersusService versusService;
  private final RealtimeEventService realtimeEventService;
  private final ObjectMapper objectMapper;

  @Transactional
  public VersusDtos.ScoreBoardResp submitAnswer(
      Long roomId,
      String userId,
      WebSocketDtos.SubmitAnswerCommand command) {

    VersusDtos.SubmitAnswerReq req = new VersusDtos.SubmitAnswerReq(
        command.questionId(),
        command.userAnswer(),
        command.correct() != null ? command.correct() : false,
        command.timeMs(),
        null,
        command.roundNo(),
        command.phase() != null ? MatchPhase.valueOf(command.phase()) : null
    );

    return submitAnswer(roomId, userId, req);
  }

  @Transactional
  public VersusDtos.ScoreBoardResp submitAnswer(
      Long roomId,
      String userId,
      VersusDtos.SubmitAnswerReq req) {

    MatchRoom room = roomRepository.findById(roomId)
        .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Room not found: " + roomId));

    MatchParticipant participant = participantRepository.findByRoomIdAndUserId(roomId, userId)
        .orElseThrow(() -> new ResponseStatusException(HttpStatus.BAD_REQUEST, "Participant not joined"));

    MatchQuestion question = questionRepository.findByRoomIdAndQuestionId(roomId, req.questionId())
        .orElseThrow(() -> new ResponseStatusException(HttpStatus.BAD_REQUEST, "Question not found: " + req.questionId()));

    validateParticipantEligibility(room, participant, question);
    validateNoDuplicateSubmission(roomId, req.questionId(), userId, question);
    validateQuestionNotExpired(roomId, question);
    validateRoomStatus(room);

    VersusDtos.ScoreBoardResp scoreboard = versusService.submitAnswer(roomId, userId, req);

    publishScoreboardUpdatedEvent(roomId, scoreboard);

    return scoreboard;
  }

  private void validateParticipantEligibility(MatchRoom room, MatchParticipant participant, MatchQuestion question) {
    if (room.getMode() == MatchMode.TOURNAMENT && participant.isEliminated()) {
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Eliminated participant cannot submit answers");
    }

    if (room.getMode() == MatchMode.GOLDENBELL) {
      GoldenbellState state = goldenbellStateRepository.findByRoomIdAndUserId(room.getId(), participant.getUserId())
          .orElse(null);
      if (state != null && !state.isAlive()) {
        if (question.getPhase() != MatchPhase.REVIVAL || !state.isRevived()) {
          throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Eliminated participant cannot submit answers");
        }
      }
    }
  }

  private void validateNoDuplicateSubmission(Long roomId, Long questionId, String userId, MatchQuestion question) {
    Optional<MatchAnswer> existingAnswer = answerRepository.findByRoomIdAndQuestionIdAndUserId(roomId, questionId, userId);

    if (existingAnswer.isPresent()) {
      MatchAnswer answer = existingAnswer.get();

      if (question.getPhase() == MatchPhase.REVIVAL) {
        log.debug("REVIVAL phase allows duplicate submission: roomId={}, questionId={}, userId={}",
            roomId, questionId, userId);
        return;
      }

      log.warn("Duplicate answer submission detected: roomId={}, questionId={}, userId={}, existingAnswerId={}",
          roomId, questionId, userId, answer.getId());
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
          "Answer already submitted for this question");
    }
  }

  /**
   * 문제 종료 여부 검증 (타임아웃 이후 제출 방어)
   * QUESTION_TIMEOUT_HANDLED 이벤트가 이미 있으면 즉시 차단
   */
  private void validateQuestionNotExpired(Long roomId, MatchQuestion question) {
    if (roomId == null || question == null) return;

    // timeout 처리 완료 마커가 이미 있으면 늦은 제출은 무조건 차단
    if (eventRepository.existsByRoomIdAndEventTypeAndPayloadJsonContaining(
        roomId, EVENT_TIMEOUT_HANDLED, "\"questionId\":" + question.getQuestionId()
    )) {
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Question time limit has expired");
    }

    try {
      List<MatchEvent> startEvents = eventRepository.findByRoomIdAndEventTypeContaining(roomId, EVENT_QUESTION_STARTED);

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
          throw e;
        } catch (Exception e) {
          log.debug("Failed to check question expiration: {}", e.getMessage());
        }
      }
    } catch (ResponseStatusException e) {
      throw e;
    } catch (Exception e) {
      log.debug("Failed to validate question expiration: {}", e.getMessage());
    }
  }

  private void validateRoomStatus(MatchRoom room) {
    if (room.getStatus() == MatchStatus.DONE) {
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Match has already finished");
    }
  }

  private void publishScoreboardUpdatedEvent(Long roomId, VersusDtos.ScoreBoardResp scoreboard) {
    try {
      Map<String, Object> payload = Map.of(
          "roomId", roomId,
          "status", scoreboard.status().name(),
          "itemCount", scoreboard.items().size(),
          "updatedAt", Instant.now().toString()
      );

      String payloadJson = objectMapper.writeValueAsString(payload);
      MatchEvent event = MatchEvent.builder()
          .roomId(roomId)
          .eventType("SCOREBOARD_UPDATED")
          .payloadJson(payloadJson)
          .build();
      MatchEvent savedEvent = eventRepository.save(event);

      realtimeEventService.broadcastEvent(savedEvent);

      log.debug("SCOREBOARD_UPDATED event published: roomId={}, itemCount={}, eventId={}",
          roomId, scoreboard.items().size(), savedEvent.getId());
    } catch (Exception e) {
      log.warn("Failed to publish SCOREBOARD_UPDATED event: roomId={}, error={}",
          roomId, e.getMessage());
    }
  }
}
