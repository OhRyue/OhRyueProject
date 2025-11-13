package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.StudySession;
import com.OhRyue.certpilot.study.domain.StudySessionItem;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.repository.StudySessionItemRepository;
import com.OhRyue.certpilot.study.repository.StudySessionRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;

@Component
@RequiredArgsConstructor
@Transactional
public class StudySessionManager {

  private final StudySessionRepository sessionRepository;
  private final StudySessionItemRepository itemRepository;
  private final ObjectMapper objectMapper;

  private static final TypeReference<Map<String, Object>> MAP_TYPE = new TypeReference<>() {};

  public StudySession ensureMicroSession(String userId, Long topicId, ExamMode examMode, int expectedCount) {
    return ensureSession(userId, Map.of("topicId", topicId), "MICRO", examMode, expectedCount);
  }

  public StudySession ensureReviewSession(String userId, Long rootTopicId, ExamMode examMode, int expectedCount) {
    return ensureSession(userId, Map.of("rootTopicId", rootTopicId), "REVIEW", examMode, expectedCount);
  }

  public Optional<StudySession> latestMicroSession(String userId, Long topicId) {
    return latestSession(userId, Map.of("topicId", topicId), "MICRO");
  }

  public Optional<StudySession> latestReviewSession(String userId, Long rootTopicId) {
    return latestSession(userId, Map.of("rootTopicId", rootTopicId), "REVIEW");
  }

  private StudySession ensureSession(String userId,
                                     Map<String, Object> scope,
                                     String mode,
                                     ExamMode examMode,
                                     int expectedCount) {
    String scopeJson = stringify(scope);
    StudySession session = sessionRepository.findFirstByUserIdAndTopicScopeJsonAndModeOrderByStartedAtDesc(
        userId, scopeJson, mode).orElse(null);
    if (session == null) {
      session = StudySession.builder()
          .userId(userId)
          .mode(mode)
          .examMode(examMode)
          .topicScopeJson(scopeJson)
          .questionCount(expectedCount)
          .status("OPEN")
          .startedAt(Instant.now())
          .build();
    } else if (session.getQuestionCount() == null || session.getQuestionCount() < expectedCount) {
      session.setQuestionCount(expectedCount);
    }
    return sessionRepository.save(session);
  }

  private Optional<StudySession> latestSession(String userId,
                                               Map<String, Object> scope,
                                               String mode) {
    String scopeJson = stringify(scope);
    return sessionRepository.findFirstByUserIdAndTopicScopeJsonAndModeOrderByStartedAtDesc(userId, scopeJson, mode);
  }

  public StudySessionItem upsertItem(StudySession session,
                                     Long questionId,
                                     int orderNo,
                                     String answerJson,
                                     Boolean correct,
                                     Integer score,
                                     String aiExplainJson) {
    return itemRepository.findBySessionIdAndQuestionId(session.getId(), questionId)
        .map(item -> {
          item.setUserAnswerJson(answerJson);
          item.setCorrect(correct);
          item.setScore(score);
          item.setAiExplainJson(aiExplainJson);
          item.setAnsweredAt(Instant.now());
          return itemRepository.save(item);
        })
        .orElseGet(() -> itemRepository.save(
            StudySessionItem.builder()
                .sessionId(session.getId())
                .orderNo(orderNo)
                .questionId(questionId)
                .userAnswerJson(answerJson)
                .correct(correct)
                .score(score)
                .aiExplainJson(aiExplainJson)
                .answeredAt(Instant.now())
                .build()
        ));
  }

  public void closeSession(StudySession session, double scorePct, Map<String, Object> summary) {
    Map<String, Object> current = loadMeta(session);
    current.putAll(summary);
    session.setScorePct(scorePct);
    session.setSummaryJson(stringify(current));
    session.setFinishedAt(Instant.now());
    session.setStatus("SUBMITTED");
    sessionRepository.save(session);
  }

  public List<StudySessionItem> items(Long sessionId) {
    return itemRepository.findBySessionIdOrderByOrderNoAsc(sessionId);
  }

  public Map<String, Object> loadMeta(StudySession session) {
    if (session.getSummaryJson() == null || session.getSummaryJson().isBlank()) {
      return new HashMap<>();
    }
    try {
      return objectMapper.readValue(session.getSummaryJson(), MAP_TYPE);
    } catch (JsonProcessingException e) {
      return new HashMap<>();
    }
  }

  public Map<String, Object> loadStepMeta(StudySession session, String stepKey) {
    Map<String, Object> meta = loadMeta(session);
    Object raw = meta.get(stepKey);
    if (raw instanceof Map<?, ?> map) {
      Map<String, Object> copy = new HashMap<>();
      map.forEach((k, v) -> copy.put(String.valueOf(k), v));
      return copy;
    }
    return new HashMap<>();
  }

  public void saveStepMeta(StudySession session, String stepKey, Map<String, Object> value) {
    Map<String, Object> meta = loadMeta(session);
    meta.put(stepKey, value);
    session.setSummaryJson(stringify(meta));
    sessionRepository.save(session);
  }

  public void updateStatus(StudySession session, String status) {
    session.setStatus(status);
    sessionRepository.save(session);
  }

  public StudySession getSession(Long sessionId) {
    return sessionRepository.findById(sessionId)
        .orElseThrow(() -> new NoSuchElementException("Study session not found: " + sessionId));
  }

  public List<Long> wrongQuestionIds(Long sessionId, String stepKey) {
    StudySession session = getSession(sessionId);
    Map<String, Object> stepMeta = loadStepMeta(session, stepKey);
    Object raw = stepMeta.get("wrongQuestionIds");
    return toLongList(raw);
  }

  private String stringify(Map<String, Object> map) {
    try {
      return objectMapper.writeValueAsString(map);
    } catch (JsonProcessingException e) {
      return "{}";
    }
  }

  private List<Long> toLongList(Object raw) {
    if (raw == null) {
      return List.of();
    }
    if (raw instanceof List<?> list) {
      List<Long> longs = new ArrayList<>();
      for (Object value : list) {
        if (value instanceof Number number) {
          longs.add(number.longValue());
        } else if (value instanceof String str && !str.isBlank()) {
          try {
            longs.add(Long.parseLong(str));
          } catch (NumberFormatException ignored) {
          }
        }
      }
      return longs;
    }
    return List.of();
  }
}

