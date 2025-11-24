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
public class StudySessionManager {

    private final StudySessionRepository sessionRepository;
    private final StudySessionItemRepository itemRepository;
    private final ObjectMapper objectMapper;

    private static final TypeReference<Map<String, Object>> MAP_TYPE = new TypeReference<>() {};

    /* ===================== 세션 생성/보장 (쓰기) ===================== */

    @Transactional
    public StudySession ensureMicroSession(String userId, Long topicId, ExamMode examMode, int expectedCount) {
        return ensureSession(userId, Map.of("topicId", topicId), "MICRO", examMode, expectedCount);
    }

    @Transactional
    public StudySession ensureReviewSession(String userId, Long rootTopicId, ExamMode examMode, int expectedCount) {
        return ensureSession(userId, Map.of("rootTopicId", rootTopicId), "REVIEW", examMode, expectedCount);
    }

    @Transactional
    protected StudySession ensureSession(String userId,
                                         Map<String, Object> scope,
                                         String mode,
                                         ExamMode examMode,
                                         int expectedCount) {
        String scopeJson = stringify(scope);
        StudySession session = sessionRepository
                .findFirstByUserIdAndTopicScopeJsonAndModeOrderByStartedAtDesc(userId, scopeJson, mode)
                .orElse(null);

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

    /* ===================== 세션 조회 (읽기) ===================== */

    public Optional<StudySession> latestMicroSession(String userId, Long topicId) {
        return latestSession(userId, Map.of("topicId", topicId), "MICRO");
    }

    public Optional<StudySession> latestReviewSession(String userId, Long rootTopicId) {
        return latestSession(userId, Map.of("rootTopicId", rootTopicId), "REVIEW");
    }

    private Optional<StudySession> latestSession(String userId,
                                                 Map<String, Object> scope,
                                                 String mode) {
        String scopeJson = stringify(scope);
        return sessionRepository.findFirstByUserIdAndTopicScopeJsonAndModeOrderByStartedAtDesc(userId, scopeJson, mode);
    }

    /* ===================== 세션 Item upsert (쓰기) ===================== */

    @Transactional
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

    /* ===================== 세션 종료/요약/상태 변경 (쓰기) ===================== */

    /**
     * 세션 종료
     * - completed: 모든 문제를 풀었는지 (항상 true)
     * - passed: 모든 문제를 맞췄는지 (scorePct >= 100.0 또는 allCorrect)
     * - xpGranted: XP가 이미 반영되었는지 (초기값 false, XP 지급 시 true로 변경)
     */
    @Transactional
    public void closeSession(StudySession session, double scorePct, Map<String, Object> summary) {
        Map<String, Object> current = loadMeta(session);
        current.putAll(summary);
        session.setScorePct(scorePct);
        session.setSummaryJson(stringify(current));
        session.setFinishedAt(Instant.now());
        session.setStatus("SUBMITTED");
        // completed, passed, xpGranted 설정
        session.setCompleted(true); // 모든 문제를 풀었으므로 true
        session.setPassed(scorePct >= 100.0); // 100% 정답이면 passed
        // xpGranted는 XP 지급 시점에 별도로 설정 (기본값 false 유지)
        sessionRepository.save(session);
    }

    /**
     * 세션 종료 (passed 여부 명시적 지정)
     */
    @Transactional
    public void closeSession(StudySession session, double scorePct, boolean passed, Map<String, Object> summary) {
        Map<String, Object> current = loadMeta(session);
        current.putAll(summary);
        session.setScorePct(scorePct);
        session.setSummaryJson(stringify(current));
        session.setFinishedAt(Instant.now());
        session.setStatus("SUBMITTED");
        session.setCompleted(true);
        session.setPassed(passed);
        sessionRepository.save(session);
    }

    /**
     * XP 지급 완료 표시 (idempotent)
     */
    @Transactional
    public void markXpGranted(StudySession session) {
        if (!Boolean.TRUE.equals(session.getXpGranted())) {
            session.setXpGranted(true);
            sessionRepository.save(session);
        }
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

    @Transactional
    public void saveStepMeta(StudySession session, String stepKey, Map<String, Object> value) {
        Map<String, Object> meta = loadMeta(session);
        meta.put(stepKey, value);
        session.setSummaryJson(stringify(meta));
        sessionRepository.save(session);
    }

    @Transactional
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

    /* ===================== 유틸 ===================== */

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
