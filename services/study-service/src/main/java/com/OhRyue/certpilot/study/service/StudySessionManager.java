package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.LearningStep;
import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.StudySession;
import com.OhRyue.certpilot.study.domain.StudySessionItem;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.repository.LearningStepRepository;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.StudySessionItemRepository;
import com.OhRyue.certpilot.study.repository.StudySessionRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.NoSuchElementException;
import java.util.Set;

@Component
@RequiredArgsConstructor
public class StudySessionManager {

    private final StudySessionRepository sessionRepository;
    private final StudySessionItemRepository itemRepository;
    private final QuestionRepository questionRepository;
    private final LearningStepRepository learningStepRepository;
    private final ObjectMapper objectMapper;

    private static final TypeReference<Map<String, Object>> MAP_TYPE = new TypeReference<>() {};

    /* ===================== 세션 생성/보장 (쓰기) ===================== */

    /**
     * LearningStep에 연결된 StudySession 조회 또는 생성
     */
    @Transactional
    public StudySession ensureStudySessionForStep(LearningStep learningStep, 
                                                   String userId, 
                                                   Long topicId, 
                                                   ExamMode examMode, 
                                                   int expectedCount) {
        // 이미 연결된 StudySession이 있으면 반환
        if (learningStep.getStudySession() != null) {
            return learningStep.getStudySession();
        }

        // 새 StudySession 생성
        String scopeJson = stringify(Map.of("topicId", topicId));
        StudySession session = StudySession.builder()
                .userId(userId)
                .mode("MICRO")  // MINI, MCQ 단계는 MICRO 모드
                .examMode(examMode)
                .topicScopeJson(scopeJson)
                .questionCount(expectedCount)
                .status("OPEN")
                .startedAt(Instant.now())
                .learningStep(learningStep)
                .build();

        session = sessionRepository.save(session);
        
        // LearningStep에 연결 (양방향 관계 설정)
        learningStep.setStudySession(session);
        
        return session;
    }

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

    /* ===================== 세션 문제 할당 (쓰기) ===================== */

    /**
     * 세션에 문제 사전 할당 (세션 시작 시점에 호출)
     * @param session StudySession
     * @param questionIds 할당할 문제 ID 목록
     */
    @Transactional
    public void allocateQuestions(StudySession session, List<Long> questionIds) {
        // 이미 할당된 문제가 있으면 스킵
        List<StudySessionItem> existingItems = itemRepository.findBySessionIdOrderByOrderNoAsc(session.getId());
        if (!existingItems.isEmpty()) {
            return; // 이미 할당됨
        }

        // 문제 할당
        for (int i = 0; i < questionIds.size(); i++) {
            Long questionId = questionIds.get(i);
            itemRepository.save(StudySessionItem.builder()
                    .sessionId(session.getId())
                    .questionId(questionId)
                    .orderNo(i + 1)
                    .userAnswerJson(null)  // 아직 답변 안함
                    .correct(null)
                    .score(null)
                    .createdAt(Instant.now())
                    .build());
        }
    }

    /**
     * LearningStep에 연결된 StudySession 생성 및 문제 할당
     */
    @Transactional
    public StudySession createAndAllocateSessionForStep(
            LearningStep learningStep,
            String userId,
            Long topicId,
            ExamMode examMode,
            QuestionType questionType,
            int questionCount) {
        // 이미 연결된 StudySession이 있으면 반환
        if (learningStep.getStudySession() != null) {
            return learningStep.getStudySession();
        }

        // 문제 랜덤 선택 (세션 시작 시점에 고정)
        List<Question> questions = questionRepository.pickRandomByTopic(
                topicId, examMode, questionType, PageRequest.of(0, questionCount));

        if (questions.isEmpty()) {
            throw new IllegalStateException("문제가 부족합니다. topicId: " + topicId + ", type: " + questionType);
        }

        // StudySession 생성
        String scopeJson = stringify(Map.of("topicId", topicId));
        StudySession session = StudySession.builder()
                .userId(userId)
                .mode("MICRO")  // MINI, MCQ 단계는 MICRO 모드
                .examMode(examMode)
                .topicScopeJson(scopeJson)
                .questionCount(questionCount)
                .status("OPEN")
                .startedAt(Instant.now())
                .learningStep(learningStep)
                .build();

        session = sessionRepository.save(session);

        // LearningStep에 연결
        learningStep.setStudySession(session);

        // 문제 할당
        List<Long> questionIds = questions.stream().map(Question::getId).toList();
        allocateQuestions(session, questionIds);

        return session;
    }

    /**
     * Review 모드용: LearningStep에 연결된 StudySession 생성 및 문제 할당 (rootTopicId 기반)
     */
    @Transactional
    public StudySession createAndAllocateSessionForReviewStep(
            LearningStep learningStep,
            String userId,
            Long rootTopicId,
            ExamMode examMode,
            QuestionType questionType,
            int questionCount,
            Set<Long> topicIds) {
        // 이미 연결된 StudySession이 있으면 반환
        if (learningStep.getStudySession() != null) {
            return learningStep.getStudySession();
        }

        // 여러 토픽에서 문제 랜덤 선택 (세션 시작 시점에 고정)
        List<Question> questions = questionRepository.pickRandomByTopicIn(
                topicIds, examMode, questionType, PageRequest.of(0, questionCount));

        if (questions.isEmpty()) {
            // 디버깅을 위한 상세 에러 메시지
            throw new IllegalStateException(
                String.format("문제가 부족합니다. rootTopicId=%d, topicIds=%s, examMode=%s, type=%s, required=%d",
                    rootTopicId, topicIds, examMode, questionType, questionCount));
        }

        // StudySession 생성
        String scopeJson = stringify(Map.of("rootTopicId", rootTopicId));
        StudySession session = StudySession.builder()
                .userId(userId)
                .mode("REVIEW")  // Review 모드
                .examMode(examMode)
                .topicScopeJson(scopeJson)
                .questionCount(questionCount)
                .status("OPEN")
                .startedAt(Instant.now())
                .learningStep(learningStep)
                .build();

        session = sessionRepository.save(session);

        // LearningStep에 연결
        learningStep.setStudySession(session);

        // 문제 할당
        List<Long> questionIds = questions.stream().map(Question::getId).toList();
        allocateQuestions(session, questionIds);

        return session;
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

    /**
     * 세션의 passed 상태 업데이트
     */
    @Transactional
    public void updatePassed(StudySession session, boolean passed) {
        session.setPassed(passed);
        sessionRepository.save(session);
    }

    /**
     * 세션 종료 공통 처리 (finalizeStudySession)
     * study_session_item 기준으로 total, correct, score_pct 계산
     * completed = 1, passed = (correct == total)
     * finished_at, summary_json 업데이트
     * learning_step도 업데이트 (status, score_pct, metadata_json)
     * 
     * @param session 완료할 StudySession
     * @return finalize 결과 정보 (total, correct, scorePct, passed)
     */
    @Transactional
    public FinalizeResult finalizeStudySession(StudySession session) {
        // 1. study_session_item 전체 조회
        List<StudySessionItem> allItems = items(session.getId());
        
        // 2. total, correct 계산
        int total = allItems.size();
        long correctCount = allItems.stream()
                .filter(item -> Boolean.TRUE.equals(item.getCorrect()))
                .count();
        int correct = (int) correctCount;
        
        // 3. score_pct 계산 (반올림하여 소수점 2자리)
        double scorePct = total > 0 ? Math.round((correct * 100.0) / total * 100.0) / 100.0 : 0.0;
        
        // 4. passed 계산: 모든 문제를 맞았는지
        boolean passed = (correct == total) && total > 0;
        
        // 5. summary_json 업데이트
        Map<String, Object> summary = loadMeta(session);
        summary.put("total", total);
        summary.put("correct", correct);
        summary.put("scorePct", scorePct);
        summary.put("passed", passed);
        summary.put("lastSubmittedAt", Instant.now().toString());
        
        // 6. StudySession 업데이트
        session.setScorePct(scorePct);
        session.setSummaryJson(stringify(summary));
        session.setFinishedAt(Instant.now());
        session.setStatus("SUBMITTED");
        session.setCompleted(true);
        session.setPassed(passed);
        sessionRepository.save(session);
        
        // 7. LearningStep 업데이트 (연결되어 있는 경우)
        LearningStep learningStep = session.getLearningStep();
        if (learningStep != null) {
            Map<String, Object> stepMetadata = new HashMap<>();
            try {
                if (learningStep.getMetadataJson() != null && !learningStep.getMetadataJson().isBlank()) {
                    stepMetadata = objectMapper.readValue(learningStep.getMetadataJson(), MAP_TYPE);
                }
            } catch (JsonProcessingException e) {
                // 기존 메타데이터가 없거나 파싱 실패 시 빈 맵 사용
            }
            
            stepMetadata.put("total", total);
            stepMetadata.put("correct", correct);
            stepMetadata.put("scorePct", (int) Math.round(scorePct));
            stepMetadata.put("passed", passed);
            stepMetadata.put("completed", true);
            stepMetadata.put("lastSubmittedAt", Instant.now().toString());
            
            learningStep.setScorePct((int) Math.round(scorePct));
            learningStep.setMetadataJson(stringify(stepMetadata));
            learningStep.setStatus("COMPLETE");
            learningStep.setUpdatedAt(Instant.now());
            learningStepRepository.save(learningStep);
        }
        
        return new FinalizeResult(total, correct, scorePct, passed);
    }
    
    /**
     * finalizeStudySession 결과를 담는 레코드
     */
    public record FinalizeResult(int total, int correct, double scorePct, boolean passed) {}

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

    /**
     * LearningSession의 모든 StudySession을 CLOSED 처리
     * "처음부터 하기" 시 기존 IN_PROGRESS 세션의 모든 StudySession을 종료하기 위해 사용
     */
    @Transactional
    public void closeAllSessionsForLearningSession(com.OhRyue.certpilot.study.domain.LearningSession learningSession) {
        // LearningSession의 모든 LearningStep을 조회
        List<com.OhRyue.certpilot.study.domain.LearningStep> steps = learningStepRepository.findByLearningSessionIdOrderByIdAsc(learningSession.getId());
        
        // 각 LearningStep에 연결된 StudySession을 CLOSED 처리
        for (com.OhRyue.certpilot.study.domain.LearningStep step : steps) {
            com.OhRyue.certpilot.study.domain.StudySession studySession = step.getStudySession();
            if (studySession != null && !"CLOSED".equals(studySession.getStatus())) {
                studySession.setStatus("CLOSED");
                if (studySession.getFinishedAt() == null) {
                    studySession.setFinishedAt(Instant.now());
                }
                sessionRepository.save(studySession);
            }
        }
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
