package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.*;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.WrittenDtos;
import com.OhRyue.certpilot.study.repository.*;
import com.OhRyue.common.auth.AuthUserUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Instant;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * WrittenService의 finalizeStudySession 호출 및 score_pct/passed 업데이트 검증 테스트
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("WrittenService - finalizeStudySession 호출 검증")
class WrittenServiceFinalizeTest {

    @Mock
    private LearningSessionService learningSessionService;
    
    @Mock
    private StudySessionManager sessionManager;
    
    @Mock
    private LearningStepRepository learningStepRepository;
    
    @Mock
    private QuestionRepository questionRepository;
    
    @Mock
    private UserAnswerRepository userAnswerRepository;
    
    @Mock
    private UserProgressRepository userProgressRepository;
    
    @Mock
    private com.OhRyue.certpilot.study.client.ProgressHookClient progressHookClient;
    
    @InjectMocks
    private WrittenService writtenService;

    private LearningSession learningSession;
    private LearningStep mcqStep;
    private StudySession studySession;
    private List<StudySessionItem> sessionItems;

    @BeforeEach
    void setUp() {
        // LearningSession 설정
        learningSession = new LearningSession();
        learningSession.setId(1L);
        learningSession.setUserId("testUser");
        learningSession.setTopicId(1001L);
        learningSession.setMode("MICRO");
        learningSession.setStatus("IN_PROGRESS");

        // LearningStep 설정
        mcqStep = new LearningStep();
        mcqStep.setId(10L);
        mcqStep.setStepCode("MCQ");
        mcqStep.setStatus("IN_PROGRESS");
        mcqStep.setMetadataJson("{}");

        // StudySession 설정
        studySession = new StudySession();
        studySession.setId(100L);
        studySession.setUserId("testUser");
        studySession.setMode("MICRO");
        studySession.setExamMode(ExamMode.WRITTEN);
        studySession.setQuestionCount(5);
        studySession.setStatus("OPEN");
        studySession.setScorePct(null);  // 초기값 null
        studySession.setPassed(null);
        studySession.setCompleted(false);
        studySession.setLearningStep(mcqStep);

        // StudySessionItem 설정 (5개 모두 정답)
        sessionItems = new ArrayList<>();
        for (int i = 1; i <= 5; i++) {
            StudySessionItem item = new StudySessionItem();
            item.setId((long) i);
            item.setSessionId(100L);
            item.setQuestionId((long) (100 + i));
            item.setOrderNo(i);
            item.setCorrect(true);
            item.setScore(100);
            item.setUserAnswerJson("{\"answer\":\"A\",\"correct\":true}");
            item.setAnsweredAt(Instant.now());
            sessionItems.add(item);
        }

        mcqStep.setStudySession(studySession);
    }

    @Test
    @DisplayName("모든 문제 정답 시 - finalizeStudySession 호출되어 score_pct=100, passed=true로 업데이트")
    void testSubmitMcq_AllCorrect_CallsFinalizeAndUpdatesScore() {
        // Given
        try (MockedStatic<AuthUserUtil> authUtil = mockStatic(AuthUserUtil.class)) {
            authUtil.when(AuthUserUtil::getCurrentUserId).thenReturn("testUser");

            WrittenDtos.McqSubmitReq req = new WrittenDtos.McqSubmitReq(
                1001L,
                Arrays.asList(
                    new WrittenDtos.McqAnswer(101L, "A"),
                    new WrittenDtos.McqAnswer(102L, "B"),
                    new WrittenDtos.McqAnswer(103L, "C"),
                    new WrittenDtos.McqAnswer(104L, "D"),
                    new WrittenDtos.McqAnswer(105L, "A")
                )
            );

            when(learningSessionService.getLearningSession(1L)).thenReturn(learningSession);
            when(learningSessionService.getStep(learningSession, "MCQ")).thenReturn(mcqStep);
            when(sessionManager.items(100L)).thenReturn(sessionItems);
            
            // Question mock 설정
            Map<Long, Question> questionMap = new HashMap<>();
            for (int i = 1; i <= 5; i++) {
                Question q = new Question();
                q.setId((long) (100 + i));
                q.setTopicId(1001L);
                q.setMode(ExamMode.WRITTEN);
                q.setType(QuestionType.MCQ);
                q.setStem("Question " + i);
                questionMap.put((long) (100 + i), q);
            }
            when(questionRepository.findByIdIn(anyList())).thenReturn(new ArrayList<>(questionMap.values()));

            // finalizeStudySession 결과 mock
            StudySessionManager.FinalizeResult finalizeResult = 
                new StudySessionManager.FinalizeResult(5, 5, 100.0, true);
            when(sessionManager.finalizeStudySession(studySession)).thenReturn(finalizeResult);
            
            // loadStepMeta mock
            Map<String, Object> stepMeta = new HashMap<>();
            stepMeta.put("total", 5);
            stepMeta.put("correct", 5);
            stepMeta.put("completed", true);
            when(sessionManager.loadStepMeta(eq(studySession), eq("mcq"))).thenReturn(stepMeta);

            // learningStepRepository mock
            when(learningStepRepository.findById(10L)).thenReturn(Optional.of(mcqStep));
            when(learningStepRepository.save(any(LearningStep.class))).thenReturn(mcqStep);

            // When
            writtenService.submitMcq(1L, req);

            // Then
            // finalizeStudySession이 호출되었는지 확인
            verify(sessionManager, atLeastOnce()).finalizeStudySession(studySession);
            
            // LearningStep이 업데이트되었는지 확인
            verify(learningStepRepository, atLeastOnce()).save(any(LearningStep.class));
        }
    }

    @Test
    @DisplayName("score_pct가 0인 경우 - finalizeStudySession 호출되어 재계산")
    void testSubmitMcq_ScorePctIsZero_CallsFinalize() {
        // Given
        try (MockedStatic<AuthUserUtil> authUtil = mockStatic(AuthUserUtil.class)) {
            authUtil.when(AuthUserUtil::getCurrentUserId).thenReturn("testUser");

            studySession.setScorePct(0.0);  // 0으로 설정된 경우
            studySession.setPassed(false);

            WrittenDtos.McqSubmitReq req = new WrittenDtos.McqSubmitReq(
                1001L,
                Arrays.asList(
                    new WrittenDtos.McqAnswer(101L, "A"),
                    new WrittenDtos.McqAnswer(102L, "B")
                )
            );

            when(learningSessionService.getLearningSession(1L)).thenReturn(learningSession);
            when(learningSessionService.getStep(learningSession, "MCQ")).thenReturn(mcqStep);
            when(sessionManager.items(100L)).thenReturn(sessionItems);
            
            Map<String, Object> stepMeta = new HashMap<>();
            stepMeta.put("total", 5);
            stepMeta.put("correct", 5);
            stepMeta.put("completed", true);
            when(sessionManager.loadStepMeta(eq(studySession), eq("mcq"))).thenReturn(stepMeta);

            StudySessionManager.FinalizeResult finalizeResult = 
                new StudySessionManager.FinalizeResult(5, 5, 100.0, true);
            when(sessionManager.finalizeStudySession(studySession)).thenReturn(finalizeResult);
            when(learningStepRepository.findById(10L)).thenReturn(Optional.of(mcqStep));
            when(learningStepRepository.save(any(LearningStep.class))).thenReturn(mcqStep);

            // When
            writtenService.submitMcq(1L, req);

            // Then
            // score_pct가 0이므로 finalizeStudySession이 호출되어야 함
            verify(sessionManager, atLeastOnce()).finalizeStudySession(studySession);
        }
    }

    @Test
    @DisplayName("모든 문제가 제출된 경우 - finalizeStudySession 호출")
    void testSubmitMcq_AllQuestionsAnswered_CallsFinalize() {
        // Given
        try (MockedStatic<AuthUserUtil> authUtil = mockStatic(AuthUserUtil.class)) {
            authUtil.when(AuthUserUtil::getCurrentUserId).thenReturn("testUser");

            // 모든 문제가 제출된 상태 (5개 모두 answered)
            WrittenDtos.McqSubmitReq req = new WrittenDtos.McqSubmitReq(
                1001L,
                Arrays.asList(
                    new WrittenDtos.McqAnswer(101L, "A"),
                    new WrittenDtos.McqAnswer(102L, "B"),
                    new WrittenDtos.McqAnswer(103L, "C"),
                    new WrittenDtos.McqAnswer(104L, "D"),
                    new WrittenDtos.McqAnswer(105L, "A")
                )
            );

            when(learningSessionService.getLearningSession(1L)).thenReturn(learningSession);
            when(learningSessionService.getStep(learningSession, "MCQ")).thenReturn(mcqStep);
            when(sessionManager.items(100L)).thenReturn(sessionItems);
            
            Map<String, Object> stepMeta = new HashMap<>();
            stepMeta.put("total", 5);
            stepMeta.put("correct", 5);
            stepMeta.put("completed", true);
            when(sessionManager.loadStepMeta(eq(studySession), eq("mcq"))).thenReturn(stepMeta);

            StudySessionManager.FinalizeResult finalizeResult = 
                new StudySessionManager.FinalizeResult(5, 5, 100.0, true);
            when(sessionManager.finalizeStudySession(studySession)).thenReturn(finalizeResult);
            when(learningStepRepository.findById(10L)).thenReturn(Optional.of(mcqStep));
            when(learningStepRepository.save(any(LearningStep.class))).thenReturn(mcqStep);

            // When
            writtenService.submitMcq(1L, req);

            // Then
            // 모든 문제가 제출되었으므로 finalizeStudySession이 호출되어야 함
            verify(sessionManager, atLeastOnce()).finalizeStudySession(studySession);
        }
    }
}









