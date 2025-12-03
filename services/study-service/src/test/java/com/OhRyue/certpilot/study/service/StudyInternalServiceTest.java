package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.StudySession;
import com.OhRyue.certpilot.study.domain.StudySessionItem;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.InternalDtos;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.StudySessionItemRepository;
import com.OhRyue.certpilot.study.repository.StudySessionRepository;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("StudyInternalService 테스트")
class StudyInternalServiceTest {

    @Mock
    private StudySessionRepository sessionRepository;

    @Mock
    private StudySessionItemRepository itemRepository;

    @Mock
    private QuestionRepository questionRepository;

    @Mock
    private ObjectMapper objectMapper;

    @InjectMocks
    private StudyInternalService studyInternalService;

    private String testUserId;
    private Long testSessionId;
    private Instant now;

    @BeforeEach
    void setUp() {
        testUserId = "test-user-123";
        testSessionId = 100L;
        now = Instant.now();
    }

    @Test
    @DisplayName("세션 상세 조회 - 정상 조회")
    void testGetSessionDetail() throws Exception {
        // Given
        StudySession session = StudySession.builder()
                .id(testSessionId)
                .userId(testUserId)
                .mode("MICRO")
                .examMode(ExamMode.WRITTEN)
                .questionCount(2)
                .scorePct(75.0)
                .startedAt(now.minusSeconds(3600))
                .finishedAt(now)
                .completed(true)
                .passed(true)
                .status("COMPLETE")
                .build();

        StudySessionItem item1 = StudySessionItem.builder()
                .id(1L)
                .sessionId(testSessionId)
                .questionId(1001L)
                .orderNo(1)
                .correct(true)
                .score(100)
                .answeredAt(now.minusSeconds(1800))
                .userAnswerJson("{\"answer\":\"1번\"}")
                .build();

        StudySessionItem item2 = StudySessionItem.builder()
                .id(2L)
                .sessionId(testSessionId)
                .questionId(1002L)
                .orderNo(2)
                .correct(false)
                .score(0)
                .answeredAt(now.minusSeconds(900))
                .userAnswerJson("{\"answer\":\"2번\"}")
                .build();

        Question question1 = Question.builder()
                .id(1001L)
                .type(QuestionType.MCQ)
                .stem("문제1")
                .answerKey("1번")
                .build();

        Question question2 = Question.builder()
                .id(1002L)
                .type(QuestionType.MCQ)
                .stem("문제2")
                .answerKey("3번")
                .build();

        when(sessionRepository.findById(testSessionId))
                .thenReturn(Optional.of(session));
        when(itemRepository.findBySessionIdOrderByOrderNoAsc(testSessionId))
                .thenReturn(List.of(item1, item2));
        when(questionRepository.findByIdIn(anyList()))
                .thenReturn(List.of(question1, question2));
        when(objectMapper.readValue(eq("{\"answer\":\"1번\"}"), any(TypeReference.class)))
                .thenReturn(Map.of("answer", "1번"));
        when(objectMapper.readValue(eq("{\"answer\":\"2번\"}"), any(TypeReference.class)))
                .thenReturn(Map.of("answer", "2번"));

        // When
        InternalDtos.StudySessionDetailDto result = studyInternalService.getSessionDetail(testSessionId, testUserId);

        // Then
        assertNotNull(result);
        assertEquals(testSessionId, result.sessionId());
        assertEquals(testUserId, result.userId());
        assertEquals(2, result.questions().size());
        assertEquals("문제1", result.questions().get(0).stem());
        assertEquals("1번", result.questions().get(0).myAnswer());
        assertTrue(result.questions().get(0).isCorrect());
    }

    @Test
    @DisplayName("세션 상세 조회 - 잘못된 userId 접근 시 예외 발생")
    void testGetSessionDetail_WrongUserId() {
        // Given
        StudySession session = StudySession.builder()
                .id(testSessionId)
                .userId("other-user")
                .mode("MICRO")
                .examMode(ExamMode.WRITTEN)
                .build();

        when(sessionRepository.findById(testSessionId))
                .thenReturn(Optional.of(session));

        // When & Then
        assertThrows(IllegalArgumentException.class, () -> {
            studyInternalService.getSessionDetail(testSessionId, testUserId);
        });
    }

    @Test
    @DisplayName("세션 상세 조회 - 세션 없음 시 예외 발생")
    void testGetSessionDetail_NotFound() {
        // Given
        when(sessionRepository.findById(testSessionId))
                .thenReturn(Optional.empty());

        // When & Then
        assertThrows(IllegalArgumentException.class, () -> {
            studyInternalService.getSessionDetail(testSessionId, testUserId);
        });
    }
}

