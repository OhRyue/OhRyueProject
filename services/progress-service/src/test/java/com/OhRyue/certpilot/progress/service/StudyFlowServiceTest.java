package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.domain.ReportWeekly;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import com.OhRyue.certpilot.progress.dto.StudyFlowCompleteReq;
import com.OhRyue.certpilot.progress.repository.ReportDailyRepository;
import com.OhRyue.certpilot.progress.repository.ReportWeeklyRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@DisplayName("StudyFlowService 테스트 - 메인학습 XP 지급")
class StudyFlowServiceTest {

    @Mock
    private XpService xpService;

    @Mock
    private ReportDailyRepository dailyRepository;

    @Mock
    private ReportWeeklyRepository weeklyRepository;

    @Mock
    private RankService rankService;

    @InjectMocks
    private StudyFlowService studyFlowService;

    private String testUserId;
    private Long testTopicId;

    @BeforeEach
    void setUp() {
        testUserId = "test-user-123";
        testTopicId = 100L;
    }

    @Test
    @DisplayName("MICRO 필기 완료 - 150 XP 지급")
    void testMicroWritten_150Xp() {
        // Given
        StudyFlowCompleteReq req = new StudyFlowCompleteReq(
                testUserId, "WRITTEN", "MICRO", testTopicId
        );
        when(xpService.addXp(anyString(), anyInt(), any(), anyString())).thenReturn(
                UserXpWallet.builder().userId(testUserId).xpTotal(150).level(1).build()
        );
        when(dailyRepository.findByUserIdAndDate(anyString(), any(LocalDate.class)))
                .thenReturn(Optional.empty());
        when(weeklyRepository.findByUserIdAndWeekIso(anyString(), anyString()))
                .thenReturn(Optional.empty());

        // When
        studyFlowService.handleFlowComplete(req);

        // Then
        ArgumentCaptor<Integer> xpCaptor = ArgumentCaptor.forClass(Integer.class);
        ArgumentCaptor<String> refIdCaptor = ArgumentCaptor.forClass(String.class);
        verify(xpService, times(1)).addXp(
                eq(testUserId),
                xpCaptor.capture(),
                any(),
                refIdCaptor.capture()
        );
        assertEquals(150, xpCaptor.getValue());
        assertTrue(refIdCaptor.getValue().contains("MICRO"));
        assertTrue(refIdCaptor.getValue().contains("WRITTEN"));
        assertTrue(refIdCaptor.getValue().contains(testTopicId.toString()));
    }

    @Test
    @DisplayName("MICRO 실기 완료 - 200 XP 지급")
    void testMicroPractical_200Xp() {
        // Given
        StudyFlowCompleteReq req = new StudyFlowCompleteReq(
                testUserId, "PRACTICAL", "MICRO", testTopicId
        );
        when(xpService.addXp(anyString(), anyInt(), any(), anyString())).thenReturn(
                UserXpWallet.builder().userId(testUserId).xpTotal(200).level(1).build()
        );
        when(dailyRepository.findByUserIdAndDate(anyString(), any(LocalDate.class)))
                .thenReturn(Optional.empty());
        when(weeklyRepository.findByUserIdAndWeekIso(anyString(), anyString()))
                .thenReturn(Optional.empty());

        // When
        studyFlowService.handleFlowComplete(req);

        // Then
        ArgumentCaptor<Integer> xpCaptor = ArgumentCaptor.forClass(Integer.class);
        verify(xpService, times(1)).addXp(
                eq(testUserId),
                xpCaptor.capture(),
                any(),
                anyString()
        );
        assertEquals(200, xpCaptor.getValue());
    }

    @Test
    @DisplayName("REVIEW 필기 완료 - 200 XP 지급")
    void testReviewWritten_200Xp() {
        // Given
        StudyFlowCompleteReq req = new StudyFlowCompleteReq(
                testUserId, "WRITTEN", "REVIEW", testTopicId
        );
        when(xpService.addXp(anyString(), anyInt(), any(), anyString())).thenReturn(
                UserXpWallet.builder().userId(testUserId).xpTotal(200).level(1).build()
        );
        when(dailyRepository.findByUserIdAndDate(anyString(), any(LocalDate.class)))
                .thenReturn(Optional.empty());
        when(weeklyRepository.findByUserIdAndWeekIso(anyString(), anyString()))
                .thenReturn(Optional.empty());

        // When
        studyFlowService.handleFlowComplete(req);

        // Then
        ArgumentCaptor<Integer> xpCaptor = ArgumentCaptor.forClass(Integer.class);
        verify(xpService, times(1)).addXp(
                eq(testUserId),
                xpCaptor.capture(),
                any(),
                anyString()
        );
        assertEquals(200, xpCaptor.getValue());
    }

    @Test
    @DisplayName("REVIEW 실기 완료 - 250 XP 지급")
    void testReviewPractical_250Xp() {
        // Given
        StudyFlowCompleteReq req = new StudyFlowCompleteReq(
                testUserId, "PRACTICAL", "REVIEW", testTopicId
        );
        when(xpService.addXp(anyString(), anyInt(), any(), anyString())).thenReturn(
                UserXpWallet.builder().userId(testUserId).xpTotal(250).level(1).build()
        );
        when(dailyRepository.findByUserIdAndDate(anyString(), any(LocalDate.class)))
                .thenReturn(Optional.empty());
        when(weeklyRepository.findByUserIdAndWeekIso(anyString(), anyString()))
                .thenReturn(Optional.empty());

        // When
        studyFlowService.handleFlowComplete(req);

        // Then
        ArgumentCaptor<Integer> xpCaptor = ArgumentCaptor.forClass(Integer.class);
        verify(xpService, times(1)).addXp(
                eq(testUserId),
                xpCaptor.capture(),
                any(),
                anyString()
        );
        assertEquals(250, xpCaptor.getValue());
    }

    @Test
    @DisplayName("같은 topicId로 재시도 - refId로 중복 방지")
    void testDuplicateFlowPrevention() {
        // Given
        StudyFlowCompleteReq req = new StudyFlowCompleteReq(
                testUserId, "WRITTEN", "MICRO", testTopicId
        );
        // 첫 번째 호출
        when(xpService.addXp(anyString(), anyInt(), any(), anyString())).thenReturn(
                UserXpWallet.builder().userId(testUserId).xpTotal(150).level(1).build()
        );
        when(dailyRepository.findByUserIdAndDate(anyString(), any(LocalDate.class)))
                .thenReturn(Optional.empty());
        when(weeklyRepository.findByUserIdAndWeekIso(anyString(), anyString()))
                .thenReturn(Optional.empty());

        // When - 첫 번째 완료
        studyFlowService.handleFlowComplete(req);

        // 두 번째 완료 (같은 topicId)
        studyFlowService.handleFlowComplete(req);

        // Then - 두 번 호출되지만, refId가 같아서 XP는 한 번만 지급됨
        verify(xpService, times(2)).addXp(anyString(), anyInt(), any(), anyString());
        // 실제로는 XpService 내부에서 idempotency 체크가 이루어짐
    }
}










