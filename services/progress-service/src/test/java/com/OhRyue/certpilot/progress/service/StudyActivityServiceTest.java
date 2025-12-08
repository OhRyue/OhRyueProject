package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.domain.ReportWeekly;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.dto.HookDtos.StudySubmitReq;
import com.OhRyue.certpilot.progress.repository.ReportDailyRepository;
import com.OhRyue.certpilot.progress.repository.ReportTagSkillRepository;
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
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@DisplayName("StudyActivityService 테스트 - 보조학습 XP 지급")
class StudyActivityServiceTest {

    @Mock
    private ReportDailyRepository dailyRepository;

    @Mock
    private ReportWeeklyRepository weeklyRepository;

    @Mock
    private ReportTagSkillRepository tagSkillRepository;

    @Mock
    private XpService xpService;

    @Mock
    private StreakService streakService;

    @Mock
    private RankService rankService;

    @InjectMocks
    private StudyActivityService studyActivityService;

    private String testUserId;

    @BeforeEach
    void setUp() {
        testUserId = "test-user-123";
    }

    @Test
    @DisplayName("보조학습 ASSIST - 정답 시 5 XP 지급")
    void testAssistCorrect_5Xp() {
        // Given
        StudySubmitReq req = new StudySubmitReq(
                testUserId, "WRITTEN", "MCQ", true, null,
                List.of("데이터베이스"), "ASSIST_WRITTEN_CATEGORY"
        );
        when(xpService.addXp(anyString(), anyInt(), any(), isNull())).thenReturn(
                UserXpWallet.builder().userId(testUserId).xpTotal(5).level(1).build()
        );
        when(dailyRepository.findByUserIdAndDate(anyString(), any(LocalDate.class)))
                .thenReturn(Optional.empty());
        when(weeklyRepository.findByUserIdAndWeekIso(anyString(), anyString()))
                .thenReturn(Optional.empty());

        // When
        studyActivityService.ingest(req);

        // Then
        ArgumentCaptor<Integer> xpCaptor = ArgumentCaptor.forClass(Integer.class);
        ArgumentCaptor<XpReason> reasonCaptor = ArgumentCaptor.forClass(XpReason.class);
        verify(xpService, times(1)).addXp(
                eq(testUserId),
                xpCaptor.capture(),
                reasonCaptor.capture(),
                isNull() // refId는 null (매번 지급)
        );
        assertEquals(5, xpCaptor.getValue());
        assertEquals(XpReason.ASSIST, reasonCaptor.getValue());
    }

    @Test
    @DisplayName("보조학습 ASSIST - 오답 시 0 XP (지급 안함)")
    void testAssistIncorrect_0Xp() {
        // Given
        StudySubmitReq req = new StudySubmitReq(
                testUserId, "WRITTEN", "MCQ", false, null,
                List.of("데이터베이스"), "ASSIST_WRITTEN_CATEGORY"
        );
        when(dailyRepository.findByUserIdAndDate(anyString(), any(LocalDate.class)))
                .thenReturn(Optional.empty());
        when(weeklyRepository.findByUserIdAndWeekIso(anyString(), anyString()))
                .thenReturn(Optional.empty());

        // When
        studyActivityService.ingest(req);

        // Then
        verify(xpService, never()).addXp(anyString(), anyInt(), any(), any()); // XP 지급 안됨
    }

    @Test
    @DisplayName("보조학습 - 정답 여러 번 해도 매번 5 XP 지급")
    void testAssistMultipleCorrect_Always5Xp() {
        // Given
        StudySubmitReq req = new StudySubmitReq(
                testUserId, "WRITTEN", "MCQ", true, null,
                List.of("데이터베이스"), "ASSIST_WRITTEN_CATEGORY"
        );
        when(xpService.addXp(anyString(), anyInt(), any(), isNull())).thenReturn(
                UserXpWallet.builder().userId(testUserId).xpTotal(5).level(1).build()
        );
        when(dailyRepository.findByUserIdAndDate(anyString(), any(LocalDate.class)))
                .thenReturn(Optional.empty());
        when(weeklyRepository.findByUserIdAndWeekIso(anyString(), anyString()))
                .thenReturn(Optional.empty());

        // When - 3번 연속 정답
        studyActivityService.ingest(req);
        studyActivityService.ingest(req);
        studyActivityService.ingest(req);

        // Then - 3번 모두 지급됨
        verify(xpService, times(3)).addXp(
                eq(testUserId),
                eq(5),
                eq(XpReason.ASSIST),
                isNull()
        );
    }

    @Test
    @DisplayName("메인학습 MICRO/REVIEW는 보조학습 XP 지급 안함")
    void testMainStudy_NoAssistXp() {
        // Given
        StudySubmitReq req = new StudySubmitReq(
                testUserId, "WRITTEN", "MCQ", true, null,
                List.of(), "MICRO_WRITTEN"
        );
        when(dailyRepository.findByUserIdAndDate(anyString(), any(LocalDate.class)))
                .thenReturn(Optional.empty());
        when(weeklyRepository.findByUserIdAndWeekIso(anyString(), anyString()))
                .thenReturn(Optional.empty());

        // When
        studyActivityService.ingest(req);

        // Then - ASSIST XP는 지급 안됨 (MICRO는 StudyFlowService에서 처리)
        verify(xpService, never()).addXp(anyString(), anyInt(), eq(XpReason.ASSIST), any());
    }
}










