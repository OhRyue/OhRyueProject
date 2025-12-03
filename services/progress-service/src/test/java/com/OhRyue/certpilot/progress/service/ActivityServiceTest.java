package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.ProgressActivity;
import com.OhRyue.certpilot.progress.domain.enums.ActivityGroup;
import com.OhRyue.certpilot.progress.domain.enums.AssistType;
import com.OhRyue.certpilot.progress.domain.enums.BattleType;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import com.OhRyue.certpilot.progress.domain.enums.MainType;
import com.OhRyue.certpilot.progress.dto.activity.ActivityDtos.*;
import com.OhRyue.certpilot.progress.feign.StudyInternalClient;
import com.OhRyue.certpilot.progress.feign.VersusInternalClient;
import com.OhRyue.certpilot.progress.feign.dto.MatchDetailDto;
import com.OhRyue.certpilot.progress.feign.dto.StudySessionDetailDto;
import com.OhRyue.certpilot.progress.repository.ProgressActivityRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@DisplayName("ActivityService 테스트")
class ActivityServiceTest {

    @Mock
    private ProgressActivityRepository activityRepository;

    @Mock
    private StudyInternalClient studyInternalClient;

    @Mock
    private VersusInternalClient versusInternalClient;

    @InjectMocks
    private ActivityService activityService;

    private String testUserId;
    private LocalDateTime now;

    @BeforeEach
    void setUp() {
        testUserId = "test-user-123";
        now = LocalDateTime.now();
    }

    @Test
    @DisplayName("오늘의 성과 요약 - 정상 조회")
    void testGetTodaySummary() {
        // Given
        LocalDateTime startOfDay = now.toLocalDate().atStartOfDay();
        LocalDateTime endOfDay = now.toLocalDate().atTime(23, 59, 59);

        ProgressActivity activity1 = createActivity(1L, 10, 8, 80.0, 10);
        ProgressActivity activity2 = createActivity(2L, 5, 4, 80.0, 5);

        when(activityRepository.findByUserIdAndFinishedAtBetween(
                eq(testUserId), any(LocalDateTime.class), any(LocalDateTime.class)))
                .thenReturn(List.of(activity1, activity2));

        // When
        TodaySummaryDto result = activityService.getTodaySummary(testUserId);

        // Then
        assertNotNull(result);
        assertEquals(15, result.solvedQuestionCount()); // 10 + 5
        assertEquals(15, result.gainedXp()); // 10 + 5
        // 정답률: (8 + 4) / (10 + 5) * 100 = 80.0
        assertEquals(80.0, result.accuracyPct(), 0.1);
        assertTrue(result.studyTimeMinutes() >= 0); // 시간은 0 이상
    }

    @Test
    @DisplayName("최근 학습 기록 5개 조회")
    void testGetRecentActivities() {
        // Given
        ProgressActivity activity1 = createActivity(1L, 10, 8, 80.0, 100);
        ProgressActivity activity2 = createActivity(2L, 5, 4, 80.0, 50);

        when(activityRepository.findTop5ByUserIdOrderByFinishedAtDesc(testUserId))
                .thenReturn(List.of(activity1, activity2));

        // When
        List<RecentActivityItemDto> result = activityService.getRecentActivities(testUserId);

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());
        assertNotNull(result.get(0).displayText());
    }

    @Test
    @DisplayName("활동 리스트 전체보기 - 페이지네이션")
    void testGetActivityList() {
        // Given
        ProgressActivity activity = createActivity(1L, 10, 8, 80.0, 100);
        Page<ProgressActivity> page = new PageImpl<>(List.of(activity), PageRequest.of(0, 20), 1);

        when(activityRepository.findByUserIdOrderByFinishedAtDesc(eq(testUserId), any()))
                .thenReturn(page);

        // When
        Page<ActivityListItemDto> result = activityService.getActivityList(testUserId, 0, 20);

        // Then
        assertNotNull(result);
        assertEquals(1, result.getTotalElements());
        assertEquals(1, result.getContent().size());
    }

    @Test
    @DisplayName("활동 상세보기 - study-service에서 조회")
    void testGetActivityDetail_StudyService() {
        // Given
        Long activityId = 1L;
        ProgressActivity activity = ProgressActivity.builder()
                .id(activityId)
                .userId(testUserId)
                .activityGroup(ActivityGroup.MAIN)
                .mainType(MainType.MICRO)
                .mode(ExamMode.WRITTEN)
                .sourceService("study")
                .sourceSessionId(100L)
                .questionCount(10)
                .correctCount(8)
                .accuracyPct(80.0)
                .xpGained(150)
                .startedAt(now.minusHours(1))
                .finishedAt(now)
                .build();

        when(activityRepository.findById(activityId)).thenReturn(Optional.of(activity));

        StudySessionDetailDto sessionDetail = new StudySessionDetailDto(
                100L,
                testUserId,
                List.of(
                        new StudySessionDetailDto.QuestionDetailDto(
                                1, 1001L, "MCQ", "문제1", "1번", "1번", true,
                                now, 5000L, 100
                        ),
                        new StudySessionDetailDto.QuestionDetailDto(
                                2, 1002L, "MCQ", "문제2", "2번", "3번", false,
                                now, 6000L, 0
                        )
                )
        );

        when(studyInternalClient.getSessionDetail(100L, testUserId))
                .thenReturn(sessionDetail);

        // When
        ActivityDetailDto result = activityService.getActivityDetail(testUserId, activityId);

        // Then
        assertNotNull(result);
        assertNotNull(result.header());
        assertEquals(ActivityGroup.MAIN, result.header().activityGroup());
        assertEquals(2, result.questions().size());
        assertEquals("문제1", result.questions().get(0).stem());
    }

    @Test
    @DisplayName("활동 상세보기 - versus-service에서 조회")
    void testGetActivityDetail_VersusService() {
        // Given
        Long activityId = 2L;
        ProgressActivity activity = ProgressActivity.builder()
                .id(activityId)
                .userId(testUserId)
                .activityGroup(ActivityGroup.BATTLE)
                .battleType(BattleType.DUEL_CATEGORY)
                .mode(ExamMode.WRITTEN)
                .sourceService("versus")
                .sourceSessionId(200L)
                .questionCount(10)
                .correctCount(7)
                .accuracyPct(70.0)
                .finalRank(1)
                .xpGained(200)
                .startedAt(now.minusHours(1))
                .finishedAt(now)
                .build();

        when(activityRepository.findById(activityId)).thenReturn(Optional.of(activity));

        MatchDetailDto matchDetail = new MatchDetailDto(
                200L,
                testUserId,
                List.of(
                        new MatchDetailDto.QuestionDetailDto(
                                1, 1, 2001L, "MCQ", "배틀 문제1", "1번", "1번", true,
                                now, 3000L, 100
                        )
                )
        );

        when(versusInternalClient.getMatchDetail(200L, testUserId))
                .thenReturn(matchDetail);

        // When
        ActivityDetailDto result = activityService.getActivityDetail(testUserId, activityId);

        // Then
        assertNotNull(result);
        assertNotNull(result.header());
        assertEquals(ActivityGroup.BATTLE, result.header().activityGroup());
        assertEquals(1, result.questions().size());
    }

    @Test
    @DisplayName("활동 생성 - 정상 생성")
    void testCreateActivity() {
        // Given
        ProgressActivityCreateReq req = new ProgressActivityCreateReq(
                testUserId,
                ActivityGroup.MAIN,
                MainType.MICRO,
                null,
                null,
                ExamMode.WRITTEN,
                100L,
                "데이터베이스 기초",
                null,
                null,
                10,
                8,
                80.0,
                null,
                150,
                "study",
                100L,
                now.minusHours(1),
                now
        );

        ProgressActivity savedActivity = ProgressActivity.builder()
                .id(1L)
                .userId(testUserId)
                .activityGroup(ActivityGroup.MAIN)
                .mainType(MainType.MICRO)
                .mode(ExamMode.WRITTEN)
                .questionCount(10)
                .correctCount(8)
                .accuracyPct(80.0)
                .xpGained(150)
                .sourceService("study")
                .sourceSessionId(100L)
                .startedAt(now.minusHours(1))
                .finishedAt(now)
                .build();

        when(activityRepository.save(any(ProgressActivity.class))).thenReturn(savedActivity);

        // When
        ProgressActivity result = activityService.createActivity(req);

        // Then
        assertNotNull(result);
        assertEquals(testUserId, result.getUserId());
        assertEquals(ActivityGroup.MAIN, result.getActivityGroup());
        assertEquals(MainType.MICRO, result.getMainType());
        verify(activityRepository, times(1)).save(any(ProgressActivity.class));
    }

    @Test
    @DisplayName("활동 상세보기 - 잘못된 userId 접근 시 예외 발생")
    void testGetActivityDetail_WrongUserId() {
        // Given
        Long activityId = 1L;
        ProgressActivity activity = ProgressActivity.builder()
                .id(activityId)
                .userId("other-user")
                .activityGroup(ActivityGroup.MAIN)
                .mode(ExamMode.WRITTEN)
                .sourceService("study")
                .sourceSessionId(100L)
                .build();

        when(activityRepository.findById(activityId)).thenReturn(Optional.of(activity));

        // When & Then
        assertThrows(IllegalArgumentException.class, () -> {
            activityService.getActivityDetail(testUserId, activityId);
        });
    }

    // Helper methods
    private ProgressActivity createActivity(Long id, int questionCount, int correctCount, 
                                           Double accuracyPct, Integer xpGained) {
        return ProgressActivity.builder()
                .id(id)
                .userId(testUserId)
                .activityGroup(ActivityGroup.MAIN)
                .mainType(MainType.MICRO)
                .mode(ExamMode.WRITTEN)
                .questionCount(questionCount)
                .correctCount(correctCount)
                .accuracyPct(accuracyPct)
                .xpGained(xpGained)
                .sourceService("study")
                .sourceSessionId(id * 100L)
                .startedAt(now.minusHours(1))
                .finishedAt(now)
                .build();
    }
}

