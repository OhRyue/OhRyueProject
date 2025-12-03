package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.dto.activity.ActivityDtos.*;
import com.OhRyue.certpilot.progress.service.ActivityService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

import static com.OhRyue.common.auth.AuthUserUtil.getCurrentUserId;

@Tag(name = "Progress - Activity", description = "학습 활동 기록 APIs")
@RestController
@RequestMapping("/api/progress/activity")
@RequiredArgsConstructor
public class ActivityController {

    private final ActivityService activityService;

    @Operation(summary = "오늘의 성과 요약")
    @GetMapping("/today-summary")
    public ResponseEntity<TodaySummaryDto> getTodaySummary() {
        String userId = getCurrentUserId();
        return ResponseEntity.ok(activityService.getTodaySummary(userId));
    }

    @Operation(summary = "최근 학습 기록 (최대 5개)")
    @GetMapping("/recent")
    public ResponseEntity<List<RecentActivityItemDto>> getRecentActivities() {
        String userId = getCurrentUserId();
        return ResponseEntity.ok(activityService.getRecentActivities(userId));
    }

    @Operation(summary = "활동 리스트 전체보기 (페이지네이션)")
    @GetMapping("/list")
    public ResponseEntity<Page<ActivityListItemDto>> getActivityList(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {
        String userId = getCurrentUserId();
        return ResponseEntity.ok(activityService.getActivityList(userId, page, size));
    }

    @Operation(summary = "활동 상세보기")
    @GetMapping("/{activityId}")
    public ResponseEntity<ActivityDetailDto> getActivityDetail(@PathVariable Long activityId) {
        String userId = getCurrentUserId();
        return ResponseEntity.ok(activityService.getActivityDetail(userId, activityId));
    }
}



