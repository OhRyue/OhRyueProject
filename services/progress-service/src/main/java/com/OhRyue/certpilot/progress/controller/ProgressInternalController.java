package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.domain.ProgressActivity;
import com.OhRyue.certpilot.progress.dto.activity.ActivityDtos.ProgressActivityCreateReq;
import com.OhRyue.certpilot.progress.service.ActivityService;
import com.OhRyue.certpilot.progress.service.StoreService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Progress - Internal", description = "내부 서비스 전용 API")
@RestController
@RequestMapping("/api/progress/internal")
@RequiredArgsConstructor
public class ProgressInternalController {

  private final StoreService storeService;
  private final ActivityService activityService;

  @Operation(summary = "사용자 기본 인벤토리 초기화")
  @PostMapping("/inventory/initialize")
  public ResponseEntity<String> initializeDefaultInventory(
      @RequestParam("userId") String userId) {
    storeService.initializeDefaultInventory(userId);
    return ResponseEntity.ok("OK");
  }

  @Operation(summary = "사용자 아이템 보유 여부 확인")
  @GetMapping("/inventory/check")
  public ResponseEntity<Boolean> checkItemOwned(
      @RequestParam("userId") String userId,
      @RequestParam("itemId") Long itemId) {
    boolean owned = storeService.hasItem(userId, itemId);
    return ResponseEntity.ok(owned);
  }

  @Operation(summary = "학습 활동 기록 생성 (study-service, versus-service에서 호출)")
  @PostMapping("/activities")
  public ResponseEntity<ProgressActivity> createActivity(@RequestBody ProgressActivityCreateReq req) {
    ProgressActivity activity = activityService.createActivity(req);
    return ResponseEntity.ok(activity);
  }
}

