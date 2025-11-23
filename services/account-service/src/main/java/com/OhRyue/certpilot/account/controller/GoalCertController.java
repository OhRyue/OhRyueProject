package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.domain.UserGoalCert;
import com.OhRyue.certpilot.account.dto.GoalCertDtos.*;
import com.OhRyue.certpilot.account.service.GoalCertService;
import com.OhRyue.common.auth.AuthUserUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Account - GoalCert", description = "목표 자격증 관리 APIs")
@RestController
@RequestMapping("/api/account/goal")
@RequiredArgsConstructor
public class GoalCertController {

  private final GoalCertService goalCertService;

  /* -------- 목표 조회 -------- */
  @Operation(summary = "내 목표 자격증 조회")
  @GetMapping
  public ResponseEntity<GoalResponse> myGoal() {
    String userId;
    try {
      userId = AuthUserUtil.getCurrentUserId();
    } catch (IllegalStateException e) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    return goalCertService.getByUser(userId)
        .map(g -> ResponseEntity.ok(GoalResponse.builder()
            .id(g.getId())
            .userId(g.getUserId())
            .certId(g.getCertId())
            .targetExamMode(g.getTargetExamMode())
            .targetRoundId(g.getTargetRoundId())
            .ddayCached(g.getDdayCached())
            .build()))
        .orElse(ResponseEntity.noContent().build());
  }

  /* -------- 목표 설정/수정 -------- */
  @Operation(summary = "내 목표 자격증 설정/수정")
  @PutMapping
  public ResponseEntity<GoalResponse> upsertMyGoal(@RequestBody @Valid GoalUpsertRequest req) {
    String userId;
    try {
      userId = AuthUserUtil.getCurrentUserId();
    } catch (IllegalStateException e) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    UserGoalCert saved = goalCertService.upsert(
        UserGoalCert.builder()
            .userId(userId)
            .certId(req.getCertId())
            .targetExamMode(req.getTargetExamMode())
            .targetRoundId(req.getTargetRoundId())
            .build()
    );
    return ResponseEntity.ok(GoalResponse.builder()
        .id(saved.getId())
        .userId(saved.getUserId())
        .certId(saved.getCertId())
        .targetExamMode(saved.getTargetExamMode())
        .targetRoundId(saved.getTargetRoundId())
        .ddayCached(saved.getDdayCached())
        .build());
  }
}
