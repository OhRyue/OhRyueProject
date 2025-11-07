package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.domain.UserGoalCert;
import com.OhRyue.certpilot.account.dto.GoalCertDtos.*;
import com.OhRyue.certpilot.account.service.GoalCertService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/account/goal")
@RequiredArgsConstructor
public class GoalCertController {

  private final GoalCertService goalCertService;

  @GetMapping("/me")
  public ResponseEntity<GoalResponse> myGoal(Authentication auth) {
    String userId = auth.getName();
    return goalCertService.getByUser(userId)
        .map(g -> ResponseEntity.ok(GoalResponse.builder()
            .id(g.getId())
            .userId(g.getUserId())
            .certId(g.getCertId())
            .targetExamMode(g.getTargetExamMode())
            .targetRoundId(g.getTargetRoundId())
            .ddayCached(g.getDdayCached())
            .build()))
        .orElse(ResponseEntity.ok().body(null));
  }

  @PutMapping("/me")
  public ResponseEntity<GoalResponse> upsertMyGoal(Authentication auth,
                                                   @RequestBody GoalUpsertRequest req) {
    String userId = auth.getName();
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
