package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.domain.UserRankScore;
import com.OhRyue.certpilot.progress.service.RankService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

import static com.OhRyue.common.auth.AuthUserUtil.getCurrentUserId;

@Tag(name = "Progress - Rank", description = "랭킹/점수 APIs")
@RestController
@RequestMapping("/api/progress/rank")
@RequiredArgsConstructor
public class RankController {

  private final RankService rank;

  @Operation(summary = "사용자 랭킹 점수 재계산(xp 기반)")
  @PostMapping("/recompute")
  public UserRankScore recompute() {
    String userId = getCurrentUserId();
    return rank.recomputeForUser(userId);
  }

  @Operation(summary = "Top-N 랭킹 조회")
  @GetMapping("/top")
  public List<UserRankScore> top(@RequestParam(defaultValue = "5") int limit) {
    return rank.topN(Math.max(1, Math.min(limit, 10)));
  }
}
