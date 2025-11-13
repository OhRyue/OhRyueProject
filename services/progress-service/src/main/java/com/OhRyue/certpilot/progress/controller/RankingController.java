package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.domain.enums.RankScope;
import com.OhRyue.certpilot.progress.dto.rank.RankDtos;
import com.OhRyue.certpilot.progress.service.BadgeService;
import com.OhRyue.certpilot.progress.service.LeaderboardService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Progress - Ranking", description = "랭킹 및 배지 조회 API")
@RestController
@RequestMapping("/api/progress")
@RequiredArgsConstructor
@Validated
public class RankingController {

  private final LeaderboardService leaderboardService;
  private final BadgeService badgeService;

  /* -------- 랭킹 -------- */
  @Operation(summary = "랭킹 조회", description = "scope: OVERALL/WEEKLY/HALL_OF_FAME/FRIENDS")
  @GetMapping("/rankings/{scope}")
  public RankDtos.LeaderboardResponse leaderboard(@PathVariable RankScope scope,
                                                  @RequestParam @NotBlank String userId,
                                                  @RequestParam(required = false) String reference) {
    return leaderboardService.leaderboard(scope, userId, reference);
  }

  @Operation(summary = "랭킹 히스토리 조회", description = "저장된 스냅샷이 없으면 즉시 재계산한 값을 반환합니다.")
  @GetMapping("/rankings/{scope}/history")
  public RankDtos.LeaderboardHistoryResponse history(@PathVariable RankScope scope,
                                                     @RequestParam(required = false) String reference) {
    return leaderboardService.history(scope, reference);
  }

  @Operation(summary = "랭킹 스냅샷 재계산", description = "관리자 전용 – 지정된 scope와 기준일로 스냅샷을 갱신합니다.")
  @PostMapping("/rankings/recompute")
  public void recompute(@Valid @RequestBody RankDtos.LeaderboardRecomputeRequest request) {
    leaderboardService.recompute(request.scope(), request.referenceDate());
  }

  /* -------- 배지 -------- */
  @Operation(summary = "사용자 배지 현황 조회")
  @GetMapping("/badges")
  public RankDtos.BadgeStatusResponse badges(@RequestParam @NotBlank String userId) {
    return badgeService.status(userId);
  }

  @Operation(summary = "배지 조건 재평가", description = "사용자의 최신 데이터를 기준으로 배지를 다시 계산합니다.")
  @PostMapping("/badges/evaluate")
  public RankDtos.BadgeStatusResponse evaluate(@Valid @RequestBody RankDtos.BadgeEvaluateRequest request) {
    return badgeService.evaluate(request.userId());
  }
}

