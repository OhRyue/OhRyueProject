package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.RecommendationDtos.*;
import com.OhRyue.certpilot.study.service.RecommendationService;
import com.OhRyue.common.auth.AuthUserUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Recommendation(추천/약점)")
@RestController
@RequestMapping("/api/study/reco")
@RequiredArgsConstructor
public class RecommendationController {

  private final RecommendationService reco;

  @Operation(summary = "약점 태그 Top-N 조회")
  @GetMapping("/weak-tags")
  public WeakTagsResp weakTags(@RequestParam(defaultValue = "6") int topN,
                               @RequestParam(defaultValue = "3") int minTried) {
    String userId = AuthUserUtil.getCurrentUserId();
    return reco.weakTags(topN, minTried);
  }

  @Operation(summary = "태그 기반 추천 퀴즈")
  @PostMapping("/tag-quiz")
  public TagQuizSet tagQuiz(@RequestBody @Valid TagQuizReq req) {
    // userId는 RecommendationService 내부에서 AuthUserUtil로 조회
    return reco.tagQuiz(req);
  }
}
