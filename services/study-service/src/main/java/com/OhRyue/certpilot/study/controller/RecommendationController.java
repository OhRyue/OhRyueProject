package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.RecommendationDtos.*;
import com.OhRyue.certpilot.study.service.RecommendationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

@Tag(name = "Recommendation(추천/약점)")
@RestController
@RequestMapping("/api/study/reco")
@RequiredArgsConstructor
public class RecommendationController {

  private final RecommendationService reco;

  private String currentUserId() {
    Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
    return authentication.getName();
  }

  @Operation(summary = "약점 태그 Top-N 조회")
  @GetMapping("/weak-tags")
  public WeakTagsResp weakTags(@RequestParam(defaultValue = "6") int topN,
                               @RequestParam(defaultValue = "3") int minTried) {
    String userId = currentUserId();
    return reco.weakTags(userId, topN, minTried);
  }

  @Operation(summary = "태그 기반 추천 퀴즈")
  @PostMapping("/tag-quiz")
  public TagQuizSet tagQuiz(@RequestBody @Valid TagQuizReq req) {
    return reco.tagQuiz(req);
  }
}
