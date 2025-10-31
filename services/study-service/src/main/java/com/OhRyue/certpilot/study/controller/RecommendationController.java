package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.RecommendationDtos.*;
import com.OhRyue.certpilot.study.service.RecommendationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/study/reco")
@RequiredArgsConstructor
public class RecommendationController {

  private final RecommendationService svc;

  @Operation(summary = "약점 태그 Top-N", description = "정답률 낮은 태그 순으로 반환(전체 로그 기준)")
  @GetMapping("/weak-tags")
  public WeakTagsResp weakTags(@RequestParam String userId,
                               @RequestParam(defaultValue = "5") int topN,
                               @RequestParam(defaultValue = "3") int minTried) {
    return svc.weakTags(userId, topN, minTried);
  }

  @Operation(
      summary = "태그 기반 추천 퀴즈",
      description = "약점도 + 난이도 가중치 + 최근 오답 가중치로 출제, 일부는 무작위 탐색 비중으로 보강",
      responses = @ApiResponse(
          responseCode = "200",
          content = @Content(
              schema = @Schema(implementation = TagQuizSet.class),
              examples = @ExampleObject(value = """
          {
            "questions": [
              {
                "id": 701,
                "text": "OSI에서 IP는 어느 계층에 속하는가?",
                "difficulty": "NORMAL",
                "choices": [
                  {"label":"A","text":"물리"},
                  {"label":"B","text":"데이터링크"},
                  {"label":"C","text":"네트워크"},
                  {"label":"D","text":"전송"}
                ]
              }
            ]
          }
          """)
          )
      )
  )
  @PostMapping("/tag-quiz")
  public TagQuizSet tagQuiz(@Valid @RequestBody TagQuizReq req) {
    return svc.tagQuiz(req);
  }
}
