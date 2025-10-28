package com.OhRyue.certpilot.reco.web;

import com.OhRyue.certpilot.quiz.web.dto.QuickSessionDto;
import com.OhRyue.certpilot.reco.service.RecommendService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name="Recommend", description="약점 기반 다음 학습 추천")
@RestController
@RequestMapping("/reco")
@RequiredArgsConstructor
public class RecommendController {

  private final RecommendService svc;

  public record RecoResponse(List<String> tags, QuickSessionDto session) {}

  @Operation(summary = "다음 퀵퀴즈 추천", description = "EMA 낮은 태그와 최근 오답 태그 기반으로 추천. withSession=true면 즉시 세션 반환")
  @GetMapping("/next-quiz")
  public RecoResponse next(@RequestParam Long userId,
                           @RequestParam(defaultValue = "3") int tagCount,
                           @RequestParam(defaultValue = "true") boolean withSession) {
    var r = svc.recommendNext(userId, tagCount, withSession);
    return new RecoResponse(r.tags(), r.session());
  }
}
