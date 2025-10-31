package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.MicroDtos.*;
import com.OhRyue.certpilot.study.dto.ReviewDtos.ReviewSet;
import com.OhRyue.certpilot.study.dto.WrongReviewDtos.WrongSet;
import com.OhRyue.certpilot.study.service.StudyFlowService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/study")
@RequiredArgsConstructor
public class StudyMainController {

  private final StudyFlowService flow;

  /* ========= 메인학습: 개념 ========= */
  @Operation(summary = "개념 로드")
  @GetMapping("/main/concept/{topicId}")
  public ConceptResp concept(@PathVariable Long topicId) {
    return flow.loadConcept(topicId);
  }

  /* ========= 메인학습: 미니체크 ========= */
  @Operation(summary = "미니체크 세트(OX 5문)")
  @GetMapping("/main/mini/{topicId}")
  public MiniSet miniSet(@PathVariable Long topicId) {
    return flow.miniSet(topicId);
  }

  @Operation(summary = "미니체크 제출/채점(정답/오답 모두 해설, 오답 시 AI 해설 포함)")
  @PostMapping("/main/mini/submit")
  public MiniSubmitResp submitMini(@RequestBody MiniSubmitReq req) {
    return flow.submitMini(req);
  }

  /* ========= 메인학습: MCQ ========= */
  @Operation(summary = "MCQ 세트(미니체크 통과 필요)")
  @GetMapping("/main/mcq/{topicId}")
  public McqSet mcqSet(@PathVariable Long topicId,
                       @RequestParam String userId) {
    return flow.mcqSet(topicId, userId);
  }

  @Operation(summary = "MCQ 제출/채점(정답/오답 모두 해설, 오답 시 AI 해설 포함)")
  @PostMapping("/main/mcq/submit")
  public McqSubmitResp submitMcq(@RequestBody McqSubmitReq req) {
    return flow.submitMcq(req);
  }

  /* ========= 메인학습: 요약카드 ========= */
  @Operation(summary = "메인학습 요약(미니/MCQ 진행도)")
  @GetMapping("/main/summary")
  public SummaryResp summary(@RequestParam String userId,
                             @RequestParam Long topicId) {
    return flow.summary(userId, topicId);
  }

  /* ========= 리뷰(총정리) 20문 ========= */
  @Operation(summary = "리뷰(총정리) 20문 - 루트 토픽의 모든 하위 포함, MCQ만")
  @GetMapping("/flow/review/{rootTopicId}")
  public ReviewSet review(@PathVariable Long rootTopicId) {
    return flow.reviewSet(rootTopicId);
  }

  /* ========= 오답복습 세트 ========= */
  @Operation(summary = "오답복습 세트 - 최근 오답 기준, 같은 토픽에서 MCQ 우선")
  @GetMapping("/flow/wrong/{topicId}")
  public WrongSet wrong(@PathVariable Long topicId,
                        @RequestParam String userId,
                        @RequestParam(defaultValue = "5") int limit) {
    return flow.wrongSet(topicId, userId, Math.max(1, Math.min(20, limit)));
  }
}
