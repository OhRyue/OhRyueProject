package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.MicroDtos.*;
import com.OhRyue.certpilot.study.dto.ReviewDtos.ReviewSet;
import com.OhRyue.certpilot.study.dto.WrongReviewDtos.WrongSet;
import com.OhRyue.certpilot.study.service.StudyFlowService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/study")
@RequiredArgsConstructor
public class MainWrittenController {

  private final StudyFlowService flow;

  /* ========= 필기학습: 개념 ========= */
  /* 개념 (리치 컨텐츠) */
  @Operation(summary = "개념 로드", description = "토픽의 개념 섹션/블록(문단/리스트/이미지/표)을 조회합니다.")
  @ApiResponse(responseCode = "200", description = "OK",
      content = @Content(schema = @Schema(implementation = ConceptResp.class),
          examples = @ExampleObject(name="ConceptResp 예시", value="""
            {
              "topicId": 1110,
              "title": "현행 시스템 분석",
              "sections": [
                {
                  "orderNo": 1,
                  "subCode": "1.1.1.1",
                  "title": "플랫폼 기능 분석",
                  "importance": 2,
                  "blocks": [
                    {"type":"heading","text":"(1) 플랫폼의 개념"},
                    {"type":"list","items":["플랫폼은 애플리케이션을 구동시키는데 필요한 소프트웨어 환경이다.","..."]},
                    {"type":"heading","text":"(2) 플랫폼 성능 특성 분석"},
                    {"type":"paragraph","text":"플랫폼 성능 분석을 통해 ..."},
                    {"type":"table","caption":"플랫폼 성능 특성 측정 항목",
                     "headers":["항목","설명"],
                     "rows":[
                       ["경과시간","작업 시작~종료까지의 총 소요시간"],
                       ["사용률","자원 사용 비율"],
                       ["응답시간","요청~첫 응답까지의 시간"],
                       ["가용성","서비스 가능한 시간 비율"]
                     ]},
                    {"type":"image","url":"https://cdn.example.com/concepts/platform_metrics.png",
                     "alt":"플랫폼 성능 지표 개념도","caption":"성능 지표 개념도"}
                  ]
                },
                {
                  "orderNo": 2,
                  "subCode": "1.1.1.2",
                  "title": "운영체제 분석",
                  "importance": 1,
                  "blocks": [ {"type":"paragraph","text":"..."} ]
                }
              ]
            }
          """)))
  @GetMapping("/concept/{topicId}")
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
