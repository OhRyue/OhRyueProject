package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.WrittenDtos.*;
import com.OhRyue.certpilot.study.dto.ReviewDtos.ReviewSet;
import com.OhRyue.certpilot.study.dto.WrongRecapDtos.WrongRecapSet;
import com.OhRyue.certpilot.study.service.WrittenService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/study/written")
@RequiredArgsConstructor
public class MainWrittenController {

  private final WrittenService written;

  /* ========= 필기학습: 개념 ========= */
  @Operation(
      summary = "개념 로드",
      description = """
        지정한 topicId의 개념을 반환합니다.
        - 섹션 배열은 중요도(★0~3), 소제목/코드, 블록(문단/리스트/이미지/표)로 구성됩니다.
        - 이미지 블록은 url/alt/caption, 표는 headers/rows를 포함합니다.
        """
  )
  @GetMapping("/concept/{topicId}")
  public ConceptResp concept(@PathVariable Long topicId) {
    return written.loadConcept(topicId);
  }

  /* ========= 메인학습: 미니체크 ========= */
  @Operation(
      summary = "미니체크 세트(OX 4문)",
      description = "지정한 topicId 범위에서 OX 문제 4문을 무작위로 제공합니다."
  )
  @GetMapping("/mini/{topicId}")
  public MiniSet miniSet(@PathVariable Long topicId) {
    return written.miniSet(topicId);
  }

  @Operation(
      summary = "미니체크 제출/채점",
      description = """
        미니체크(OX) 사용자의 답안을 채점하고 결과를 저장합니다.
        - 각 문항에 대해 정답/오답 여부, 기본 해설(explanation)을 제공합니다.
        - 응답에 wrongQuestionIds(오답 문항 ID 배열)를 포함합니다.
        """
  )
  @PostMapping("/mini/submit")
  public MiniSubmitResp submitMini(@RequestBody MiniSubmitReq req) {
    return written.submitMini(req);
  }

  /* ========= 메인학습: 문제세트(필기=객관식) ========= */
  @Operation(
      summary = "문제세트(객관식 5문)",
      description = """
        지정한 topicId 범위에서 객관식(MCQ) 5문을 무작위로 제공합니다.
        - 미니체크 통과 여부와 무관하게 세트 제공 자체는 가능하나, 프런트는 단계 규칙에 따라 '미니체크 제출 후에만' 접근시키는 UI 제어를 권장합니다.
        """
  )
  @GetMapping("/mcq/{topicId}")
  public McqSet mcqSet(@PathVariable Long topicId,
                       @RequestParam(required = false) String userId) {
    return written.mcqSet(topicId, userId);
  }

  @Operation(
      summary = "MCQ 제출/채점(오답 시 AI 해설 포함)",
      description = """
        객관식(MCQ) 사용자의 답안을 채점하고 결과를 저장합니다.
        - 각 문항마다 정오/정답 라벨(correctLabel), 기본 해설, 오답 시 AI 맞춤 해설을 제공합니다.
        - 응답에 wrongQuestionIds(오답 문항 ID 배열)를 포함합니다.
        """
  )
  @PostMapping("/mcq/submit")
  public McqSubmitResp submitMcq(@Valid @RequestBody McqSubmitReq req) {
    return written.submitMcq(req);
  }

  /* ========= 리뷰(총정리) 20문 ========= */
  @Operation(
      summary = "리뷰(총정리) 20문 - 객관식",
      description = """
        루트 토픽(rootTopicId) 이하 모든 하위 토픽을 포함한 범위에서 객관식 20문제를 무작위로 제공합니다.
        - 제출/채점 API는 MCQ 제출 API를 그대로 재사용할 수 있습니다.
        """
  )
  @GetMapping("/review/{rootTopicId}")
  public ReviewSet review(@PathVariable Long rootTopicId) {
    return written.reviewSet(rootTopicId);
  }

  @Operation(
      summary = "리뷰(총정리) 제출",
      description = "리뷰(총정리) 응답을 제출합니다."
  )
  @PostMapping("/review/submit")
  public McqSubmitResp reviewSubmit(@Valid @RequestBody McqSubmitReq req) {
    return written.reviewSubmitWritten(req);
  }

  /* ========= 틀린문제 다시보기 ========= */
  @Operation(
      summary = "오답 보기(최근 오답 기반)",
      description = """
      최근 오답을 기준으로 같은 토픽의 문항을 모아 '문제/내 답/정답/DB 해설'을 보여줍니다(재풀이 아님).
      - limit는 1~20 범위를 권장(기본 5).
      """
  )
  @GetMapping("/wrong-recap/{topicId}")
  public WrongRecapSet wrongRecap(@PathVariable Long topicId,
                                  @RequestParam String userId,
                                  @RequestParam(defaultValue = "5") int limit) {
    return written.wrongRecap(topicId, userId, Math.max(1, Math.min(20, limit)));
  }

  @Operation(
      summary = "오답 보기(by-ids)",
      description = """
        제출 응답의 wrongQuestionIds를 그대로 전달하면, 해당 ID에 대한 요약을 반환합니다.
        - 예) GET /api/study/written/flow/wrong-recap/by-ids?ids=1,5,9
        - userId를 함께 주면 '내 답'을 채워 반환합니다(선택).
        """
  )
  @GetMapping("/flow/wrong-recap/by-ids")
  public WrongRecapSet wrongRecapByIds(@RequestParam String ids,
                                       @RequestParam(required = false) String userId) {
    return written.wrongRecapByIds(ids, userId);
  }

  /* ========= 메인학습: 요약카드 ========= */
  @Operation(
      summary = "메인학습 요약(미니/MCQ 진행도 + AI 학습 요약)",
      description = """
        미니체크/객관식 진행 지표를 집계해 요약을 반환합니다.
        - miniTotal/miniCorrect/miniPassed, mcqTotal/mcqCorrect, aiSummary, completed
        """
  )
  @GetMapping("/summary")
  public SummaryResp summary(@RequestParam String userId,
                             @RequestParam Long topicId) {
    return written.summary(userId, topicId);
  }

  /* ========= 미니체크 즉시 채점 ========= */
  @Operation(summary = "미니체크 즉시 채점", description = "한 문제만 바로 채점하고 DB 해설을 반환합니다.")
  @PostMapping("/mini/grade-one")
  public MiniGradeOneResp gradeOneMini(@RequestBody MiniGradeOneReq req) {
    return written.gradeOneMini(req);
  }

  /* ========= MCQ 즉시 채점 ========= */
  @Operation(summary = "MCQ 즉시 채점(오답 시 AI 해설 포함)", description = "한 문제만 즉시 채점합니다.")
  @PostMapping("/mcq/grade-one")
  public McqGradeOneResp gradeOneMcq(@RequestBody McqGradeOneReq req) {
    return written.gradeOneMcq(req);
  }
}
