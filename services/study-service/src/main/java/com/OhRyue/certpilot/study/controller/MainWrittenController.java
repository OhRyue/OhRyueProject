package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.MicroDtos.*;
import com.OhRyue.certpilot.study.dto.ReviewDtos.ReviewSet;
import com.OhRyue.certpilot.study.dto.WrongRecapDtos;
import com.OhRyue.certpilot.study.service.WrittenService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/study")
@RequiredArgsConstructor
public class MainWrittenController {

    private final WrittenService flow;

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
        return flow.loadConcept(topicId);
    }

    /* ========= 메인학습: 미니체크 ========= */
    @Operation(
            summary = "미니체크 세트(OX 5문)",
            description = """
        지정한 topicId 범위에서 OX 문제 5문을 무작위로 제공합니다.
        """
    )
    @GetMapping("/main/mini/{topicId}")
    public MiniSet miniSet(@PathVariable Long topicId) {
        return flow.miniSet(topicId);
    }

    @Operation(
            summary = "미니체크 제출/채점",
            description = """
        미니체크(OX) 사용자의 답안을 채점하고 결과를 저장합니다.
        - 각 문항에 대해 정답/오답 여부, 기본 해설(explanation)을 제공합니다.
        - 미니체크 결과는 진행도(UserProgress)에 반영되며, 정답률 등 요약 카드에서 활용됩니다.
        """
    )
    @PostMapping("/main/mini/submit")
    public MiniSubmitResp submitMini(@RequestBody MiniSubmitReq req) {
        return flow.submitMini(req);
    }

    /* ========= 메인학습: MCQ ========= */
    @Operation(
            summary = "MCQ 세트(객관식)",
            description = """
        지정한 topicId 범위에서 객관식(MCQ) 문제를 무작위로 제공합니다.
        - 미니체크 통과 여부와 무관하게 세트 제공 자체는 가능하지만, 프런트는 단계 규칙에 따라 '미니체크 제출 후에만' 접근시키는 UI 제어를 권장합니다.
        - 각 문항은 보기(label/text) 배열과 이미지(imageUrl, 선택)를 포함할 수 있습니다.
        """
    )
    @GetMapping("/main/mcq/{topicId}")
    public McqSet mcqSet(@PathVariable Long topicId,
                         @RequestParam String userId) {
        return flow.mcqSet(topicId, userId);
    }

    @Operation(
            summary = "MCQ 제출/채점(오답 시 AI 해설 포함)",
            description = """
        객관식(MCQ) 사용자의 답안을 채점하고 결과를 저장합니다.
        - 각 문항마다 정오 및 정답 라벨(correctLabel), 기본 해설, 오답 시 AI 맞춤 해설을 제공합니다.
        """
    )
    @PostMapping("/main/mcq/submit")
    public McqSubmitResp submitMcq(@RequestBody McqSubmitReq req) {
        return flow.submitMcq(req);
    }

    /* ========= 메인학습: 요약카드 ========= */
    @Operation(
            summary = "메인학습 요약(미니/MCQ 진행도 + AI 학습 요약)",
            description = """
        미니체크/객관식 진행 지표를 집계해 요약을 반환합니다.
        - 반환 필드: miniTotal/miniCorrect/miniPassed, mcqTotal/mcqCorrect, aiSummary(LLM 실패 시 폴백 문구), completed(학습 완료 여부).
        - '문제를 제출하지 않으면 다음 단계로 넘어갈 수 없음' 정책에서, completed는 '각 단계가 최소 1회 이상 제출되었는지'로 판정됩니다.
        """
    )
    @GetMapping("/main/summary")
    public SummaryResp summary(@RequestParam String userId,
                               @RequestParam Long topicId) {
        return flow.summary(userId, topicId);
    }

    /* ========= 리뷰(총정리) 20문 ========= */
    @Operation(
            summary = "리뷰(총정리) 20문 - MCQ",
            description = """
        루트 토픽(rootTopicId) 이하 모든 하위 토픽을 포함한 범위에서 객관식 20문제를 무작위로 제공합니다.
        - 세부/세세항목을 통합한 총정리 세트로, 약점 파악과 복습에 사용합니다.
        - 제출/채점 API는 프런트에서 MCQ 제출 API를 그대로 재사용할 수 있습니다.
        """
    )
    @GetMapping("/flow/review/{rootTopicId}")
    public ReviewSet review(@PathVariable Long rootTopicId) {
        return flow.reviewSet(rootTopicId);
    }

    /* ========= 오답복습 세트 ========= */
    @Operation(
            summary = "오답 보기",
            description = """
      최근 오답을 기준으로 같은 토픽의 문항을 모아 '문제/내 답/정답/DB 해설'을 보여줍니다.
      - limit는 1~20 범위를 권장(기본 5).
      """
    )
    @GetMapping("/flow/wrong-recap/{topicId}")
    public WrongRecapDtos.WrongRecapSet wrongRecap(@PathVariable Long topicId,
                                                   @RequestParam String userId,
                                                   @RequestParam(defaultValue = "5") int limit) {
        return flow.wrongRecap(topicId, userId, Math.max(1, Math.min(20, limit)));
    }
}
