package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.WrittenDtos.*;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSet;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSubmitReq;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSubmitResp;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalGradeOneReq;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalGradeOneResp;
import com.OhRyue.certpilot.study.dto.WrongRecapDtos.WrongRecapSet;
import com.OhRyue.certpilot.study.service.PracticalService;
import com.OhRyue.certpilot.study.service.WrittenService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "main-practical-controller", description = "실기: 개념/미니/세트/제출/단건채점/리뷰20/리뷰제출/리캡/요약")
@RestController
@RequestMapping("/api/study/practical")
@RequiredArgsConstructor
public class MainPracticalController {

    private final PracticalService practical;
    private final WrittenService flow; // 개념/미니, 오답요약 by-ids 재사용

    /* ====== 개념 ====== */
    @Operation(summary = "실기 개념 로드", description = "필기와 동일한 리치 컨텐츠 구조로 topicId의 개념을 반환합니다.")
    @GetMapping("/concept/{topicId}")
    public ConceptResp concept(@PathVariable Long topicId) {
        return flow.loadConcept(topicId);
    }

    /* ====== 미니체크 ====== */
    @Operation(summary = "실기 미니체크(OX 4문)", description = "실기 흐름에서도 OX 4문 세트를 제공합니다. (제출 시 AI 해설 없음)")
    @GetMapping("/mini/{topicId}")
    public MiniSet miniSet(@PathVariable Long topicId) {
        return flow.miniSet(topicId);
    }

    @Operation(summary = "실기 미니체크 제출/채점", description = "실기 미니체크는 AI 해설을 사용하지 않습니다. 정오/DB 해설만 저장/반환합니다.")
    @PostMapping("/mini/submit")
    public MiniSubmitResp submitMini(@RequestBody MiniSubmitReq req) {
        return flow.submitMini(req);
    }

    /* ====== 세트/제출/단건 즉시 채점 ====== */
    @Operation(summary = "실기 세트(단답/서술 혼합)", description = "topicId 범위에서 SHORT/LONG 유형을 섞어 5문 제공(고정).")
    @GetMapping("/set/{topicId}")
    public PracticalSet set(@PathVariable Long topicId,
                            @RequestParam(required = false) Integer count) {
        return practical.practicalSet(topicId, count);
    }

    @Operation(summary = "실기 제출/채점(AI)", description = "LLM 기반 점수(0~100)와 맞춤 해설을 반환하고 진행도에 반영합니다.")
    @PostMapping("/submit")
    public PracticalSubmitResp submit(@Valid @RequestBody PracticalSubmitReq req) {
        return practical.submitPractical(req);
    }

    @Operation(summary = "실기 단건 즉시 채점(AI)", description = "한 문제만 바로 AI 채점하고 DB/AI 해설을 반환합니다.")
    @PostMapping("/grade-one")
    public PracticalGradeOneResp gradeOne(@Valid @RequestBody PracticalGradeOneReq req) {
        return practical.gradeOnePractical(req);
    }

    /* ====== 리뷰(총정리) 20문 & 제출 ====== */
    @Operation(
            summary = "실기 리뷰(총정리) 20문(단답/서술)",
            description = """
      루트 토픽 이하 범위에서 SHORT/LONG 문제만 무작위 20문을 제공합니다.
      - 제출/채점은 /api/study/practical/review/submit 로 PracticalSubmitReq를 보내세요(AI 채점/해설).
      """
    )
    @GetMapping("/review/{rootTopicId}")
    public PracticalSet review(@PathVariable Long rootTopicId) {
        return practical.practicalReviewSet(rootTopicId);
    }

    @Operation(summary = "실기 리뷰(총정리) 제출", description = "리뷰(총정리) 응답을 제출합니다. 내부적으로 실기 제출 로직(AI 채점/해설)을 재사용합니다.")
    @PostMapping("/review/submit")
    public PracticalSubmitResp reviewSubmit(@Valid @RequestBody PracticalSubmitReq req) {
        return practical.submitPractical(req);
    }

    /* ====== 틀린문제 다시보기 ====== */
    @Operation(summary = "실기 오답 요약 보기(최근 오답 기반)", description = "최근 오답을 기준으로 '문제/내 답/정답(있는 경우)/DB 해설'을 보여줍니다.")
    @GetMapping("/wrong-recap/{topicId}")
    public WrongRecapSet wrongRecap(@PathVariable Long topicId,
                                    @RequestParam String userId,
                                    @RequestParam(defaultValue = "5") int limit) {
        return flow.wrongRecap(topicId, userId, Math.max(1, Math.min(20, limit)));
    }

    @Operation(
            summary = "실기 오답 보기(by-ids)",
            description = """
        제출 응답의 wrongQuestionIds를 그대로 전달하면, 해당 ID에 대한 요약을 반환합니다.
        - 예) GET /api/study/practical/flow/wrong-recap/by-ids?ids=1,5,9[&userId=me]
        - userId를 주면 '내 답'을 채워 반환합니다.
        """
    )
    @GetMapping("/flow/wrong-recap/by-ids")
    public WrongRecapSet wrongRecapByIds(@RequestParam String ids,
                                         @RequestParam(required = false) String userId) {
        return flow.wrongRecapByIds(ids, userId);
    }

    /* ========= 요약카드 ========= */
    @Operation(summary = "실기 요약(aiSummary + completed)", description = "실기 진행 요약(평균 점수/풀이 개수/aiSummary/완료여부)를 반환합니다.")
    @GetMapping("/summary")
    public SummaryResp summary(@RequestParam String userId,
                               @RequestParam Long topicId) {
        return practical.summary(userId, topicId);
    }
}
