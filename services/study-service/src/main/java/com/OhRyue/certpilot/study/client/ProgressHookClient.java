package com.OhRyue.certpilot.study.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

@FeignClient(name = "progress-service", path = "/api/progress")
public interface ProgressHookClient {

    /**
     * 개별 문제 제출용 Hook
     *  - question / tag 기반으로 세부 통계(XP, 리포트)에 반영
     */
    @PostMapping("/hook/submit")
    void submit(@RequestBody SubmitPayload payload);

    record SubmitPayload(
            String userId,
            String examMode,    // "WRITTEN" / "PRACTICAL"
            String questionType,// "OX" / "MCQ" / "SHORT" / "LONG"
            Boolean correct,
            Integer score,
            List<String> tags,
            String source       // "STUDY_SERVICE" 등
    ) {}

    /**
     * 학습 플로우(세트) 완료용 Hook
     *  - XP는 "세트 최초 1번" 규칙으로 progress-service 에서 처리
     *  - examMode: "WRITTEN" / "PRACTICAL"
     *  - flowType: "MICRO" / "REVIEW"
     *  - topicId:  필기 → 세부 토픽 id, 실기 Review → rootTopicId
     */
    @PostMapping("/hook/flow-complete")
    void flowComplete(@RequestBody FlowCompletePayload payload);

    record FlowCompletePayload(
            String userId,
            String examMode,  // "WRITTEN" / "PRACTICAL"
            String flowType,  // "MICRO" / "REVIEW"
            Long topicId
    ) {}
}
