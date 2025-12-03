package com.OhRyue.certpilot.study.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(
    name = "progress-service",
    url = "${PROGRESS_SERVICE_URL:http://progress-service:8083}"
)
public interface ProgressActivityClient {

    /**
     * 학습 활동 기록 생성
     * study-service에서 세션 종료 시 progress-service에 Activity를 생성하도록 호출
     * 내부 API이므로 service-to-service 직접 호출 (gateway를 거치지 않음)
     */
    @PostMapping("/api/progress/internal/activities")
    void createActivity(@RequestBody ProgressActivityCreateReq req);

    record ProgressActivityCreateReq(
            String userId,
            String activityGroup,  // MAIN, ASSIST, BATTLE
            String mainType,       // MICRO, REVIEW (MAIN일 때만)
            String assistType,     // CATEGORY, WEAKNESS, DIFFICULTY (ASSIST일 때만)
            String battleType,     // DUEL_CATEGORY, DUEL_DIFFICULTY, TOURNAMENT, GOLDENBELL (BATTLE일 때만)
            String mode,           // WRITTEN, PRACTICAL
            Long topicId,
            String topicName,
            String weaknessTagName,
            String difficulty,     // EASY, NORMAL, HARD
            Integer questionCount,
            Integer correctCount,
            Double accuracyPct,
            Integer finalRank,
            Integer xpGained,
            String sourceService,  // "study"
            Long sourceSessionId,  // study_session.id
            java.time.LocalDateTime startedAt,
            java.time.LocalDateTime finishedAt
    ) {}
}
