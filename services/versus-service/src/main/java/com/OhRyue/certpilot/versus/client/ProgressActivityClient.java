package com.OhRyue.certpilot.versus.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(
    name = "progress-activity-client",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/progress/internal"
)
public interface ProgressActivityClient {

    /**
     * 학습 활동 기록 생성
     * versus-service에서 매치 종료 시 progress-service에 Activity를 생성하도록 호출
     */
    @PostMapping("/activities")
    void createActivity(@RequestBody ProgressActivityCreateReq req);

    record ProgressActivityCreateReq(
            String userId,
            String activityGroup,  // MAIN, ASSIST, BATTLE
            String mainType,        // MICRO, REVIEW (MAIN일 때만)
            String assistType,      // CATEGORY, WEAKNESS, DIFFICULTY (ASSIST일 때만)
            String battleType,      // DUEL_CATEGORY, DUEL_DIFFICULTY, TOURNAMENT, GOLDENBELL (BATTLE일 때만)
            String mode,            // WRITTEN, PRACTICAL
            Long topicId,
            String topicName,
            String weaknessTagName,
            String difficulty,      // EASY, NORMAL, HARD
            Integer questionCount,
            Integer correctCount,
            Double accuracyPct,
            Integer finalRank,
            Integer xpGained,
            String sourceService,   // "versus"
            Long sourceSessionId,   // match_room.id
            java.time.LocalDateTime startedAt,
            java.time.LocalDateTime finishedAt
    ) {}
}



