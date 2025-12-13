package com.OhRyue.certpilot.versus.dto;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchPhase;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.fasterxml.jackson.annotation.JsonInclude;

import java.time.Instant;
import java.util.List;

/**
 * 방 상태 스냅샷 DTO
 * 
 * WebSocket JOIN_ROOM 시 서버가 클라이언트에게 전달하는 방의 현재 상태 정보
 * 재접속(새로고침 포함) 시에도 동일한 스냅샷을 받아 현재 대전 상태를 복구할 수 있음
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public record RoomSnapshotDto(
        /**
         * 방 기본 정보
         */
        RoomInfo room,

        /**
         * 참가자 목록
         * - userId, nickname, skinId
         * - eliminated 여부
         * - finalScore, rank (종료 시)
         */
        List<ParticipantInfo> participants,

        /**
         * 현재 진행 중인 문제 정보
         * - 문제가 진행 중이 아니면 null
         * - questionId, roundNo, phase, orderNo, timeLimitSec
         * - endTime: 문제 종료 시간 (남은 시간 계산용)
         * - remainingSeconds: 남은 시간 (초) - 클라이언트 계산용 참고값
         */
        CurrentQuestionSnapshot currentQuestion,

        /**
         * 쉬는 시간 정보
         * - 쉬는 시간 중일 때만 null이 아님
         * - nextQuestionId, nextRoundNo, nextPhase
         * - questionStartAt: 다음 문제 시작 시간
         * - remainingSeconds: 남은 시간 (초)
         */
        IntermissionSnapshot intermission,

        /**
         * 현재 라운드 번호
         * - 문제가 진행 중이면 currentQuestion.roundNo
         * - 쉬는 시간 중이면 intermission.nextRoundNo
         * - 그 외에는 null
         */
        Integer currentRoundNo,

        /**
         * 현재 페이즈 (MAIN, FINAL, REVIVAL)
         * - 문제가 진행 중이면 currentQuestion.phase
         * - 쉬는 시간 중이면 intermission.nextPhase
         * - 그 외에는 null
         */
        MatchPhase currentPhase,

        /**
         * 스코어보드 정보
         * - 참가자별 점수, 정답 수, 순위 등
         */
        ScoreboardSnapshot scoreboard,

        /**
         * 스냅샷 생성 시각
         */
        Instant snapshotAt
) {
    /**
     * 방 기본 정보
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record RoomInfo(
            Long roomId,
            MatchMode mode,
            MatchStatus status,
            String examMode,
            Instant createdAt,
            Instant scheduledAt,
            Boolean isBotMatch
    ) {}

    /**
     * 참가자 정보
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record ParticipantInfo(
            String userId,
            String nickname,
            Long skinId,
            boolean eliminated,
            boolean revived,
            Integer finalScore,
            Integer rank,
            Instant joinedAt
    ) {}

    /**
     * 현재 문제 스냅샷
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record CurrentQuestionSnapshot(
            Long questionId,
            Integer roundNo,
            MatchPhase phase,
            Integer orderNo,
            Integer timeLimitSec,
            /**
             * 문제 종료 시간 (ISO 8601)
             * 클라이언트는 이 시간과 현재 시간을 비교하여 남은 시간 계산
             */
            Instant endTime,
            /**
             * 남은 시간 (초) - 참고용
             * 서버에서 계산한 값이지만, 클라이언트는 endTime을 기준으로 재계산 권장
             */
            Long remainingSeconds
    ) {}

    /**
     * 쉬는 시간 스냅샷
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record IntermissionSnapshot(
            Long nextQuestionId,
            Integer nextRoundNo,
            MatchPhase nextPhase,
            Integer durationSec,
            /**
             * 다음 문제 시작 시간 (ISO 8601)
             */
            Instant questionStartAt,
            /**
             * 남은 시간 (초) - 참고용
             */
            Long remainingSeconds
    ) {}

    /**
     * 스코어보드 스냅샷
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record ScoreboardSnapshot(
            List<ScoreboardItem> items
    ) {}

    /**
     * 스코어보드 아이템
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record ScoreboardItem(
            String userId,
            String nickname,
            Long skinId,
            int correctCount,
            int totalCount,
            int score,
            Long totalTimeMs,
            Integer rank,
            boolean alive,
            boolean revived
    ) {}
}





