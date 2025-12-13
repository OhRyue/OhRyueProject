package com.OhRyue.certpilot.versus.dto;

import com.fasterxml.jackson.annotation.JsonInclude;

import jakarta.validation.constraints.NotNull;

/**
 * WebSocket 메시지 DTO
 */
public class WebSocketDtos {

    /**
     * JOIN_ROOM 명령 메시지
     * 
     * 클라이언트 -> 서버: /app/versus/join
     * 
     * 예시:
     * {
     *   "command": "JOIN_ROOM",
     *   "roomId": 123
     * }
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record JoinRoomCommand(
            @NotNull
            Long roomId
    ) {
        public static final String COMMAND = "JOIN_ROOM";
    }

    /**
     * JOIN_ROOM 응답 메시지
     * 
     * 서버 -> 클라이언트: /user/queue/versus/join
     * 
     * 예시:
     * {
     *   "type": "JOIN_ROOM_RESPONSE",
     *   "success": true,
     *   "roomId": 123,
     *   "snapshot": { ... }
     * }
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record JoinRoomResponse(
            String type,
            boolean success,
            Long roomId,
            String message,
            RoomSnapshotDto snapshot
    ) {
        public static final String TYPE = "JOIN_ROOM_RESPONSE";

        public static JoinRoomResponse success(Long roomId, RoomSnapshotDto snapshot) {
            return new JoinRoomResponse(TYPE, true, roomId, null, snapshot);
        }

        public static JoinRoomResponse failure(Long roomId, String message) {
            return new JoinRoomResponse(TYPE, false, roomId, message, null);
        }
    }

    /**
     * SUBMIT_ANSWER 명령 메시지
     * 
     * 클라이언트 -> 서버: /app/versus/answer
     * 
     * 예시:
     * {
     *   "command": "SUBMIT_ANSWER",
     *   "roomId": 123,
     *   "questionId": 456,
     *   "userAnswer": "A"
     * }
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record SubmitAnswerCommand(
            @NotNull
            Long roomId,
            @NotNull
            Long questionId,
            @NotNull
            String userAnswer,
            /**
             * 클라이언트가 계산한 정답 여부 (서버 검증과 비교용, 참고용)
             */
            Boolean correct,
            /**
             * 문제 풀이 소요 시간 (밀리초, 선택사항)
             * 서버에서 QUESTION_STARTED 이벤트 기준으로 자동 계산되므로 이 값은 무시됩니다.
             */
            Integer timeMs,
            /**
             * 라운드 번호 (자동 설정되므로 보통 null)
             */
            Integer roundNo,
            /**
             * 페이즈 (MAIN, FINAL 등, 자동 설정되므로 보통 null)
             */
            String phase
    ) {
        public static final String COMMAND = "SUBMIT_ANSWER";
    }

    /**
     * SUBMIT_ANSWER 응답 메시지
     * 
     * 서버 -> 클라이언트: /user/queue/versus/answer
     * 
     * 예시:
     * {
     *   "type": "SUBMIT_ANSWER_RESPONSE",
     *   "success": true,
     *   "roomId": 123,
     *   "questionId": 456,
     *   "message": null,
     *   "scoreboard": { ... }
     * }
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record SubmitAnswerResponse(
            String type,
            boolean success,
            Long roomId,
            Long questionId,
            String message,
            /**
             * 업데이트된 스코어보드 (성공 시)
             */
            VersusDtos.ScoreBoardResp scoreboard
    ) {
        public static final String TYPE = "SUBMIT_ANSWER_RESPONSE";

        public static SubmitAnswerResponse success(Long roomId, Long questionId, VersusDtos.ScoreBoardResp scoreboard) {
            return new SubmitAnswerResponse(TYPE, true, roomId, questionId, null, scoreboard);
        }

        public static SubmitAnswerResponse failure(Long roomId, Long questionId, String message) {
            return new SubmitAnswerResponse(TYPE, false, roomId, questionId, message, null);
        }
    }

    /**
     * HEARTBEAT 명령 메시지
     *
     * 클라이언트 -> 서버: /app/versus/heartbeat
     *
     * 예시:
     * {
     *   "command": "HEARTBEAT",
     *   "roomId": 123
     * }
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record HeartbeatCommand(
            @NotNull
            Long roomId
    ) {
        public static final String COMMAND = "HEARTBEAT";
    }

    /**
     * HEARTBEAT 응답 메시지
     *
     * 서버 -> 클라이언트: /user/queue/versus/heartbeat
     *
     * 예시:
     * {
     *   "type": "HEARTBEAT_RESPONSE",
     *   "success": true,
     *   "roomId": 123,
     *   "message": null
     * }
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record HeartbeatResponse(
            String type,
            boolean success,
            Long roomId,
            String message
    ) {
        public static final String TYPE = "HEARTBEAT_RESPONSE";

        public static HeartbeatResponse success(Long roomId) {
            return new HeartbeatResponse(TYPE, true, roomId, null);
        }

        public static HeartbeatResponse failure(Long roomId, String message) {
            return new HeartbeatResponse(TYPE, false, roomId, message);
        }
    }

    /**
     * REQUEST_MATCH 명령 메시지
     *
     * 클라이언트 -> 서버: /app/versus/match/request
     *
     * 예시:
     * {
     *   "command": "REQUEST_MATCH",
     *   "mode": "DUEL",
     *   "certId": "1",
     *   "matchingMode": "CATEGORY",
     *   "topicId": 101,
     *   "difficulty": null,
     *   "examMode": "WRITTEN"
     * }
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record RequestMatchCommand(
            @NotNull
            String mode, // DUEL, TOURNAMENT
            @NotNull
            String certId,
            @NotNull
            String matchingMode, // CATEGORY, DIFFICULTY
            Long topicId, // CATEGORY 모드일 때 필수
            String difficulty, // DIFFICULTY 모드일 때 필수 (EASY, NORMAL, HARD)
            @NotNull
            String examMode // WRITTEN, PRACTICAL
    ) {
        public static final String COMMAND = "REQUEST_MATCH";
    }

    /**
     * CANCEL_MATCH 명령 메시지
     *
     * 클라이언트 -> 서버: /app/versus/match/cancel
     *
     * 예시:
     * {
     *   "command": "CANCEL_MATCH",
     *   "mode": "DUEL"
     * }
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record CancelMatchCommand(
            @NotNull
            String mode // DUEL, TOURNAMENT
    ) {
        public static final String COMMAND = "CANCEL_MATCH";
    }

    /**
     * REQUEST_MATCH / CANCEL_MATCH 응답 메시지
     *
     * 서버 -> 클라이언트: /user/queue/versus/match
     *
     * 예시:
     * {
     *   "type": "MATCH_RESPONSE",
     *   "success": true,
     *   "matching": true,
     *   "roomId": null,
     *   "waitingCount": 1,
     *   "message": null
     * }
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record MatchResponse(
            String type,
            boolean success,
            boolean matching, // 매칭 중 여부
            Long roomId, // 매칭 성공 시 방 ID
            Integer waitingCount, // 대기 인원 수
            String message
    ) {
        public static final String TYPE = "MATCH_RESPONSE";

        public static MatchResponse success(boolean matching, Long roomId, Integer waitingCount) {
            return new MatchResponse(TYPE, true, matching, roomId, waitingCount, null);
        }

        public static MatchResponse failure(String message) {
            return new MatchResponse(TYPE, false, false, null, null, message);
        }
    }
}


