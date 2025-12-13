package com.OhRyue.certpilot.versus.controller;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.dto.MatchingDtos;
import com.OhRyue.certpilot.versus.dto.RoomSnapshotDto;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.dto.WebSocketDtos;
import com.OhRyue.certpilot.versus.service.AnswerSubmissionService;
import com.OhRyue.certpilot.versus.service.PresenceService;
import com.OhRyue.certpilot.versus.service.RedisMatchingQueueService;
import com.OhRyue.certpilot.versus.service.RoomSnapshotService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.messaging.simp.annotation.SendToUser;
import org.springframework.stereotype.Controller;

import java.security.Principal;

/**
 * WebSocket 메시지 핸들러
 * 
 * STOMP 기반 WebSocket 메시징 처리
 * 
 * 엔드포인트:
 * - 클라이언트 -> 서버: /app/versus/join, /app/versus/answer
 * - 서버 -> 클라이언트: /user/queue/versus/join, /user/queue/versus/answer
 */
@Controller
@RequiredArgsConstructor
@Slf4j
public class WebSocketController {

    private final RoomSnapshotService roomSnapshotService;
    private final AnswerSubmissionService answerSubmissionService;
    private final PresenceService presenceService;
    private final RedisMatchingQueueService redisMatchingQueueService;
    private final SimpMessagingTemplate messagingTemplate;

    /**
     * JOIN_ROOM 명령 처리
     * 
     * 클라이언트가 방에 입장할 때 호출
     * - roomId를 받아서 현재 방 상태 스냅샷을 반환
     * - userId는 WebSocket 인증 정보(Principal)에서 추출
     * 
     * @param command JOIN_ROOM 명령 (roomId 포함)
     * @param principal WebSocket 인증 정보 (JWT에서 추출된 userId)
     * @return JOIN_ROOM 응답 (스냅샷 포함)
     */
    @MessageMapping("/versus/join")
    @SendToUser("/queue/versus/join")
    public WebSocketDtos.JoinRoomResponse handleJoinRoom(
            @Payload WebSocketDtos.JoinRoomCommand command,
            Principal principal
    ) {
        String userId = principal != null ? principal.getName() : null;
        Long roomId = command.roomId();

        log.info("WebSocket JOIN_ROOM 요청: userId={}, roomId={}", userId, roomId);

        if (roomId == null) {
            log.warn("JOIN_ROOM: roomId가 null입니다. userId={}", userId);
            return WebSocketDtos.JoinRoomResponse.failure(null, "roomId는 필수입니다.");
        }

        try {
            // 방 상태 스냅샷 생성
            RoomSnapshotDto snapshot = roomSnapshotService.createSnapshot(roomId);

            log.info("JOIN_ROOM 성공: userId={}, roomId={}, status={}",
                    userId, roomId, snapshot.room().status());

            return WebSocketDtos.JoinRoomResponse.success(roomId, snapshot);

        } catch (IllegalArgumentException e) {
            log.warn("JOIN_ROOM 실패: userId={}, roomId={}, error={}",
                    userId, roomId, e.getMessage());
            return WebSocketDtos.JoinRoomResponse.failure(roomId, e.getMessage());

        } catch (Exception e) {
            log.error("JOIN_ROOM 오류: userId={}, roomId={}", userId, roomId, e);
            return WebSocketDtos.JoinRoomResponse.failure(roomId, "방 입장 중 오류가 발생했습니다.");
        }
    }

    /**
     * SUBMIT_ANSWER 명령 처리
     * 
     * 클라이언트가 답안을 제출할 때 호출
     * - roomId, questionId, userAnswer를 받아서 답안 제출 처리
     * - userId는 WebSocket 인증 정보(Principal)에서 추출
     * 
     * @param command SUBMIT_ANSWER 명령
     * @param principal WebSocket 인증 정보 (JWT에서 추출된 userId)
     * @return SUBMIT_ANSWER 응답 (스코어보드 포함)
     */
    @MessageMapping("/versus/answer")
    @SendToUser("/queue/versus/answer")
    public WebSocketDtos.SubmitAnswerResponse handleSubmitAnswer(
            @Payload WebSocketDtos.SubmitAnswerCommand command,
            Principal principal
    ) {
        String userId = principal != null ? principal.getName() : null;
        Long roomId = command.roomId();
        Long questionId = command.questionId();

        log.info("WebSocket SUBMIT_ANSWER 요청: userId={}, roomId={}, questionId={}",
                userId, roomId, questionId);

        if (roomId == null) {
            log.warn("SUBMIT_ANSWER: roomId가 null입니다. userId={}", userId);
            return WebSocketDtos.SubmitAnswerResponse.failure(null, null, "roomId는 필수입니다.");
        }

        if (questionId == null) {
            log.warn("SUBMIT_ANSWER: questionId가 null입니다. userId={}, roomId={}", userId, roomId);
            return WebSocketDtos.SubmitAnswerResponse.failure(roomId, null, "questionId는 필수입니다.");
        }

        if (command.userAnswer() == null || command.userAnswer().isBlank()) {
            log.warn("SUBMIT_ANSWER: userAnswer가 비어있습니다. userId={}, roomId={}, questionId={}",
                    userId, roomId, questionId);
            return WebSocketDtos.SubmitAnswerResponse.failure(roomId, questionId, "userAnswer는 필수입니다.");
        }

        try {
            // AnswerSubmissionService를 통해 답안 제출 처리
            VersusDtos.ScoreBoardResp scoreboard = answerSubmissionService.submitAnswer(
                    roomId, userId, command);

            log.info("SUBMIT_ANSWER 성공: userId={}, roomId={}, questionId={}",
                    userId, roomId, questionId);

            return WebSocketDtos.SubmitAnswerResponse.success(roomId, questionId, scoreboard);

        } catch (org.springframework.web.server.ResponseStatusException e) {
            log.warn("SUBMIT_ANSWER 실패: userId={}, roomId={}, questionId={}, error={}",
                    userId, roomId, questionId, e.getReason());
            return WebSocketDtos.SubmitAnswerResponse.failure(roomId, questionId, e.getReason());

        } catch (Exception e) {
            log.error("SUBMIT_ANSWER 오류: userId={}, roomId={}, questionId={}", userId, roomId, questionId, e);
            return WebSocketDtos.SubmitAnswerResponse.failure(roomId, questionId, "답안 제출 중 오류가 발생했습니다.");
        }
    }

    /**
     * HEARTBEAT 명령 처리
     * 
     * 클라이언트가 하트비트를 전송할 때 호출
     * - roomId를 받아서 Redis에 접속 상태 업데이트
     * - userId는 WebSocket 인증 정보(Principal)에서 추출
     * 
     * @param command HEARTBEAT 명령 (roomId 포함)
     * @param principal WebSocket 인증 정보 (JWT에서 추출된 userId)
     */
    @MessageMapping("/versus/heartbeat")
    @SendToUser("/queue/versus/heartbeat")
    public WebSocketDtos.HeartbeatResponse handleHeartbeat(
            @Payload WebSocketDtos.HeartbeatCommand command,
            Principal principal
    ) {
        String userId = principal != null ? principal.getName() : null;
        Long roomId = command.roomId();

        log.debug("WebSocket HEARTBEAT 요청: userId={}, roomId={}", userId, roomId);

        if (roomId == null) {
            log.warn("HEARTBEAT: roomId가 null입니다. userId={}", userId);
            return WebSocketDtos.HeartbeatResponse.failure(null, "roomId는 필수입니다.");
        }

        try {
            // Redis에 접속 상태 업데이트
            presenceService.updatePresence(roomId, userId);

            log.debug("HEARTBEAT 성공: userId={}, roomId={}", userId, roomId);
            return WebSocketDtos.HeartbeatResponse.success(roomId);

        } catch (Exception e) {
            log.error("HEARTBEAT 오류: userId={}, roomId={}", userId, roomId, e);
            return WebSocketDtos.HeartbeatResponse.failure(roomId, "하트비트 처리 중 오류가 발생했습니다.");
        }
    }

    /**
     * REQUEST_MATCH 명령 처리
     * 
     * 클라이언트가 매칭을 요청할 때 호출
     * 
     * @param command REQUEST_MATCH 명령
     * @param principal WebSocket 인증 정보
     * @return MATCH_RESPONSE
     */
    @MessageMapping("/versus/match/request")
    @SendToUser("/queue/versus/match")
    public WebSocketDtos.MatchResponse handleRequestMatch(
            @Payload WebSocketDtos.RequestMatchCommand command,
            Principal principal
    ) {
        String userId = principal != null ? principal.getName() : null;

        log.info("WebSocket REQUEST_MATCH 요청: userId={}, mode={}", userId, command.mode());

        if (userId == null) {
            return WebSocketDtos.MatchResponse.failure("인증이 필요합니다.");
        }

        try {
            // MatchMode 변환
            MatchMode mode = MatchMode.valueOf(command.mode());

            // MatchingDtos.MatchRequest 생성
            MatchingDtos.MatchRequest matchRequest = new MatchingDtos.MatchRequest(
                    mode,
                    command.certId(),
                    command.matchingMode(),
                    command.topicId(),
                    command.difficulty(),
                    command.examMode()
            );

            // 매칭 요청
            MatchingDtos.MatchStatusResp status = redisMatchingQueueService.requestMatch(userId, matchRequest);

            log.info("REQUEST_MATCH 성공: userId={}, matching={}, roomId={}, waitingCount={}",
                    userId, status.matching(), status.roomId(), status.waitingCount());

            return WebSocketDtos.MatchResponse.success(
                    status.matching(),
                    status.roomId(),
                    status.waitingCount()
            );

        } catch (IllegalArgumentException e) {
            log.warn("REQUEST_MATCH 실패: userId={}, error={}", userId, e.getMessage());
            return WebSocketDtos.MatchResponse.failure("잘못된 매칭 모드입니다: " + e.getMessage());

        } catch (Exception e) {
            log.error("REQUEST_MATCH 오류: userId={}", userId, e);
            return WebSocketDtos.MatchResponse.failure("매칭 요청 중 오류가 발생했습니다.");
        }
    }

    /**
     * CANCEL_MATCH 명령 처리
     * 
     * 클라이언트가 매칭을 취소할 때 호출
     * 
     * @param command CANCEL_MATCH 명령
     * @param principal WebSocket 인증 정보
     * @return MATCH_RESPONSE
     */
    @MessageMapping("/versus/match/cancel")
    @SendToUser("/queue/versus/match")
    public WebSocketDtos.MatchResponse handleCancelMatch(
            @Payload WebSocketDtos.CancelMatchCommand command,
            Principal principal
    ) {
        String userId = principal != null ? principal.getName() : null;

        log.info("WebSocket CANCEL_MATCH 요청: userId={}, mode={}", userId, command.mode());

        if (userId == null) {
            return WebSocketDtos.MatchResponse.failure("인증이 필요합니다.");
        }

        try {
            // 매칭 취소
            redisMatchingQueueService.cancelMatch(userId);

            log.info("CANCEL_MATCH 성공: userId={}", userId);

            return WebSocketDtos.MatchResponse.success(false, null, null);

        } catch (Exception e) {
            log.error("CANCEL_MATCH 오류: userId={}", userId, e);
            return WebSocketDtos.MatchResponse.failure("매칭 취소 중 오류가 발생했습니다.");
        }
    }
}

