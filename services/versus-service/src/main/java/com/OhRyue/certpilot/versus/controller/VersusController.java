package com.OhRyue.certpilot.versus.controller;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.OhRyue.certpilot.versus.dto.MatchingDtos;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.service.DemoMatchingService;
import com.OhRyue.certpilot.versus.service.MatchingQueueService;
import com.OhRyue.certpilot.versus.service.VersusMatchService;
import com.OhRyue.certpilot.versus.service.VersusService;
import com.OhRyue.common.auth.AuthUserUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@Tag(name = "Versus(대전)", description = "실시간 대전/토너먼트/골든벨 API")
@RestController
@RequestMapping("/api/versus")
@RequiredArgsConstructor
public class VersusController {

  private final VersusService versusService;
  private final MatchingQueueService matchingQueueService;
  private final DemoMatchingService demoMatchingService;
  private final VersusMatchService versusMatchService;

  @Operation(summary = "헬스 체크")
  @GetMapping("/ping")
  public Map<String, Object> ping() {
    return Map.of("service", "versus", "ok", true);
  }

  /* -------- 방 조회 & 생성 -------- */
  @Operation(
      summary = "대전 방 목록 조회",
      description = "대전 방 목록을 조회합니다. mode와 status로 필터링 가능합니다.\n\n" +
          "- mode: DUEL(1:1), TOURNAMENT(토너먼트), GOLDENBELL(골든벨)\n" +
          "- status: WAIT(대기), ONGOING(진행중), DONE(종료)"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "조회 성공")
  })
  @GetMapping("/rooms")
  public List<VersusDtos.RoomSummary> rooms(
      @Parameter(description = "대전 모드 (DUEL, TOURNAMENT, GOLDENBELL)", example = "DUEL")
      @RequestParam(required = false) MatchMode mode,
      @Parameter(description = "방 상태 (WAIT, ONGOING, DONE)", example = "WAIT")
      @RequestParam(required = false) MatchStatus status) {
    return versusService.listRooms(mode, status);
  }

  @Operation(
      summary = "예약된 대전 방 목록 조회",
      description = "예약 시간이 설정되어 있고 아직 시작하지 않은 대전 방 목록을 조회합니다.\n\n" +
          "**필터링:**\n" +
          "- mode: DUEL(1:1), TOURNAMENT(토너먼트), GOLDENBELL(골든벨)\n" +
          "- 예약 시간이 현재 시간 이후인 방만 조회\n" +
          "- 예약 시간 순으로 정렬 (가까운 시간부터)\n\n" +
          "**응답:**\n" +
          "- scheduledAt: 예약 시작 시간 (ISO 8601 형식, 예약이 없으면 null)"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "조회 성공")
  })
  @GetMapping("/rooms/scheduled")
  public List<VersusDtos.RoomSummary> scheduledRooms(
      @Parameter(description = "대전 모드 (DUEL, TOURNAMENT, GOLDENBELL)", example = "GOLDENBELL")
      @RequestParam(required = false) MatchMode mode) {
    return versusService.listScheduledRooms(mode);
  }

  @Operation(
      summary = "대기 중인 대전 방 목록 조회",
      description = "WAIT 상태인 대전 방 목록을 조회합니다. 예약 시간과 무관하게 모든 WAIT 상태 방을 조회합니다.\n\n" +
          "**용도:**\n" +
          "- 토너먼트 등 예약 시간이 없는 방 조회에 적합\n" +
          "- 예약 시간이 설정되지 않은 방도 포함\n\n" +
          "**필터링:**\n" +
          "- mode: DUEL(1:1), TOURNAMENT(토너먼트), GOLDENBELL(골든벨)\n" +
          "- mode를 지정하지 않으면 모든 모드의 WAIT 상태 방 조회\n" +
          "- 생성 시간 역순으로 정렬 (최신 방부터)\n\n" +
          "**응답:**\n" +
          "- scheduledAt: 예약 시작 시간 (예약이 없으면 null)"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "조회 성공")
  })
  @GetMapping("/rooms/waiting")
  public List<VersusDtos.RoomSummary> waitingRooms(
      @Parameter(description = "대전 모드 (DUEL, TOURNAMENT, GOLDENBELL). 기본값: TOURNAMENT", example = "TOURNAMENT")
      @RequestParam(required = false) MatchMode mode) {
    // mode가 지정되지 않으면 기본값으로 TOURNAMENT 사용
    MatchMode targetMode = mode != null ? mode : MatchMode.TOURNAMENT;
    return versusService.listWaitingRooms(targetMode);
  }

  @Operation(
      summary = "대전 방 생성",
      description = "새로운 대전 방을 생성합니다.\n\n" +
          "**방 생성 방법 2가지:**\n" +
          "1. **scopeJson 사용 (권장)**: 문제를 자동으로 생성합니다.\n" +
          "   - scopeJson 예시: `{\"examMode\":\"WRITTEN\",\"difficulty\":\"NORMAL\",\"topicScope\":\"ALL\"}`\n" +
          "2. **questions 직접 제공**: 미리 준비한 문제 리스트를 제공합니다.\n\n" +
          "**인원 제한:**\n" +
          "- DUEL: 최대 2명\n" +
          "- TOURNAMENT: 최대 8명\n" +
          "- GOLDENBELL: 최대 20명\n\n" +
          "**주의사항:**\n" +
          "- participants는 초대할 사용자 ID 리스트입니다. 방 생성자는 자동으로 참가됩니다.\n" +
          "- scopeJson과 questions 중 하나는 반드시 제공해야 합니다."
  )
  @ApiResponses(value = {
      @ApiResponse(
          responseCode = "200",
          description = "방 생성 성공",
          content = @Content(schema = @Schema(implementation = VersusDtos.RoomDetailResp.class))
      ),
      @ApiResponse(responseCode = "400", description = "잘못된 요청 (인원 제한 초과, 필수 필드 누락 등)")
  })
  @PostMapping("/rooms")
  public VersusDtos.RoomDetailResp createRoom(
      @io.swagger.v3.oas.annotations.parameters.RequestBody(
          description = "방 생성 요청",
          required = true,
          content = @Content(
              examples = {
                  @ExampleObject(
                      name = "DUEL 모드 (scopeJson 사용 - 권장)",
                      value = """
                          {
                            "mode": "DUEL",
                            "scopeJson": "{\\"examMode\\":\\"WRITTEN\\",\\"difficulty\\":\\"NORMAL\\",\\"topicScope\\":\\"ALL\\"}",
                            "participants": ["user2"]
                          }
                          """
                  ),
                  @ExampleObject(
                      name = "TOURNAMENT 모드 (scopeJson 사용 - 권장)",
                      value = """
                          {
                            "mode": "TOURNAMENT",
                            "scopeJson": "{\\"examMode\\":\\"WRITTEN\\",\\"difficulty\\":\\"NORMAL\\",\\"topicScope\\":\\"ALL\\"}",
                            "participants": ["user2", "user3", "user4", "user5", "user6", "user7", "user8"]
                          }
                          """
                  ),
                  @ExampleObject(
                      name = "TOURNAMENT 모드 (특정 카테고리, scopeJson 사용)",
                      value = """
                          {
                            "mode": "TOURNAMENT",
                            "scopeJson": "{\\"examMode\\":\\"WRITTEN\\",\\"difficulty\\":\\"NORMAL\\",\\"topicScope\\":\\"CATEGORY\\",\\"topicId\\":101}",
                            "participants": ["user2", "user3", "user4", "user5", "user6", "user7", "user8"]
                          }
                          """
                  ),
                  @ExampleObject(
                      name = "TOURNAMENT 모드 (questions 직접 제공 - 선택사항)",
                      value = """
                          {
                            "mode": "TOURNAMENT",
                            "participants": ["user2", "user3", "user4", "user5", "user6", "user7", "user8"],
                            "questions": [
                              {"questionId": 1, "roundNo": 1, "phase": "MAIN", "order": 1, "timeLimitSec": 10},
                              {"questionId": 2, "roundNo": 1, "phase": "MAIN", "order": 2, "timeLimitSec": 10},
                              {"questionId": 3, "roundNo": 1, "phase": "MAIN", "order": 3, "timeLimitSec": 10}
                            ]
                          }
                          """
                  )
              }
          )
      )
      @Valid @RequestBody VersusDtos.CreateRoomReq req) {
    // 🔹 JWT에서 현재 로그인한 사용자 ID 추출 (방 생성자)
    String creatorUserId = AuthUserUtil.getCurrentUserId();
    return versusService.createRoom(req, creatorUserId);
  }

  @Operation(
      summary = "대전 방 상세조회",
      description = "특정 방의 상세 정보를 조회합니다.\n\n" +
          "반환 정보:\n" +
          "- 방 정보 (모드, 상태, 참가자 수 등)\n" +
          "- 참가자 목록 및 상태\n" +
          "- 문제 목록\n" +
          "- 현재 스코어보드"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "조회 성공"),
      @ApiResponse(responseCode = "404", description = "방을 찾을 수 없음")
  })
  @GetMapping("/rooms/{roomId}")
  public VersusDtos.RoomDetailResp roomDetail(
      @Parameter(description = "방 ID", example = "1", required = true)
      @PathVariable Long roomId) {
    return versusService.getRoom(roomId);
  }

  /* -------- 참가/시작 -------- */
  @Operation(
      summary = "대전 방 참가",
      description = "대기 중인 방에 참가합니다.\n\n" +
          "**주의사항:**\n" +
          "- JWT 토큰에서 현재 로그인한 사용자 ID를 자동으로 가져옵니다.\n" +
          "- 방 상태가 WAIT일 때만 참가 가능합니다.\n" +
          "- 모드별 최대 인원 제한을 확인합니다.\n" +
          "  - DUEL: 최대 2명\n" +
          "  - TOURNAMENT: 최대 8명\n" +
          "  - GOLDENBELL: 최대 20명"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "참가 성공"),
      @ApiResponse(responseCode = "400", description = "참가 실패 (인원 제한 초과, 이미 시작된 방 등)"),
      @ApiResponse(responseCode = "401", description = "인증 실패 (JWT 토큰 필요)")
  })
  @PostMapping("/rooms/{roomId}/join")
  public VersusDtos.RoomDetailResp joinRoom(
      @Parameter(description = "방 ID", example = "1", required = true)
      @PathVariable Long roomId) {
    // 🔹 JWT 에서 현재 로그인한 사용자 ID 추출
    String userId = AuthUserUtil.getCurrentUserId();
    return versusService.joinRoom(roomId, userId);
  }

  @Operation(
      summary = "하트비트 업데이트",
      description = "사용자의 연결 상태를 업데이트합니다.\n\n" +
          "**용도:**\n" +
          "- 대기 중인 방(WAIT): 프론트엔드에서 주기적으로(예: 30초마다) 호출하여 사용자가 아직 연결되어 있음을 알림\n" +
          "- DUEL 모드 진행 중(ONGOING): 게임 진행 중에도 연결 상태를 유지하여 상대방이 떠났는지 감지\n" +
          "- 사이트를 닫거나 연결이 끊기면 하트비트가 중단되어 자동으로 참가자에서 제거됨\n\n" +
          "**동작 방식:**\n" +
          "- WAIT 상태: 모든 모드(DUEL, TOURNAMENT, GOLDENBELL)에서 동작\n" +
          "- ONGOING 상태: DUEL 모드에서만 동작 (TOURNAMENT, GOLDENBELL은 불필요)\n" +
          "- DUEL 모드에서 상대방이 하트비트를 보내지 않으면 1분 후 자동으로 게임이 종료됩니다.\n\n" +
          "**권장 호출 주기:** 30초마다"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "하트비트 업데이트 성공"),
      @ApiResponse(responseCode = "400", description = "하트비트가 허용되지 않는 상태 (ONGOING TOURNAMENT/GOLDENBELL 등)"),
      @ApiResponse(responseCode = "404", description = "방 또는 참가자를 찾을 수 없음"),
      @ApiResponse(responseCode = "401", description = "인증 실패 (JWT 토큰 필요)")
  })
  @PostMapping("/rooms/{roomId}/heartbeat")
  public Map<String, Object> heartbeat(
      @Parameter(description = "방 ID", example = "1", required = true)
      @PathVariable Long roomId) {
    String userId = AuthUserUtil.getCurrentUserId();
    versusService.updateHeartbeat(roomId, userId);
    return Map.of("success", true, "message", "Heartbeat updated");
  }

  @Operation(
      summary = "대전 방 시작",
      description = "대기 중인 방을 시작합니다.\n\n" +
          "**시작 조건:**\n" +
          "- 방 상태가 WAIT여야 합니다.\n" +
          "- 최소 인원이 모여야 합니다.\n" +
          "  - DUEL: 최소 2명\n" +
          "  - TOURNAMENT: 최소 2명\n" +
          "  - GOLDENBELL: 최소 2명\n\n" +
          "**시작 후:**\n" +
          "- 방 상태가 ONGOING으로 변경됩니다.\n" +
          "- MATCH_STARTED 이벤트가 기록됩니다."
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "시작 성공"),
      @ApiResponse(responseCode = "400", description = "시작 실패 (이미 시작됨, 최소 인원 부족 등)")
  })
  @PostMapping("/rooms/{roomId}/start")
  public VersusDtos.RoomDetailResp startRoom(
      @Parameter(description = "방 ID", example = "1", required = true)
      @PathVariable Long roomId) {
    return versusService.startRoom(roomId);
  }

  /* -------- 문제 응답 & 스코어 -------- */
  @Operation(
      summary = "답안 제출/채점",
      description = "사용자가 문제에 대한 답안을 제출하고 채점합니다.\n\n" +
          "**중요:**\n" +
          "- JWT 토큰에서 현재 로그인한 사용자 ID를 자동으로 가져옵니다.\n" +
          "- `userAnswer` 필드는 **반드시 제공**해야 합니다. (서버 사이드 정답 검증용)\n\n" +
          "**userAnswer 형식:**\n" +
          "- MCQ/OX: 선택한 label (예: \"A\", \"B\", \"O\", \"X\")\n" +
          "- SHORT/LONG: 입력한 텍스트 (예: \"정규화\", \"데이터베이스 설계\")\n\n" +
          "**점수 계산:**\n" +
          "- 정답 여부와 소요 시간에 따라 점수가 계산됩니다.\n" +
          "- 서버에서 실제 정답을 검증하므로 `correct` 값은 참고용입니다.\n\n" +
          "**자동 처리:**\n" +
          "- 모든 참가자가 답안을 제출하면 다음 문제로 진행됩니다.\n" +
          "- 매치가 완료되면 자동으로 보상이 지급됩니다."
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "답안 제출 성공"),
      @ApiResponse(responseCode = "400", description = "제출 실패 (이미 제출함, 탈락한 참가자 등)"),
      @ApiResponse(responseCode = "401", description = "인증 실패 (JWT 토큰 필요)")
  })
  @PostMapping("/rooms/{roomId}/answers")
  public VersusDtos.ScoreBoardResp submitAnswer(
      @Parameter(description = "방 ID", example = "1", required = true)
      @PathVariable Long roomId,
      @io.swagger.v3.oas.annotations.parameters.RequestBody(
          description = "답안 제출 요청",
          required = true,
          content = @Content(
              examples = {
                  @ExampleObject(
                      name = "MCQ 답안 제출",
                      value = """
                          {
                            "questionId": 1001,
                            "userAnswer": "A",
                            "correct": true,
                            "timeMs": 5000,
                            "roundNo": 1,
                            "phase": "MAIN"
                          }
                          """
                  ),
                  @ExampleObject(
                      name = "SHORT 답안 제출",
                      value = """
                          {
                            "questionId": 1002,
                            "userAnswer": "정규화",
                            "correct": true,
                            "timeMs": 8000,
                            "roundNo": 1,
                            "phase": "MAIN"
                          }
                          """
                  )
              }
          )
      )
      @Valid @RequestBody VersusDtos.SubmitAnswerReq req) {
    // 🔹 여기서도 userId는 JWT에서만 가져옴
    String userId = AuthUserUtil.getCurrentUserId();
    return versusService.submitAnswer(roomId, userId, req);
  }

  @Operation(
      summary = "실시간 스코어보드 조회",
      description = "현재 방의 스코어보드를 조회합니다.\n\n" +
          "**스코어보드 정보:**\n" +
          "- 각 참가자의 정답 수, 총 문제 수, 점수, 소요 시간\n" +
          "- 현재 순위\n" +
          "- 생존 여부 (GOLDENBELL 모드)\n" +
          "- 부활 여부 (GOLDENBELL 모드)"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "조회 성공")
  })
  @GetMapping("/rooms/{roomId}/scoreboard")
  public VersusDtos.ScoreBoardResp scoreboard(
      @Parameter(description = "방 ID", example = "1", required = true)
      @PathVariable Long roomId) {
    return versusService.scoreboard(roomId);
  }

  /* -------- 실시간 상태 & 타임라인 -------- */
  @Operation(
      summary = "대전 진행 타임라인 조회",
      description = "방의 진행 이벤트 타임라인을 조회합니다.\n\n" +
          "**이벤트 종류:**\n" +
          "- ROOM_CREATED: 방 생성\n" +
          "- PLAYER_JOINED: 참가자 참가\n" +
          "- MATCH_STARTED: 매치 시작\n" +
          "- ANSWER_SUBMITTED: 답안 제출\n" +
          "- ROUND_COMPLETED: 라운드 완료\n" +
          "- MATCH_FINISHED: 매치 종료\n\n" +
          "**limit 파라미터:**\n" +
          "- 조회할 이벤트 개수 (기본값: 50, 최대: 200)"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "조회 성공")
  })
  @GetMapping("/rooms/{roomId}/timeline")
  public List<VersusDtos.TimelineEvent> timeline(
      @Parameter(description = "방 ID", example = "1", required = true)
      @PathVariable Long roomId,
      @Parameter(description = "조회할 이벤트 개수 (기본값: 50, 최대: 200)", example = "50")
      @RequestParam(defaultValue = "50") int limit) {
    return versusService.timeline(roomId, limit);
  }

  @Operation(
      summary = "대전 방 종합 상태 조회",
      description = "방의 모든 상태 정보를 한 번에 조회합니다.\n\n" +
          "**포함 정보:**\n" +
          "- 방 상세 정보 (방 정보, 참가자, 문제 목록, 스코어보드)\n" +
          "- 타임라인 이벤트\n" +
          "- 실시간 스냅샷 (현재 라운드, 페이즈, 최종 업데이트 시간)"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "조회 성공")
  })
  @GetMapping("/rooms/{roomId}/state")
  public VersusDtos.RoomStateResp roomState(
      @Parameter(description = "방 ID", example = "1", required = true)
      @PathVariable Long roomId,
      @Parameter(description = "타임라인 이벤트 조회 개수 (기본값: 50)", example = "50")
      @RequestParam(defaultValue = "50") int limit) {
    return versusService.roomState(roomId, limit);
  }

  /* -------- 실시간 매칭 -------- */
  @Operation(
      summary = "실시간 매칭 요청 (실사용자 매칭)",
      description = "1:1 배틀 또는 토너먼트 실시간 매칭을 요청합니다.\n\n" +
          "**매칭 모드:**\n" +
          "- CATEGORY: 같은 2레벨 토픽을 선택한 사람끼리 매칭 (topicId 필수)\n" +
          "- DIFFICULTY: 같은 난이도를 선택한 사람끼리 매칭 (difficulty 필수)\n\n" +
          "**매칭 성공 시:**\n" +
          "- 1:1 배틀: 2명 모이면 자동으로 방 생성 및 시작\n" +
          "- 토너먼트: 8명 모이면 자동으로 방 생성 및 시작\n\n" +
          "**주의사항:**\n" +
          "- JWT 토큰에서 현재 로그인한 사용자 ID를 자동으로 가져옵니다.\n" +
          "- 매칭 중에는 다른 매칭 요청을 할 수 없습니다.\n" +
          "- **실사용자 매칭**: 더미 플레이어 없이 실제 사용자만 매칭됩니다."
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "매칭 요청 성공"),
      @ApiResponse(responseCode = "400", description = "잘못된 요청"),
      @ApiResponse(responseCode = "401", description = "인증 실패")
  })
  @PostMapping("/match/request")
  public MatchingDtos.MatchStatusResp requestMatch(
      @io.swagger.v3.oas.annotations.parameters.RequestBody(
          description = "매칭 요청",
          required = true
      )
      @Valid @RequestBody MatchingDtos.MatchRequest request) {
    String userId = AuthUserUtil.getCurrentUserId();
    // 실사용자 매칭: 더미 플레이어 없이 순수하게 매칭 큐만 사용
    return matchingQueueService.requestMatch(userId, request);
  }

  @Operation(
      summary = "매칭 상태 조회",
      description = "현재 사용자의 매칭 상태를 조회합니다.\n\n" +
          "**반환 정보:**\n" +
          "- matching: 매칭 중 여부\n" +
          "- roomId: 매칭 성공 시 방 ID\n" +
          "- waitingCount: 현재 대기 인원 수"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "조회 성공")
  })
  @GetMapping("/match/status")
  public MatchingDtos.MatchStatusResp getMatchStatus() {
    String userId = AuthUserUtil.getCurrentUserId();
    return matchingQueueService.getMatchStatus(userId);
  }

  @Operation(
      summary = "매칭 취소",
      description = "현재 진행 중인 매칭을 취소합니다.\n\n" +
          "**주의사항:**\n" +
          "- JWT 토큰에서 현재 로그인한 사용자 ID를 자동으로 가져옵니다.\n" +
          "- 매칭이 이미 성공한 경우(방이 생성된 경우) 취소할 수 없습니다."
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "매칭 취소 성공"),
      @ApiResponse(responseCode = "401", description = "인증 실패")
  })
  @PostMapping("/match/cancel")
  public Map<String, Object> cancelMatch(
      @io.swagger.v3.oas.annotations.parameters.RequestBody(
          description = "매칭 취소 요청",
          required = true
      )
      @Valid @RequestBody MatchingDtos.CancelMatchRequest request) {
    String userId = AuthUserUtil.getCurrentUserId();
    matchingQueueService.cancelMatch(userId);
    return Map.of("success", true, "message", "매칭이 취소되었습니다.");
  }

  @Operation(
      summary = "시연용 즉시 매칭",
      description = "시연을 위한 즉시 매칭 기능입니다.\n\n" +
          "**기능:**\n" +
          "- 더미 플레이어를 자동으로 생성하여 즉시 매칭 완료\n" +
          "- 1:1 배틀: 더미 플레이어 1명 자동 생성\n" +
          "- 토너먼트: 더미 플레이어 7명 자동 생성\n\n" +
          "**주의사항:**\n" +
          "- 시연 전용 기능입니다.\n" +
          "- 더미 플레이어는 자동으로 답안을 제출하지 않습니다."
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "즉시 매칭 성공"),
      @ApiResponse(responseCode = "401", description = "인증 실패")
  })
  @PostMapping("/match/demo/instant")
  public MatchingDtos.MatchStatusResp instantMatchDemo(
      @io.swagger.v3.oas.annotations.parameters.RequestBody(
          description = "매칭 요청",
          required = true
      )
      @Valid @RequestBody MatchingDtos.MatchRequest request) {
    String userId = AuthUserUtil.getCurrentUserId();
    return demoMatchingService.instantMatchWithDemo(userId, request);
  }

    @Operation(
            summary = "연습 봇과 1:1 배틀 시작",
            description = "연습 봇과 즉시 1:1 배틀을 시작합니다.\n\n" +
                    "**기능:**\n" +
                    "- DUEL 방 자동 생성\n" +
                    "- 사용자 + 봇 1명 자동 참가 (총 2명)\n" +
                    "- 문제 자동 생성 (필기/실기 모드 선택 가능)\n" +
                    "- **필기 모드 (WRITTEN)**: OX 2개 + MCQ 8개 (총 10문제)\n" +
                    "- **실기 모드 (PRACTICAL)**: SHORT 8개 + LONG 2개 (총 10문제)\n" +
                    "- 봇 자동 플레이 시작 (1.5~3초 간격으로 답안 제출)\n\n" +
                    "**봇 동작:**\n" +
                    "- 70% 확률 정답, 30% 확률 오답\n" +
                    "- 각 문제마다 1.5~3초 랜덤 딜레이\n" +
                    "- 정답 시 점수 획득, 오답 시 0점\n\n" +
                    "**파라미터:**\n" +
                    "- **examMode**: \"WRITTEN\" (기본값) 또는 \"PRACTICAL\"\n" +
                    "- **scopeType**: CATEGORY (카테고리 모드) 또는 DIFFICULTY (난이도 모드, 기본값)\n" +
                    "- **topicId**: 카테고리 모드일 때 2레벨 토픽 ID (필수)\n" +
                    "- **difficulty**: 난이도 모드일 때 EASY, NORMAL (기본값), HARD\n\n" +
                    "**예시:**\n" +
                    "- 필기 모드 (난이도): `?examMode=WRITTEN&scopeType=DIFFICULTY&difficulty=HARD`\n" +
                    "- 실기 모드 (카테고리): `?examMode=PRACTICAL&scopeType=CATEGORY&topicId=101`\n\n" +
                    "**이벤트:**\n" +
                    "- ROOM_CREATED, PLAYER_JOINED, MATCH_STARTED\n" +
                    "- BOT_ANSWERED, SCORE_UPDATED (봇 답안 제출 시)\n" +
                    "- BOT_PLAY_COMPLETED (봇 플레이 완료 시)"
    )
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "DUEL 봇 매칭 시작 성공"),
            @ApiResponse(responseCode = "401", description = "인증 실패"),
            @ApiResponse(responseCode = "400", description = "잘못된 파라미터")
    })
    @PostMapping("/match/duel/bot")
    public VersusMatchService.DuelWithBotResult startDuelWithBot(
            @Parameter(description = "시험 모드 (WRITTEN: 필기, PRACTICAL: 실기)", example = "WRITTEN")
            @RequestParam(required = false, defaultValue = "WRITTEN") String examMode,
            @Parameter(description = "모드 타입 (CATEGORY: 카테고리 모드, DIFFICULTY: 난이도 모드)", example = "DIFFICULTY")
            @RequestParam(required = false, defaultValue = "DIFFICULTY") String scopeType,
            @Parameter(description = "카테고리 모드일 때 2레벨 토픽 ID", example = "101")
            @RequestParam(required = false) Long topicId,
            @Parameter(description = "난이도 모드일 때 난이도 (EASY, NORMAL, HARD)", example = "NORMAL")
            @RequestParam(required = false, defaultValue = "NORMAL") String difficulty) {

        String userId = AuthUserUtil.getCurrentUserId();
        return versusMatchService.startDuelWithBot(userId, examMode, scopeType, topicId, difficulty);
    }

    @Operation(
            summary = "연습 봇과 토너먼트 시작",
            description = "연습 봇과 즉시 토너먼트(8강)를 시작합니다.\n\n" +
                    "**기능:**\n" +
                    "- TOURNAMENT 방 자동 생성\n" +
                    "- 사용자 + 봇 7명 자동 참가 (총 8명)\n" +
                    "- 문제 자동 생성 (필기/실기 모드 선택 가능)\n" +
                    "- **필기 모드 (WRITTEN)**: 1R OX 3개, 2R MCQ 3개, 3R MCQ 3개 (총 9문제)\n" +
                    "- **실기 모드 (PRACTICAL)**: 1R SHORT 3개, 2R SHORT 3개, 3R SHORT 1개 + LONG 2개 (총 9문제)\n" +
                    "- 모든 봇이 각 라운드 문제를 자동으로 풀고 답안 제출\n" +
                    "- 라운드별 탈락 처리 자동 진행\n\n" +
                    "**봇 구성:**\n" +
                    "- EASY 봇 2명, NORMAL 봇 3명, HARD 봇 2명\n" +
                    "- 난이도별 정답률 및 시간 지연 적용\n\n" +
                    "**파라미터:**\n" +
                    "- examMode: \"WRITTEN\" (기본값) 또는 \"PRACTICAL\""
    )
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "토너먼트 봇 매칭 시작 성공"),
            @ApiResponse(responseCode = "401", description = "인증 실패"),
            @ApiResponse(responseCode = "400", description = "잘못된 examMode (WRITTEN 또는 PRACTICAL만 가능)")
    })
    @PostMapping("/match/tournament/bot")
    public VersusMatchService.TournamentWithBotResult startTournamentWithBot(
            @Parameter(description = "시험 모드 (WRITTEN: 필기, PRACTICAL: 실기)", example = "WRITTEN")
            @RequestParam(required = false, defaultValue = "WRITTEN") String examMode) {
        String userId = AuthUserUtil.getCurrentUserId();
        return versusMatchService.startTournamentWithBot(userId, examMode);
    }

    @Operation(
            summary = "연습 봇과 골든벨 시작",
            description = "연습 봇과 즉시 골든벨을 시작합니다.\n\n" +
                    "**기능:**\n" +
                    "- GOLDENBELL 방 자동 생성\n" +
                    "- 사용자 + 봇 19명 자동 참가 (총 20명)\n" +
                    "- examMode에 따라 필기/실기 골든벨 자동 구성\n" +
                    "- **필기 골든벨 (WRITTEN)**: OX 2개, MCQ 2개, MCQ(REVIVAL) 1개, MCQ(FINAL) 2개\n" +
                    "- **실기 골든벨 (PRACTICAL)**: SHORT 7개 (LONG 제거)\n" +
                    "- 모든 생존 봇이 각 라운드 문제를 자동으로 풀고 답안 제출\n" +
                    "- 오답 시 즉시 탈락, 부활전 자동 처리\n\n" +
                    "**봇 구성:**\n" +
                    "- EASY 봇 6명, NORMAL 봇 7명, HARD 봇 6명\n" +
                    "- 난이도별 정답률 및 시간 지연 적용\n\n" +
                    "**파라미터:**\n" +
                    "- examMode: \"WRITTEN\" (기본값) 또는 \"PRACTICAL\""
    )
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "골든벨 봇 매칭 시작 성공"),
            @ApiResponse(responseCode = "401", description = "인증 실패"),
            @ApiResponse(responseCode = "400", description = "잘못된 examMode (WRITTEN 또는 PRACTICAL만 가능)")
    })
    @PostMapping("/match/goldenbell/bot")
    public VersusMatchService.GoldenbellWithBotResult startGoldenbellWithBot(
            @Parameter(description = "시험 모드: WRITTEN(필기) 또는 PRACTICAL(실기), 기본값: WRITTEN")
            @RequestParam(required = false, defaultValue = "WRITTEN") String examMode) {
        String userId = AuthUserUtil.getCurrentUserId();
        return versusMatchService.startGoldenbellWithBot(userId, examMode);
    }

    @Operation(
      summary = "타임라인 이벤트 조회",
      description = "방의 타임라인 이벤트를 조회합니다.\n\n" +
          "**이벤트 종류:**\n" +
          "- ROOM_CREATED, PLAYER_JOINED, MATCH_STARTED\n" +
          "- QUESTIONS_REGISTERED\n" +
          "- BOT_ANSWERED, ANSWER_SUBMITTED\n" +
          "- SCORE_UPDATED, ROUND_COMPLETED\n" +
          "- PLAYER_ELIMINATED, MATCH_FINISHED\n" +
          "- BOT_PLAY_COMPLETED"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "조회 성공")
  })
  @GetMapping("/rooms/{roomId}/events")
  public List<VersusDtos.TimelineEvent> getEvents(
      @Parameter(description = "방 ID", example = "1", required = true)
      @PathVariable Long roomId,
      @Parameter(description = "조회할 이벤트 개수 (기본값: 50, 최대: 200)", example = "50")
      @RequestParam(defaultValue = "50") int limit) {
    return versusService.timeline(roomId, limit);
  }

  @Operation(
      summary = "방의 문제 목록 조회",
      description = "방에 등록된 문제 목록을 조회합니다.\n\n" +
          "**용도:**\n" +
          "- 답안 제출 시 questionId 확인\n" +
          "- 더미 문제 생성 시 questionId 확인 (90001~90010)\n\n" +
          "**응답:**\n" +
          "- questionId: 답안 제출 시 사용할 문제 ID\n" +
          "- roundNo: 라운드 번호\n" +
          "- phase: 페이즈 (MAIN, FINAL)\n" +
          "- orderNo: 문제 순서\n" +
          "- timeLimitSec: 시간 제한 (초)"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "조회 성공")
  })
  @GetMapping("/rooms/{roomId}/questions")
  public List<VersusDtos.QuestionInfo> getRoomQuestions(
      @Parameter(description = "방 ID", example = "1", required = true)
      @PathVariable Long roomId) {
    return versusService.getRoomQuestions(roomId);
  }

  @Operation(
      summary = "문제별 답안 목록 조회 (골든벨용)",
      description = "특정 문제에 대한 모든 사용자의 답안을 조회합니다.\n\n" +
          "**모드별 동작:**\n" +
          "- GOLDENBELL: 모든 사용자의 답안 반환 (단답식/서술형 답안 텍스트 포함)\n" +
          "- DUEL/TOURNAMENT: 빈 리스트 반환 (상대방 답 안 띄움)\n\n" +
          "**답안 정보:**\n" +
          "- userAnswer: 사용자가 제출한 답안 (OX/MCQ: label, SHORT/LONG: 텍스트)\n" +
          "- correct: 정답 여부\n" +
          "- timeMs: 문제 풀이 소요 시간\n" +
          "- scoreDelta: 점수 변화량"
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "조회 성공")
  })
  @GetMapping("/rooms/{roomId}/questions/{questionId}/answers")
  public VersusDtos.QuestionAnswersResp getQuestionAnswers(
      @Parameter(description = "방 ID", example = "1", required = true)
      @PathVariable Long roomId,
      @Parameter(description = "문제 ID", example = "1001", required = true)
      @PathVariable Long questionId) {
    return versusService.getQuestionAnswers(roomId, questionId);
  }

  @Operation(
      summary = "문제 시작 이벤트 기록 (테스트용)",
      description = "모든 모드(1:1 배틀, 토너먼트, 골든벨)에서 문제 시작 이벤트를 직접 기록합니다. (Swagger 테스트용)\n\n" +
          "**사용 시나리오:**\n" +
          "- 모든 모드에서 다음 문제가 시작될 때 QUESTION_STARTED 이벤트를 기록합니다.\n" +
          "- 모든 참가자가 동시에 시작하도록 `allParticipants: true`로 기록됩니다.\n" +
          "- 시간 계산의 기준점이 됩니다.\n\n" +
          "**자동 기록 시점:**\n" +
          "- `startRoom` 호출 시: 첫 번째 문제 자동 기록\n" +
          "- `ROUND_COMPLETED` 후: 다음 문제가 있으면 자동 기록\n\n" +
          "**주의사항:**\n" +
          "- 실제 게임에서는 위 시점에서 자동으로 기록되므로 이 API는 테스트용입니다."
  )
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "이벤트 기록 성공"),
      @ApiResponse(responseCode = "400", description = "잘못된 요청 (문제가 없거나 이미 기록됨)")
  })
  @PostMapping("/rooms/{roomId}/questions/{questionId}/start")
  public Map<String, Object> startQuestion(
      @Parameter(description = "방 ID", example = "1", required = true)
      @PathVariable Long roomId,
      @Parameter(description = "문제 ID", example = "1001", required = true)
      @PathVariable Long questionId) {
    return versusService.recordQuestionStartEvent(roomId, questionId);
  }
}
