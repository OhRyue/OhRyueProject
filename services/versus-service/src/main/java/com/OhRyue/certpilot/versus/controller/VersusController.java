package com.OhRyue.certpilot.versus.controller;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.service.VersusService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@Tag(name = "Versus(대전)", description = "실시간 대전/토너먼트/골든벨 API")
@RestController
@RequestMapping("/api/versus")
@RequiredArgsConstructor
public class VersusController {

  private final VersusService versusService;

  @Operation(summary = "헬스 체크")
  @GetMapping("/ping")
  public Map<String, Object> ping() {
    return Map.of("service", "versus", "ok", true);
  }

  /* -------- 방 조회 & 생성 -------- */
  @Operation(summary = "대전 방 목록 조회")
  @GetMapping("/rooms")
  public List<VersusDtos.RoomSummary> rooms(@RequestParam(required = false) MatchMode mode,
                                            @RequestParam(required = false) MatchStatus status) {
    return versusService.listRooms(mode, status);
  }

  @Operation(summary = "대전 방 생성")
  @PostMapping("/rooms")
  public VersusDtos.RoomDetailResp createRoom(@Valid @RequestBody VersusDtos.CreateRoomReq req) {
    return versusService.createRoom(req);
  }

  @Operation(summary = "대전 방 상세조회")
  @GetMapping("/rooms/{roomId}")
  public VersusDtos.RoomDetailResp roomDetail(@PathVariable Long roomId) {
    return versusService.getRoom(roomId);
  }

  /* -------- 참가/시작 -------- */
  @Operation(summary = "대전 방 참가")
  @PostMapping("/rooms/{roomId}/join")
  public VersusDtos.RoomDetailResp joinRoom(@PathVariable Long roomId,
                                            @Valid @RequestBody VersusDtos.JoinRoomReq req) {
    return versusService.joinRoom(roomId, req.userId());
  }

  @Operation(summary = "대전 방 시작")
  @PostMapping("/rooms/{roomId}/start")
  public VersusDtos.RoomDetailResp startRoom(@PathVariable Long roomId) {
    return versusService.startRoom(roomId);
  }

  /* -------- 문제 응답 & 스코어 -------- */
  @Operation(summary = "답안 제출/채점")
  @PostMapping("/rooms/{roomId}/answers")
  public VersusDtos.ScoreBoardResp submitAnswer(@PathVariable Long roomId,
                                                @Valid @RequestBody VersusDtos.SubmitAnswerReq req) {
    return versusService.submitAnswer(roomId, req);
  }

  @Operation(summary = "실시간 스코어보드 조회")
  @GetMapping("/rooms/{roomId}/scoreboard")
  public VersusDtos.ScoreBoardResp scoreboard(@PathVariable Long roomId) {
    return versusService.scoreboard(roomId);
  }

  /* -------- 실시간 상태 & 타임라인 -------- */
  @Operation(summary = "대전 진행 타임라인 조회")
  @GetMapping("/rooms/{roomId}/timeline")
  public List<VersusDtos.TimelineEvent> timeline(@PathVariable Long roomId,
                                                 @RequestParam(defaultValue = "50") int limit) {
    return versusService.timeline(roomId, limit);
  }

  @Operation(summary = "대전 방 종합 상태 조회")
  @GetMapping("/rooms/{roomId}/state")
  public VersusDtos.RoomStateResp roomState(@PathVariable Long roomId,
                                            @RequestParam(defaultValue = "50") int limit) {
    return versusService.roomState(roomId, limit);
  }
}