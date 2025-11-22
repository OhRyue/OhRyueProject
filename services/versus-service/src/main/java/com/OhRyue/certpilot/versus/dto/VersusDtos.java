package com.OhRyue.certpilot.versus.dto;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchPhase;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import java.time.Instant;
import java.util.List;
import java.util.Map;

public class VersusDtos {

  @Valid
  public record CreateRoomReq(
      @NotNull MatchMode mode,
      String scopeJson,
      // 🔸 초대 대상자(다른 사람들)의 userId 리스트는 도메인 데이터라서 그대로 유지
      List<@NotBlank String> participants,
      List<@Valid QuestionInfo> questions,
      String tournamentBracketJson,
      Integer tournamentBracketRound,
      String goldenbellRuleJson
  ) {}

  // 🔥 JoinRoomReq 는 더 이상 필요 없으므로 제거했습니다.
  // public record JoinRoomReq(...)

  public record SubmitAnswerReq(
      @NotNull Long questionId,
      // 🔥 userId 제거 – JWT에서 가져옵니다.
      boolean correct,
      @Min(0) Integer timeMs,
      Integer scoreDelta,
      Integer roundNo,
      MatchPhase phase
  ) {}

  public record RoomSummary(
      Long roomId,
      MatchMode mode,
      MatchStatus status,
      int participantCount,
      Instant createdAt
  ) {}

  public record QuestionInfo(
      @NotNull Long questionId,
      @NotNull Integer roundNo,
      MatchPhase phase,
      @NotNull Integer order,
      @NotNull Integer timeLimitSec
  ) {}

  public record ParticipantSummary(
      String userId,
      Integer finalScore,
      Integer rank,
      boolean alive,
      boolean revived,
      Instant joinedAt
  ) {}

  public record RoomDetailResp(
      RoomSummary room,
      List<ParticipantSummary> participants,
      List<QuestionInfo> questions,
      String tournamentBracketJson,
      String goldenbellRuleJson,
      ScoreBoardResp scoreboard
  ) {}

  public record ScoreBoardItem(
      String userId,
      int correctCount,
      int totalCount,
      int score,
      Long totalTimeMs,
      Integer rank,
      boolean alive,
      boolean revived
  ) {}

  public record ScoreBoardResp(
      Long roomId,
      MatchStatus status,
      List<ScoreBoardItem> items
  ) {}

  public record TimelineEvent(
      String type,
      Instant occurredAt,
      Map<String, Object> payload
  ) {}

  public record RealtimeSnapshot(
      ScoreBoardResp scoreboard,
      Integer activeRound,
      MatchPhase activePhase,
      Instant updatedAt
  ) {}

  public record RoomStateResp(
      RoomDetailResp detail,
      List<TimelineEvent> timeline,
      RealtimeSnapshot realtime
  ) {}
}
