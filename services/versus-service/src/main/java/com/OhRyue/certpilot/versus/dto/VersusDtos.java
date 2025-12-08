package com.OhRyue.certpilot.versus.dto;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchPhase;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import java.time.Instant;
import java.util.List;
import java.util.Map;

public class VersusDtos {

  @Valid
  @Schema(description = "대전 방 생성 요청")
  public record CreateRoomReq(
      @NotNull
      @Schema(description = "대전 모드 (DUEL: 1:1, TOURNAMENT: 토너먼트, GOLDENBELL: 골든벨)", 
          example = "DUEL", required = true)
      MatchMode mode,
      
      @Schema(description = "문제 생성 범위 JSON (scopeJson 사용 시 문제 자동 생성)\n" +
          "예시: {\"examMode\":\"WRITTEN\",\"difficulty\":\"NORMAL\",\"topicScope\":\"ALL\"}", 
          example = "{\"examMode\":\"WRITTEN\",\"difficulty\":\"NORMAL\",\"topicScope\":\"ALL\"}")
      String scopeJson,
      
      @Schema(description = "초대할 참가자 ID 리스트 (방 생성자는 자동 참가)\n" +
          "인원 제한: DUEL(최대 2명), TOURNAMENT(최대 8명), GOLDENBELL(최대 20명)", 
          example = "[\"user2\"]")
      List<@NotBlank String> participants,
      
      @Schema(description = "문제 리스트 (questions 제공 시 scopeJson 무시)\n" +
          "scopeJson과 questions 중 하나는 반드시 제공해야 함")
      List<@Valid QuestionInfo> questions,
      
      @Schema(description = "토너먼트 브라켓 JSON (TOURNAMENT 모드용)")
      String tournamentBracketJson,
      
      @Schema(description = "토너먼트 브라켓 라운드 (TOURNAMENT 모드용)")
      Integer tournamentBracketRound,
      
      @Schema(description = "골든벨 규칙 JSON (GOLDENBELL 모드용)")
      String goldenbellRuleJson,
      
      @Schema(description = "예약 시작 시간 (GOLDENBELL 모드용, ISO 8601 형식)\n" +
          "예: \"2024-12-25T14:00:00Z\"\n" +
          "설정 시 10분 전부터 입장 가능, 시간이 되면 자동 시작")
      Instant scheduledAt,
      
      @Schema(description = "방 생성자 자동 참가 스킵 여부 (기본값: false)\n" +
          "true로 설정하면 방 생성자가 자동으로 참가하지 않습니다.\n" +
          "0명인 방을 만들고 싶을 때 사용합니다.", example = "false")
      Boolean skipCreatorJoin
  ) {}

  // JoinRoomReq 는 더 이상 필요 없으므로 제거했습니다.
  // public record JoinRoomReq(...)

  @Schema(description = "답안 제출 요청")
  public record SubmitAnswerReq(
      @NotNull
      @Schema(description = "문제 ID", example = "1001", required = true)
      Long questionId,
      
      @Schema(description = "사용자가 제출한 답안 (필수)\n" +
          "- MCQ/OX: 선택한 label (예: \"A\", \"B\", \"O\", \"X\")\n" +
          "- SHORT/LONG: 입력한 텍스트 (예: \"정규화\", \"데이터베이스 설계\")", 
          example = "A", required = true)
      String userAnswer,
      
      @Schema(description = "클라이언트가 계산한 정답 여부 (서버 검증과 비교용, 참고용)", 
          example = "true")
      boolean correct,
      
      @Schema(description = "문제 풀이 소요 시간 (밀리초, 선택사항)\n" +
          "**중요**: 서버에서 QUESTION_STARTED 이벤트 기준으로 자동 계산되므로 이 값은 무시됩니다.\n" +
          "하위 호환성을 위해 유지되지만, null로 보내도 됩니다.", 
          example = "5000")
      Integer timeMs,
      
      @Schema(description = "점수 변화량 (자동 계산되므로 보통 null)", example = "100")
      Integer scoreDelta,
      
      @Schema(description = "라운드 번호 (자동 설정되므로 보통 null)", example = "1")
      Integer roundNo,
      
      @Schema(description = "페이즈 (MAIN, FINAL 등, 자동 설정되므로 보통 null)", example = "MAIN")
      MatchPhase phase
  ) {}

  public record RoomSummary(
      Long roomId,
      MatchMode mode,
      MatchStatus status,
      int participantCount,
      Instant createdAt,
      @Schema(description = "예약 시작 시간 (GOLDENBELL 모드용, ISO 8601 형식). 예약이 없으면 null", example = "2024-12-25T14:00:00Z")
      Instant scheduledAt,
      @Schema(description = "시험 모드 (WRITTEN: 필기, PRACTICAL: 실기). scopeJson에서 추출. 없으면 null", example = "WRITTEN")
      String examMode
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
      @Schema(description = "사용자 닉네임", example = "플레이어123")
      String nickname,
      @Schema(description = "사용자 스킨 ID", example = "1")
      Long skinId,
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
      @Schema(description = "사용자 닉네임", example = "플레이어123")
      String nickname,
      @Schema(description = "사용자 스킨 ID", example = "1")
      Long skinId,
      int correctCount,
      int totalCount,
      int score,
      Long totalTimeMs,
      Integer rank,
      boolean alive,
      boolean revived
  ) {}

  public record CurrentQuestionInfo(
      @Schema(description = "현재 진행 중인 문제 ID", example = "92")
      Long questionId,
      @Schema(description = "현재 라운드 번호", example = "1")
      Integer roundNo,
      @Schema(description = "현재 페이즈 (MAIN, FINAL, REVIVAL)", example = "MAIN")
      MatchPhase phase,
      @Schema(description = "문제 순서 번호", example = "1")
      Integer orderNo,
      @Schema(description = "시간 제한 (초)", example = "5")
      Integer timeLimitSec,
      @Schema(description = "문제 종료 시간 (ISO 8601 형식). 문제가 진행 중이 아니면 null", example = "2025-12-02T03:15:06Z")
      Instant endTime
  ) {}

  public record IntermissionInfo(
      @Schema(description = "다음 문제 ID", example = "205")
      Long nextQuestionId,
      @Schema(description = "다음 라운드 번호", example = "4")
      Integer nextRoundNo,
      @Schema(description = "다음 페이즈 (MAIN, FINAL, REVIVAL)", example = "FINAL")
      MatchPhase nextPhase,
      @Schema(description = "쉬는 시간 (초)", example = "5")
      Integer durationSec,
      @Schema(description = "쉬는 시간 시작 시간 (ISO 8601 형식)", example = "2025-12-04T10:26:03.837006519Z")
      Instant startedAt,
      @Schema(description = "다음 문제 시작 시간 (ISO 8601 형식)", example = "2025-12-04T10:26:08.837006519Z")
      Instant questionStartAt
  ) {}

  public record ScoreBoardResp(
      Long roomId,
      MatchStatus status,
      List<ScoreBoardItem> items,
      @Schema(description = "현재 진행 중인 문제 정보. 문제가 진행 중이 아니면 null")
      CurrentQuestionInfo currentQuestion,
      @Schema(description = "쉬는 시간 정보. 쉬는 시간 중일 때만 null이 아님")
      IntermissionInfo intermission,
      @Schema(description = "매치 종료 시 progress-service에서 계산된 XP 지급 결과")
      List<XpResult> xpResults
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

  /**
   * 답안 정보 (프론트엔드 표시용)
   */
  public record AnswerInfo(
      String userId,
      @Schema(description = "사용자 닉네임", example = "플레이어123")
      String nickname,
      @Schema(description = "사용자 스킨 ID", example = "1")
      Long skinId,
      String userAnswer,
      boolean correct,
      Integer timeMs,
      Integer scoreDelta,
      Instant submittedAt
  ) {}

  /**
   * 문제별 답안 목록 응답
   */
  public record QuestionAnswersResp(
      Long questionId,
      List<AnswerInfo> answers
  ) {}

  @Schema(description = "XP 지급 결과")
  public record XpResult(
      @Schema(description = "사용자 ID", example = "user1")
      String userId,
      @Schema(description = "이번 경기에서 지급된 XP", example = "30")
      Integer xpDelta,
      @Schema(description = "지급 사유", example = "DUEL_WIN")
      String reason,
      @Schema(description = "지급 후 누적 XP", example = "1500")
      Long totalXp,
      @Schema(description = "레벨업 여부", example = "false")
      Boolean leveledUp
  ) {}
}
