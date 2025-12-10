package com.OhRyue.certpilot.progress.dto.activity;

import com.OhRyue.certpilot.progress.domain.enums.ActivityGroup;
import com.OhRyue.certpilot.progress.domain.enums.AssistType;
import com.OhRyue.certpilot.progress.domain.enums.BattleType;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import com.OhRyue.certpilot.progress.domain.enums.MainType;
import com.OhRyue.certpilot.progress.domain.enums.MainStepType;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;

import java.time.LocalDateTime;
import java.util.List;

public final class ActivityDtos {

    private ActivityDtos() {}

    @Schema(description = "오늘의 성과 요약")
    public record TodaySummaryDto(
            @Schema(description = "오늘 푼 문제 수") int solvedQuestionCount,
            @Schema(description = "오늘 학습 시간(분)") int studyTimeMinutes,
            @Schema(description = "오늘 전체 정답률(%)") double accuracyPct,
            @Schema(description = "오늘 획득 XP") int gainedXp
    ) {}

    @Schema(description = "최근 학습 기록 항목 (5개)")
    public record RecentActivityItemDto(
            @Schema(description = "활동 ID") Long activityId,
            @Schema(description = "활동 그룹") ActivityGroup activityGroup,
            @Schema(description = "메인 타입") MainType mainType,
            @Schema(description = "메인 스텝 타입 (MICRO일 때만)") MainStepType mainStepType,
            @Schema(description = "보조 타입") AssistType assistType,
            @Schema(description = "배틀 타입") BattleType battleType,
            @Schema(description = "모드 (필기/실기)") ExamMode mode,
            @Schema(description = "표시용 텍스트", example = "메인학습 · Micro(MINI) · 필기") String displayText,
            @Schema(description = "시작 시간") LocalDateTime startedAt,
            @Schema(description = "종료 시간") LocalDateTime finishedAt
    ) {}

    @Schema(description = "활동 리스트 항목 (전체보기)")
    public record ActivityListItemDto(
            @Schema(description = "활동 ID") Long activityId,
            @Schema(description = "활동 그룹") ActivityGroup activityGroup,
            @Schema(description = "메인 타입") MainType mainType,
            @Schema(description = "메인 스텝 타입 (MICRO일 때만)") MainStepType mainStepType,
            @Schema(description = "보조 타입") AssistType assistType,
            @Schema(description = "배틀 타입") BattleType battleType,
            @Schema(description = "모드 (필기/실기)") ExamMode mode,
            @Schema(description = "토픽명") String topicName,
            @Schema(description = "약점 태그명") String weaknessTagName,
            @Schema(description = "난이도") String difficulty,
            @Schema(description = "정답률(%)") Double accuracyPct,
            @Schema(description = "최종 순위") Integer finalRank,
            @Schema(description = "획득 XP") Integer xpGained,
            @Schema(description = "수행 시간") LocalDateTime performedAt
    ) {}

    @Schema(description = "활동 상세 헤더")
    public record ActivityDetailHeaderDto(
            @Schema(description = "활동 ID") Long activityId,
            @Schema(description = "활동 그룹") ActivityGroup activityGroup,
            @Schema(description = "메인 타입") MainType mainType,
            @Schema(description = "메인 스텝 타입 (MICRO일 때만)") MainStepType mainStepType,
            @Schema(description = "보조 타입") AssistType assistType,
            @Schema(description = "배틀 타입") BattleType battleType,
            @Schema(description = "모드 (필기/실기)") ExamMode mode,
            @Schema(description = "토픽명") String topicName,
            @Schema(description = "약점 태그명") String weaknessTagName,
            @Schema(description = "난이도") String difficulty,
            @Schema(description = "수행 시간") LocalDateTime performedAt,
            @Schema(description = "문제 수") Integer questionCount,
            @Schema(description = "정답 수") Integer correctCount,
            @Schema(description = "정답률(%)") Double accuracyPct,
            @Schema(description = "최종 순위") Integer finalRank,
            @Schema(description = "획득 XP") Integer xpGained
    ) {}

    @Schema(description = "문제 상세 정보")
    public record QuestionDetailDto(
            @Schema(description = "문제 순번") int order,
            @Schema(description = "문제 ID") Long questionId,
            @Schema(description = "문제 유형 (OX, MCQ, SHORT, LONG)") String questionType,
            @Schema(description = "문제 지문") String stem,
            @Schema(description = "내 답") String myAnswer,
            @Schema(description = "정답") String correctAnswer,
            @Schema(description = "정답 여부") boolean isCorrect,
            @Schema(description = "답변 시각") LocalDateTime answeredAt,
            @Schema(description = "소요 시간(밀리초)") Long timeTakenMs,
            @Schema(description = "점수 (배틀에서 사용)") Integer score
    ) {}

    @Schema(description = "활동 상세 정보")
    public record ActivityDetailDto(
            @Schema(description = "헤더 정보") ActivityDetailHeaderDto header,
            @ArraySchema(
                    schema = @Schema(implementation = QuestionDetailDto.class),
                    arraySchema = @Schema(description = "문제 리스트")
            )
            @Schema(description = "문제 리스트") List<QuestionDetailDto> questions
    ) {}

    @Schema(description = "활동 생성 요청 (내부 API용)")
    public record ProgressActivityCreateReq(
            @Schema(description = "사용자 ID") String userId,
            @Schema(description = "활동 그룹") ActivityGroup activityGroup,
            @Schema(description = "메인 타입") MainType mainType,
            @Schema(description = "메인 스텝 타입 (MICRO일 때만)") MainStepType mainStepType,
            @Schema(description = "보조 타입") AssistType assistType,
            @Schema(description = "배틀 타입") BattleType battleType,
            @Schema(description = "모드 (필기/실기)") ExamMode mode,
            @Schema(description = "토픽 ID") Long topicId,
            @Schema(description = "토픽명") String topicName,
            @Schema(description = "약점 태그명") String weaknessTagName,
            @Schema(description = "난이도 (EASY/NORMAL/HARD)") String difficulty,
            @Schema(description = "문제 수") Integer questionCount,
            @Schema(description = "정답 수") Integer correctCount,
            @Schema(description = "정답률(%)") Double accuracyPct,
            @Schema(description = "최종 순위") Integer finalRank,
            @Schema(description = "획득 XP") Integer xpGained,
            @Schema(description = "원본 서비스명 (study/versus)") String sourceService,
            @Schema(description = "원본 세션/매치 ID") Long sourceSessionId,
            @Schema(description = "시작 시간") LocalDateTime startedAt,
            @Schema(description = "종료 시간") LocalDateTime finishedAt
    ) {}
}



