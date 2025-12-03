package com.OhRyue.certpilot.versus.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.time.LocalDateTime;
import java.util.List;

public final class InternalDtos {

    private InternalDtos() {}

    @Schema(description = "매치 상세 조회 응답 (내부 API용)")
    public record MatchDetailDto(
            @Schema(description = "매치 ID (roomId)") Long matchId,
            @Schema(description = "사용자 ID") String userId,
            @Schema(description = "문제 리스트") List<QuestionDetailDto> questions
    ) {}

    @Schema(description = "문제 상세 정보")
    public record QuestionDetailDto(
            @Schema(description = "문제 순번") int order,
            @Schema(description = "라운드 번호") Integer roundNo,
            @Schema(description = "문제 ID") Long questionId,
            @Schema(description = "문제 유형 (OX, MCQ, SHORT, LONG)") String questionType,
            @Schema(description = "문제 지문") String stem,
            @Schema(description = "내 답") String myAnswer,
            @Schema(description = "정답") String correctAnswer,
            @Schema(description = "정답 여부") boolean isCorrect,
            @Schema(description = "답변 시각") LocalDateTime answeredAt,
            @Schema(description = "소요 시간(밀리초)") Long timeTakenMs,
            @Schema(description = "점수") Integer score
    ) {}
}



