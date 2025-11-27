package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import java.util.List;
import java.util.Map;

public class VersusDtos {

    @Schema(description = "Versus 모드용 문제 생성 요청")
    public record VersusQuestionRequest(
        @NotBlank
        @Schema(description = "시험 모드 (WRITTEN: 필기, PRACTICAL: 실기)", 
            example = "WRITTEN", required = true)
        String examMode,
        
        @Schema(description = "토픽 범위 (ALL: 전체, SPECIFIC: 특정 토픽)", 
            example = "ALL")
        String topicScope,
        
        @Schema(description = "특정 토픽 ID (topicScope가 SPECIFIC일 때 필수)", 
            example = "100")
        Long topicId,
        
        @Schema(description = "난이도 (EASY, NORMAL, HARD)", 
            example = "NORMAL")
        String difficulty,
        
        @Min(1)
        @Schema(description = "생성할 문제 개수", 
            example = "10", required = true)
        Integer count,
        
        @Valid
        @Schema(description = "문제 유형별 개수 지정", 
            example = "[{\"type\":\"OX\",\"count\":2},{\"type\":\"MCQ\",\"count\":8}]", 
            required = true)
        List<QuestionTypeSpec> questionTypes
    ) {}

    @Schema(description = "문제 유형별 개수 지정")
    public record QuestionTypeSpec(
        @NotBlank
        @Schema(description = "문제 유형 (OX, MCQ, SHORT, LONG)", 
            example = "MCQ", required = true)
        String type,
        
        @Min(1)
        @Schema(description = "해당 유형의 문제 개수", 
            example = "8", required = true)
        Integer count
    ) {}

    public record QuestionDto(
        Long id,
        String mode,  // WRITTEN, PRACTICAL
        String type,  // OX, MCQ, SHORT, LONG
        String difficulty,
        String stem,
        String answerKey,
        String solutionText,
        Map<String, Object> payloadJson
    ) {}

    @Schema(description = "사용자 답안")
    public record UserAnswerDto(
        @NotBlank
        @Schema(description = "사용자가 제출한 답안\n" +
            "- MCQ/OX: label (예: \"A\", \"B\", \"O\", \"X\")\n" +
            "- SHORT/LONG: 텍스트 (예: \"정규화\")", 
            example = "A", required = true)
        String answer,
        
        @Schema(description = "답안 유형 (label: MCQ/OX, text: SHORT/LONG)", 
            example = "label")
        String answerType
    ) {}

    public record AnswerValidationResult(
        boolean correct,
        String correctAnswer,
        String explanation
    ) {}
}

