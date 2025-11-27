package com.OhRyue.certpilot.versus.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@FeignClient(
    name = "study-service", 
    path = "/api/study",
    fallback = StudyServiceClientFallback.class
)
public interface StudyServiceClient {

    /**
     * Versus 모드용 문제 세트 생성
     * scopeJson 기반으로 문제를 생성합니다.
     */
    @PostMapping("/versus/questions")
    List<QuestionDto> generateVersusQuestions(@RequestBody VersusQuestionRequest request);

    /**
     * 문제 정보 조회 (정답 검증용)
     */
    @GetMapping("/versus/questions/{questionId}")
    QuestionDto getQuestion(@PathVariable Long questionId);

    /**
     * 정답 검증
     */
    @PostMapping("/versus/questions/{questionId}/validate")
    AnswerValidationResult validateAnswer(
        @PathVariable Long questionId,
        @RequestBody UserAnswerDto userAnswer
    );

    record VersusQuestionRequest(
        String examMode,  // WRITTEN, PRACTICAL
        String topicScope,  // ALL, 특정 topicId
        Long topicId,
        String difficulty,  // EASY, NORMAL, HARD
        Integer count,
        List<QuestionTypeSpec> questionTypes  // [{type: "OX", count: 2}, {type: "MCQ", count: 8}]
    ) {}

    record QuestionTypeSpec(
        String type,  // OX, MCQ, SHORT, LONG
        Integer count
    ) {}

    record QuestionDto(
        Long id,
        String mode,  // WRITTEN, PRACTICAL
        String type,  // OX, MCQ, SHORT, LONG
        String difficulty,
        String stem,
        String answerKey,
        String solutionText,
        Map<String, Object> payloadJson
    ) {}

    record UserAnswerDto(
        String answer,  // 사용자가 제출한 답안
        String answerType  // label (MCQ/OX), text (SHORT/LONG)
    ) {}

    record AnswerValidationResult(
        boolean correct,
        String correctAnswer,
        String explanation
    ) {}
}

