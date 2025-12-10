package com.OhRyue.certpilot.progress.feign.dto;

import java.time.LocalDateTime;
import java.util.List;

public record StudySessionDetailDto(
        Long sessionId,
        String userId,
        List<QuestionDetailDto> questions,
        String topicScopeJson  // topicId 또는 rootTopicId를 포함한 JSON 문자열
) {
    public record QuestionDetailDto(
            int order,
            Long questionId,
            String questionType,
            String stem,
            String myAnswer,
            String correctAnswer,
            boolean isCorrect,
            LocalDateTime answeredAt,
            Long timeTakenMs,
            Integer score
    ) {}
}



