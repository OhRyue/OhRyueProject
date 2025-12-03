package com.OhRyue.certpilot.progress.feign.dto;

import java.time.LocalDateTime;
import java.util.List;

public record MatchDetailDto(
        Long matchId,
        String userId,
        List<QuestionDetailDto> questions
) {
    public record QuestionDetailDto(
            int order,
            Integer roundNo,
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



