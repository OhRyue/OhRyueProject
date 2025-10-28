package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;

public record LearnSubmitRequest(
        Long userId,
        Long conceptId,
        List<Answer> miniAnswers,
        List<Answer> quizAnswers
) {
    public record Answer(Long questionId, Integer chosenIdx, Long elapsedMs) {}
}
