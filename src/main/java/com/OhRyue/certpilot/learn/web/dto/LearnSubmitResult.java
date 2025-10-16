package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;

public record LearnSubmitResult(
        int miniCorrect, int miniTotal,
        int quizCorrect, int quizTotal,
        List<WeakTag> weakTags,
        List<String> nextActions
) {
    public record WeakTag(String tag, double ema, int attempts) {}
}
