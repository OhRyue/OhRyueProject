package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;

public record LearnTodayDto(
        Long conceptId,
        String conceptTitle,
        String conceptSummary,
        String conceptPitfalls,
        List<MiniQ> miniChecks,
        List<QuizQ> quiz
) {
    public record MiniQ(Long id, String stem, java.util.List<String> choices) {}
    public record QuizQ(Long id, String stem, java.util.List<String> choices, Integer difficulty) {}
}
