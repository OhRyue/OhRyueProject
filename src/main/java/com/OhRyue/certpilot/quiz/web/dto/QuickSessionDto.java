package com.OhRyue.certpilot.quiz.web.dto;

import java.util.List;

public record QuickSessionDto(String sessionId, List<Item> items) {
    public record Item(Long id, String stem, List<String> choices, Integer answerIdx, String explain) {}
}
