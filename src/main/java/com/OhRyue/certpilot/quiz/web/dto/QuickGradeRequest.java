package com.OhRyue.certpilot.quiz.web.dto;

import java.util.List;

public record QuickGradeRequest(String sessionId, List<Answer> answers, Long userId) {
    public record Answer(Long id, Integer choiceIdx) {}
}
