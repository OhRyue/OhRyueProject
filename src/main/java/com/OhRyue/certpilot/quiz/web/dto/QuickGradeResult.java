package com.OhRyue.certpilot.quiz.web.dto;

import java.util.List;
import java.util.Map;

public record QuickGradeResult(
    int score,
    int total,
    List<Long> wrongIds,
    Map<Long, String> explanations,   // DB 기본 해설(exp)
    Map<Long, String> aiExplanations  // AI 해설(실패 시 DB 해설로 폴백)
) {}
