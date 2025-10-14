package com.OhRyue.certpilot.quiz.web.dto;

import java.util.List;
import java.util.Map;

public record QuickGradeResult(int score, int total, List<Long> wrongIds, Map<Long, String> explanations) {}
