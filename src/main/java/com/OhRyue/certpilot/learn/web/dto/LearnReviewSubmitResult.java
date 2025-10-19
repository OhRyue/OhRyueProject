package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;
import java.util.Map;

public record LearnReviewSubmitResult(
    int score,
    int total,
    Map<Long, Boolean> correctness,
    Map<Long, String> explanations,
    String aiSummary
) {}
