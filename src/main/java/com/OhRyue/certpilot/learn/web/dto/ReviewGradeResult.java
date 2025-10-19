package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;
import java.util.Map;

public record ReviewGradeResult(
    int score,
    int total,
    Map<Long, Boolean> correctness,
    String aiSummary
) {}
