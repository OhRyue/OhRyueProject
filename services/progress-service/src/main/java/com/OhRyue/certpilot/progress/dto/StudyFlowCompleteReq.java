package com.OhRyue.certpilot.progress.dto;

public record StudyFlowCompleteReq(
        String userId,
        String examMode,   // "WRITTEN" / "PRACTICAL"
        String flowType,   // "MICRO" / "REVIEW"
        Long topicId,
        Double accuracyPct // 정답률 (0.0 ~ 100.0), null 가능
) {}
