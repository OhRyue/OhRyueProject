package com.OhRyue.certpilot.progress.dto;

public record StudyFlowCompleteReq(
        String userId,
        String examMode,   // "WRITTEN" / "PRACTICAL"
        String flowType,   // "MICRO" / "REVIEW"
        Long topicId
) {}
