package com.OhRyue.certpilot.learn.web.dto;

public record LearnReviewStartRequest(
    Long userId,
    Long certId,
    Long detailTopicId, // level=3 (세부항목)
    Integer count       // 기본 20
) {}
