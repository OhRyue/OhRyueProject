package com.OhRyue.certpilot.learn.web.dto;

public record LearnMicroStartRequest(
    Long userId,
    Long certId,
    Long microTopicId   // level=4 (세세항목)
) {}
