package com.OhRyue.certpilot.progress.feign.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public record ProfileSummaryResponse(
    String userId,
    String nickname,
    Long skinId,
    String timezone,
    String lang
) {}

