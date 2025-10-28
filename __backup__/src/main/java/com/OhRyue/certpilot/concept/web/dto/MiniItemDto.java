package com.OhRyue.certpilot.concept.web.dto;

import java.util.List;

public record MiniItemDto(
        Long id,
        String stem,
        List<String> choices,   // ← JSON 파싱 결과
        Integer answerIdx,
        String description
) {}
