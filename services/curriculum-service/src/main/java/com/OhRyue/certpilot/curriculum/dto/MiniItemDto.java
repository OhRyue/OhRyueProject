package com.OhRyue.certpilot.curriculum.dto;

import java.util.List;

public record MiniItemDto(
        Long id,
        String stem,
        List<String> choices,
        Integer answerIdx,
        String explanation
) {}

