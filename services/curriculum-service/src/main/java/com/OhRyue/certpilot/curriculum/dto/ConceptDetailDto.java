package com.OhRyue.certpilot.curriculum.dto;

public record ConceptDetailDto(
        Long id, Long certId, String category, String title,
        String summary, String pitfalls, String examplesJson, String tagsJson
) {}

