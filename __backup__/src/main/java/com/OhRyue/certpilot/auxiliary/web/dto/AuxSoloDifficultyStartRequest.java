package com.OhRyue.certpilot.auxiliary.web.dto;

/** 혼자풀기 - 난이도 범위 시작 요청 */
public record AuxSoloDifficultyStartRequest(
        Long userId,
        Integer minDifficulty,   // 기본 1
        Integer maxDifficulty,   // 기본 3
        Integer count            // 기본 10
) {}
