package com.OhRyue.certpilot.auxiliary.web.dto;

/** 혼자풀기 - 약점 보완 시작 요청 (EMA/오답 기반 태그 활용) */
public record AuxSoloWeakStartRequest(
        Long userId,
        Integer count          // 기본 10
) {}
