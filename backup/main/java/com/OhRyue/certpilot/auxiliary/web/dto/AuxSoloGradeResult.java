package com.OhRyue.certpilot.auxiliary.web.dto;

import java.util.List;
import java.util.Map;

/** 혼자풀기 채점 결과 (문항별 AI 해설 포함) */
public record AuxSoloGradeResult(
        int score,
        int total,
        List<Long> wrongIds,
        Map<Long, String> explanations   // qid -> AI 해설(폴백: DB 해설)
) {}
