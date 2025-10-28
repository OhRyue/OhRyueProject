package com.OhRyue.certpilot.auxiliary.web.dto;

import java.util.List;

/** 혼자풀기 채점 요청 */
public record AuxSoloGradeRequest(
        String sessionId,                  // 선택(지금은 저장 안 하지만 추후 확장 대비)
        Long userId,                       // 있으면 오답노트/EMA 갱신
        List<Answer> answers               // { id, chosenIdx }
) {
    public record Answer(Long id, Integer chosenIdx) {}
}
