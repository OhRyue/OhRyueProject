package com.OhRyue.certpilot.auxiliary.web.dto;

import java.util.List;

/** 혼자풀기 - 카테고리(Topic) 지정 시작 요청 */
public record AuxSoloCategoryStartRequest(
        Long userId,
        List<Long> topicIds,   // topic_id 목록(세부/세세 아무 레벨 상관없이 가능)
        Integer count          // 기본 10
) {}
