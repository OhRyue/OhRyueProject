package com.OhRyue.certpilot.cert.dto;

import java.util.List;

public class CurriculumDtos {

    public record TopicResponse(
            Long id,
            Long certId,
            Long parentId,
            String code,
            String title,
            String emoji,
            String examMode,
            Integer orderNo
    ) {}

    public record TopicListResponse(
            List<TopicResponse> topics
    ) {}

    public record ConceptResponse(
            Long topicId,
            String sectionsJson
    ) {}
}
