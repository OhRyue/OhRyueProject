package com.OhRyue.certpilot.study.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@FeignClient(
        name = "cert-service",
        path = "/api/certs"
)
public interface CertCurriculumClient {

    /* ========================= Topic ========================= */

    @GetMapping("/topics/{topicId}")
    TopicResponse getTopic(@PathVariable("topicId") Long topicId);

    @GetMapping("/topics")
    TopicListResponse listTopics(@RequestParam(value = "certId", required = false) Long certId,
                                 @RequestParam(value = "mode", required = false) String examMode,
                                 @RequestParam(value = "parentId", required = false) Long parentId);

    @GetMapping("/topics/search")
    TopicListResponse searchTopics(@RequestParam(value = "code", required = false) String code,
                                   @RequestParam(value = "title", required = false) String title);

    /* ========================= Concept ========================= */

    @GetMapping("/concepts/{topicId}")
    ConceptResponse getConcept(@PathVariable("topicId") Long topicId);

    /* ========================= INTERNAL ========================= */

    @GetMapping("/internal/curriculum/topics/{rootTopicId}/descendants")
    List<Long> getDescendantTopicIds(@PathVariable("rootTopicId") Long rootTopicId);

    /* ========================= DTOs ========================= */

    record TopicResponse(
            Long id,
            Long certId,
            Long parentId,
            String code,
            String title,
            String emoji,
            String examMode,
            Integer orderNo
    ) {}

    record TopicListResponse(
            List<TopicResponse> topics
    ) {}

    record ConceptResponse(
            Long topicId,
            String sectionsJson
    ) {}
}
