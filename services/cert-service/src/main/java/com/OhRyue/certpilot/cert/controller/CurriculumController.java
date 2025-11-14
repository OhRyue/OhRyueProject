package com.OhRyue.certpilot.cert.controller;

import com.OhRyue.certpilot.cert.domain.Concept;
import com.OhRyue.certpilot.cert.domain.Topic;
import com.OhRyue.certpilot.cert.domain.enums.ExamMode;
import com.OhRyue.certpilot.cert.dto.CurriculumDtos.*;
import com.OhRyue.certpilot.cert.repository.ConceptRepository;
import com.OhRyue.certpilot.cert.repository.TopicRepository;
import com.OhRyue.certpilot.cert.service.TopicTreeService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.NoSuchElementException;

@Tag(name = "Curriculum(토픽/개념)")
@RestController
@RequestMapping("/api/certs")
@RequiredArgsConstructor
public class CurriculumController {

    private final TopicRepository topicRepository;
    private final ConceptRepository conceptRepository;
    private final TopicTreeService topicTreeService;

    /* ========================= Topic ========================= */

    @Operation(
            summary = "토픽 조회",
            description = "단일 토픽 상세 정보(id 기준)를 반환합니다."
    )
    @GetMapping("/topics/{topicId}")
    public TopicResponse getTopic(@PathVariable Long topicId) {
        Topic topic = topicRepository.findById(topicId)
                .orElseThrow(() -> new NoSuchElementException("Topic not found: " + topicId));
        return toTopicResponse(topic);
    }

    @Operation(
            summary = "토픽 목록 조회",
            description = """
                커리큘럼 토픽 목록을 조회합니다.
                - certId, mode(WRITTEN/PRACTICAL), parentId로 필터링 가능합니다.
                - 아무 파라미터도 없으면 전체 토픽을 반환합니다.
                """
    )
    @GetMapping("/topics")
    public TopicListResponse listTopics(@RequestParam(required = false) Long certId,
                                        @RequestParam(required = false) String mode,
                                        @RequestParam(required = false) Long parentId) {

        ExamMode examMode = null;
        if (mode != null && !mode.isBlank()) {
            examMode = ExamMode.valueOf(mode);
        }

        List<Topic> topics;

        if (certId != null && examMode != null && parentId != null) {
            topics = topicRepository.findByCertIdAndExamMode(certId, examMode)
                    .stream()
                    .filter(t -> parentId.equals(t.getParentId()))
                    .toList();
        } else if (certId != null && examMode != null) {
            topics = topicRepository.findByCertIdAndExamMode(certId, examMode);
        } else if (certId != null) {
            topics = topicRepository.findByCertId(certId);
        } else if (examMode != null) {
            topics = topicRepository.findByExamMode(examMode);
        } else if (parentId != null) {
            topics = topicRepository.findByParentId(parentId);
        } else {
            topics = topicRepository.findAll();
        }

        return new TopicListResponse(
                topics.stream().map(this::toTopicResponse).toList()
        );
    }

    @Operation(
            summary = "토픽 코드/제목 검색",
            description = "code LIKE / title LIKE 검색을 수행합니다."
    )
    @GetMapping("/topics/search")
    public TopicListResponse searchTopics(@RequestParam(required = false) String code,
                                          @RequestParam(required = false) String title) {

        List<Topic> topics;

        if (code != null && !code.isBlank() && title != null && !title.isBlank()) {
            topics = topicRepository.findByCodeContaining(code).stream()
                    .filter(t -> t.getTitle() != null && t.getTitle().contains(title))
                    .toList();
        } else if (code != null && !code.isBlank()) {
            topics = topicRepository.findByCodeContaining(code);
        } else if (title != null && !title.isBlank()) {
            topics = topicRepository.findByTitleContaining(title);
        } else {
            topics = topicRepository.findAll();
        }

        return new TopicListResponse(
                topics.stream().map(this::toTopicResponse).toList()
        );
    }

    /* ========================= Concept ========================= */

    @Operation(
            summary = "개념(리치 블록) 조회",
            description = "topicId 기준 개념 섹션 JSON을 반환합니다."
    )
    @GetMapping("/concepts/{topicId}")
    public ConceptResponse getConcept(@PathVariable Long topicId) {
        Concept concept = conceptRepository.findByTopicId(topicId)
                .orElseThrow(() -> new NoSuchElementException("Concept not found for topicId: " + topicId));
        return new ConceptResponse(concept.getTopicId(), concept.getSectionsJson());
    }

    /* ========================= INTERNAL API ========================= */

    @Operation(
            summary = "[internal] 토픽 트리 후손 id 조회",
            description = """
                rootTopicId를 포함한 모든 후손 토픽 id 리스트를 반환합니다.
                - study-service 등 내부 마이크로서비스에서만 사용합니다.
                """
    )
    @GetMapping("/internal/curriculum/topics/{rootTopicId}/descendants")
    public List<Long> descendantTopicIds(@PathVariable Long rootTopicId) {
        return topicTreeService.descendantsOf(rootTopicId);
    }

    /* ========================= Mapper ========================= */

    private TopicResponse toTopicResponse(Topic topic) {
        return new TopicResponse(
                topic.getId(),
                topic.getCertId(),
                topic.getParentId(),
                topic.getCode(),
                topic.getTitle(),
                topic.getEmoji(),
                topic.getExamMode().name(),
                topic.getOrderNo()
        );
    }
}
