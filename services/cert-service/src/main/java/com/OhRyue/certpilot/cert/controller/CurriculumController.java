package com.OhRyue.certpilot.cert.controller;

import com.OhRyue.certpilot.cert.domain.Concept;
import com.OhRyue.certpilot.cert.domain.Topic;
import com.OhRyue.certpilot.cert.domain.enums.ExamMode;
import com.OhRyue.certpilot.cert.repository.ConceptRepository;
import com.OhRyue.certpilot.cert.repository.TopicRepository;
import com.OhRyue.certpilot.cert.service.TopicTreeService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.NoSuchElementException;

@Tag(name = "Curriculum(토픽/개념)")
@RestController
@RequestMapping(
        value = "/api/cert",
        produces = MediaType.APPLICATION_JSON_VALUE
)
@RequiredArgsConstructor
public class CurriculumController {

    private final TopicRepository topicRepository;
    private final ConceptRepository conceptRepository;
    private final TopicTreeService topicTreeService;

    /* ========================= 공통 escape ========================= */

    private String escape(String s) {
        if (s == null) return "";
        return s
                .replace("\\", "\\\\")
                .replace("\"", "\\\"");
    }

    /* ========================= Topic ========================= */

    @Operation(
            summary = "토픽 조회",
            description = "단일 토픽 상세 정보(id 기준)를 반환합니다."
    )
    @GetMapping("/topics/{topicId}")
    public String getTopic(@PathVariable Long topicId) {
        Topic t = topicRepository.findById(topicId)
                .orElseThrow(() -> new NoSuchElementException("Topic not found: " + topicId));

        StringBuilder sb = new StringBuilder();
        sb.append('{')
                .append("\"id\":").append(t.getId()).append(',')
                .append("\"certId\":").append(t.getCertId()).append(',');

        sb.append("\"parentId\":");
        if (t.getParentId() == null) sb.append("null,");
        else sb.append(t.getParentId()).append(',');

        sb.append("\"code\":\"").append(escape(t.getCode())).append("\",")
                .append("\"title\":\"").append(escape(t.getTitle())).append("\",");

        if (t.getEmoji() == null) {
            sb.append("\"emoji\":null,");
        } else {
            sb.append("\"emoji\":\"").append(escape(t.getEmoji())).append("\",");
        }

        sb.append("\"examMode\":\"").append(t.getExamMode().name()).append("\",")
                .append("\"orderNo\":").append(t.getOrderNo())
                .append('}');

        return sb.toString();
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
    public String listTopics(@RequestParam(required = false) Long certId,
                             @RequestParam(value = "mode", required = false) ExamMode mode,
                             @RequestParam(required = false) Long parentId) {

        List<Topic> topics;

        if (certId != null && mode != null && parentId != null) {
            topics = topicRepository.findByCertIdAndExamMode(certId, mode)
                    .stream()
                    .filter(t -> parentId.equals(t.getParentId()))
                    .toList();
        } else if (certId != null && mode != null) {
            topics = topicRepository.findByCertIdAndExamMode(certId, mode);
        } else if (mode != null && parentId != null) {
            // mode와 parentId만 있는 경우: examMode와 parentId로 필터링
            topics = topicRepository.findByExamModeAndParentId(mode, parentId);
        } else if (certId != null) {
            topics = topicRepository.findByCertId(certId);
        } else if (mode != null) {
            topics = topicRepository.findByExamMode(mode);
        } else if (parentId != null) {
            topics = topicRepository.findByParentId(parentId);
        } else {
            topics = topicRepository.findAll();
        }

        StringBuilder sb = new StringBuilder();
        sb.append("{\"topics\":[");
        for (int i = 0; i < topics.size(); i++) {
            Topic t = topics.get(i);
            if (i > 0) sb.append(',');

            sb.append('{')
                    .append("\"id\":").append(t.getId()).append(',')
                    .append("\"certId\":").append(t.getCertId()).append(',');

            sb.append("\"parentId\":");
            if (t.getParentId() == null) sb.append("null,");
            else sb.append(t.getParentId()).append(',');

            sb.append("\"code\":\"").append(escape(t.getCode())).append("\",")
                    .append("\"title\":\"").append(escape(t.getTitle())).append("\",");

            if (t.getEmoji() == null) {
                sb.append("\"emoji\":null,");
            } else {
                sb.append("\"emoji\":\"").append(escape(t.getEmoji())).append("\",");
            }

            sb.append("\"examMode\":\"").append(t.getExamMode().name()).append("\",")
                    .append("\"orderNo\":").append(t.getOrderNo())
                    .append('}');
        }
        sb.append("]}");

        return sb.toString();
    }

    @Operation(
            summary = "토픽 코드/제목 검색",
            description = "code LIKE / title LIKE 검색을 수행합니다."
    )
    @GetMapping("/topics/search")
    public String searchTopics(@RequestParam(required = false) String code,
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

        StringBuilder sb = new StringBuilder();
        sb.append("{\"topics\":[");
        for (int i = 0; i < topics.size(); i++) {
            Topic t = topics.get(i);
            if (i > 0) sb.append(',');

            sb.append('{')
                    .append("\"id\":").append(t.getId()).append(',')
                    .append("\"certId\":").append(t.getCertId()).append(',');

            sb.append("\"parentId\":");
            if (t.getParentId() == null) sb.append("null,");
            else sb.append(t.getParentId()).append(',');

            sb.append("\"code\":\"").append(escape(t.getCode())).append("\",")
                    .append("\"title\":\"").append(escape(t.getTitle())).append("\",");

            if (t.getEmoji() == null) {
                sb.append("\"emoji\":null,");
            } else {
                sb.append("\"emoji\":\"").append(escape(t.getEmoji())).append("\",");
            }

            sb.append("\"examMode\":\"").append(t.getExamMode().name()).append("\",")
                    .append("\"orderNo\":").append(t.getOrderNo())
                    .append('}');
        }
        sb.append("]}");

        return sb.toString();
    }

    /* ========================= Concept ========================= */

    @Operation(
            summary = "개념(리치 블록) 조회",
            description = "topicId 기준 개념 섹션 JSON을 반환합니다."
    )
    @GetMapping("/concepts/{topicId}")
    public String getConcept(@PathVariable Long topicId) {
        Concept concept = conceptRepository.findByTopicId(topicId)
                .orElseThrow(() -> new NoSuchElementException("Concept not found for topicId: " + topicId));

        StringBuilder sb = new StringBuilder();
        sb.append('{')
                .append("\"topicId\":").append(concept.getTopicId()).append(',')
                // sectionsJson 자체가 JSON 문자열이므로, JSON String 으로 감싸주기
                .append("\"sectionsJson\":\"")
                .append(escape(concept.getSectionsJson()))
                .append("\"")
                .append('}');

        return sb.toString();
    }

    /* ========================= INTERNAL API ========================= */

    @Operation(
            summary = "[internal] 토픽 트리 후손 id 조회",
            description = """
                rootTopicId를 포함한 모든 후손 토픽 id 리스트를 반환합니다.
                - study-service 등 내부 마이크로서비스에서만 사용합니다.
                """
    )
    @GetMapping(
            value = "/internal/curriculum/topics/{rootTopicId}/descendants",
            produces = MediaType.APPLICATION_JSON_VALUE
    )
    public String descendantTopicIds(@PathVariable Long rootTopicId) {
        List<Long> ids = topicTreeService.descendantsOf(rootTopicId);
        // 예: {"ids":[10001,11001,...]}  ← 껍데기만 하드코딩, 값은 서비스에서 동적
        return "{\"ids\":" + ids.toString() + "}";
    }
}
