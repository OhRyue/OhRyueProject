package com.OhRyue.certpilot.concept.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.common.error.NotFoundException;
import com.OhRyue.certpilot.concept.domain.Concept;
import com.OhRyue.certpilot.concept.domain.ConceptCheck;
import com.OhRyue.certpilot.concept.domain.repo.ConceptCheckRepository;
import com.OhRyue.certpilot.concept.domain.repo.ConceptRepository;
import com.OhRyue.certpilot.concept.web.dto.*;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
public class ConceptService {

    private final ConceptRepository conceptRepo;
    private final ConceptCheckRepository checkRepo;
    private final ObjectMapper om = new ObjectMapper();

    public Page<ConceptSummaryDto> list(Long certId, String category, Pageable pageable) {
        Page<Concept> page = conceptRepo.findByCertIdAndCategoryContainingIgnoreCase(certId, category == null ? "" : category, pageable);
        return page.map(c -> new ConceptSummaryDto(c.getId(), c.getCertId(), c.getCategory(), c.getTitle()));
    }

    public ConceptDetailDto detail(Long id) {
        Concept c = conceptRepo.findById(id).orElseThrow(() -> new NotFoundException("concept not found: " + id));
        return new ConceptDetailDto(
                c.getId(), c.getCertId(), c.getCategory(), c.getTitle(),
                c.getSummary(), c.getPitfalls(), c.getExamplesJson(), c.getTagsJson()
        );
    }

    @Transactional(readOnly = true)
    public List<MiniItemDto> miniCheck(Long conceptId) {
        List<ConceptCheck> list = checkRepo.findByConceptId(conceptId);

        return list.stream().map(cc -> {
            List<String> choices;
            try {
                choices = om.readValue(cc.getChoicesJson(), new TypeReference<List<String>>() {});
            } catch (Exception e) {
                // 파싱 실패 시 안전하게 빈 리스트 반환(또는 throw로 500 대신 422 등)
                choices = Collections.emptyList();
            }
            return new MiniItemDto(
                    cc.getId(),
                    cc.getStem(),
                    choices,
                    cc.getAnswerIdx(),
                    cc.getDescription()
            );
        }).toList();
    }


    private List<String> parseList(String json) {
        try {
            return om.readValue(json, new TypeReference<>() {});
        } catch (Exception e) {
            throw new IllegalArgumentException("invalid choices_json");
        }
    }
}
