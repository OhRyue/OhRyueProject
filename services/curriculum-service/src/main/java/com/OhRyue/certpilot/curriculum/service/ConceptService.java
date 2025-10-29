package com.OhRyue.certpilot.curriculum.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.common.error.NotFoundException;
import com.OhRyue.certpilot.curriculum.entity.Concept;
import com.OhRyue.certpilot.curriculum.entity.ConceptCheck;
import com.OhRyue.certpilot.curriculum.repository.ConceptCheckRepository;
import com.OhRyue.certpilot.curriculum.repository.ConceptRepository;
import com.OhRyue.certpilot.curriculum.dto.*;
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
    Page<Concept> page = conceptRepo.findByCertIdAndCategoryContainingIgnoreCase(
        certId, category == null ? "" : category, pageable);
    return page.map(c -> new ConceptSummaryDto(
        c.getId(), c.getCertId(), c.getCategory(), c.getTitle()
    ));
  }

  public ConceptDetailDto detail(Long id) {
    Concept c = conceptRepo.findById(id)
        .orElseThrow(() -> new NotFoundException("concept not found: " + id));
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
        choices = Collections.emptyList();
      }
      return new MiniItemDto(
          cc.getId(),
          cc.getStem(),
          choices,
          cc.getAnswerIdx(),
          cc.getExplanation()
      );
    }).toList();
  }
}

