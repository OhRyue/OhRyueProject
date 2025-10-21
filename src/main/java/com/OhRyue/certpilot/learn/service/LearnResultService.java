package com.OhRyue.certpilot.learn.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.common.error.NotFoundException;
import com.OhRyue.certpilot.concept.domain.ConceptCheck;
import com.OhRyue.certpilot.concept.domain.repo.ConceptCheckRepository;
import com.OhRyue.certpilot.learn.web.dto.MicroResultDetailDto;
import com.OhRyue.certpilot.learn.web.dto.ReviewResultDetailDto;
import com.OhRyue.certpilot.learnresult.domain.MicroResult;
import com.OhRyue.certpilot.learnresult.domain.MicroResultItem;
import com.OhRyue.certpilot.learnresult.domain.ReviewResult;
import com.OhRyue.certpilot.learnresult.domain.ReviewResultItem;
import com.OhRyue.certpilot.learnresult.domain.repo.MicroResultItemRepository;
import com.OhRyue.certpilot.learnresult.domain.repo.MicroResultRepository;
import com.OhRyue.certpilot.learnresult.domain.repo.ReviewResultItemRepository;
import com.OhRyue.certpilot.learnresult.domain.repo.ReviewResultRepository;
import com.OhRyue.certpilot.question.domain.Question;
import com.OhRyue.certpilot.question.domain.repo.QuestionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service
@RequiredArgsConstructor
public class LearnResultService {

  private final MicroResultRepository microResultRepo;
  private final MicroResultItemRepository microItemRepo;
  private final ReviewResultRepository reviewResultRepo;
  private final ReviewResultItemRepository reviewItemRepo;

  private final ConceptCheckRepository ccRepo;
  private final QuestionRepository qRepo;

  private final ObjectMapper om = new ObjectMapper();

  // ========= Micro 상세 =========
  @Transactional(readOnly = true)
  public MicroResultDetailDto getMicroDetail(Long resultId) {
    MicroResult r = microResultRepo.findById(resultId)
        .orElseThrow(() -> new NotFoundException("micro_result not found: " + resultId));

    var header = new MicroResultDetailDto.Header(
        r.getId(), r.getUserId(), r.getCertId(), r.getTopicId(), r.getConceptId(),
        r.getScore(), r.getTotal(), r.getCreatedAt()
    );

    var items = microItemRepo.findByResultIdOrderByOrdNoAsc(r.getId());

    // 미니/퀴즈 참조를 한 번에 로드
    Map<Long, ConceptCheck> ccMap = new HashMap<>();
    Map<Long, Question> qMap = new HashMap<>();

    // refId 분리
    List<Long> ccIds = new ArrayList<>();
    List<Long> qIds  = new ArrayList<>();
    for (MicroResultItem it : items) {
      if ("MINI".equalsIgnoreCase(it.getItemType())) ccIds.add(it.getRefId());
      else qIds.add(it.getRefId());
    }
    if (!ccIds.isEmpty()) {
      for (var cc : ccRepo.findAllById(ccIds)) ccMap.put(cc.getId(), cc);
    }
    if (!qIds.isEmpty()) {
      for (var q : qRepo.findAllById(qIds)) qMap.put(q.getId(), q);
    }

    var dtoItems = items.stream().map(it -> {
      String stem; List<String> choices;
      if ("MINI".equalsIgnoreCase(it.getItemType())) {
        var cc = ccMap.get(it.getRefId());
        stem = cc != null ? cc.getStem() : "(deleted concept_check)";
        choices = readStrList(cc != null ? cc.getChoicesJson() : null);
      } else {
        var q = qMap.get(it.getRefId());
        stem = q != null ? q.getStem() : "(deleted question)";
        choices = readStrList(q != null ? q.getChoicesJson() : null);
      }
      return new MicroResultDetailDto.Item(
          it.getItemType(),
          it.getRefId(),
          stem,
          choices,
          it.getChosenIdx(),
          it.isCorrect(),
          it.getExplanation(),
          it.getOrdNo()
      );
    }).toList();

    return new MicroResultDetailDto(header, dtoItems);
  }

  // ========= Review 상세 =========
  @Transactional(readOnly = true)
  public ReviewResultDetailDto getReviewDetail(Long resultId) {
    ReviewResult r = reviewResultRepo.findById(resultId)
        .orElseThrow(() -> new NotFoundException("review_result not found: " + resultId));

    var header = new ReviewResultDetailDto.Header(
        r.getId(), r.getUserId(), r.getCertId(), r.getDetailTopicId(),
        r.getScore(), r.getTotal(), r.getAiSummary(), r.getCreatedAt()
    );

    var items = reviewItemRepo.findByResultIdOrderByOrdNoAsc(r.getId());

    // 문항 로드
    Map<Long, Question> qMap = new HashMap<>();
    var qIds = items.stream().map(ReviewResultItem::getQuestionId).toList();
    if (!qIds.isEmpty()) {
      for (var q : qRepo.findAllById(qIds)) qMap.put(q.getId(), q);
    }

    var dtoItems = items.stream().map(it -> {
      var q = qMap.get(it.getQuestionId());
      String stem = q != null ? q.getStem() : "(deleted question)";
      List<String> choices = readStrList(q != null ? q.getChoicesJson() : null);
      return new ReviewResultDetailDto.Item(
          it.getQuestionId(),
          stem,
          choices,
          it.getChosenIdx(),
          it.isCorrect(),
          it.getAiExplanation(),
          it.getOrdNo()
      );
    }).toList();

    return new ReviewResultDetailDto(header, dtoItems);
  }

  // ===== util =====
  private List<String> readStrList(String json) {
    if (json == null || json.isBlank()) return List.of();
    try {
      return om.readValue(json, new TypeReference<List<String>>() {});
    } catch (Exception e) {
      return List.of();
    }
  }
}
