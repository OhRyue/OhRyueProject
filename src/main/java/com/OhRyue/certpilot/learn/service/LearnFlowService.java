package com.OhRyue.certpilot.learn.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.common.error.NotFoundException;
import com.OhRyue.certpilot.concept.domain.Concept;
import com.OhRyue.certpilot.concept.domain.repo.ConceptCheckRepository;
import com.OhRyue.certpilot.concept.domain.repo.ConceptRepository;
import com.OhRyue.certpilot.curriculum.domain.Topic;
import com.OhRyue.certpilot.curriculum.domain.repo.TopicRepository;
import com.OhRyue.certpilot.learn.web.dto.*;
import com.OhRyue.certpilot.question.domain.Question;
import com.OhRyue.certpilot.question.domain.repo.QuestionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service
@RequiredArgsConstructor
public class LearnFlowService {

  private final TopicRepository topicRepo;
  private final ConceptRepository conceptRepo;
  private final ConceptCheckRepository ccRepo;
  private final QuestionRepository qRepo;

  private final ObjectMapper om = new ObjectMapper();

  /** [세세항목] 개념 + 미니체크(2~3) + 문제 5문항 */
  @Transactional(readOnly = true)
  public LearnMicroStartDto startMicro(LearnMicroStartRequest req) {
    Topic micro = topicRepo.findById(req.microTopicId())
        .orElseThrow(() -> new NotFoundException("topic not found: " + req.microTopicId()));
    if (!Objects.equals(micro.getLevel(), 4)) {
      throw new IllegalArgumentException("microTopicId must be level=4");
    }

    // 1) 세세항목에 연결된 Concept 1개 (필요 시 다건→최신 1개)
    Concept concept = conceptRepo.findAll().stream()
        .filter(c -> Objects.equals(c.getTopicId(), micro.getId()))
        .findFirst()
        .orElseThrow(() -> new NotFoundException("concept not mapped to micro topic: " + micro.getId()));

    // 2) 미니체크 2~3
    var minis = ccRepo.findByConceptIdOrderByIdAsc(concept.getId()).stream()
        .limit(3)
        .map(cc -> new LearnMicroStartDto.Mini(
            cc.getId(), cc.getStem(), readStrList(cc.getChoicesJson())
        )).toList();

    // 3) 세세항목 범위의 문제 5
    List<Question> pool = qRepo.findLatestByTopicIds(List.of(micro.getId()), 50);
    if (pool.size() < 5) {
      // 부족 시 최신 50로 보충(추후 토픽/난이도 로직 확장)
      pool = qRepo.findTop50ByDifficultyBetweenOrderByIdDesc(1, 3);
    }
    var quiz = pool.stream().limit(5).map(q ->
        new LearnMicroStartDto.Quiz(
            q.getId(), q.getStem(), readStrList(q.getChoicesJson()), q.getDifficulty())
    ).toList();

    return new LearnMicroStartDto(
        micro.getId(),
        concept.getId(), concept.getTitle(), concept.getSummary(), concept.getPitfalls(),
        minis, quiz
    );
  }

  /** [세부항목] 하위 세세항목 전체 범위에서 총정리 20문항 */
  @Transactional(readOnly = true)
  public LearnReviewStartDto startReview(LearnReviewStartRequest req) {
    Topic detail = topicRepo.findById(req.detailTopicId())
        .orElseThrow(() -> new NotFoundException("topic not found: " + req.detailTopicId()));
    if (!Objects.equals(detail.getLevel(), 3)) {
      throw new IllegalArgumentException("detailTopicId must be level=3");
    }

    // 하위 세세항목 id들
    List<Long> microIds = topicRepo.findByParentIdOrderByOrdAscIdAsc(detail.getId()).stream()
        .filter(t -> Objects.equals(t.getLevel(), 4))
        .map(Topic::getId).toList();

    List<Question> pool = microIds.isEmpty()
        ? List.of()
        : qRepo.findLatestByTopicIds(microIds, 200);

    if (pool.size() < 20) {
      pool = qRepo.findTop50ByDifficultyBetweenOrderByIdDesc(1, 3);
    }

    var quiz20 = pool.stream().limit(20).map(q ->
        new LearnReviewStartDto.Quiz(
            q.getId(), q.getStem(), readStrList(q.getChoicesJson()), q.getDifficulty())
    ).toList();

    return new LearnReviewStartDto(detail.getId(), detail.getName(), quiz20);
  }

  // ===== helpers =====
  private List<String> readStrList(String json) {
    if (json == null || json.isBlank()) return List.of();
    try { return om.readValue(json, new TypeReference<>(){}); }
    catch (Exception e) { return List.of(); }
  }
}
