package com.OhRyue.certpilot.learn.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.common.error.NotFoundException;
import com.OhRyue.certpilot.concept.domain.Concept;
import com.OhRyue.certpilot.concept.domain.repo.ConceptCheckRepository;
import com.OhRyue.certpilot.concept.domain.repo.ConceptRepository;
import com.OhRyue.certpilot.curriculum.domain.Topic;
import com.OhRyue.certpilot.curriculum.domain.repo.TopicRepository;
import com.OhRyue.certpilot.learn.domain.LearnSession;
import com.OhRyue.certpilot.learn.domain.repo.LearnSessionRepository;
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
  private final LearnSessionRepository learnSessionRepo;

  private final ObjectMapper om = new ObjectMapper();

  // =========================
  // 1) Micro: 세세항목(4레벨) 1개에 대해
  //    개념 + 미니체크(2~3) + 퀴즈 5
  // =========================
  @Transactional(readOnly = true)
  public LearnMicroStartDto startMicro(LearnMicroStartRequest req) {
    Long microId = req.microTopicId();

    Topic micro = topicRepo.findByIdAndCertId(microId, req.certId())
        .orElseThrow(() -> new NotFoundException("micro topic not found: " + microId));

    // 연결된 개념 1건 선택 (최근)
    Concept concept = conceptRepo.findTop1ByTopicIdOrderByIdDesc(micro.getId())
        .orElseThrow(() -> new NotFoundException("concept not linked to micro topic: " + micro.getId()));

    // learn_session 기록 (선택)
    if (req.userId() != null) {
      learnSessionRepo.save(new LearnSession(req.userId(), concept.getId()));
    }

    // 미니체크 2~3 (리포는 전체 → 서비스에서 limit)
    var checks = ccRepo.findByConceptId(concept.getId()).stream()
        .limit(3)
        .map(cc -> new LearnMicroStartDto.Mini(cc.getId(), cc.getStem(), readStrList(cc.getChoicesJson())))
        .toList();

    // topic 기반 문제 5 (부족 시 폴백)
    var quiz5 = pickForMicroByTopicIds(List.of(micro.getId()), 5);

    return new LearnMicroStartDto(
        concept.getId(),
        concept.getTitle(),
        concept.getSummary(),
        concept.getPitfalls(),
        checks,
        quiz5
    );
  }

  // =========================
  // 2) Review: 세부항목(3레벨) 하위 전체 범위에서
  //    20문제 (부족 시 폴백)
  // =========================
  @Transactional(readOnly = true)
  public LearnReviewStartDto startReview(LearnReviewStartRequest req) {
    Long detailId = req.detailTopicId();
    int wanted = Optional.ofNullable(req.count()).orElse(20);
    wanted = Math.max(5, Math.min(wanted, 50)); // 안전 가드

    // detail(3레벨) 확인
    Topic detail = topicRepo.findByIdAndCertId(detailId, req.certId())
        .orElseThrow(() -> new NotFoundException("detail topic not found: " + detailId));
    if (detail.getLevel() != 3) {
      throw new IllegalArgumentException("detailTopicId must be level=3 topic");
    }

    // 하위 세세항목(4레벨) id 목록 (정렬 없는 findByParentId 사용)
    var micros = topicRepo.findByParentId(detail.getId());
    List<Long> microIds = micros.stream().map(Topic::getId).toList();
    if (microIds.isEmpty()) {
      // 세세항목이 아직 없으면, detail 자체로도 시도(시드 부족 대응)
      microIds = List.of(detail.getId());
    }

    // topic 기반 문제 wanted개 (부족 시 폴백)
    var picked = pickForReviewByTopicIds(microIds, wanted);

    return new LearnReviewStartDto(
        detail.getId(),
        picked.size(),
        picked
    );
  }

  // ====== 헬퍼들 ======

  /** Micro 용(세세항목): LearnMicroStartDto.Quiz 리스트 */
  private List<LearnMicroStartDto.Quiz> pickForMicroByTopicIds(List<Long> topicIds, int count) {
    List<Question> pool = qRepo.findLatestByTopicIds(topicIds, count);

    if (pool.size() < count) {
      // 폴백: 최근 난이도 1~3 중 추가
      var fill = qRepo.findTop50ByDifficultyBetweenOrderByIdDesc(1, 3);
      pool = mergeUnique(pool, fill).stream().limit(count).toList();
    }

    return pool.stream()
        .map(q -> new LearnMicroStartDto.Quiz(q.getId(), q.getStem(), readStrList(q.getChoicesJson()), q.getDifficulty()))
        .toList();
  }

  /** Review 용(세부항목): LearnReviewStartDto.Quiz 리스트 */
  private List<LearnReviewStartDto.Quiz> pickForReviewByTopicIds(List<Long> topicIds, int count) {
    List<Question> pool = qRepo.findLatestByTopicIds(topicIds, count);

    if (pool.size() < count) {
      var fill = qRepo.findTop50ByDifficultyBetweenOrderByIdDesc(1, 3);
      pool = mergeUnique(pool, fill).stream().limit(count).toList();
    }

    return pool.stream()
        .map(q -> new LearnReviewStartDto.Quiz(q.getId(), q.getStem(), readStrList(q.getChoicesJson()), q.getDifficulty()))
        .toList();
  }

  private List<String> readStrList(String json) {
    if (json == null || json.isBlank()) return List.of();
    try { return om.readValue(json, new TypeReference<>(){}); }
    catch (Exception e) { return List.of(); }
  }

  private List<Question> mergeUnique(List<Question> a, List<Question> b) {
    Map<Long, Question> m = new LinkedHashMap<>();
    for (var q : a) m.put(q.getId(), q);
    for (var q : b) m.putIfAbsent(q.getId(), q);
    return new ArrayList<>(m.values());
  }
}
