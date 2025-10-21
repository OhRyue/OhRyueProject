package com.OhRyue.certpilot.learn.service;

import com.OhRyue.certpilot.learnresult.domain.MicroResult;
import com.OhRyue.certpilot.learnresult.domain.MicroResultItem;
import com.OhRyue.certpilot.learnresult.domain.repo.MicroResultItemRepository;
import com.OhRyue.certpilot.learnresult.domain.repo.MicroResultRepository;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.ai.AiExplainService;
import com.OhRyue.certpilot.common.error.NotFoundException;
import com.OhRyue.certpilot.concept.domain.Concept;
import com.OhRyue.certpilot.concept.domain.ConceptCheck;
import com.OhRyue.certpilot.concept.domain.repo.ConceptCheckRepository;
import com.OhRyue.certpilot.concept.domain.repo.ConceptRepository;
import com.OhRyue.certpilot.curriculum.domain.Topic;
import com.OhRyue.certpilot.curriculum.domain.repo.TopicRepository;
import com.OhRyue.certpilot.learn.domain.*;
import com.OhRyue.certpilot.learn.domain.repo.*;
import com.OhRyue.certpilot.learn.web.dto.*;
import com.OhRyue.certpilot.question.domain.Question;
import com.OhRyue.certpilot.question.domain.repo.QuestionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class LearnFlowService {

  private final TopicRepository topicRepo;
  private final ConceptRepository conceptRepo;
  private final ConceptCheckRepository ccRepo;
  private final QuestionRepository qRepo;
  private final LearnSessionRepository learnSessionRepo;

  private final MicroResultRepository microResultRepo;
  private final MicroResultItemRepository microResultItemRepo;

  private final AiExplainService aiExplain;

  private final ObjectMapper om = new ObjectMapper();

  // ===== Micro 시작 =====
  @Transactional // 세션 저장 때문에 readOnly=false
  public LearnMicroStartDto startMicro(LearnMicroStartRequest req) {
    Long microId = req.microTopicId();

    Topic micro = topicRepo.findByIdAndCertId(microId, req.certId())
        .orElseThrow(() -> new NotFoundException("micro topic not found: " + microId));

    Concept concept = conceptRepo.findTop1ByTopicIdOrderByIdDesc(micro.getId())
        .orElseThrow(() -> new NotFoundException("concept not linked to micro topic: " + micro.getId()));

    if (req.userId() != null) {
      learnSessionRepo.save(new LearnSession(req.userId(), concept.getId()));
    }

    var checks = ccRepo.findByConceptId(concept.getId()).stream()
        .limit(4)
        .map(cc -> new LearnMicroStartDto.Mini(
            cc.getId(),
            cc.getStem(),
            readStrList(cc.getChoicesJson()),
            null
        ))
        .toList();

    List<Question> qPool = qRepo.findLatestByTopicIds(List.of(micro.getId()), 5);
    var quiz5 = qPool.stream()
        .map(q -> new LearnMicroStartDto.Quiz(
            q.getId(), q.getStem(), readStrList(q.getChoicesJson()), q.getDifficulty()
        ))
        .toList();

    return new LearnMicroStartDto(
        concept.getId(),
        concept.getTitle(),
        concept.getSummary(),
        concept.getPitfalls(),
        checks,
        quiz5
    );
  }

  // ===== Micro 제출/채점 + 결과 저장 =====
  @Transactional
  public LearnMicroSubmitResult submitMicro(LearnMicroSubmitRequest req) {
    var miniAnswers = Optional.ofNullable(req.miniAnswers()).orElse(List.of());
    var quizAnswers = Optional.ofNullable(req.quizAnswers()).orElse(List.of());

    int total = miniAnswers.size() + quizAnswers.size();
    int score = 0;

    // 미니체크 채점
    Map<Long, ConceptCheck> ccMap = ccRepo.findAllById(
        miniAnswers.stream().map(LearnMicroSubmitRequest.MiniAnswer::id).toList()
    ).stream().collect(Collectors.toMap(ConceptCheck::getId, c -> c));

    Map<Long, String> miniExplanations = new LinkedHashMap<>();
    List<Long> wrongIds = new ArrayList<>();

    int ord = 0;
    List<MicroResultItem> toSaveItems = new ArrayList<>();

    for (var a : miniAnswers) {
      var cc = ccMap.get(a.id());
      if (cc == null) continue;

      boolean correct = Objects.equals(cc.getAnswerIdx(), a.choiceIdx());
      if (correct) score++; else wrongIds.add(cc.getId());
      String exp = Optional.ofNullable(cc.getExplanation()).orElse("");
      miniExplanations.put(cc.getId(), exp);

      // 결과 아이템 (미니체크는 오답노트 저장 X)
      toSaveItems.add(MicroResultItem.builder()
          .itemType("MINI")
          .refId(cc.getId())
          .chosenIdx(a.choiceIdx())
          .correct(correct)
          .explanation(exp)
          .ordNo(ord++)
          .build());
    }

    // 퀴즈 채점
    Map<Long, Question> qMap = qRepo.findAllById(
        quizAnswers.stream().map(LearnMicroSubmitRequest.QuizAnswer::id).toList()
    ).stream().collect(Collectors.toMap(Question::getId, q -> q, (a,b)->a, LinkedHashMap::new));

    Map<Long, String> quizExplanations = new LinkedHashMap<>();

    for (var a : quizAnswers) {
      var q = qMap.get(a.id());
      if (q == null) continue;

      boolean correct = Objects.equals(q.getAnswerIdx(), a.choiceIdx());
      if (correct) score++; else wrongIds.add(q.getId());

      String dbExp = Optional.ofNullable(q.getExp()).orElse("");
      String ai = aiExplain.explainForQuestion(q, a.choiceIdx(), dbExp);
      quizExplanations.put(q.getId(), ai);

      toSaveItems.add(MicroResultItem.builder()
          .itemType("QUIZ")
          .refId(q.getId())
          .chosenIdx(a.choiceIdx())
          .correct(correct)
          .explanation(ai)
          .ordNo(ord++)
          .build());
    }

    // 결과 저장(헤더)
    Long conceptId = conceptRepo.findTop1ByTopicIdOrderByIdDesc(req.microTopicId())
        .map(Concept::getId)
        .orElse(null);

    MicroResult header = microResultRepo.save(
        MicroResult.builder()
            .userId(req.userId())
            .certId(null)
            .topicId(req.microTopicId())
            .conceptId(conceptId)
            .score(score)
            .total(total)
            .build()
    );

    // 결과 아이템 저장
    Long rid = header.getId();
    toSaveItems.forEach(i -> i.setResultId(rid));
    microResultItemRepo.saveAll(toSaveItems);

    return new LearnMicroSubmitResult(score, total, wrongIds, miniExplanations, quizExplanations);
  }

  // ===== Review 시작 =====
  @Transactional(readOnly = true)
  public LearnReviewStartDto startReview(LearnReviewStartRequest req) {
    Long detailId = req.detailTopicId();
    int wanted = Optional.ofNullable(req.count()).orElse(20);
    wanted = Math.max(1, Math.min(wanted, 100));

    Topic detail = topicRepo.findByIdAndCertId(detailId, req.certId())
        .orElseThrow(() -> new NotFoundException("detail topic not found: " + detailId));
    if (detail.getLevel() != 3) {
      throw new IllegalArgumentException("detailTopicId must be level=3 topic");
    }

    var micros = topicRepo.findByParentId(detail.getId());
    List<Long> topicScope = micros.isEmpty()
        ? List.of(detail.getId())
        : micros.stream().map(Topic::getId).toList();

    List<Question> picked = qRepo.findLatestByTopicIds(topicScope, wanted);

    var quiz = picked.stream()
        .map(q -> new LearnReviewStartDto.Quiz(q.getId(), q.getStem(), readStrList(q.getChoicesJson()), q.getDifficulty()))
        .toList();

    return new LearnReviewStartDto(detail.getId(), quiz.size(), quiz);
  }

  // ===== 유틸 =====
  private List<String> readStrList(String json) {
    if (json == null || json.isBlank()) return List.of();
    try { return om.readValue(json, new TypeReference<List<String>>(){}); }
    catch (Exception e) { return List.of(); }
  }
}
