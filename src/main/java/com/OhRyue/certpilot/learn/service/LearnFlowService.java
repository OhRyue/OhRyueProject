package com.OhRyue.certpilot.learn.service;

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
import com.OhRyue.certpilot.learn.domain.LearnSession;
import com.OhRyue.certpilot.learn.domain.repo.LearnSessionRepository;
import com.OhRyue.certpilot.learn.web.dto.*;
import com.OhRyue.certpilot.question.domain.Question;
import com.OhRyue.certpilot.question.domain.repo.QuestionRepository;
import com.OhRyue.certpilot.learnresult.domain.MicroResult;
import com.OhRyue.certpilot.learnresult.domain.MicroResultItem;
import com.OhRyue.certpilot.learnresult.domain.repo.MicroResultItemRepository;
import com.OhRyue.certpilot.learnresult.domain.repo.MicroResultRepository;
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

  private final AiExplainService aiExplain;

  private final MicroResultRepository microResultRepo;
  private final MicroResultItemRepository microItemRepo;

  private final ObjectMapper om = new ObjectMapper();

  // =========================
  // 1) Micro: 세세항목(4레벨)
  //    개념 + 미니체크(OX 4) + 퀴즈 5
  //    (정답/해설은 "시작" 응답에 미포함)
  // =========================
  @Transactional // learn_session INSERT 때문에 readOnly=false
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
            q.getId(),
            q.getStem(),
            readStrList(q.getChoicesJson()),
            q.getDifficulty()
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

  // =========================
  // 2) Review: 세부항목(3레벨) 아래 전체 범위
  //    문제 N(기본 20) — topic 기반
  // =========================
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
        .map(q -> new LearnReviewStartDto.Quiz(
            q.getId(), q.getStem(), readStrList(q.getChoicesJson()), q.getDifficulty()
        ))
        .toList();

    return new LearnReviewStartDto(detail.getId(), quiz.size(), quiz);
  }

  // =========================
  // 3) Micro 제출/채점 (+ 결과 저장)
  //    - 미니체크: DB explanation 사용(제출 시 노출)
  //    - 퀴즈: AI 개인화 해설(실패 시 DB exp 폴백은 AiExplainService 내부)
  //    - 오답노트: 미니체크는 저장하지 않음(요청 반영)
  // =========================
  @Transactional
  public LearnMicroSubmitResult submitMicro(LearnMicroSubmitRequest req) {
    var miniAnswers = Optional.ofNullable(req.miniAnswers()).orElse(List.of());
    var quizAnswers = Optional.ofNullable(req.quizAnswers()).orElse(List.of());

    int total = miniAnswers.size() + quizAnswers.size();
    int score = 0;

    Map<Long, ConceptCheck> ccMap = ccRepo.findAllById(
        miniAnswers.stream().map(LearnMicroSubmitRequest.MiniAnswer::id).toList()
    ).stream().collect(Collectors.toMap(ConceptCheck::getId, c -> c));

    Map<Long, String> miniExplanations = new LinkedHashMap<>();
    List<Long> wrongIds = new ArrayList<>();

    for (var a : miniAnswers) {
      var cc = ccMap.get(a.id());
      if (cc == null) continue;

      boolean correct = Objects.equals(cc.getAnswerIdx(), a.choiceIdx());
      if (correct) score++; else wrongIds.add(cc.getId());

      miniExplanations.put(cc.getId(), Optional.ofNullable(cc.getExplanation()).orElse(""));
    }

    Map<Long, Question> qMap = qRepo.findAllById(
        quizAnswers.stream().map(LearnMicroSubmitRequest.QuizAnswer::id).toList()
    ).stream().collect(Collectors.toMap(Question::getId, q -> q, (a, b) -> a, LinkedHashMap::new));

    Map<Long, String> quizExplanations = new LinkedHashMap<>();

    for (var a : quizAnswers) {
      var q = qMap.get(a.id());
      if (q == null) continue;

      boolean correct = Objects.equals(q.getAnswerIdx(), a.choiceIdx());
      if (correct) score++; else wrongIds.add(q.getId());

      String dbExp = Optional.ofNullable(q.getExp()).orElse("");
      String ai = aiExplain.explainForQuestion(q, a.choiceIdx(), dbExp);
      quizExplanations.put(q.getId(), ai);
    }

    // ===== 결과 저장 =====
    // topicId는 요청에서 받음(권장 반영)
    Long topicId = req.microTopicId();

    // conceptId는 미니체크가 있다면 그 개념으로 저장(없으면 null 허용)
    Long conceptId = ccMap.values().stream().findFirst()
        .map(ConceptCheck::getConceptId).orElse(null);

    MicroResult mr = microResultRepo.save(new MicroResult(req.userId(), conceptId, topicId, score, total));

    // 미니 아이템 저장(오답노트 저장은 하지 않음)
    for (var a : miniAnswers) {
      var cc = ccMap.get(a.id());
      if (cc == null) continue;
      boolean ok = Objects.equals(cc.getAnswerIdx(), a.choiceIdx());
      microItemRepo.save(new MicroResultItem(
          mr.getId(),
          "MINI",
          cc.getId(),
          ok,
          a.choiceIdx(),
          cc.getAnswerIdx(),
          null,
          Optional.ofNullable(cc.getExplanation()).orElse("")
      ));
    }
    // 퀴즈 아이템 저장
    for (var a : quizAnswers) {
      var q = qMap.get(a.id());
      if (q == null) continue;
      boolean ok = Objects.equals(q.getAnswerIdx(), a.choiceIdx());
      String tag = extractFirstTag(q.getMetaJson());
      String expSnap = quizExplanations.getOrDefault(q.getId(), Optional.ofNullable(q.getExp()).orElse(""));
      microItemRepo.save(new MicroResultItem(
          mr.getId(),
          "QUIZ",
          q.getId(),
          ok,
          a.choiceIdx(),
          q.getAnswerIdx(),
          tag,
          expSnap
      ));
    }

    return new LearnMicroSubmitResult(score, total, wrongIds, miniExplanations, quizExplanations);
  }

  // ===== 유틸 =====
  private List<String> readStrList(String json) {
    if (json == null || json.isBlank()) return List.of();
    try {
      return om.readValue(json, new TypeReference<List<String>>() {});
    } catch (Exception e) {
      return List.of();
    }
  }

  /** meta_json.tags[0] 추출 (없으면 null) */
  private String extractFirstTag(String metaJson) {
    if (metaJson == null || metaJson.isBlank()) return null;
    try {
      var node = om.readTree(metaJson);
      var tags = node.get("tags");
      if (tags != null && tags.isArray() && tags.size() > 0) {
        return tags.get(0).asText(null);
      }
    } catch (Exception ignore) {}
    return null;
  }
}
