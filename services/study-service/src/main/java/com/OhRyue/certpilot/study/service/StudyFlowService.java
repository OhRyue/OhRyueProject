package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.*;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.MicroDtos.*;
import com.OhRyue.certpilot.study.dto.ReviewDtos.*;
import com.OhRyue.certpilot.study.dto.WrongReviewDtos.*;
import com.OhRyue.certpilot.study.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 메인학습 흐름 + 리뷰(총정리) + 오답복습
 * - 리포지토리 특수 메서드 없이도 동작하도록 findAll→필터링으로 구성 (규모 커지면 파생쿼리 추가 권장)
 */
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class StudyFlowService {

  private final TopicRepository topicRepo;
  private final ConceptRepository conceptRepo;
  private final QuestionRepository questionRepo;
  private final QuestionChoiceRepository choiceRepo;
  private final UserProgressRepository progressRepo;

  private final UserAnswerRepository answerRepo;
  private final QuestionTagRepository tagRepo;
  private final AIExplanationService ai;
  private final TopicTreeService topicTree; // 하위 토픽 탐색

  /* ========================= 개념 ========================= */

  public ConceptResp loadConcept(Long topicId) {
    var topic = topicRepo.findById(topicId).orElseThrow(() -> new NoSuchElementException("Topic not found: " + topicId));
    var conceptOpt = conceptRepo.findByTopicId(topicId);

    // 기본값
    String topicTitle = topic.getTitle();
    java.util.List<ConceptResp.Section> sections = java.util.List.of();

    if (conceptOpt.isPresent()) {
      var concept = conceptOpt.get();
      var json = concept.getBlocksJson();

      if (json != null && !json.isBlank()) {
        try {
          // blocksJson 전체를 파싱해서 sections 배열만 뽑는다.
          // 구조: { "sections": [ {orderNo, subCode, title, importance, blocks:[...]} , ... ] }
          com.fasterxml.jackson.databind.ObjectMapper om = new com.fasterxml.jackson.databind.ObjectMapper();
          com.fasterxml.jackson.databind.JsonNode root = om.readTree(json);
          var arr = root.get("sections");

          java.util.List<ConceptResp.Section> list = new java.util.ArrayList<>();
          if (arr != null && arr.isArray()) {
            for (var node : arr) {
              int orderNo = node.hasNonNull("orderNo") ? node.get("orderNo").asInt() : 0;
              String subCode = node.hasNonNull("subCode") ? node.get("subCode").asText() : "";
              String secTitle = node.hasNonNull("title") ? node.get("title").asText() : "";
              int importance = node.hasNonNull("importance") ? node.get("importance").asInt() : 0;

              java.util.List<ConceptResp.Block> blocks = new java.util.ArrayList<>();
              var blocksNode = node.get("blocks");
              if (blocksNode != null && blocksNode.isArray()) {
                for (var b : blocksNode) {
                  String type     = b.hasNonNull("type")     ? b.get("type").asText() : null;
                  String text     = b.hasNonNull("text")     ? b.get("text").asText() : null;
                  String url      = b.hasNonNull("url")      ? b.get("url").asText() : null;
                  String alt      = b.hasNonNull("alt")      ? b.get("alt").asText() : null;
                  String caption  = b.hasNonNull("caption")  ? b.get("caption").asText() : null;

                  java.util.List<String> items   = new java.util.ArrayList<>();
                  java.util.List<String> headers = new java.util.ArrayList<>();
                  java.util.List<java.util.List<String>> rows = new java.util.ArrayList<>();

                  var itemsNode = b.get("items");
                  if (itemsNode != null && itemsNode.isArray()) {
                    itemsNode.forEach(it -> items.add(it.asText()));
                  }
                  var headersNode = b.get("headers");
                  if (headersNode != null && headersNode.isArray()) {
                    headersNode.forEach(h -> headers.add(h.asText()));
                  }
                  var rowsNode = b.get("rows");
                  if (rowsNode != null && rowsNode.isArray()) {
                    for (var r : rowsNode) {
                      java.util.List<String> row = new java.util.ArrayList<>();
                      r.forEach(cell -> row.add(cell.asText()));
                      rows.add(row);
                    }
                  }

                  blocks.add(new ConceptResp.Block(type, text, items, url, alt, caption, headers, rows));
                }
              }

              list.add(new ConceptResp.Section(orderNo, subCode, secTitle, importance, blocks));
            }
          }
          // orderNo 기준 정렬
          list.sort(java.util.Comparator.comparing(ConceptResp.Section::orderNo));
          sections = java.util.List.copyOf(list);

        } catch (Exception ignore) {
          // 파싱 실패 시 레거시 content만 내려가게 비움
          sections = java.util.List.of();
        }
      }
    }

    return new ConceptResp(topicId, topicTitle, sections);
  }

  /* ========================= 미니체크(OX) ========================= */

  public MiniSet miniSet(Long topicId) {
    List<Question> pool = questionRepo.findAll().stream()
        .filter(q -> Objects.equals(q.getTopicId(), topicId))
        .filter(q -> q.getType() == QuestionType.OX)
        .sorted(Comparator.comparingLong(Question::getId))
        .collect(Collectors.toList());
    Collections.shuffle(pool);

    List<MiniQuestion> items = pool.stream()
        .limit(5)
        .map(q -> new MiniQuestion(q.getId(), nzs(q.getText())))
        .toList();

    return new MiniSet(items);
  }

  @Transactional
  public MiniSubmitResp submitMini(MiniSubmitReq req) {
    List<Long> ids = req.answers().stream().map(MiniAnswer::questionId).toList();
    Map<Long, Question> qsById = questionRepo.findAllById(ids).stream()
        .collect(Collectors.toMap(Question::getId, it -> it));

    int correctCnt = 0;
    List<MiniSubmitItem> items = new ArrayList<>();

    for (MiniAnswer a : req.answers()) {
      Question q = require(qsById.get(a.questionId()), "Question not found: " + a.questionId());

      boolean correct = Objects.equals(q.getOxAnswer(), a.answer());
      if (correct) correctCnt++;

      String baseExpl = nzs(q.getExplanation());
      String aiExpl = correct ? "" : nzs(ai.explainWrongForOX(String.valueOf(a.answer()), q));

      answerRepo.save(UserAnswer.builder()
          .userId(req.userId())
          .questionId(q.getId())
          .correct(correct)
          .answerText(String.valueOf(a.answer()))
          .createdAt(Instant.now())
          .build());

      items.add(new MiniSubmitItem(q.getId(), correct, baseExpl, aiExpl));
    }

    boolean passed = (correctCnt == req.answers().size());

    UserProgress p = progressRepo
        .findByUserIdAndTopicIdAndExamMode(req.userId(), req.topicId(), ExamMode.WRITTEN)
        .orElseGet(() -> UserProgress.builder()
            .userId(req.userId())
            .topicId(req.topicId())
            .examMode(ExamMode.WRITTEN)
            .build());
    p.setMiniTotal(req.answers().size());
    p.setMiniCorrect(correctCnt);
    p.setMiniPassed(passed);
    p.setUpdatedAt(Instant.now());
    progressRepo.save(p);

    return new MiniSubmitResp(req.answers().size(), correctCnt, passed, items);
  }

  /* ========================= MCQ ========================= */

  public McqSet mcqSet(Long topicId, String userId) {
    UserProgress p = progressRepo.findByUserIdAndTopicIdAndExamMode(userId, topicId, ExamMode.WRITTEN)
        .orElseThrow(() -> new IllegalStateException("미니체크 통과 정보가 없습니다. 먼저 미니체크를 통과하세요."));
    if (!Boolean.TRUE.equals(p.isMiniPassed())) {
      throw new IllegalStateException("미니체크 통과 필요");
    }

    List<Question> pool = questionRepo.findAll().stream()
        .filter(q -> Objects.equals(q.getTopicId(), topicId))
        .filter(q -> q.getType() == QuestionType.MCQ)
        .sorted(Comparator.comparingLong(Question::getId))
        .collect(Collectors.toList());
    Collections.shuffle(pool);

    List<Question> picked = pool.stream().limit(4).toList();

    List<McqQuestion> dto = picked.stream().map(q -> {
      List<McqChoice> choices = choiceRepo.findByQuestionId(q.getId()).stream()
          .sorted(Comparator.comparing(QuestionChoice::getLabel))
          .map(c -> new McqChoice(c.getLabel(), nzs(c.getText())))
          .toList();
      return new McqQuestion(q.getId(), nzs(q.getText()), choices, q.getImageUrl());
    }).toList();

    return new McqSet(dto);
  }

  @Transactional
  public McqSubmitResp submitMcq(McqSubmitReq req) {
    List<Long> ids = req.answers().stream().map(McqAnswer::questionId).toList();
    List<Question> qs = questionRepo.findAllById(ids);

    Map<Long, String> correctMap = new HashMap<>();
    for (Question q : qs) {
      for (QuestionChoice c : choiceRepo.findByQuestionId(q.getId())) {
        if (Boolean.TRUE.equals(c.isCorrect())) {
          correctMap.put(q.getId(), c.getLabel());
          break;
        }
      }
    }

    int correctCnt = 0;
    List<McqSubmitItem> items = new ArrayList<>();

    for (McqAnswer a : req.answers()) {
      String correctLabel = correctMap.get(a.questionId());
      boolean ok = Objects.equals(correctLabel, a.label());
      if (ok) correctCnt++;

      Question q = qs.stream().filter(x -> Objects.equals(x.getId(), a.questionId()))
          .findFirst().orElseThrow();
      String baseExpl = nzs(q.getExplanation());
      String aiExpl = ok ? "" : nzs(ai.explainWrongForMCQ(a.label(), correctLabel, q));

      answerRepo.save(UserAnswer.builder()
          .userId(req.userId())
          .questionId(a.questionId())
          .correct(ok)
          .answerText(a.label())
          .createdAt(Instant.now())
          .build());

      items.add(new McqSubmitItem(a.questionId(), ok, correctLabel, baseExpl, aiExpl));
    }

    UserProgress p = progressRepo
        .findByUserIdAndTopicIdAndExamMode(req.userId(), req.topicId(), ExamMode.WRITTEN)
        .orElseThrow(() -> new IllegalStateException("진행 정보가 없습니다. 미니체크부터 시작하세요."));
    p.setMcqTotal(req.answers().size());
    p.setMcqCorrect(correctCnt);
    p.setUpdatedAt(Instant.now());
    progressRepo.save(p);

    return new McqSubmitResp(req.answers().size(), correctCnt, items);
  }

  /* ========================= 요약 ========================= */

  public SummaryResp summary(String userId, Long topicId) {
    UserProgress p = progressRepo.findByUserIdAndTopicIdAndExamMode(userId, topicId, ExamMode.WRITTEN)
        .orElseThrow(() -> new NoSuchElementException("진행 정보가 없습니다."));
    return new SummaryResp(nz(p.getMiniTotal()), nz(p.getMiniCorrect()), Boolean.TRUE.equals(p.isMiniPassed()),
        nz(p.getMcqTotal()), nz(p.getMcqCorrect()));
  }

  /* ========================= 리뷰(총정리) 20문 ========================= */

  public ReviewSet reviewSet(Long rootTopicId) {
    // 루트 포함 모든 하위 토픽
    Set<Long> topicIds = topicTree.descendantIds(rootTopicId);
    List<Question> pool = questionRepo.findAll().stream()
        .filter(q -> topicIds.contains(q.getTopicId()))
        .filter(q -> q.getType() == QuestionType.MCQ)
        .collect(Collectors.toList());
    Collections.shuffle(pool);
    List<Question> pick = pool.stream().limit(20).toList();

    var items = pick.stream().map(q -> {
      var choices = choiceRepo.findByQuestionId(q.getId()).stream()
          .sorted(Comparator.comparing(QuestionChoice::getLabel))
          .map(c -> new ReviewQuestion.Choice(c.getLabel(), nzs(c.getText())))
          .toList();
      return new ReviewQuestion(q.getId(), nzs(q.getText()), choices, q.getImageUrl());
    }).toList();

    return new ReviewSet(items);
  }

  /* ========================= 오답복습 세트 ========================= */

  public WrongSet wrongSet(Long topicId, String userId, int limit) {
    // 최근 오답 → 같은 토픽의 MCQ/OX 우선
    // 리포 메서드 제약 고려: findAll 후 필터링

    // 1) 사용자 답안 중 '오답'만 최근순
    List<UserAnswer> wrongLogs = answerRepo.findAll().stream()
        .filter(a -> Objects.equals(a.getUserId(), userId))
        .filter(a -> {
          // UserAnswer.correct가 primitive boolean인 경우 Lombok은 isCorrect()를 생성합니다.
          // (Boolean 래퍼였다면 getCorrect()가 생성되었을 것)
          return !a.isCorrect();
        })
        .sorted(Comparator.comparing(UserAnswer::getCreatedAt).reversed())
        .toList();

    // 2) 최근 오답의 questionId 집합(상한 200)
    LinkedHashSet<Long> wrongQids = new LinkedHashSet<>();
    for (UserAnswer a : wrongLogs) {
      wrongQids.add(a.getQuestionId());
      if (wrongQids.size() >= 200) break;
    }

    // 3) 같은 토픽 + MCQ/OX 필터
    List<Question> pool = questionRepo.findAllById(wrongQids).stream()
        .filter(q -> Objects.equals(q.getTopicId(), topicId))
        .filter(q -> q.getType() == QuestionType.MCQ || q.getType() == QuestionType.OX)
        .collect(Collectors.toList());
    Collections.shuffle(pool);
    List<Question> pick = pool.stream().limit(limit).toList();

    // 4) DTO 변환 (제네릭 안전: 변수에 타입을 먼저 명시)
    List<WrongQuestion> items = new ArrayList<>();
    for (Question q : pick) {
      List<WrongQuestion.Choice> choices;
      if (q.getType() == QuestionType.MCQ) {
        choices = choiceRepo.findByQuestionId(q.getId()).stream()
            .sorted(Comparator.comparing(QuestionChoice::getLabel))
            .map(c -> new WrongQuestion.Choice(c.getLabel(), nzs(c.getText())))
            .toList();
      } else {
        choices = Collections.emptyList(); // List<WrongQuestion.Choice> 타입으로 안전
      }
      items.add(new WrongQuestion(q.getId(), q.getType().name(), nzs(q.getText()), choices, q.getImageUrl()));
    }

    return new WrongSet(items);
  }

  /* ========================= 유틸 ========================= */

  private static <T> T require(T v, String msg) {
    if (v == null) throw new NoSuchElementException(msg);
    return v;
  }
  private static String nzs(String s) { return (s == null) ? "" : s; }
  private static int nz(Integer v) { return v == null ? 0 : v; }
}
