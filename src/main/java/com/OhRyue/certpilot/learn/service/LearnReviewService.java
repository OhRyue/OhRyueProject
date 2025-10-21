package com.OhRyue.certpilot.learn.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.ai.AiExplainService;
import com.OhRyue.certpilot.learnresult.domain.ReviewResult;
import com.OhRyue.certpilot.learnresult.domain.ReviewResultItem;
import com.OhRyue.certpilot.learnresult.domain.repo.ReviewResultItemRepository;
import com.OhRyue.certpilot.learnresult.domain.repo.ReviewResultRepository;
import com.OhRyue.certpilot.learn.web.dto.LearnReviewSubmitRequest;
import com.OhRyue.certpilot.learn.web.dto.LearnReviewSubmitResult;
import com.OhRyue.certpilot.question.domain.Question;
import com.OhRyue.certpilot.question.domain.repo.QuestionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class LearnReviewService {

  private final QuestionRepository qRepo;
  private final AiExplainService aiExplain;

  private final ReviewResultRepository reviewResultRepo;
  private final ReviewResultItemRepository reviewResultItemRepo;

  private final ObjectMapper om = new ObjectMapper();

  @Transactional
  public LearnReviewSubmitResult submitReview(LearnReviewSubmitRequest req) {
    var answers = Optional.ofNullable(req.answers()).orElse(List.of());
    var ids = answers.stream().map(LearnReviewSubmitRequest.Item::questionId).toList();

    Map<Long, Question> qMap = qRepo.findAllById(ids).stream()
        .collect(Collectors.toMap(Question::getId, q -> q, (a,b)->a, LinkedHashMap::new));

    int total = answers.size();
    int score = 0;

    Map<Long, Boolean> correctness = new LinkedHashMap<>();
    Map<Long, String> explanations = new LinkedHashMap<>();
    List<String> weakTags = new ArrayList<>();

    int ord = 0;
    List<ReviewResultItem> toSave = new ArrayList<>();

    for (var a : answers) {
      var q = qMap.get(a.questionId());
      if (q == null) continue;

      boolean ok = Objects.equals(q.getAnswerIdx(), a.chosenIdx());
      correctness.put(q.getId(), ok);
      if (ok) score++; else {
        String tag = extractFirstTag(q.getMetaJson());
        if (tag != null) weakTags.add(tag);
      }

      String dbExp = Optional.ofNullable(q.getExp()).orElse("");
      String ai = aiExplain.explainForQuestion(q, a.chosenIdx(), dbExp);
      explanations.put(q.getId(), ai);

      toSave.add(ReviewResultItem.builder()
          .questionId(q.getId())
          .chosenIdx(a.chosenIdx())
          .correct(ok)
          .aiExplanation(ai)
          .ordNo(ord++)
          .build());
    }

    String aiSummary;
    try {
      aiSummary = aiExplain.summarizeReview(correctness, weakTags);
    } catch (Exception e) {
      aiSummary = "[요약(폴백)] 총 " + total + "문항 중 정답 " + score + "개. "
          + (weakTags.isEmpty() ? "반복 실수 없음." : "반복 실수 태그: " + String.join(", ", weakTags));
    }

    // 헤더 저장
    ReviewResult header = reviewResultRepo.save(
        ReviewResult.builder()
            .userId(req.userId())
            .certId(null) // 필요 시 요청 확장
            .detailTopicId(req.detailTopicId())
            .score(score)
            .total(total)
            .aiSummary(aiSummary)
            .build()
    );

    Long rid = header.getId();
    toSave.forEach(i -> i.setResultId(rid));
    reviewResultItemRepo.saveAll(toSave);

    return new LearnReviewSubmitResult(score, total, correctness, explanations, aiSummary);
  }

  /** meta_json.tags[0] 추출(없으면 null) */
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
