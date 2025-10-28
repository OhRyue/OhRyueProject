package com.OhRyue.certpilot.learn.service;

import com.OhRyue.certpilot.ai.AiExplainService;
import com.OhRyue.certpilot.learn.web.dto.LearnReviewSubmitRequest;
import com.OhRyue.certpilot.learn.web.dto.LearnReviewSubmitResult;
import com.OhRyue.certpilot.learnresult.domain.ReviewResult;
import com.OhRyue.certpilot.learnresult.domain.ReviewResultItem;
import com.OhRyue.certpilot.learnresult.domain.repo.ReviewResultItemRepository;
import com.OhRyue.certpilot.learnresult.domain.repo.ReviewResultRepository;
import com.OhRyue.certpilot.question.domain.Question;
import com.OhRyue.certpilot.question.domain.repo.QuestionRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
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

  // Review 결과 저장
  private final ReviewResultRepository reviewResultRepo;
  private final ReviewResultItemRepository reviewResultItemRepo;

  private final ObjectMapper om = new ObjectMapper();

  /** 세부항목 Review 제출/채점 + 문항별 AI 해설 + 요약 + 결과 저장 */
  @Transactional
  public LearnReviewSubmitResult submitReview(LearnReviewSubmitRequest req) {
    var answers = Optional.ofNullable(req.answers()).orElse(List.of());
    var ids = answers.stream().map(LearnReviewSubmitRequest.Item::questionId).toList();

    Map<Long, Question> qMap = qRepo.findAllById(ids).stream()
        .collect(Collectors.toMap(Question::getId, q -> q, (a, b) -> a, LinkedHashMap::new));

    int total = answers.size();
    int score = 0;

    Map<Long, Boolean> correctness = new LinkedHashMap<>();
    Map<Long, String> explanations = new LinkedHashMap<>();
    List<String> weakTags = new ArrayList<>();

    for (var a : answers) {
      var q = qMap.get(a.questionId());
      if (q == null) continue;

      boolean ok = Objects.equals(q.getAnswerIdx(), a.chosenIdx());
      correctness.put(q.getId(), ok);
      if (ok) score++;

      String dbExp = Optional.ofNullable(q.getExp()).orElse("");
      String ai = aiExplain.explainForQuestion(q, a.chosenIdx(), dbExp);
      explanations.put(q.getId(), ai);

      if (!ok) {
        String tag = extractFirstTag(q.getMetaJson());
        if (tag != null) weakTags.add(tag);
      }
    }

    String aiSummary;
    try {
      aiSummary = aiExplain.summarizeReview(correctness, weakTags);
    } catch (Exception e) {
      aiSummary = "[요약(폴백)] 총 " + total + "문항 중 정답 " + score + "개. "
          + (weakTags.isEmpty() ? "반복 실수 없음." : "반복 실수 태그: " + String.join(", ", weakTags));
    }

    // 결과 저장 (userId 있을 때만)
    if (req.userId() != null) {
      ReviewResult header = reviewResultRepo.save(
          ReviewResult.builder()
              .userId(req.userId())
              .certId(req.certId())
              .detailTopicId(req.detailTopicId())
              .score(score)
              .total(total)
              .aiSummary(aiSummary)
              .build()
      );

      int ord = 0;
      for (var a : answers) {
        var q = qMap.get(a.questionId());
        if (q == null) continue;
        reviewResultItemRepo.save(
            ReviewResultItem.builder()
                .resultId(header.getId())
                .questionId(q.getId())
                .chosenIdx(a.chosenIdx())
                .correct(Objects.equals(q.getAnswerIdx(), a.chosenIdx()))
                .aiExplanation(explanations.getOrDefault(q.getId(), ""))
                .ordNo(ord++)
                .build()
        );
      }
    }

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
