package com.OhRyue.certpilot.learn.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.ai.AiExplainService;
import com.OhRyue.certpilot.learn.web.dto.ReviewGradeRequest;
import com.OhRyue.certpilot.learn.web.dto.ReviewGradeResult;
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

  @Transactional
  public ReviewGradeResult gradeReview(ReviewGradeRequest req) {
    var ids = req.answers().stream().map(ReviewGradeRequest.Item::questionId).toList();
    Map<Long, Question> map = qRepo.findAllById(ids).stream()
        .collect(Collectors.toMap(Question::getId, q -> q));

    int total = req.answers().size();
    int score = 0;
    Map<Long, Boolean> correctness = new LinkedHashMap<>();

    for (var a : req.answers()) {
      var q = map.get(a.questionId());
      if (q == null) continue;
      boolean ok = Objects.equals(q.getAnswerIdx(), a.chosenIdx());
      if (ok) score++;
      correctness.put(a.questionId(), ok);
    }

    // 간이 약점 태그: meta_json.tags[0]
    List<String> weakTags = new ArrayList<>();
    var om = new ObjectMapper();
    for (var e : correctness.entrySet()) {
      if (!e.getValue()) {
        var q = map.get(e.getKey());
        try {
          var node = om.readTree(q.getMetaJson());
          if (node != null && node.has("tags") && node.get("tags").isArray() && node.get("tags").size() > 0) {
            weakTags.add(node.get("tags").get(0).asText());
          }
        } catch (Exception ignore) {}
      }
    }

    String aiSummary;
    try {
      aiSummary = aiExplain.summarizeReview(correctness, weakTags);
    } catch (Exception e) {
      aiSummary = "[폴백 요약] 총 %d문항 중 %d문항 정답. 반복 실수: %s"
          .formatted(total, score, weakTags.isEmpty() ? "없음" : String.join(", ", weakTags));
    }

    return new ReviewGradeResult(score, total, correctness, aiSummary);
  }
}
