package com.OhRyue.certpilot.learn.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.ai.AiExplainService;
import com.OhRyue.certpilot.learn.web.dto.LearnReviewSubmitRequest;
import com.OhRyue.certpilot.learn.web.dto.LearnReviewSubmitResult;
import com.OhRyue.certpilot.question.domain.Question;
import com.OhRyue.certpilot.question.domain.repo.QuestionRepository;
import com.OhRyue.certpilot.ability.domain.AbilityProfile;
import com.OhRyue.certpilot.ability.domain.AbilityProfileId;
import com.OhRyue.certpilot.ability.domain.repo.AbilityProfileRepository;
import com.OhRyue.certpilot.wrongnote.domain.WrongNote;
import com.OhRyue.certpilot.wrongnote.domain.WrongNoteStatus;
import com.OhRyue.certpilot.wrongnote.domain.repo.WrongNoteRepository;
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

  // 제출 시 능력/오답 갱신
  private final AbilityProfileRepository abilityRepo;
  private final WrongNoteRepository wrongRepo;

  /** 메타 파싱용 */
  private final ObjectMapper om = new ObjectMapper();

  /**
   * 세부항목 Review 제출/채점 + 문항별 AI 해설 + 최종 요약
   */
  @Transactional
  public LearnReviewSubmitResult submitReview(LearnReviewSubmitRequest req) {
    var answers = Optional.ofNullable(req.answers()).orElse(List.of());

    // 문제 조회 맵
    var ids = answers.stream().map(LearnReviewSubmitRequest.Item::questionId).toList();
    Map<Long, Question> qMap = qRepo.findAllById(ids).stream()
        .collect(Collectors.toMap(Question::getId, q -> q, (a, b) -> a, LinkedHashMap::new));

    int total = answers.size();
    int score = 0;

    // 정오표 + 문항별 해설 + 약점 태그
    Map<Long, Boolean> correctness = new LinkedHashMap<>();
    Map<Long, String> explanations = new LinkedHashMap<>();
    List<String> weakTags = new ArrayList<>();

    for (var a : answers) {
      var q = qMap.get(a.questionId());
      if (q == null) continue;

      boolean ok = Objects.equals(q.getAnswerIdx(), a.chosenIdx());
      correctness.put(q.getId(), ok);
      if (ok) score++;

      // 문항별 AI 해설(실패 시 DB 폴백)
      String dbExp = Optional.ofNullable(q.getExp()).orElse("");
      String ai = aiExplain.explainForQuestion(q, a.chosenIdx(), dbExp);
      explanations.put(q.getId(), ai);

      // 약점 태그 수집 + ★능력/오답 갱신
      String tag = extractFirstTagOrDefault(q.getMetaJson(), "general");
      if (!ok) weakTags.add(tag);

      if (req.userId() != null) {
        upsertAbility(req.userId(), tag, ok);
        if (!ok) {
          upsertWrongNote(req.userId(), q.getId(), tag);
        }
      }
    }

    // 최종 요약
    String aiSummary;
    try {
      aiSummary = aiExplain.summarizeReview(correctness, weakTags);
    } catch (Exception e) {
      aiSummary = "[요약(폴백)] 총 " + total + "문항 중 정답 " + score + "개. "
          + (weakTags.isEmpty() ? "반복 실수 항목 없음." : "반복 실수 태그: " + String.join(", ", weakTags));
    }

    return new LearnReviewSubmitResult(score, total, correctness, explanations, aiSummary);
  }

  private String extractFirstTagOrDefault(String metaJson, String def) {
    if (metaJson == null || metaJson.isBlank()) return def;
    try {
      var node = om.readTree(metaJson);
      var tags = node.get("tags");
      if (tags != null && tags.isArray() && tags.size() > 0) {
        return tags.get(0).asText(def);
      }
    } catch (Exception ignore) {}
    return def;
  }

  private void upsertAbility(Long userId, String tag, boolean correct) {
    AbilityProfileId id = new AbilityProfileId(userId, tag);
    AbilityProfile ap = abilityRepo.findById(id).orElseGet(() -> new AbilityProfile(userId, tag));
    ap.applyResult(correct, 0.3); // EMA 학습률 0.3
    abilityRepo.save(ap);
  }

  private void upsertWrongNote(Long userId, Long qid, String tag) {
    WrongNote wn = wrongRepo.findByUserIdAndQuestionIdAndTag(userId, qid, tag)
        .orElseGet(() -> new WrongNote(userId, qid, tag));
    wn.markWrongOnce();
    wn.setStatus(WrongNoteStatus.todo);
    wrongRepo.save(wn);
  }
}
