package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.QuestionChoice;
import com.OhRyue.certpilot.study.repository.QuestionChoiceRepository;
import com.OhRyue.certpilot.study.service.llm.AiClient;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class AIExplanationService {

  private static final String CB = "llm";
  private static final String FALLBACK_EXPLAIN = "오답 해설을 불러오지 못했습니다. 정답 해설을 먼저 확인하세요.";
  private static final String FALLBACK_GRADE_MSG = "채점 실패 — 재시도 바랍니다.";

  private final AiClient aiClient;
  private final QuestionChoiceRepository choiceRepository;

  /* ===================== 필기: OX 오답 해설 ===================== */
  @Retry(name = CB, fallbackMethod = "fallbackExplainOx")
  @CircuitBreaker(name = CB, fallbackMethod = "fallbackExplainOx")
  public String explainWrongForOX(boolean userAnswer, Question question) {
    AiClient.ExplainRequest request = new AiClient.ExplainRequest(
        "WRITTEN",
        "OX",
        question.getStem(),
        List.of(),
        normalizeAnswerKey(question.getAnswerKey()),
        userAnswer ? "O" : "X",
        question.getSolutionText(),
        Map.of(
            "questionId", question.getId(),
            "topicId", question.getTopicId()
        )
    );

    AiClient.ExplainResponse response = aiClient.explain(request);
    if (response == null) {
      return FALLBACK_EXPLAIN;
    }
    return Optional.ofNullable(response.whyWrong()).filter(s -> !s.isBlank()).orElse(FALLBACK_EXPLAIN);
  }

  private String fallbackExplainOx(boolean userAnswer, Question question, Throwable throwable) {
    return FALLBACK_EXPLAIN;
  }

  /* ===================== 필기: MCQ 오답 해설 ===================== */
  @Retry(name = CB, fallbackMethod = "fallbackExplainMcq")
  @CircuitBreaker(name = CB, fallbackMethod = "fallbackExplainMcq")
  public String explainWrongForMCQ(String userLabel, String correctLabel, Question question) {
    List<AiClient.Choice> choices = choiceRepository.findByQuestionId(question.getId()).stream()
        .sorted(Comparator.comparing(QuestionChoice::getLabel))
        .map(choice -> new AiClient.Choice(choice.getLabel(), choice.getContent()))
        .collect(Collectors.toList());

    AiClient.ExplainRequest request = new AiClient.ExplainRequest(
        "WRITTEN",
        "MCQ",
        question.getStem(),
        choices,
        correctLabel,
        userLabel,
        question.getSolutionText(),
        Map.of(
            "questionId", question.getId(),
            "topicId", question.getTopicId()
        )
    );

    AiClient.ExplainResponse response = aiClient.explain(request);
    if (response == null) {
      return FALLBACK_EXPLAIN;
    }
    return Optional.ofNullable(response.whyWrong()).filter(s -> !s.isBlank()).orElse(FALLBACK_EXPLAIN);
  }

  private String fallbackExplainMcq(String userLabel, String correctLabel, Question question, Throwable throwable) {
    return FALLBACK_EXPLAIN;
  }

  /* ===================== 실기: 채점 + 해설 ===================== */
  @Retry(name = CB, fallbackMethod = "fallbackGradePractical")
  @CircuitBreaker(name = CB, fallbackMethod = "fallbackGradePractical")
  public PracticalResult explainAndScorePractical(Question question, String userAnswerText) {
    AiClient.GradeRequest request = new AiClient.GradeRequest(
        question.getStem(),
        question.getSolutionText(),
        userAnswerText,
        Map.of(
            "questionId", question.getId(),
            "topicId", question.getTopicId()
        )
    );

    AiClient.GradeResponse response = aiClient.grade(request);
    if (response == null) {
      int score = heuristicScore(question, userAnswerText);
      return new PracticalResult(score, FALLBACK_GRADE_MSG, List.of());
    }

    int score = Optional.ofNullable(response.score()).orElse(heuristicScore(question, userAnswerText));
    String explain = Optional.ofNullable(response.explain()).filter(s -> !s.isBlank()).orElse(FALLBACK_GRADE_MSG);
    List<String> tips = Optional.ofNullable(response.tips()).orElse(List.of());

    return new PracticalResult(clamp(score), explain, tips);
  }

  private PracticalResult fallbackGradePractical(Question question, String userAnswerText, Throwable throwable) {
    int score = heuristicScore(question, userAnswerText);
    return new PracticalResult(score, FALLBACK_GRADE_MSG, List.of());
  }

  /* ===================== 필기 요약 ===================== */
  @Retry(name = CB, fallbackMethod = "fallbackWrittenSummary")
  @CircuitBreaker(name = CB, fallbackMethod = "fallbackWrittenSummary")
  public String summarizeWritten(String topicName, int total, int correct, List<String> weakTags) {
    double acc = total == 0 ? 0.0 : (correct * 100.0) / total;
    AiClient.SummaryRequest request = new AiClient.SummaryRequest(
        topicName,
        Map.of("total", total, "correct", correct, "accPct", String.format(Locale.ROOT, "%.1f", acc)),
        List.of(),
        weakTags,
        Map.of("mode", "WRITTEN")
    );
    AiClient.SummaryResponse response = aiClient.summary(request);
    if (response == null) {
      return defaultWrittenSummary(total, correct);
    }
    String oneLiner = Optional.ofNullable(response.oneLiner()).orElse("");
    List<String> bullets = Optional.ofNullable(response.bullets()).orElse(List.of());
    String nextReco = Optional.ofNullable(response.nextReco()).orElse("");

    return assembleSummary(oneLiner, bullets, nextReco);
  }

  private String fallbackWrittenSummary(String topicName, int total, int correct, List<String> weakTags, Throwable throwable) {
    return defaultWrittenSummary(total, correct);
  }

  /* ===================== 실기 요약 ===================== */
  @Retry(name = CB, fallbackMethod = "fallbackPracticalSummary")
  @CircuitBreaker(name = CB, fallbackMethod = "fallbackPracticalSummary")
  public String summarizePractical(String topicName, int total, int avgScore, List<String> mistakes) {
    AiClient.SummaryRequest request = new AiClient.SummaryRequest(
        topicName,
        Map.of("total", total, "avgScore", avgScore),
        List.of(),
        mistakes,
        Map.of("mode", "PRACTICAL")
    );

    AiClient.SummaryResponse response = aiClient.summary(request);
    if (response == null) {
      return defaultPracticalSummary(total, avgScore);
    }
    String oneLiner = Optional.ofNullable(response.oneLiner()).orElse("");
    List<String> bullets = Optional.ofNullable(response.bullets()).orElse(List.of());
    String nextReco = Optional.ofNullable(response.nextReco()).orElse("");
    return assembleSummary(oneLiner, bullets, nextReco);
  }

  private String fallbackPracticalSummary(String topicName, int total, int avgScore, List<String> mistakes, Throwable throwable) {
    return defaultPracticalSummary(total, avgScore);
  }

  /* ===================== 유틸 ===================== */
  private static String normalizeAnswerKey(String answerKey) {
    if (answerKey == null) return "";
    return answerKey.trim();
  }

  private static String assembleSummary(String oneLiner, List<String> bullets, String nextReco) {
    StringBuilder builder = new StringBuilder();
    if (!oneLiner.isBlank()) {
      builder.append(oneLiner.trim());
    }
    if (!bullets.isEmpty()) {
      if (builder.length() > 0) builder.append("\n");
      bullets.forEach(b -> builder.append("• ").append(b).append("\n"));
    }
    if (!nextReco.isBlank()) {
      if (builder.length() > 0) builder.append("\n");
      builder.append("Next: ").append(nextReco.trim());
    }
    return builder.toString().strip();
  }

  private static String defaultWrittenSummary(int total, int correct) {
    double acc = total == 0 ? 0.0 : (correct * 100.0) / total;
    return "필기 학습을 마쳤습니다. 총 " + total + "문제 중 " + correct + "문제를 맞혀 정확도 " +
        String.format(Locale.ROOT, "%.1f", acc) + "% 입니다. 오답 태그를 중심으로 복습을 이어가세요.";
  }

  private static String defaultPracticalSummary(int total, int avgScore) {
    return "실기 학습을 마쳤습니다. 총 " + total + "문제를 풀어 평균 " + avgScore + "점을 기록했습니다. " +
        "핵심 키워드를 정리하고 재서술 훈련을 반복해보세요.";
  }

  private static int heuristicScore(Question question, String userText) {
    String reference = Optional.ofNullable(question.getSolutionText()).orElse("").toLowerCase(Locale.ROOT);
    String answer = Optional.ofNullable(userText).orElse("").toLowerCase(Locale.ROOT);
    if (reference.isBlank() || answer.isBlank()) return 0;
    String[] tokens = Arrays.stream(reference.split("[^a-zA-Z0-9가-힣]+"))
        .filter(s -> s.length() >= 2)
        .limit(6)
        .toArray(String[]::new);
    if (tokens.length == 0) return 0;
    long matches = Arrays.stream(tokens).filter(answer::contains).count();
    return clamp((int) Math.round(matches * 100.0 / tokens.length));
  }

  private static int clamp(int value) {
    return Math.max(0, Math.min(100, value));
  }

  public record PracticalResult(int score, String explain, List<String> tips) {}
}
