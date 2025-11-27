package com.OhRyue.certpilot.study.service.llm;

import java.util.List;
import java.util.Map;

public interface AiClient {

  ExplainResponse explain(ExplainRequest request);

  GradeResponse grade(GradeRequest request);

  SummaryResponse summary(SummaryRequest request);

  /* ===================== DTOs ===================== */

  record Choice(String label, String content) {}

  record ExplainRequest(
      String mode,
      String type,
      String question,
      List<Choice> choices,
      String correctAnswer,
      String userAnswer,
      String solutionText,
      Map<String, Object> meta
  ) {}

  record ExplainResponse(
      String whyWrong,
      String correctReason,
      List<String> confusions
  ) {}

  record GradeRequest(
      String question,
      String rubric,
      String userAnswer,
      Map<String, Object> meta
  ) {}

  record GradeResponse(
      Boolean correct,  // 맞음(true) / 틀림(false)
      String explain,
      List<String> tips
  ) {}

  record SummaryRequest(
      String topicName,
      Map<String, Object> stats,
      List<String> keyPoints,
      List<String> mistakes,
      Map<String, Object> meta
  ) {}

  record SummaryResponse(
      String oneLiner,
      List<String> bullets,
      String nextReco
  ) {}
}


