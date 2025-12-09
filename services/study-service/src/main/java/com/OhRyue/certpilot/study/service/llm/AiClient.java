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
      String correctAnswer,
      String userAnswer,
      Map<String, Object> meta
  ) {}

  record GradeResponse(
      Boolean correct,  // 맞음(true) / 틀림(false)
      String explain,  // 하위 호환성을 위한 조합된 해설
      String explainCorrect,  // 정답 및 채점 기준이 왜 맞는지 (최대 150자)
      String explainUser,  // 사용자의 답안이 왜 맞거나 틀렸는지 (최대 150자)
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


