package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.QuestionChoice;
import com.OhRyue.certpilot.study.repository.QuestionChoiceRepository;
import com.OhRyue.certpilot.study.service.llm.LlmClient;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class AIExplanationService {

  private final LlmClient llm;
  private final QuestionChoiceRepository choiceRepo; // ✅ 추가

  /* ==== OX 오답 해설 ==== */
  public String explainWrongForOX(String userAnswer, Question q) {
    try {
      var req = new LlmClient.LlmExplainReq(
          "WRITTEN", "OX", "ko-KR",
          null, q.getId(), q.getTopicId(), Collections.emptyList(),
          nzs(q.getText()), null,
          userAnswer, String.valueOf(q.getOxAnswer()),
          nzs(q.getExplanation()),
          Map.of("tone", "encouraging", "bullet", true, "maxTokens", 200),
          Map.of("traceId", UUID.randomUUID().toString())
      );
      var resp = llm.explain(req);
      return nzs(resp.explanation());
    } catch (Exception e) {
      return "오답 포인트: 핵심 개념을 다시 확인해보세요.";
    }
  }

  /* ==== MCQ 오답 해설 ==== */
  public String explainWrongForMCQ(String userLabel, String correctLabel, Question q) {
    try {
      List<LlmClient.Choice> choices = choiceRepo.findByQuestionId(q.getId()).stream()
          .sorted(Comparator.comparing(QuestionChoice::getLabel))
          .map(c -> new LlmClient.Choice(c.getLabel(), nzs(c.getText())))
          .collect(Collectors.toList());

      var req = new LlmClient.LlmExplainReq(
          "WRITTEN", "MCQ", "ko-KR",
          null, q.getId(), q.getTopicId(), Collections.emptyList(),
          nzs(q.getText()), choices,
          userLabel, correctLabel,
          nzs(q.getExplanation()),
          Map.of("tone","encouraging","bullet",true,"maxTokens",300),
          Map.of("traceId", UUID.randomUUID().toString())
      );
      var resp = llm.explain(req);
      return nzs(resp.explanation());
    } catch (Exception e) {
      return "오답 포인트: 선지의 차이를 비교해보세요.";
    }
  }

  /* ==== 실기(주관식) 채점/해설 ==== */
  public PracticalResult explainAndScorePractical(String type, Question q, String userText) {
    try {
      var req = new LlmClient.LlmExplainReq(
          "PRACTICAL", type, "ko-KR",
          null, q.getId(), q.getTopicId(), Collections.emptyList(),
          nzs(q.getText()), null,
          nzs(userText), null,
          nzs(q.getExplanation()),
          Map.of("tone","instructor","bullet",true,"maxTokens",400,"scoreRubric","0-100"),
          Map.of("traceId", UUID.randomUUID().toString())
      );
      var resp = llm.explain(req);

      Integer score = null;
      try {
        if (resp != null && resp.confidence() != null) {
          score = clamp((int)Math.round(resp.confidence() * 100.0));
        }
      } catch (Exception ignore) {}

      if (score == null) score = heuristicScore(q, userText);

      String aiExpl = (resp != null && resp.explanation()!=null)
          ? resp.explanation()
          : "채점 근거: 핵심 키워드 일치/불일치를 기준으로 평가했습니다.";

      return new PracticalResult(score, aiExpl);
    } catch (Exception e) {
      return new PracticalResult(heuristicScore(q, userText),
          "AI 서버 응답 지연으로 간이 채점 결과를 제공합니다. 핵심 키워드 포함 여부를 기준으로 평가했습니다.");
    }
  }

  /* ==== 간이 채점 ==== */
  private static int heuristicScore(Question q, String userText) {
    String base = nzs(q.getExplanation()).toLowerCase(Locale.ROOT);
    String ans  = nzs(userText).toLowerCase(Locale.ROOT);
    if (base.isBlank() || ans.isBlank()) return 0;
    String[] toks = Arrays.stream(base.split("[^a-zA-Z0-9가-힣]+"))
        .filter(s -> s.length() >= 2).limit(6).toArray(String[]::new);
    if (toks.length == 0) return 0;
    long hit = Arrays.stream(toks).filter(ans::contains).count();
    return clamp((int)Math.round((hit * 100.0) / toks.length));
  }

  private static int clamp(int v){ return Math.max(0, Math.min(100, v)); }
  private static String nzs(String s){ return s==null? "": s; }

  /* 내부 결과 DTO */
  public record PracticalResult(Integer score, String explanation) {}
}
