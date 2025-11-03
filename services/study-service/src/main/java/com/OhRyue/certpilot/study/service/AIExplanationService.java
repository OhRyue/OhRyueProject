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
    private final QuestionChoiceRepository choiceRepo;

    /* ==== OX 오답 해설 ==== */
    public String explainWrongForOX(String userAnswer, Question q) {
        try {
            var req = new LlmClient.LlmExplainReq(
                    "WRITTEN", "OX", "ko-KR",
                    null, q.getId(), q.getTopicId(), Collections.emptyList(),
                    nzs(q.getText()), null,
                    userAnswer, String.valueOf(q.getOxAnswer()),
                    nzs(q.getExplanation()),
                    Map.of("tone","encouraging","bullet",true,"maxTokens",200),
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

    /* ==== (수정) 필기 요약 폴백 ==== */
    public String summarizeWrittenKorean(String userId, Long topicId,
                                         int miniTotal, int miniCorrect,
                                         int mcqTotal, int mcqCorrect,
                                         boolean completed, int streakDays) {
        // LLM 호출은 그대로(성공 시 그 결과 사용)
        try {
            String prompt = """
          당신은 정보처리기사 학습 코치입니다. 다음 지표를 바탕으로 한국어 2~4문장의 코칭 요약을 작성하세요.
          - 모드: 필기(WRITTEN)
          - 사용자: %s
          - 토픽: %d
          - 미니체크: %d/%d
          - 객관식: %d/%d
          - 완료 여부: %s
          - 연속 학습: %d일
          """.formatted(userId, topicId, miniCorrect, miniTotal, mcqCorrect, mcqTotal,
                    completed ? "완료" : "미완료", streakDays);

            var req = new LlmClient.LlmExplainReq(
                    "WRITTEN", "SUMMARY", "ko-KR",
                    userId, null, topicId, Collections.emptyList(),
                    prompt, null, null, null,
                    "요약과 한 줄 팁을 제공해 주세요.",
                    Map.of("tone","coach","bullet",false,"maxTokens",280),
                    Map.of("traceId", UUID.randomUUID().toString(), "task","written_summary")
            );
            var resp = llm.explain(req);
            String text = (resp != null && resp.explanation()!=null) ? resp.explanation().trim() : "";
            if (!text.isEmpty()) return clampLen(text, 600);
        } catch (Exception ignore) {}

        // ==== 폴백 ====
        int denom = Math.max(1, miniTotal + mcqTotal);
        int acc = Math.round((miniCorrect + mcqCorrect) * 100f / denom);

        String tip = (acc >= 85)
                ? "정확도가 높습니다. 다음 세부항목으로 범위를 넓혀보세요. 💡"
                : (acc >= 60)
                ? "오답이 잦은 태그를 중심으로 보조학습을 권합니다. 💡"
                : "개념 → OX 재확인 후 쉬운 난이도로 문제 수를 줄여 집중해보세요. 💡";

        return "오늘은 OX %d문제 중 %d개, 객관식 %d문제 중 %d개를 맞혔습니다. 연속 학습 %d일을 이어가고 있어요. %s"
                .formatted(miniTotal, miniCorrect, mcqTotal, mcqCorrect, streakDays, tip);
    }

    /* ==== (유지) 실기 요약 폴백 ==== */
    public String summarizePracticalKorean(String userId, Long topicId,
                                           int total, int avgScore, int streakDays) {
        try {
            String prompt = """
          당신은 정보처리기사 실기 학습 코치입니다. 아래 지표를 바탕으로 한국어 2~4문장 요약과 한 줄 팁을 작성하세요.
          - 사용자: %s
          - 토픽: %d
          - 풀이 문항 수: %d
          - 평균 점수(0~100): %d
          - 연속 학습: %d일
          """.formatted(userId, topicId, total, avgScore, streakDays);

            var req = new LlmClient.LlmExplainReq(
                    "PRACTICAL", "SUMMARY", "ko-KR",
                    userId, null, topicId, Collections.emptyList(),
                    prompt, null, null, null,
                    "요약과 한 줄 팁을 제공해 주세요.",
                    Map.of("tone","coach","bullet",false,"maxTokens",300),
                    Map.of("traceId", UUID.randomUUID().toString(), "task","practical_summary")
            );
            var resp = llm.explain(req);
            String text = (resp != null && resp.explanation()!=null) ? resp.explanation().trim() : "";
            if (!text.isEmpty()) return clampLen(text, 600);
        } catch (Exception ignore) {}

        String tip = (avgScore >= 85)
                ? "키워드-근거-예시 구조를 유지해 고득점을 안정화하세요. 🔧"
                : (avgScore >= 60)
                ? "오답 키워드를 3개로 요약하고 바로 재서술 훈련을 해보세요. 🔧"
                : "핵심 용어 정의→한 문장 설명→예시 순으로 짧게 훈련해 보세요. 🔧";
        return "실기 %d문제를 풀어 평균 %d점을 기록했습니다. %s".formatted(total, avgScore, tip);
    }

    /* ==== 유틸 ==== */
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
    private static String clampLen(String s, int max){ return s == null ? "" : (s.length() <= max ? s : s.substring(0, max)); }
    private static String nzs(String s){ return s==null? "": s; }

    public record PracticalResult(Integer score, String explanation) {}
}
