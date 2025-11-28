package com.OhRyue.certpilot.study.service.llm;

import com.OhRyue.certpilot.study.config.LlmProperties;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.util.retry.Retry;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class OpenAiClient implements AiClient {

  private final WebClient webClient;
  private final LlmProperties props;

  @Override
  public ExplainResponse explain(ExplainRequest request) {
    String system = """
        당신은 정보처리기사 학습 코치입니다.
        - 반드시 JSON 형식으로만 응답합니다.
        - keys = why_wrong(string, 25~80자), correct_reason(string, 25~80자), confusions(array of string, max 3).
        - 불필요한 텍스트나 설명을 추가하지 마세요.
        """;

    String user = buildExplainPrompt(request);
    Map<String, Object> json = invoke(system, user);
    return new ExplainResponse(
        Jsons.optString(json, "why_wrong"),
        Jsons.optString(json, "correct_reason"),
        Jsons.optListString(json, "confusions")
    );
  }

  @Override
  public GradeResponse grade(GradeRequest request) {
    String system = """
        당신은 정보처리기사 실기 채점관입니다

        출력 형식 규칙:
        - JSON 객체로만 답하세요
        - 속성:
          - correct: boolean        // 사용자 답안이 정답인지 여부 (true=맞음, false=틀림)
          - explainCorrect: string  // 정답 및 채점 기준이 왜 맞는지 (최대 150자)
          - explainUser: string     // 사용자의 답안이 왜 맞거나 틀렸는지 (최대 150자)
          - tips: string[]          // 최대 3개, 각 50자 이내, 다음에 맞추기 위한 팁

        설명 작성 규칙:
        - 모든 문자열은 한국어로 작성합니다
        - explainCorrect에는 모범답안 관점의 해설을 씁니다
          - 핵심 개념, 키워드, 이유를 간단히 정리합니다
        - explainUser에는 수험자 답안을 직접 언급하며 평가합니다
          - 어떤 부분이 정확한지, 어떤 부분이 부족하거나 잘못되었는지 구체적으로 말합니다
          - 오개념이 있다면 왜 틀렸는지 짧게 설명합니다

        채점 기준:
        - rubric과 정답을 기준으로 엄격하게 채점합니다
        - 핵심 키워드가 모두 포함되고 의미가 같으면 correct=true
        - 핵심 키워드가 빠졌거나 의미가 다르면 correct=false
        - 표현만 달라지고 의미가 동일하면 정답으로 인정합니다

        주의:
        - JSON 이외의 텍스트는 절대 출력하지 마세요
        """;
    String user = buildGradePrompt(request);
    Map<String, Object> json = invoke(system, user);
    
    String explainCorrect = Jsons.optString(json, "explainCorrect");
    String explainUser = Jsons.optString(json, "explainUser");
    // 하위 호환성을 위해 두 해설을 조합한 explain 생성
    String explain = combineExplanations(explainCorrect, explainUser);
    
    return new GradeResponse(
        Jsons.optBoolean(json, "correct"),
        explain,
        explainCorrect,
        explainUser,
        Jsons.optListString(json, "tips")
    );
  }
  
  private static String combineExplanations(String explainCorrect, String explainUser) {
    if (explainCorrect == null || explainCorrect.isBlank()) {
      return explainUser != null ? explainUser : "";
    }
    if (explainUser == null || explainUser.isBlank()) {
      return explainCorrect;
    }
    return explainCorrect + "\n\n" + explainUser;
  }

  @Override
  public SummaryResponse summary(SummaryRequest request) {
    String system = """
        당신은 정보처리기사 학습 코치입니다.
        - JSON만 반환하세요.
        - keys = one_liner(string<=120자), bullets(array of string, max 3), next_reco(string<=80자).
        """;
    String user = buildSummaryPrompt(request);
    Map<String, Object> json = invoke(system, user);
    return new SummaryResponse(
        Jsons.optString(json, "one_liner"),
        Jsons.optListString(json, "bullets"),
        Jsons.optString(json, "next_reco")
    );
  }

  /* ===================== Prompt builders ===================== */
  private static String buildExplainPrompt(ExplainRequest req) {
    StringBuilder sb = new StringBuilder();
    sb.append("MODE=").append(req.mode()).append(" TYPE=").append(req.type()).append("\n");
    sb.append("QUESTION:\n").append(Optional.ofNullable(req.question()).orElse("")).append("\n");

    if (req.choices() != null && !req.choices().isEmpty()) {
      sb.append("\nCHOICES:\n");
      req.choices().forEach(choice ->
          sb.append(choice.label()).append(") ").append(choice.content()).append("\n"));
    }

    sb.append("\nCORRECT: ").append(Optional.ofNullable(req.correctAnswer()).orElse("")).append("\n");
    sb.append("USER: ").append(Optional.ofNullable(req.userAnswer()).orElse("")).append("\n");

    if (req.solutionText() != null && !req.solutionText().isBlank()) {
      sb.append("SOLUTION_HINT: ").append(req.solutionText()).append("\n");
    }

    if (req.meta() != null && !req.meta().isEmpty()) {
      sb.append("META: ").append(req.meta()).append("\n");
    }

    sb.append("출력은 JSON 객체 한 줄로 작성하세요.");
    return sb.toString();
  }

  private static String buildGradePrompt(GradeRequest req) {
    StringBuilder sb = new StringBuilder();
    sb.append("PRACTICAL QUESTION:\n").append(Optional.ofNullable(req.question()).orElse("")).append("\n\n");
    if (req.rubric() != null && !req.rubric().isBlank()) {
      sb.append("RUBRIC:\n").append(req.rubric()).append("\n\n");
    }
    sb.append("USER ANSWER:\n").append(Optional.ofNullable(req.userAnswer()).orElse("")).append("\n\n");
    if (req.meta() != null && !req.meta().isEmpty()) {
      sb.append("META: ").append(req.meta()).append("\n");
    }
    sb.append("점수와 피드백을 JSON으로 작성하세요.");
    return sb.toString();
  }

  private static String buildSummaryPrompt(SummaryRequest req) {
    StringBuilder sb = new StringBuilder();
    sb.append("TOPIC: ").append(Optional.ofNullable(req.topicName()).orElse("")).append("\n");
    sb.append("STATS: ").append(Optional.ofNullable(req.stats()).orElse(Map.of())).append("\n");
    sb.append("KEY POINTS: ").append(Optional.ofNullable(req.keyPoints()).orElse(List.of())).append("\n");
    sb.append("COMMON MISTAKES: ").append(Optional.ofNullable(req.mistakes()).orElse(List.of())).append("\n");
    if (req.meta() != null && !req.meta().isEmpty()) {
      sb.append("META: ").append(req.meta()).append("\n");
    }
    sb.append("학습 요약을 JSON 객체로만 출력하세요.");
    return sb.toString();
  }

  /* ===================== OpenAI invocation ===================== */
  @SuppressWarnings("unchecked")
  private Map<String, Object> invoke(String system, String user) {
    Map<String, Object> body = Map.of(
        "model", props.getModel(),
        "response_format", Map.of("type", "json_object"),
        "messages", List.of(
            Map.of("role", "system", "content", system),
            Map.of("role", "user", "content", user)
        ),
        "temperature", 0.3
    );

    Map<?, ?> response = webClient.post()
        .uri(Objects.requireNonNullElse(props.getBaseUrl(), "https://api.openai.com/v1") + "/chat/completions")
        .header(HttpHeaders.AUTHORIZATION, "Bearer " + props.getApiKey())
        .contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON)
        .bodyValue(body)
        .retrieve()
        .bodyToMono(Map.class)
        .timeout(Duration.ofMillis(Math.max(props.getTimeoutMs(), 1000)))
        .retryWhen(Retry.max(1))
        .block();

    if (response == null) return Map.of();
    String content = Jsons.extractChoiceContent(response);
    Map<String, Object> json = Jsons.parseJsonObject(content);
    json.putIfAbsent("trace_id", UUID.randomUUID().toString());
    return json;
  }
}
