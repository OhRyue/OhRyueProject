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
        당신은 정보처리기사 실기 채점관입니다.
        - JSON 객체로만 답하세요.
        - keys = correct(boolean), explain(string<=200자), tips(array of string, max 3).
        - correct는 사용자 답안이 정답인지 틀렸는지를 판단합니다 (true=맞음, false=틀림).
        - rubric과 정답을 기준으로 엄격하게 판단하세요.
        """;
    String user = buildGradePrompt(request);
    Map<String, Object> json = invoke(system, user);
    return new GradeResponse(
        Jsons.optBoolean(json, "correct"),
        Jsons.optString(json, "explain"),
        Jsons.optListString(json, "tips")
    );
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
