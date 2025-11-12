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

@Component
@RequiredArgsConstructor
public class OpenAiClient {

  private final WebClient webClient;
  private final LlmProperties props;

  /**
   * OpenAI Chat Completions 호출 → LlmExplainResp 매핑
   */
  public LlmClient.LlmExplainResp explain(LlmClient.LlmExplainReq req) {
    String system = """
            당신은 정보처리기사 학습 코치/채점관입니다.
            - 한국어로 간결하고 친절하게 설명합니다.
            - 반드시 JSON 객체로만 답합니다(기타 텍스트 금지).
            출력 스키마:
            {
              "explanation": string,
              "bullets": string[],
              "keypoints": string[],
              "nextActions": string[],
              "confidence": number,  // 0.0~1.0
              "usage": {"promptTokens": number, "completionTokens": number, "latencyMs": number},
              "modelInfo": {"model": string, "version": string},
              "traceId": string
            }
            """;

    String user = buildUserPrompt(req);

    Map<String, Object> body = Map.of(
        "model", "gpt-4o-mini",
        "response_format", Map.of("type", "json_object"),
        "messages", List.of(
            Map.of("role","system", "content", system),
            Map.of("role","user",   "content", user)
        ),
        "temperature", 0.3
    );

    long start = System.currentTimeMillis();

    Map<?,?> resp = webClient.post()
        .uri(Objects.requireNonNullElse(props.getBaseUrl(), "https://api.openai.com/v1") + "/chat/completions")
        .header(HttpHeaders.AUTHORIZATION, "Bearer " + props.getApiKey())
        .contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON)
        .bodyValue(body)
        .retrieve()
        .bodyToMono(Map.class)
        .timeout(Duration.ofMillis(Math.max(1000, props.getTimeoutMs())))
        .retryWhen(Retry.max(1))
        .block();

    // 메시지 content(JSON 문자열) 추출
    String content = Jsons.extractChoiceContent(resp);
    Map<String,Object> json = Jsons.parseJsonObject(content);

    Map<String,Object> usage = Jsons.optMap(resp, "usage");
    Integer promptTok = Jsons.optInt(usage, "prompt_tokens");
    Integer compTok   = Jsons.optInt(usage, "completion_tokens");

    int latency = (int)(System.currentTimeMillis() - start);

    return new LlmClient.LlmExplainResp(
        Jsons.optString(json, "explanation"),
        Jsons.optListString(json, "bullets"),
        Jsons.optListString(json, "keypoints"),
        Jsons.optListString(json, "nextActions"),
        Jsons.optDouble(json, "confidence"),
        new LlmClient.LlmExplainResp.Usage(promptTok, compTok, latency),
        new LlmClient.LlmExplainResp.ModelInfo("gpt-4o-mini", "chat-completions"),
        Jsons.optString(json, "traceId")
    );
  }

  private String buildUserPrompt(LlmClient.LlmExplainReq r){
    StringBuilder b = new StringBuilder();
    b.append("mode=").append(r.mode()).append(", type=").append(r.type())
        .append(", locale=").append(r.locale()).append("\n");
    if (r.userId()!=null)    b.append("userId=").append(r.userId()).append("\n");
    if (r.questionId()!=null)b.append("questionId=").append(r.questionId()).append("\n");
    if (r.topicId()!=null)   b.append("topicId=").append(r.topicId()).append("\n");
    if (r.tags()!=null && !r.tags().isEmpty()) b.append("tags=").append(r.tags()).append("\n");

    b.append("\n[문제]\n").append(nullToEmpty(r.questionText())).append("\n");
    if (r.choices()!=null && !r.choices().isEmpty()){
      b.append("\n[선지]\n");
      r.choices().forEach(c -> b.append(c.label()).append(") ").append(nullToEmpty(c.text())).append("\n"));
    }
    if (r.userAnswer()!=null)    b.append("\n[사용자답]\n").append(r.userAnswer()).append("\n");
    if (r.correctAnswer()!=null) b.append("\n[정답]\n").append(r.correctAnswer()).append("\n");
    if (r.baseExplanation()!=null && !r.baseExplanation().isBlank()){
      b.append("\n[DB 해설 요약]\n").append(r.baseExplanation()).append("\n");
    }

    // 스타일 힌트
    b.append("\n[출력 규칙]\n- 오직 JSON 객체만 출력\n- explanation 최대 5문장, bullets 3~5개, nextActions 2~3개\n");
    return b.toString();
  }

  private static String nullToEmpty(String s){ return s==null ? "" : s; }
}
