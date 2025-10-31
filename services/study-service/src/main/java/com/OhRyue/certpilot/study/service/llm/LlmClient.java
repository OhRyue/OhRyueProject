package com.OhRyue.certpilot.study.service.llm;

import com.OhRyue.certpilot.study.service.llm.LlmFeignConfig;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;
import java.util.Map;

/**
 * LLM Explain API
 * POST {baseUrl}/explain
 */
@FeignClient(
    name = "llmClient",
    configuration = LlmFeignConfig.class
)
public interface LlmClient {

  @PostMapping("/explain")
  LlmExplainResp explain(@RequestBody LlmExplainReq req);

  // ---- Request / Response 모델 ----

  record Choice(String label, String text) {}

  /** 요청 모델 **/
  record LlmExplainReq(
      String mode,                // WRITTEN | PRACTICAL
      String type,                // OX | MCQ | SHORT | LONG
      String locale,              // "ko-KR" 등

      String userId,              // 선택: 개인화 힌트
      Long questionId,            // 선택: 추적용
      Long topicId,               // 선택: 컨텍스트
      List<String> tags,          // 선택: 약점/주제 태그

      String questionText,        // 문제 본문
      List<Choice> choices,       // 선택: MCQ일 때
      String userAnswer,          // 사용자 응답 (라벨/텍스트/true|false/서술)
      String correctAnswer,       // 선택: 정답 (라벨/텍스트)

      String baseExplanation,     // DB 해설 요약
      Map<String,Object> style,   // 선택: 톤/길이/글머리 등
      Map<String,Object> meta     // 선택: traceId 등
  ) {}

  /**
   * 응답 모델(풍부한 메타 포함)
   */
  record LlmExplainResp(
      String explanation,         // 최종 맞춤 해설(문장형)
      List<String> bullets,       // 요점 리스트
      List<String> keypoints,     // 키워드 요약
      List<String> nextActions,   // 다음 학습 행동

      Double confidence,          // 확신도(선택)
      Usage usage,                // 토큰/지연(선택)
      ModelInfo modelInfo,        // 모델 정보(선택)
      String traceId              // 서버에서 반환한 트레이스 ID
  ) {
    public record Usage(Integer promptTokens, Integer completionTokens, Integer latencyMs) {}
    public record ModelInfo(String model, String version) {}
  }
}
