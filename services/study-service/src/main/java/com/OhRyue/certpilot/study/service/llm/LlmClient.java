package com.OhRyue.certpilot.study.service.llm;

import java.util.List;
import java.util.Map;

/**
 * LLM Explain Port (Feign 제거 버전)
 */
public interface LlmClient {

  LlmExplainResp explain(LlmExplainReq req);

  /* ---------- 모델 ---------- */

  record Choice(String label, String text) {}

  /** 요청 모델 **/
  record LlmExplainReq(
      String mode,                // WRITTEN | PRACTICAL
      String type,                // OX | MCQ | SHORT | LONG | SUMMARY
      String locale,              // "ko-KR" 등

      String userId,              // 선택: 개인화 힌트
      Long questionId,            // 선택: 추적용
      Long topicId,               // 선택: 컨텍스트
      List<String> tags,          // 선택: 약점/주제 태그

      String questionText,        // 문제 본문 또는 요약 프롬프트
      List<Choice> choices,       // 선택: MCQ일 때 선지
      String userAnswer,          // 사용자 응답
      String correctAnswer,       // 선택: 정답

      String baseExplanation,     // DB 해설 요약
      Map<String,Object> style,   // 선택: 톤/길이/글머리 등
      Map<String,Object> meta     // 선택: traceId 등
  ) {}

  /** 응답 모델 **/
  record LlmExplainResp(
      String explanation,         // 최종 맞춤 해설
      List<String> bullets,       // 요점 리스트
      List<String> keypoints,     // 키워드 요약
      List<String> nextActions,   // 다음 학습 행동
      Double confidence,          // 확신도(0~1)
      Usage usage,                // 토큰/지연
      ModelInfo modelInfo,        // 모델 정보
      String traceId              // 트레이스 ID
  ) {
    public record Usage(Integer promptTokens, Integer completionTokens, Integer latencyMs) {}
    public record ModelInfo(String model, String version) {}
  }
}
