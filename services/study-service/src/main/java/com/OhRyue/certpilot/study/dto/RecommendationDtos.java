package com.OhRyue.certpilot.study.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;

import java.util.List;

/**
 * 약점 태그/추천 퀴즈 DTO
 */
public class RecommendationDtos {

  /* 약점 태그 응답 */
  @Deprecated
  public record WeakTag(String tag, int correct, int total, double accuracy, double weakness) {}
  
  /**
   * 약점 태그 통계 (TagViewDto 포함)
   */
  public record WeakTagStatDto(
      com.OhRyue.common.dto.TagViewDto tag,
      double scorePct,
      int questionCount
  ) {}
  
  public record WeakTagsResp(List<WeakTagStatDto> tags) {}

  /* 태그 기반 퀴즈 요청
     - allowedDifficulties, recentWrongWeight 등은 null이면 서버 기본값 사용
   */
  public record TagQuizReq(
      @NotNull List<String> tags,         // 선호/약점 태그 목록
      List<String> allowedDifficulties,   // 예: ["EASY","NORMAL","HARD"]
      @Min(1) Integer count,              // 총 문항 수(기본 study.reco.default-count)
      Double recentWrongWeight,           // 최근 오답 가중치 승수
      Integer recentDays,                 // 최근 오답 기간(일)
      Integer minTriedPerTag,             // 태그 최소 풀이 수
      Double perTagQuotaRatio,            // 태그별 최대 비중(0~1)
      Double exploreRatio                 // 무작위 보강 비중(0~1)
  ) {}

  /* 추천 결과 */
  public record QuizQ(Long id, String text, String difficulty, List<Choice> choices) {
    public record Choice(String label, String text) {}
  }
  public record TagQuizSet(List<QuizQ> questions) {}
}
