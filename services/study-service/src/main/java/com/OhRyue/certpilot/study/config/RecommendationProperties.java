package com.OhRyue.certpilot.study.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * 추천(약점/태그 퀴즈) 파라미터 기본값
 */
@Getter @Setter
@Configuration
@ConfigurationProperties(prefix = "study.reco")
public class RecommendationProperties {

  /** 태그 퀴즈 기본 문항 수 */
  private int defaultCount = 20;

  /** 허용 난이도(쉼표구분) 예: EASY,NORMAL,HARD */
  private String allowedDifficulties = "EASY,NORMAL,HARD";

  /** 최근 오답 가중치 (0~∞) */
  private double recentWrongWeight = 0.8;

  /** 최근 일수 */
  private int recentDays = 14;

  /** 태그 약점으로 판단하려면 최소 풀이 수 */
  private int minTriedPerTag = 3;

  /** 태그별 최대 비중 (0~1) */
  private double perTagQuotaRatio = 0.4;

  /** 탐색(무작위 보강) 비중 (0~1) */
  private double exploreRatio = 0.2;

  /** 난이도별 승수 */
  private double difficultyWeightEasy = 0.9;
  private double difficultyWeightNormal = 1.0;
  private double difficultyWeightHard = 1.1;
}
