package com.OhRyue.certpilot.study.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * 약점 보완 퀴즈 추천 파라미터
 */
@Configuration
@ConfigurationProperties(prefix = "study.reco")
@Getter @Setter
public class RecommendationProperties {

  /** 기본 추천 문제 수 */
  private int defaultCount = 20;

  /** 허용 난이도 (콤마 구분 문자열을 YAML에서 넣고, 서비스에서 파싱해 사용) */
  private String allowedDifficulties = "EASY,NORMAL,HARD";

  /** 난이도 가중치 (EASY/NORMAL/HARD 순) */
  private double difficultyWeightEasy  = 0.8;
  private double difficultyWeightNormal= 1.0;
  private double difficultyWeightHard  = 1.2;

  /** 최근 오답의 가중치 (정답률 기반 점수에 더해지는 승수) */
  private double recentWrongWeight = 1.5;

  /** 최근 오답으로 판단할 기간(일) */
  private int recentDays = 14;

  /** 태그별 최소 풀이 수(노이즈 제거) */
  private int minTriedPerTag = 3;

  /** 동일 태그에서 중복 출제 방지: 태그별 최대 비중(0~1) */
  private double perTagQuotaRatio = 0.5;

  /** 무작위 보강(탐색) 비중(0~1) */
  private double exploreRatio = 0.2;
}
