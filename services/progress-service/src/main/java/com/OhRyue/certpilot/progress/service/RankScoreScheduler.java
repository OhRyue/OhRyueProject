package com.OhRyue.certpilot.progress.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class RankScoreScheduler {

  private final RankService rankService;

  /**
   * 매일 새벽 전체 사용자 랭킹 점수를 재계산한다.
   */
  @Scheduled(cron = "0 0 2 * * *")
  public void recomputeAll() {
    long total = rankService.recomputeAll();
    log.debug("Recomputed rank scores for {} users", total);
  }
}

