package com.OhRyue.certpilot.cert.scheduler;

import com.OhRyue.certpilot.cert.service.QnetSyncService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class QnetSyncScheduler {

  private final QnetSyncService qnetSyncService;

  /**
   * 매일 06:00에 시험 일정, 공개문제를 순차적으로 동기화합니다.
   * 주의: 공공데이터포털 API만 사용 (Q-Net API는 네트워크 연결 불가로 제외)
   */
  @Scheduled(cron = "0 0 6 * * *")
  public void syncAll() {
    log.info("[공공데이터포털] Scheduled sync start (exam + open only)");
    var scheduleResult = qnetSyncService.syncExamSchedules(null, null, null);
    log.info("[공공데이터포털] Schedule sync result: {}", scheduleResult);
    // qualification 동기화는 Q-Net API가 필요하므로 제외
    // var qualificationResult = qnetSyncService.syncQualifications();
    // log.info("[공공데이터포털] Qualification sync result: {}", qualificationResult);
    var openQuestionResult = qnetSyncService.syncOpenQuestions(null);
    log.info("[공공데이터포털] Open question sync result: {}", openQuestionResult);
    log.info("[공공데이터포털] Scheduled sync finished");
  }
}

