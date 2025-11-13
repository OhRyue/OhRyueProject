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
   * 매일 06:00에 시험 일정, 자격정보, 공개문제를 순차적으로 동기화합니다.
   */
  @Scheduled(cron = "0 0 6 * * *")
  public void syncAll() {
    log.info("[QNET] Scheduled sync start");
    var scheduleResult = qnetSyncService.syncExamSchedules(null, null, null);
    log.info("[QNET] Schedule sync result: {}", scheduleResult);
    var qualificationResult = qnetSyncService.syncQualifications();
    log.info("[QNET] Qualification sync result: {}", qualificationResult);
    var openQuestionResult = qnetSyncService.syncOpenQuestions(null);
    log.info("[QNET] Open question sync result: {}", openQuestionResult);
    log.info("[QNET] Scheduled sync finished");
  }
}

