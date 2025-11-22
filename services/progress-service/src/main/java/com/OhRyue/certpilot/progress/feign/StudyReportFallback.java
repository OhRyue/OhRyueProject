package com.OhRyue.certpilot.progress.feign;

import com.OhRyue.certpilot.progress.dto.ReportDtos.RecentRecordsResp;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeProgressCard;
import org.springframework.stereotype.Component;

import java.util.List;

/** 폴백: 네트워크/라우팅 문제 시 안전한 기본값 반환(Null 금지) */
@Component
public class StudyReportFallback implements StudyReportClient {

  @Override
  public RecentRecordsResp recent(int limit) {
    // 실패 시에도 records는 빈 리스트로 보장
    return new RecentRecordsResp(List.of());
  }

  @Override
  public HomeProgressCard progressCard(Long certId) {
    // 실패 시 진행률 0, 토픽 정보 0으로 초기화
    return new HomeProgressCard(
        0,      // totalTopics
        0,      // completedTopics
        0,      // pendingTopics
        0.0,    // completionRate
        null    // lastStudiedAt
    );
  }
}
