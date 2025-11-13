package com.OhRyue.certpilot.progress.feign;

import com.OhRyue.certpilot.progress.dto.ReportDtos.RecentRecordsResp;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeProgressCard;
import org.springframework.stereotype.Component;

import java.util.List;

/** 폴백: 네트워크/라우팅 문제 시 빈 리스트 반환(Null 금지) */
@Component
public class StudyReportFallback implements StudyReportClient {
  @Override
  public RecentRecordsResp recent(String userId, int limit) {
    return new RecentRecordsResp(List.of());
  }

  @Override
  public HomeProgressCard progressCard(String userId, Long certId) {
    return new HomeProgressCard(0, 0, 0, 0.0, null);
  }
}
