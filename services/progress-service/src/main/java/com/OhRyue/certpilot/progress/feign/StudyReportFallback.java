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
  public String progressCardRaw(Long certId, String mode) {
    // 실패 시 빈 JSON 반환
    return "{}";
  }
}
