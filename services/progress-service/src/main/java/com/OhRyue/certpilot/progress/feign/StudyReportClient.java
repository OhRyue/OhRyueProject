package com.OhRyue.certpilot.progress.feign;

import com.OhRyue.certpilot.progress.dto.ReportDtos.RecentRecordsResp;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(
    name = "study-report",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/study",
    fallback = StudyReportFallback.class
)
public interface StudyReportClient {

  /** study-service 쪽 시그니처와 정확히 일치해야 합니다. */
  @GetMapping("/report/recent-records")
  RecentRecordsResp recent(
      @RequestParam("userId") String userId,
      @RequestParam(value = "limit", defaultValue = "30") int limit
  );
}
