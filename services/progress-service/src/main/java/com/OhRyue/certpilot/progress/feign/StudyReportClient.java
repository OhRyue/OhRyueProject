package com.OhRyue.certpilot.progress.feign;

import com.OhRyue.certpilot.progress.dto.ReportDtos.RecentRecordsResp;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeProgressCard;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(
    name = "study-report",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/study",
    fallback = StudyReportFallback.class
)
public interface StudyReportClient {

  /**
   * study-service의 /api/study/report/recent-records 와 시그니처를 맞춘다.
   * userId는 JWT에서 가져오므로 파라미터 없음.
   */
  @GetMapping("/report/recent-records")
  RecentRecordsResp recent(
      @RequestParam(value = "limit", defaultValue = "30") int limit
  );

  /**
   * /api/study/report/progress-card
   * userId는 JWT에서 가져오고, certId만 QueryParam으로 전달.
   */
  @GetMapping("/report/progress-card")
  HomeProgressCard progressCard(
      @RequestParam("certId") Long certId
  );
}
