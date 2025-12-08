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
   * userId는 JWT에서 가져오고, certId와 mode를 QueryParam으로 전달.
   * 
   * 주의: study-service가 실제로는 {"written":{...},"practical":{...}} 형태로 반환하므로
   * String으로 받아서 수동으로 파싱해야 함
   */
  @GetMapping("/report/progress-card")
  String progressCardRaw(
      @RequestParam("certId") Long certId,
      @RequestParam("mode") String mode
  );
}
