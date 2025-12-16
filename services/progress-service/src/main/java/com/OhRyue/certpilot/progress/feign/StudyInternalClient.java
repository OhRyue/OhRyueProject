package com.OhRyue.certpilot.progress.feign;

import com.OhRyue.certpilot.progress.config.FeignConfig;
import com.OhRyue.certpilot.progress.feign.dto.StudySessionDetailDto;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(
    name = "study-service",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/study/internal",
    configuration = FeignConfig.class
)
public interface StudyInternalClient {

  @GetMapping("/sessions/{sessionId}/detail")
  StudySessionDetailDto getSessionDetail(
      @PathVariable("sessionId") Long sessionId,
      @RequestParam("userId") String userId
  );
}
