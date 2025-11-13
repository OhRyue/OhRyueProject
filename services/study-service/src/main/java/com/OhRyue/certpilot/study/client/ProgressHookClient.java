package com.OhRyue.certpilot.study.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

@FeignClient(name = "progress-service", path = "/api/progress")
public interface ProgressHookClient {

  @PostMapping("/hook/submit")
  void submit(@RequestBody SubmitPayload payload);

  record SubmitPayload(
      String userId,
      String examMode,
      String questionType,
      Boolean correct,
      Integer score,
      List<String> tags,
      String source
  ) {}
}


