package com.OhRyue.certpilot.cert.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(
    name = "account-goal-client",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/account/internal"
)
public interface AccountGoalClient {

  @GetMapping("/users/goal")
  GoalResponse goal(@RequestParam("userId") String userId);

  record GoalResponse(
      Long id,
      String userId,
      Long certId,
      String targetExamMode,
      Long targetRoundId,
      Integer ddayCached
  ) {}
}

