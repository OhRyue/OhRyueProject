package com.OhRyue.certpilot.account.feign;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(
    name = "progress-service",
    url = "${PROGRESS_SERVICE_URL:http://progress-service:8083}"
)
public interface ProgressClient {

  @PostMapping("/api/progress/internal/inventory/initialize")
  String initializeDefaultInventory(@RequestParam("userId") String userId);

  @GetMapping("/api/progress/internal/inventory/check")
  Boolean checkItemOwned(@RequestParam("userId") String userId, @RequestParam("itemId") Long itemId);
}

