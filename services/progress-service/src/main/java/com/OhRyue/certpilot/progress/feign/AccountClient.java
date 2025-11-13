package com.OhRyue.certpilot.progress.feign;

import com.OhRyue.certpilot.progress.feign.dto.AccountMeResponse;
import com.OhRyue.certpilot.progress.feign.dto.ProfileSummaryResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@FeignClient(
    name = "account-service",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/account"
)
public interface AccountClient {

  @GetMapping("/me")
  AccountMeResponse me(@RequestHeader("Authorization") String authorization);

  @GetMapping("/internal/users/summaries")
  List<ProfileSummaryResponse> summaries(@RequestHeader("Authorization") String authorization,
                                         @RequestParam("ids") List<String> userIds);
}

