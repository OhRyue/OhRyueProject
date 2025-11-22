package com.OhRyue.certpilot.progress.feign;

import com.OhRyue.certpilot.progress.feign.dto.AccountMeResponse;
import com.OhRyue.certpilot.progress.feign.dto.ProfileSummaryResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@FeignClient(
    name = "account-service",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/account"
)
public interface AccountClient {

  /**
   * 현재 로그인한 사용자 정보 조회
   * - Authorization 헤더는 공통 Feign Interceptor에서 자동으로 붙는다고 가정합니다.
   */
  @GetMapping("/me")
  AccountMeResponse me();

  /**
   * 여러 사용자 프로필 요약 조회
   * - Authorization 헤더 역시 인터셉터를 통해 자동 전달.
   */
  @GetMapping("/internal/users/summaries")
  List<ProfileSummaryResponse> summaries(@RequestParam("ids") List<String> userIds);
}
