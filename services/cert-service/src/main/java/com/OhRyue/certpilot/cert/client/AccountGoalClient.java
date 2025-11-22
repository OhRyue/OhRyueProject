package com.OhRyue.certpilot.cert.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;

@FeignClient(
    name = "account-goal-client",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/account/internal"
)
public interface AccountGoalClient {

  /**
   * 현재 로그인한 사용자의 목표 자격증/회차 정보를 조회합니다.
   *
   * - account-service: GET /api/account/internal/users/goal
   * - userId는 컨트롤러에서 AuthUserUtil.getCurrentUserId()로 추출
   * - 이 Feign 호출 시 Authorization 헤더는 공통 Feign Interceptor에서 자동 전달된다고 가정
   */
  @GetMapping("/users/goal")
  GoalResponse goal();

  record GoalResponse(
      Long id,
      String userId,
      Long certId,
      String targetExamMode,
      Long targetRoundId,
      Integer ddayCached
  ) {}
}
