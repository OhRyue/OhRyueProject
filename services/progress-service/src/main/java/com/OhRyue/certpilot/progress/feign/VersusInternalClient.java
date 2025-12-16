package com.OhRyue.certpilot.progress.feign;

import com.OhRyue.certpilot.progress.config.FeignConfig;
import com.OhRyue.certpilot.progress.feign.dto.MatchDetailDto;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(
    name = "versus-service",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/versus/internal",
    configuration = FeignConfig.class
)
public interface VersusInternalClient {

  @GetMapping("/matches/{matchId}/detail")
  MatchDetailDto getMatchDetail(
      @PathVariable("matchId") Long matchId,
      @RequestParam("userId") String userId
  );
}
