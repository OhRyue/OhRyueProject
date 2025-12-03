package com.OhRyue.certpilot.progress.feign;

import com.OhRyue.certpilot.progress.feign.dto.MatchDetailDto;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(
    name = "versus-service",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/versus/internal"
)
public interface VersusInternalClient {

    /**
     * 매치 상세 조회 (내부 API)
     * progress-service에서 versus-service의 내부 API를 호출하여 매치 상세 정보를 가져옴
     */
    @GetMapping("/matches/{matchId}/detail")
    MatchDetailDto getMatchDetail(
            @PathVariable("matchId") Long matchId,
            @RequestParam("userId") String userId
    );
}



