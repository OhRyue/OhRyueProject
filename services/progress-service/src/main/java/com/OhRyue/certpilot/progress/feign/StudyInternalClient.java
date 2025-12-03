package com.OhRyue.certpilot.progress.feign;

import com.OhRyue.certpilot.progress.feign.dto.StudySessionDetailDto;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(
    name = "study-service",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/study/internal"
)
public interface StudyInternalClient {

    /**
     * 세션 상세 조회 (내부 API)
     * progress-service에서 study-service의 내부 API를 호출하여 세션 상세 정보를 가져옴
     */
    @GetMapping("/sessions/{sessionId}/detail")
    StudySessionDetailDto getSessionDetail(
            @PathVariable("sessionId") Long sessionId,
            @RequestParam("userId") String userId
    );
}



