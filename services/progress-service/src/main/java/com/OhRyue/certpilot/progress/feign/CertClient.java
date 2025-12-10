package com.OhRyue.certpilot.progress.feign;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

/**
 * cert-service의 토픽 정보 조회용 Feign Client
 */
@FeignClient(
    name = "cert-service",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/cert"
)
public interface CertClient {

    /**
     * 토픽 정보 조회
     * @param topicId 토픽 ID
     * @return 토픽 정보 JSON 문자열 (수동 파싱 필요)
     */
    @GetMapping("/topics/{topicId}")
    String getTopic(@PathVariable("topicId") Long topicId);
}



