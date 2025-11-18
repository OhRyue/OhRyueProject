package com.OhRyue.certpilot.cert.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.Map;

/**
 * Q-Net OpenAPI (openapi.q-net.or.kr) 전용 클라이언트.
 * - 주 사용 용도: 자격(종목) 기본 정보 조회
 */
@FeignClient(name = "qnetClient", url = "${cert.qnet.base-url}")
public interface QnetFeignClient {

    /**
     * 국가기술자격 종목 정보 조회
     *
     * 예시:
     *   /InquiryQualInfoService/getList?serviceKey=...&numOfRows=10&pageNo=1&_type=json
     *
     * 실제 파라미터 구성은 QnetSyncService 에서 Map 으로 전달합니다.
     */
    @GetMapping(value = "/InquiryQualInfoService/getList", produces = MediaType.APPLICATION_JSON_VALUE)
    String getQualificationInfo(@RequestParam Map<String, String> params);
}
