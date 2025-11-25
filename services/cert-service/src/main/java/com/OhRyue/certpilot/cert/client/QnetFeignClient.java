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
     *   /InquiryQualInfo/getList?serviceKey=...&numOfRows=10&pageNo=1&_type=json
     *
     * 실제 파라미터 구성은 QnetSyncService 에서 Map 으로 전달합니다.
     */
    @GetMapping(value = "/InquiryQualInfo/getList", produces = MediaType.APPLICATION_JSON_VALUE)
    String getQualificationInfo(@RequestParam Map<String, String> params);

    /**
     * (1) 국가전문자격 시험 시행일정 정보 조회
     * 
     * 예시:
     *   /InquiryTestDatesNationalProfessionalQualificationSVC/getList?serviceKey=...&seriesCd=21
     */
    @GetMapping(value = "/InquiryTestDatesNationalProfessionalQualificationSVC/getList", produces = MediaType.APPLICATION_JSON_VALUE)
    String getNationalProfessionalQualificationSchedule(@RequestParam Map<String, String> params);

    /**
     * (2) 국가기술자격 종목별 시험정보 조회
     * 
     * 예시:
     *   /InquiryTestInformationNTQSVC/getPEList?serviceKey=...
     */
    @GetMapping(value = "/InquiryTestInformationNTQSVC/getPEList", produces = MediaType.APPLICATION_JSON_VALUE)
    String getTechnicalQualificationInfo(@RequestParam Map<String, String> params);

    /**
     * (6) 자격정보 교과과정 정보 조회
     * 
     * 예시:
     *   /InquiryCurriCulumSVC/getList?ServiceKey=...&numOfRows=10&pageNo=1
     */
    @GetMapping(value = "/InquiryCurriCulumSVC/getList", produces = MediaType.APPLICATION_JSON_VALUE)
    String getCurriculumInfo(@RequestParam Map<String, String> params);

    /**
     * (7) Q-net 컨텐츠 관련 정보 조회
     * 
     * 예시:
     *   /InquiryContentsSVC/getList?ServiceKey=...&numOfRows=10&pageNo=1
     */
    @GetMapping(value = "/InquiryContentsSVC/getList", produces = MediaType.APPLICATION_JSON_VALUE)
    String getContentsInfo(@RequestParam Map<String, String> params);
}
