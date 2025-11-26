package com.OhRyue.certpilot.cert.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.Map;

/**
 * data.go.kr (B490007) 기반 Q-Net 연계 OpenAPI 클라이언트.
 *
 * - 시험 일정: /qualExamSchd/getQualExamSchdList
 * - 공개 문제: /openQst/getOpenQstList
 *
 * application.yml 예시:
 *   cert:
 *     qnet:
 *       data-url: https://apis.data.go.kr/B490007
 */
@FeignClient(name = "datagoClient", url = "${cert.qnet.data-url}")
public interface DataFeignClient {

    /**
     * 국가자격 시험일정 조회
     * 예시:
     *   /qualExamSchd/getQualExamSchdList?serviceKey=...&numOfRows=10&pageNo=1&dataFormat=json&qualgbCd=T&jmCd=1234&implYy=2024
     */
    @GetMapping(value = "/qualExamSchd/getQualExamSchdList", produces = MediaType.APPLICATION_JSON_VALUE)
    String getExamSchedules(@RequestParam Map<String, String> params);

    /**
     * 공개문제 목록 조회
     * 예시:
     *   /openQst/getOpenQstList?serviceKey=...&numOfRows=10&pageNo=1&dataFormat=json&qualgbCd=T&jmCd=1234
     */
    @GetMapping(value = "/openQst/getOpenQstList", produces = MediaType.APPLICATION_JSON_VALUE)
    String getOpenQuestions(@RequestParam Map<String, String> params);

    /**
     * 공개문제 상세 조회
     * - data.go.kr(B490007) /openQst/getOpenQst 사용
     * - artlSeq, qualgbCd 파라미터로 상세 정보 조회 (contents, fileList 포함)
     * 예시:
     *   /openQst/getOpenQst?serviceKey=...&dataFormat=xml&qualgbCd=T&artlSeq=5011000
     */
    @GetMapping(value = "/openQst/getOpenQst", produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
    String getOpenQuestionDetail(@RequestParam Map<String, String> params);

    /**
     * 종목 정보 조회 (출제경향, 출제기준, 취득방법 등)
     * - data.go.kr(B490007) /qualInfo/getQualInfoList 사용
     * 예시:
     *   /qualInfo/getQualInfoList?serviceKey=...&jmCd=1320
     */
    @GetMapping(value = "/qualInfo/getQualInfoList", produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
    String getQualificationInfo(@RequestParam Map<String, String> params);
}
