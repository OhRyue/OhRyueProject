package com.OhRyue.certpilot.cert.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.Map;

@FeignClient(name = "qnetClient", url = "${cert.qnet.base-url}")
public interface QnetFeignClient {

  @GetMapping(value = "/InquiryQualInfoService/getList", produces = MediaType.APPLICATION_JSON_VALUE)
  String getQualificationInfo(@RequestParam Map<String, String> params);

  @GetMapping(value = "/InquirySchduleByQual/getList", produces = MediaType.APPLICATION_JSON_VALUE)
  String getExamSchedule(@RequestParam Map<String, String> params);
}

