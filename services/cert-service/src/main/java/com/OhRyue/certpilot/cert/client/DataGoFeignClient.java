package com.OhRyue.certpilot.cert.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.Map;

@FeignClient(name = "datagoClient", url = "${cert.qnet.data-url}")
public interface DataGoFeignClient {

  @GetMapping(value = "/openQes/getList", produces = MediaType.APPLICATION_JSON_VALUE)
  String getOpenQuestions(@RequestParam Map<String, String> params);
}

