package com.OhRyue.certpilot.learn.client;

import com.OhRyue.certpilot.shared.dto.PageResponse;
import com.OhRyue.certpilot.shared.dto.QuestionDto;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

// Eureka 서비스명
@FeignClient(name = "question-service", path = "/api/questions")
public interface QuestionClient {

  @GetMapping
  PageResponse<QuestionDto> list(@RequestParam int page, @RequestParam int size);
}
