package com.OhRyue.certpilot.learn.service;

import com.OhRyue.certpilot.learn.client.QuestionClient;
import com.OhRyue.certpilot.shared.dto.PageResponse;
import com.OhRyue.certpilot.shared.dto.QuestionDto;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
public class QuestionFacade {

  private final QuestionClient client;

  @CircuitBreaker(name = "questionApi", fallbackMethod = "fallbackList")
  @Retry(name = "questionApi")
  public List<QuestionDto> list(int page, int size) {
    PageResponse<QuestionDto> resp = client.list(page, size);
    return (resp != null && resp.getContent() != null) ? resp.getContent() : Collections.emptyList();
  }

  // fallback 시그니처: (원본 파라미터 + Throwable)
  private List<QuestionDto> fallbackList(int page, int size, Throwable t) {
    return Collections.emptyList();
  }
}
