package com.OhRyue.certpilot.study.service.llm;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class LlmClientAdapter implements LlmClient {
  private final OpenAiClient openAi;

  @Override
  public LlmExplainResp explain(LlmExplainReq req) {
    return openAi.explain(req);
  }
}
