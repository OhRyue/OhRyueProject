package com.OhRyue.certpilot.progress.feign;

import com.OhRyue.common.dto.TagViewDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;

@Slf4j
@Component
public class StudyTagFallback implements StudyTagClient {

  @Override
  public List<TagViewDto> getTags(String domain) {
    log.warn("StudyTagClient.getTags fallback called for domain: {}", domain);
    return Collections.emptyList();
  }

  @Override
  public List<TagViewDto> getTagsByCodes(List<String> codes) {
    log.warn("StudyTagClient.getTagsByCodes fallback called for codes: {}", codes);
    return Collections.emptyList();
  }
}





