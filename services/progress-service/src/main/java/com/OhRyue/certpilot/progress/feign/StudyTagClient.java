package com.OhRyue.certpilot.progress.feign;

import com.OhRyue.common.dto.TagViewDto;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@FeignClient(
    name = "study-tag",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/study/tags",
    fallback = StudyTagFallback.class
)
public interface StudyTagClient {

  /**
   * 태그 목록 조회
   * - Study: GET /api/study/tags?domain=WRITTEN
   * - domain 파라미터 없으면 WRITTEN + PRACTICAL 전체 반환
   */
  @GetMapping
  List<TagViewDto> getTags(@RequestParam(value = "domain", required = false) String domain);

  /**
   * 태그 코드 목록으로 태그 정보 조회
   * - Study: GET /api/study/tags/by-codes?codes=DB_SQL_TX,DB_MODEL_NORMAL
   * - 반환: List<TagViewDto> (정렬: domain, orderNo, code)
   */
  @GetMapping("/by-codes")
  List<TagViewDto> getTagsByCodes(@RequestParam("codes") List<String> codes);
}

