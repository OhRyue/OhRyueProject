package com.OhRyue.certpilot.study.controller;

import com.OhRyue.common.dto.TagViewDto;
import com.OhRyue.certpilot.study.service.TagQueryService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Tag", description = "태그 마스터 조회 API")
@RestController
@RequestMapping("/api/study/tags")
@RequiredArgsConstructor
public class TagController {

  private final TagQueryService tagQueryService;

  @Operation(
      summary = "태그 목록 조회",
      description = "태그 마스터 목록을 조회합니다. domain 파라미터로 필기/실기를 필터링할 수 있습니다.\n\n" +
          "- domain이 없으면 WRITTEN + PRACTICAL 전체 반환\n" +
          "- domain=WRITTEN 또는 domain=PRACTICAL로 필터링 가능\n\n" +
          "정렬: orderNo ASC, code ASC"
  )
  @GetMapping
  public List<TagViewDto> getTags(
      @Parameter(description = "도메인 필터 (WRITTEN | PRACTICAL)", example = "WRITTEN")
      @RequestParam(required = false) String domain
  ) {
    return tagQueryService.getTags(domain);
  }

  @Operation(
      summary = "태그 코드 목록으로 태그 정보 조회",
      description = "태그 코드 목록을 받아서 해당 태그들의 상세 정보를 반환합니다.\n\n" +
          "프론트엔드에서 태그 코드만 가지고 있을 때, 한글 라벨과 설명을 조회하는 용도로 사용합니다."
  )
  @GetMapping("/by-codes")
  public List<TagViewDto> getTagsByCodes(
      @Parameter(description = "태그 코드 목록 (쉼표로 구분)", example = "DB_SQL_TX,DB_MODEL_NORMAL")
      @RequestParam List<String> codes
  ) {
    return tagQueryService.getTagsByCodes(codes).values().stream()
        .sorted((a, b) -> {
          // domain, orderNo, code 순으로 정렬
          int domainCompare = a.domain().compareTo(b.domain());
          if (domainCompare != 0) return domainCompare;
          int orderCompare = Integer.compare(
              a.orderNo() != null ? a.orderNo() : Integer.MAX_VALUE,
              b.orderNo() != null ? b.orderNo() : Integer.MAX_VALUE
          );
          if (orderCompare != 0) return orderCompare;
          return a.code().compareTo(b.code());
        })
        .toList();
  }
}

