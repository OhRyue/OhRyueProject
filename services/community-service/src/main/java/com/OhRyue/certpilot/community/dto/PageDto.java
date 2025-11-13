package com.OhRyue.certpilot.community.dto;

import org.springframework.data.domain.Page;

public record PageDto(
    int page,
    int size,
    long totalElements,
    int totalPages
) {
  public static PageDto of(Page<?> page) {
    return new PageDto(page.getNumber(), page.getSize(), page.getTotalElements(), page.getTotalPages());
  }
}
