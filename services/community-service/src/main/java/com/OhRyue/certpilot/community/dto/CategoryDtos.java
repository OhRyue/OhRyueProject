package com.OhRyue.certpilot.community.dto;

import java.util.List;

public class CategoryDtos {

  public record CategoryResponse(
      String code,
      String name
  ) {}

  public record CategoryListResponse(
      List<CategoryResponse> categories
  ) {}
}


