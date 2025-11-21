package com.OhRyue.certpilot.community.controller;

import com.OhRyue.certpilot.community.dto.CategoryDtos;
import com.OhRyue.certpilot.community.service.CategoryService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.stream.Collectors;

@Tag(name = "Community - Categories", description = "커뮤니티 카테고리 목록 APIs")
@RestController
@RequestMapping("/api/community/categories")
@RequiredArgsConstructor
public class CategoryController {

    private final CategoryService categoryService;

    @Operation(summary = "커뮤니티 카테고리 목록 조회")
    @GetMapping
    public CategoryDtos.CategoryListResponse categories() {
        var categories = categoryService.findAll().stream()
                .map(cat -> new CategoryDtos.CategoryResponse(cat.getCode(), cat.getName()))
                .collect(Collectors.toList());
        return new CategoryDtos.CategoryListResponse(categories);
    }
}
