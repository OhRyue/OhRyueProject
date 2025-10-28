package com.OhRyue.certpilot.concept.web;

import com.OhRyue.certpilot.concept.service.ConceptService;
import com.OhRyue.certpilot.concept.web.dto.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name="Concept", description="개념/미니체크 API")
@RestController
@RequestMapping("/concepts")
@RequiredArgsConstructor
public class ConceptController {

    private final ConceptService svc;

    @Operation(summary="개념 목록(페이지/카테고리)")
    @GetMapping
    public Page<ConceptSummaryDto> list(@RequestParam Long certId,
                                        @RequestParam(required=false, defaultValue = "") String category,
                                        @RequestParam(defaultValue = "0") int page,
                                        @RequestParam(defaultValue = "20") int size) {
        return svc.list(certId, category, PageRequest.of(page, Math.min(size, 100), Sort.by("id").descending()));
    }

    @Operation(summary="개념 상세")
    @GetMapping("/{id}")
    public ConceptDetailDto detail(@PathVariable Long id) { return svc.detail(id); }

    @Operation(summary="미니체크 2~3개")
    @GetMapping("/{id}/mini-check")
    public List<MiniItemDto> mini(@PathVariable Long id) { return svc.miniCheck(id); }
}
