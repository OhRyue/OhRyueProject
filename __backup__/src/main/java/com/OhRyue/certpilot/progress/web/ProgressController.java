package com.OhRyue.certpilot.progress.web;

import com.OhRyue.certpilot.progress.web.dto.ProgressOverviewDto;
import com.OhRyue.certpilot.progress.service.ProgressService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Progress", description = "학습 리포트(능력/오답 현황)")
@RestController
@RequestMapping("/progress")
@RequiredArgsConstructor
public class ProgressController {

    private final ProgressService svc;

    @Operation(summary = "유저 학습 리포트 조회(태그별 능력/오답 + 최근 오답 10)")
    @GetMapping("/overview")
    public ProgressOverviewDto overview(@RequestParam Long userId) {
        return svc.overview(userId);
    }
}
