package com.OhRyue.certpilot.auxiliary.web;

import com.OhRyue.certpilot.auxiliary.web.dto.*;
import com.OhRyue.certpilot.auxiliary.service.AuxSoloService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "AuxSolo", description = "보조학습 · 혼자풀기(카테고리/난이도/약점)")
@RestController
@RequestMapping("/aux/solo")
@RequiredArgsConstructor
public class AuxSoloController {

    private final AuxSoloService svc;

    @Operation(summary = "혼자풀기 시작 - 카테고리(Topic) 지정")
    @PostMapping("/category/start")
    public AuxSoloStartDto startByCategory(@RequestBody AuxSoloCategoryStartRequest req) {
        return svc.startByCategory(req);
    }

    @Operation(summary = "혼자풀기 시작 - 난이도 범위")
    @PostMapping("/difficulty/start")
    public AuxSoloStartDto startByDifficulty(@RequestBody AuxSoloDifficultyStartRequest req) {
        return svc.startByDifficulty(req);
    }

    @Operation(summary = "혼자풀기 시작 - 약점 보완(EMA/오답 태그)")
    @PostMapping("/weak/start")
    public AuxSoloStartDto startByWeak(@RequestBody AuxSoloWeakStartRequest req) {
        return svc.startByWeak(req);
    }

    @Operation(summary = "혼자풀기 채점 - 오답노트/EMA 갱신 + AI 해설")
    @PostMapping("/grade")
    public AuxSoloGradeResult grade(@RequestBody AuxSoloGradeRequest req) {
        return svc.grade(req);
    }
}
