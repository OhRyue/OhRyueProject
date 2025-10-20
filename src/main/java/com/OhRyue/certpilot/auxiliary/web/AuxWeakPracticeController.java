package com.OhRyue.certpilot.auxiliary.web;

import com.OhRyue.certpilot.aux.service.AuxWeakPracticeService;
import com.OhRyue.certpilot.quiz.web.dto.QuickSessionDto;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Aux:WeakPractice", description = "보조학습 - 약점 보완 세션 시작(Quick)")
@RestController
@RequestMapping("/aux/solo")
@RequiredArgsConstructor
public class AuxWeakPracticeController {

    private final AuxWeakPracticeService svc;

    public record StartWeakRequest(Long userId, Integer count) {}

    @Operation(summary = "약점 보완 세션 시작(약점 태그 1~3개 자동 선택)")
    @PostMapping("/weak/start")
    public QuickSessionDto startWeak(@RequestBody StartWeakRequest req) {
        int count = req.count() == null ? 5 : req.count();
        return svc.startWeakPractice(req.userId(), count);
    }
}
