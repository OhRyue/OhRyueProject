package com.OhRyue.certpilot.learn.web;

import com.OhRyue.certpilot.learn.service.LearnService;
import com.OhRyue.certpilot.learn.web.dto.*;
import io.swagger.v3.oas.annotations.Hidden;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Hidden
@RestController
@RequestMapping("/learn")
@RequiredArgsConstructor
public class LearnController {

    private final LearnService svc;

    @Operation(summary = "제출/채점(미니/퀴즈)")
    @PostMapping("/submit")
    public LearnSubmitResult submit(@RequestBody LearnSubmitRequest req) {
        return svc.submit(req);
    }
}
