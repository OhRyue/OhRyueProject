package com.OhRyue.certpilot.learn.web;

import com.OhRyue.certpilot.learn.service.LearnOrchestrationService;
import com.OhRyue.certpilot.shared.dto.LearnSessionDto;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/learn")
public class LearnController {

  private final LearnOrchestrationService service;

  @GetMapping("/session/start")
  public LearnSessionDto start(@RequestParam(defaultValue = "0") int page,
                               @RequestParam(defaultValue = "5") int size) {
    return service.startSession(page, size);
  }
}
