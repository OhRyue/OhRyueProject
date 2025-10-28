package com.OhRyue.certpilot.question.web;

import com.OhRyue.certpilot.question.service.QuestionQueryService;
import com.OhRyue.certpilot.shared.dto.PageResponse;
import com.OhRyue.certpilot.shared.dto.QuestionDto;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/questions")
public class QuestionController {
  private final QuestionQueryService questionQueryService;

  @GetMapping
  public PageResponse<QuestionDto> list(@RequestParam(defaultValue="0") int page,
                                        @RequestParam(defaultValue="20") int size) {
    return questionQueryService.findAll(PageRequest.of(page, size));
  }

  @GetMapping("/latest")
  public List<QuestionDto> latestByTopics(@RequestParam List<Long> topicIds,
                                          @RequestParam(defaultValue="10") int limit) {
    return questionQueryService.latestByTopics(topicIds, limit);
  }
}
