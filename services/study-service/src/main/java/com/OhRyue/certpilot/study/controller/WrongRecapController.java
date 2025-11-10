package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.WrongRecapDtos.WrongRecapSet;
import com.OhRyue.certpilot.study.service.WrittenService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Wrong Recap(틀린 문제 다시보기)")
@RestController
@RequestMapping("/api/study/wrong")
@RequiredArgsConstructor
public class WrongRecapController {

  private final WrittenService written;

  @Operation(summary = "내 최근 틀린 문제 다시보기(토픽 범위)", description = "limit 기본 20")
  @GetMapping("/recap")
  public WrongRecapSet recap(@RequestParam Long topicId,
                             @RequestParam String userId,
                             @RequestParam(defaultValue = "20") int limit) {
    return written.wrongRecap(topicId, userId, limit);
  }

  @Operation(summary = "특정 문제 ID 목록으로 다시보기")
  @GetMapping("/recap-by-ids")
  public WrongRecapSet recapByIds(@RequestParam String ids,
                                  @RequestParam(required = false) String userId) {
    return written.wrongRecapByIds(ids, userId);
  }
}
