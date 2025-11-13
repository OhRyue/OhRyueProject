package com.OhRyue.certpilot.community.controller;

import com.OhRyue.certpilot.community.dto.ModerationDtos;
import com.OhRyue.certpilot.community.service.ModerationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Community - Moderation", description = "신고 및 차단 APIs")
@RestController
@RequestMapping("/api/community/moderation")
@RequiredArgsConstructor
public class ModerationController {

  private final ModerationService moderationService;

  @Operation(summary = "게시글/댓글 신고")
  @PostMapping("/report")
  public ResponseEntity<ModerationDtos.ReportResponse> report(@RequestHeader("X-User-Id") String userId,
                                                              @Valid @RequestBody ModerationDtos.ReportRequest request) {
    return ResponseEntity.status(HttpStatus.CREATED).body(moderationService.report(userId, request));
  }

  @Operation(summary = "사용자 차단")
  @PostMapping("/blocks")
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public void block(@RequestHeader("X-User-Id") String userId,
                    @Valid @RequestBody ModerationDtos.BlockRequest request) {
    moderationService.block(userId, request.blockedUserId());
  }

  @Operation(summary = "차단 해제")
  @DeleteMapping("/blocks/{blockedUserId}")
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public void unblock(@RequestHeader("X-User-Id") String userId,
                      @PathVariable String blockedUserId) {
    moderationService.unblock(userId, blockedUserId);
  }

  @Operation(summary = "차단 목록 조회")
  @GetMapping("/blocks")
  public ModerationDtos.BlockListResponse blocks(@RequestHeader("X-User-Id") String userId) {
    return moderationService.listBlocks(userId);
  }
}

