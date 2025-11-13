package com.OhRyue.certpilot.community.controller;

import com.OhRyue.certpilot.community.dto.ReactionDtos;
import com.OhRyue.certpilot.community.service.ReactionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "Community - Reactions", description = "커뮤니티 좋아요(리액션) APIs")
@RestController
@RequestMapping("/api/community/reactions")
@RequiredArgsConstructor
public class ReactionController {

  private final ReactionService reactionService;

  @Operation(summary = "좋아요/취소 토글")
  @PostMapping("/toggle")
  public ResponseEntity<ReactionDtos.ToggleResponse> toggle(@RequestHeader("X-User-Id") String userId,
                                                            @Valid @RequestBody ReactionDtos.ToggleRequest request) {
    ReactionDtos.ToggleRequest enriched = new ReactionDtos.ToggleRequest(
        request.targetType(),
        request.targetId(),
        userId
    );
    return ResponseEntity.ok(reactionService.toggle(enriched));
  }
}

