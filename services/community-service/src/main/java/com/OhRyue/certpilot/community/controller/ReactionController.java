package com.OhRyue.certpilot.community.controller;

import com.OhRyue.common.auth.AuthUserUtil;
import com.OhRyue.certpilot.community.dto.ReactionDtos;
import com.OhRyue.certpilot.community.service.ReactionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Community - Reactions", description = "커뮤니티 좋아요(리액션) APIs")
@RestController
@RequestMapping("/api/community/reactions")
@RequiredArgsConstructor
public class ReactionController {

    private final ReactionService reactionService;

    @Operation(summary = "좋아요/취소 토글")
    @PostMapping("/toggle")
    public ResponseEntity<ReactionDtos.ToggleResponse> toggle(@Valid @RequestBody ReactionDtos.ToggleRequest request) {
        String userId = AuthUserUtil.getCurrentUserId();
        return ResponseEntity.ok(reactionService.toggle(userId, request));
    }
}
