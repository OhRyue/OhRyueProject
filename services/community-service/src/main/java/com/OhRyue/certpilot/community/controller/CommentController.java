package com.OhRyue.certpilot.community.controller;

import com.OhRyue.certpilot.community.dto.CommentDtos;
import com.OhRyue.certpilot.community.dto.PageDto;
import com.OhRyue.certpilot.community.service.CommentService;
import com.OhRyue.certpilot.community.service.ModerationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Community - Comments", description = "커뮤니티 댓글 CRUD APIs")
@RestController
@RequestMapping("/api/community")
@RequiredArgsConstructor
public class CommentController {

  private final CommentService commentService;
  private final ModerationService moderationService;

  /* -------- 댓글 조회 -------- */
  @Operation(summary = "게시글 댓글 목록 조회")
  @GetMapping("/posts/{postId}/comments")
  public ResponseEntity<?> list(@PathVariable Long postId,
                                @RequestHeader(value = "X-User-Id", required = false) String userId,
                                @RequestParam(value = "page", defaultValue = "0") int page,
                                @RequestParam(value = "size", defaultValue = "50") int size) {
    java.util.Set<String> blocked = userId == null ? java.util.Collections.emptySet()
        : moderationService.blockedUserIds(userId);
    var commentPage = commentService.list(postId, userId, page, size, blocked);
    return ResponseEntity.ok(
        java.util.Map.of(
            "page", PageDto.of(commentPage),
            "items", commentPage.getContent()
        )
    );
  }

  /* -------- 댓글 작성 -------- */
  @Operation(summary = "게시글에 댓글 작성")
  @PostMapping("/posts/{postId}/comments")
  public ResponseEntity<CommentDtos.CommentResponse> create(@PathVariable Long postId,
                                                            @RequestHeader("X-User-Id") String userId,
                                                            @Valid @RequestBody CommentDtos.CommentCreateRequest request) {
    var enriched = new CommentDtos.CommentCreateRequest(
        userId,
        request.anonymous(),
        request.content()
    );
    var created = commentService.create(postId, enriched);
    return ResponseEntity.status(HttpStatus.CREATED)
        .body(commentService.toResponse(created, userId));
  }

  /* -------- 댓글 수정 -------- */
  @Operation(summary = "댓글 수정")
  @PutMapping("/comments/{commentId}")
  public CommentDtos.CommentResponse update(@PathVariable Long commentId,
                                            @RequestHeader("X-User-Id") String userId,
                                            @Valid @RequestBody CommentDtos.CommentUpdateRequest request) {
    var updated = commentService.update(commentId, userId, request);
    return commentService.toResponse(updated, userId);
  }

  /* -------- 댓글 삭제 -------- */
  @Operation(summary = "댓글 삭제")
  @DeleteMapping("/comments/{commentId}")
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public void delete(@PathVariable Long commentId,
                     @RequestHeader("X-User-Id") String userId) {
    commentService.delete(commentId, userId);
  }
}

