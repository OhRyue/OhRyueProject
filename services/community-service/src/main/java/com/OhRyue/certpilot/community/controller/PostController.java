package com.OhRyue.certpilot.community.controller;

import com.OhRyue.certpilot.community.dto.PostDtos;
import com.OhRyue.certpilot.community.service.PostService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Community - Posts", description = "커뮤니티 게시글 APIs")
@RestController
@RequestMapping("/api/community/posts")
@RequiredArgsConstructor
public class PostController {

  private final PostService postService;

  /* -------- 게시글 목록 -------- */
  @Operation(summary = "게시글 목록 조회")
  @GetMapping
  public PostDtos.PostListResponse list(@RequestParam(value = "category", required = false) String categoryCode,
                                        @RequestParam(value = "keyword", required = false) String keyword,
                                        @RequestParam(value = "sort", defaultValue = "LATEST") PostDtos.SortType sort,
                                        @RequestParam(value = "page", defaultValue = "0") int page,
                                        @RequestParam(value = "size", defaultValue = "20") int size,
                                        @RequestParam(value = "days", required = false) Integer days,
                                        @RequestParam(value = "today", defaultValue = "false") boolean todayOnly,
                                        @RequestParam(value = "anonymousOnly", defaultValue = "false") boolean anonymousOnly,
                                        @RequestParam(value = "mine", defaultValue = "false") boolean mine,
                                        @RequestHeader(value = "X-User-Id", required = false) String userId) {
    Integer effectiveDays = days != null ? days : (todayOnly ? 1 : null);
    PostDtos.PostQuery query = new PostDtos.PostQuery(
        categoryCode,
        keyword,
        sort,
        effectiveDays,
        anonymousOnly,
        mine
    );
    return postService.list(query, page, size, userId);
  }

  /* -------- 게시글 상세 -------- */
  @Operation(summary = "게시글 상세 조회")
  @GetMapping("/{postId}")
  public PostDtos.PostDetailResponse detail(@PathVariable Long postId,
                                            @RequestHeader(value = "X-User-Id", required = false) String userId,
                                            @RequestParam(value = "commentSize", defaultValue = "100") int commentSize) {
    return postService.getDetail(postId, userId, commentSize);
  }

  /* -------- 게시글 작성 -------- */
  @Operation(summary = "게시글 작성")
  @PostMapping
  public ResponseEntity<PostDtos.PostSummary> create(@RequestHeader("X-User-Id") String userId,
                                                     @Valid @RequestBody PostDtos.PostCreateRequest request) {
    PostDtos.PostCreateRequest enriched = new PostDtos.PostCreateRequest(
        request.categoryCode(),
        request.title(),
        request.content(),
        userId,
        request.anonymous()
    );
    var created = postService.create(enriched);
    return ResponseEntity.status(HttpStatus.CREATED).body(postService.summarize(created));
  }

  /* -------- 게시글 수정 -------- */
  @Operation(summary = "게시글 수정")
  @PutMapping("/{postId}")
  public PostDtos.PostSummary update(@PathVariable Long postId,
                                     @RequestHeader("X-User-Id") String userId,
                                     @Valid @RequestBody PostDtos.PostUpdateRequest request) {
    var updated = postService.update(postId, userId, request);
    return postService.summarize(updated);
  }

  /* -------- 게시글 삭제 -------- */
  @Operation(summary = "게시글 삭제")
  @DeleteMapping("/{postId}")
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public void delete(@PathVariable Long postId,
                     @RequestHeader("X-User-Id") String userId) {
    postService.delete(postId, userId);
  }

  /* -------- HOT 게시글 -------- */
  @Operation(summary = "인기 게시글 조회")
  @GetMapping("/hot")
  public PostDtos.HotPostResponse hot(@RequestParam(value = "days", defaultValue = "3") int days,
                                      @RequestParam(value = "limit", defaultValue = "5") int limit,
                                      @RequestHeader(value = "X-User-Id", required = false) String userId) {
    return postService.hotPosts(days, limit, userId);
  }

  /* -------- 오늘의 게시글 -------- */
  @Operation(summary = "오늘 등록된 게시글 조회", description = "오늘 작성된 게시글을 정렬 기준에 맞게 반환합니다.")
  @GetMapping("/today")
  public PostDtos.PostListResponse today(@RequestParam(value = "sort", defaultValue = "LATEST") PostDtos.SortType sort,
                                         @RequestParam(value = "limit", defaultValue = "10") int limit,
                                         @RequestHeader(value = "X-User-Id", required = false) String userId) {
    return postService.today(sort, limit, userId);
  }

  /* -------- 내 활동 -------- */
  @Operation(summary = "내 커뮤니티 활동 요약")
  @GetMapping("/my/activity")
  public PostDtos.MyActivityResponse myActivity(@RequestHeader("X-User-Id") String userId,
                                                @RequestParam(value = "postLimit", defaultValue = "5") int postLimit,
                                                @RequestParam(value = "commentLimit", defaultValue = "5") int commentLimit) {
    return postService.myActivity(userId, postLimit, commentLimit);
  }

  /* -------- 게시글 메트릭 -------- */
  @Operation(summary = "게시글 메트릭 조회", description = "좋아요/댓글/조회수 카운터 제공")
  @GetMapping("/{postId}/metrics")
  public PostDtos.PostMetrics metrics(@PathVariable Long postId) {
    return postService.metrics(postId);
  }
}

