package com.OhRyue.certpilot.community.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import java.time.Instant;
import java.util.List;

public class PostDtos {

    public enum SortType {
        LATEST,
        HOT
    }

    public record PostQuery(
            String categoryCode,
            String keyword,
            SortType sort,
            Integer days,
            Boolean anonymousOnly,
            Boolean mine
    ) {}

    public record PostCreateRequest(
            @NotBlank String categoryCode,
            @NotBlank @Size(max = 200) String title,
            @NotBlank String content,
            boolean anonymous
    ) {}

    public record PostUpdateRequest(
            @NotBlank @Size(max = 200) String title,
            @NotBlank String content,
            boolean anonymous
    ) {}

    public record PostSummary(
            Long id,
            String categoryCode,
            String categoryName,
            String title,
            String excerpt,
            boolean anonymous,
            String authorId,
            String authorDisplayName,
            int likeCount,
            int commentCount,
            int viewCount,
            Instant createdAt
    ) {}

    public record PostListResponse(
            PageDto page,
            List<PostSummary> items
    ) {}

    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record PostDetailResponse(
            PostSummary post,
            String content,
            List<CommentDtos.CommentResponse> comments,
            boolean likedByMe,
            boolean canEdit,
            Instant updatedAt
    ) {}

    public record HotPostResponse(
            Instant from,
            List<PostSummary> items
    ) {}

    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record MyActivityResponse(
            List<PostSummary> myPosts,
            List<CommentDtos.CommentResponse> myComments
    ) {}

    @Schema(description = "게시글 메트릭")
    public record PostMetrics(
            Long postId,
            int likeCount,
            int commentCount,
            int viewCount
    ) {}
}
