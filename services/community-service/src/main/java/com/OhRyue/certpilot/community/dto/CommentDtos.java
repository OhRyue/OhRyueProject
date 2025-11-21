package com.OhRyue.certpilot.community.dto;

import jakarta.validation.constraints.NotBlank;

import java.time.Instant;

public class CommentDtos {

    public record CommentCreateRequest(
            boolean anonymous,
            @NotBlank String content
    ) {}

    public record CommentUpdateRequest(
            boolean anonymous,
            @NotBlank String content
    ) {}

    public record CommentResponse(
            Long id,
            Long postId,
            boolean anonymous,
            String authorId,
            String authorDisplayName,
            String content,
            int likeCount,
            boolean likedByMe,
            Instant createdAt,
            Instant updatedAt
    ) {}
}
