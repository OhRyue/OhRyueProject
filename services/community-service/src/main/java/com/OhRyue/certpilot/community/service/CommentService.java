package com.OhRyue.certpilot.community.service;

import com.OhRyue.certpilot.community.domain.Comment;
import com.OhRyue.certpilot.community.domain.Post;
import com.OhRyue.certpilot.community.domain.ReactionTargetType;
import com.OhRyue.certpilot.community.dto.CommentDtos;
import com.OhRyue.certpilot.community.exception.ForbiddenException;
import com.OhRyue.certpilot.community.exception.ResourceNotFoundException;
import com.OhRyue.certpilot.community.repository.CommentRepository;
import com.OhRyue.certpilot.community.repository.PostRepository;
import com.OhRyue.certpilot.community.repository.ReactionRepository;
import io.micrometer.core.annotation.Timed;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Clock;
import java.time.Instant;
import java.util.List;
import java.util.Set;

@Service
@RequiredArgsConstructor
public class CommentService {

    private final CommentRepository commentRepository;
    private final PostRepository postRepository;
    private final ReactionRepository reactionRepository;
    private final Clock clock = Clock.systemUTC();

    @Timed(value = "community.comments.create", histogram = true)
    @Transactional
    public Comment create(Long postId, String authorId, CommentDtos.CommentCreateRequest request) {
        Post post = postRepository.findByIdAndDeletedAtIsNull(postId)
                .orElseThrow(() -> new ResourceNotFoundException("게시글을 찾을 수 없습니다."));

        Comment comment = new Comment();
        comment.setPostId(post.getId());
        comment.setAuthorId(authorId);
        comment.setAnonymous(request.anonymous());
        comment.setContent(request.content());
        comment.setCreatedAt(Instant.now(clock));
        commentRepository.save(comment);

        post.setCommentCount(post.getCommentCount() + 1);
        return comment;
    }

    @Timed(value = "community.comments.list", histogram = true)
    @Transactional(readOnly = true)
    public org.springframework.data.domain.Page<CommentDtos.CommentResponse> list(Long postId,
                                                                                  String viewerId,
                                                                                  int page,
                                                                                  int size,
                                                                                  Set<String> blockedUserIds) {
        Pageable pageable = PageRequest.of(
                page, size, Sort.by(Sort.Direction.ASC, "createdAt"));
        var pageData = commentRepository
                .findByPostIdAndDeletedAtIsNullOrderByCreatedAtAsc(postId, pageable)
                .map(comment -> toResponse(comment, viewerId));

        if (blockedUserIds == null || blockedUserIds.isEmpty()) {
            return pageData;
        }

        List<CommentDtos.CommentResponse> filtered = pageData.getContent().stream()
                .filter(comment -> comment == null
                        || comment.authorId() == null
                        || !blockedUserIds.contains(comment.authorId()))
                .toList();

        return new PageImpl<>(filtered, pageable, filtered.size());
    }

    public CommentDtos.CommentResponse toResponse(Comment comment, String viewerId) {
        boolean liked = viewerId != null &&
                reactionRepository.findByTargetTypeAndTargetIdAndUserId(
                        ReactionTargetType.COMMENT, comment.getId(), viewerId
                ).isPresent();

        return new CommentDtos.CommentResponse(
                comment.getId(),
                comment.getPostId(),
                comment.isAnonymous(),
                comment.getAuthorId(),
                comment.isAnonymous() ? "익명" : comment.getAuthorId(),
                comment.getContent(),
                comment.getLikeCount(),
                liked,
                comment.getCreatedAt(),
                comment.getUpdatedAt()
        );
    }

    @Timed(value = "community.comments.recent-by-user", histogram = true)
    @Transactional(readOnly = true)
    public List<CommentDtos.CommentResponse> recentByUser(String userId, int limit) {
        int normalizedLimit = Math.max(1, Math.min(limit, 50));
        Pageable pageable = PageRequest.of(0, normalizedLimit, Sort.by(Sort.Direction.DESC, "createdAt"));
        return commentRepository.findByAuthorIdAndDeletedAtIsNull(userId, pageable).stream()
                .map(comment -> toResponse(comment, userId))
                .toList();
    }

    @Timed(value = "community.comments.update", histogram = true)
    @Transactional
    public Comment update(Long id, String requesterId, CommentDtos.CommentUpdateRequest request) {
        Comment comment = commentRepository.findByIdAndDeletedAtIsNull(id)
                .orElseThrow(() -> new ResourceNotFoundException("댓글을 찾을 수 없습니다."));
        validateAuthor(comment.getAuthorId(), requesterId);
        comment.setAnonymous(request.anonymous());
        comment.setContent(request.content());
        comment.setUpdatedAt(Instant.now(clock));
        return comment;
    }

    @Timed(value = "community.comments.delete", histogram = true)
    @Transactional
    public void delete(Long id, String requesterId) {
        Comment comment = commentRepository.findByIdAndDeletedAtIsNull(id)
                .orElseThrow(() -> new ResourceNotFoundException("댓글을 찾을 수 없습니다."));
        validateAuthor(comment.getAuthorId(), requesterId);
        if (comment.getDeletedAt() == null) {
            comment.setDeletedAt(Instant.now(clock));
            Post post = postRepository.findById(comment.getPostId())
                    .orElseThrow(() -> new ResourceNotFoundException("게시글을 찾을 수 없습니다."));
            int newCount = Math.max(0, post.getCommentCount() - 1);
            post.setCommentCount(newCount);
        }
    }

    private void validateAuthor(String authorId, String requesterId) {
        if (requesterId == null || !requesterId.equals(authorId)) {
            throw new ForbiddenException("작성자만 수정/삭제할 수 있습니다.");
        }
    }
}
