package com.OhRyue.certpilot.community.service;

import com.OhRyue.certpilot.community.client.NotificationClient;
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
import lombok.extern.slf4j.Slf4j;
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

@Slf4j
@Service
@RequiredArgsConstructor
public class CommentService {

    private final CommentRepository commentRepository;
    private final PostRepository postRepository;
    private final ReactionRepository reactionRepository;
    private final NotificationClient notificationClient;
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

        // 알림 발송
        sendCommentNotifications(post, comment, authorId);

        return comment;
    }

    private void sendCommentNotifications(Post post, Comment comment, String commentAuthorId) {
        try {
            String postAuthorId = post.getAuthorId();
            String postTitle = post.getTitle();
            if (postTitle != null && postTitle.length() > 50) {
                postTitle = postTitle.substring(0, 47) + "...";
            }

            log.info("Sending comment notifications: postId={}, postAuthorId={}, commentAuthorId={}", 
                    post.getId(), postAuthorId, commentAuthorId);

            // 1. 게시글 작성자에게 알림 (본인이 댓글 단 경우 제외)
            if (!postAuthorId.equals(commentAuthorId)) {
                log.info("Creating POST_COMMENTED notification for user: {}", postAuthorId);
                try {
                    notificationClient.create(new com.OhRyue.certpilot.community.client.NotificationClient.NotificationCreateRequest(
                            postAuthorId,
                            "POST_COMMENTED",
                            "게시글에 새 댓글이 달렸어요",
                            String.format("'%s' 게시글에 새로운 댓글이 달렸습니다.", postTitle != null ? postTitle : "게시글"),
                            java.util.Map.of(
                                    "postId", post.getId(),
                                    "commentId", comment.getId(),
                                    "actorUserId", commentAuthorId
                            )
                    ));
                    log.info("Successfully created POST_COMMENTED notification for user: {}", postAuthorId);
                } catch (Exception e) {
                    log.error("Failed to create POST_COMMENTED notification for user {}: {}", postAuthorId, e.getMessage(), e);
                }
            }

            // 2. 이전에 댓글을 단 사용자들에게 알림
            List<String> priorCommentUserIds = commentRepository.findDistinctUserIdsByPostId(post.getId());
            log.info("Found {} prior comment users for postId={}", priorCommentUserIds.size(), post.getId());
            
            for (String targetUserId : priorCommentUserIds) {
                // 본인, 게시글 작성자는 제외 (이미 위에서 처리)
                if (targetUserId.equals(commentAuthorId) || targetUserId.equals(postAuthorId)) {
                    continue;
                }

                log.info("Creating COMMENT_REPLIED notification for user: {}", targetUserId);
                try {
                    notificationClient.create(new com.OhRyue.certpilot.community.client.NotificationClient.NotificationCreateRequest(
                            targetUserId,
                            "COMMENT_REPLIED",
                            "댓글이 달린 게시글에 새 댓글이 생겼어요",
                            String.format("'%s' 게시글에 새로운 댓글이 달렸습니다.", postTitle != null ? postTitle : "게시글"),
                            java.util.Map.of(
                                    "postId", post.getId(),
                                    "commentId", comment.getId(),
                                    "actorUserId", commentAuthorId
                            )
                    ));
                    log.info("Successfully created COMMENT_REPLIED notification for user: {}", targetUserId);
                } catch (Exception e) {
                    log.error("Failed to create COMMENT_REPLIED notification for user {}: {}", targetUserId, e.getMessage(), e);
                }
            }
        } catch (Exception e) {
            log.error("Failed to send comment notifications: postId={}, commentId={}, authorId={}",
                    post.getId(), comment.getId(), commentAuthorId, e);
        }
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
