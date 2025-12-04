package com.OhRyue.certpilot.community.service;

import com.OhRyue.certpilot.community.client.NotificationClient;
import com.OhRyue.certpilot.community.domain.Comment;
import com.OhRyue.certpilot.community.domain.Post;
import com.OhRyue.certpilot.community.domain.Reaction;
import com.OhRyue.certpilot.community.domain.ReactionTargetType;
import com.OhRyue.certpilot.community.dto.ReactionDtos;
import com.OhRyue.certpilot.community.exception.BadRequestException;
import com.OhRyue.certpilot.community.exception.ResourceNotFoundException;
import com.OhRyue.certpilot.community.repository.CommentRepository;
import com.OhRyue.certpilot.community.repository.PostRepository;
import com.OhRyue.certpilot.community.repository.ReactionRepository;
import io.micrometer.core.annotation.Timed;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Clock;
import java.time.Instant;
import java.util.Map;

@Slf4j
@Service
@RequiredArgsConstructor
public class ReactionService {

    private final ReactionRepository reactionRepository;
    private final PostRepository postRepository;
    private final CommentRepository commentRepository;
    private final NotificationClient notificationClient;
    private final Clock clock = Clock.systemUTC();

    @Timed(value = "community.reactions.toggle", histogram = true)
    @Transactional
    public ReactionDtos.ToggleResponse toggle(String userId, ReactionDtos.ToggleRequest request) {
        if (userId == null || userId.isBlank()) {
            throw new BadRequestException("userId는 필수입니다.");
        }

        ReactionTargetType type = request.targetType();
        Long targetId = request.targetId();

        Reaction existing = reactionRepository.findByTargetTypeAndTargetIdAndUserId(
                type, targetId, userId).orElse(null);

        boolean toggledOn;
        long likeCount;
        if (existing != null) {
            reactionRepository.delete(existing);
            adjustLikeCount(type, targetId, -1);
            toggledOn = false;
        } else {
            Reaction reaction = new Reaction();
            reaction.setTargetType(type);
            reaction.setTargetId(targetId);
            reaction.setUserId(userId);
            reaction.setCreatedAt(Instant.now(clock));
            reactionRepository.save(reaction);
            adjustLikeCount(type, targetId, +1);
            toggledOn = true;

            // 좋아요가 추가된 경우 알림 발송 (게시글에만)
            if (type == ReactionTargetType.POST) {
                sendPostLikedNotification(userId, targetId);
            }
        }
        likeCount = reactionRepository.countByTargetTypeAndTargetId(type, targetId);
        return new ReactionDtos.ToggleResponse(type, targetId, toggledOn, likeCount);
    }

    private void sendPostLikedNotification(String likeUserId, Long postId) {
        try {
            Post post = postRepository.findByIdAndDeletedAtIsNull(postId)
                    .orElse(null);
            if (post == null) {
                log.warn("Post not found for notification: postId={}", postId);
                return;
            }

            String postAuthorId = post.getAuthorId();
            // 본인 게시글에 좋아요를 누른 경우 알림 발송하지 않음
            if (postAuthorId.equals(likeUserId)) {
                log.debug("Skipping notification: user liked their own post. postId={}, userId={}", postId, likeUserId);
                return;
            }

            String postTitle = post.getTitle();
            if (postTitle != null && postTitle.length() > 50) {
                postTitle = postTitle.substring(0, 47) + "...";
            }

            log.info("Creating POST_LIKED notification: postId={}, postAuthorId={}, likeUserId={}", 
                    postId, postAuthorId, likeUserId);
            
            notificationClient.create(new NotificationClient.NotificationCreateRequest(
                    postAuthorId,
                    "POST_LIKED",
                    "게시글에 좋아요가 달렸어요",
                    String.format("'%s' 게시글에 새로운 좋아요가 달렸습니다.", postTitle != null ? postTitle : "게시글"),
                    Map.of(
                            "postId", postId,
                            "actorUserId", likeUserId
                    )
            ));
            
            log.info("Successfully created POST_LIKED notification for user: {}", postAuthorId);
        } catch (Exception e) {
            log.error("Failed to send post liked notification: postId={}, likeUserId={}, error={}", 
                    postId, likeUserId, e.getMessage(), e);
        }
    }

    private void adjustLikeCount(ReactionTargetType type, Long targetId, int delta) {
        if (type == ReactionTargetType.POST) {
            Post post = postRepository.findByIdAndDeletedAtIsNull(targetId)
                    .orElseThrow(() -> new ResourceNotFoundException("게시글을 찾을 수 없습니다."));
            post.setLikeCount(Math.max(0, post.getLikeCount() + delta));
        } else if (type == ReactionTargetType.COMMENT) {
            Comment comment = commentRepository.findByIdAndDeletedAtIsNull(targetId)
                    .orElseThrow(() -> new ResourceNotFoundException("댓글을 찾을 수 없습니다."));
            comment.setLikeCount(Math.max(0, comment.getLikeCount() + delta));
        } else {
            throw new BadRequestException("지원하지 않는 대상 타입입니다.");
        }
    }
}
