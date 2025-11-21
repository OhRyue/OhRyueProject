package com.OhRyue.certpilot.community.service;

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
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Clock;
import java.time.Instant;

@Service
@RequiredArgsConstructor
public class ReactionService {

    private final ReactionRepository reactionRepository;
    private final PostRepository postRepository;
    private final CommentRepository commentRepository;
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
        }
        likeCount = reactionRepository.countByTargetTypeAndTargetId(type, targetId);
        return new ReactionDtos.ToggleResponse(type, targetId, toggledOn, likeCount);
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
