package com.OhRyue.certpilot.community.service;

import com.OhRyue.certpilot.community.domain.PostReport;
import com.OhRyue.certpilot.community.domain.ReactionTargetType;
import com.OhRyue.certpilot.community.domain.UserBlock;
import com.OhRyue.certpilot.community.domain.enums.ReportStatus;
import com.OhRyue.certpilot.community.dto.ModerationDtos;
import com.OhRyue.certpilot.community.exception.BadRequestException;
import com.OhRyue.certpilot.community.exception.ResourceNotFoundException;
import com.OhRyue.certpilot.community.repository.CommentRepository;
import com.OhRyue.certpilot.community.repository.PostReportRepository;
import com.OhRyue.certpilot.community.repository.PostRepository;
import com.OhRyue.certpilot.community.repository.UserBlockRepository;
import io.micrometer.core.annotation.Timed;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ModerationService {

  private final PostRepository postRepository;
  private final CommentRepository commentRepository;
  private final PostReportRepository postReportRepository;
  private final UserBlockRepository userBlockRepository;

  @Timed(value = "community.moderation.report", histogram = true)
  @Transactional
  public ModerationDtos.ReportResponse report(String reporterId, ModerationDtos.ReportRequest request) {
    if (reporterId == null || reporterId.isBlank()) {
      throw new BadRequestException("신고자 정보가 필요합니다.");
    }

    ReactionTargetType type = request.targetType();
    Long targetId = request.targetId();
    validateTargetExists(type, targetId);

    if (postReportRepository.existsByReporterIdAndTargetTypeAndTargetId(reporterId, type, targetId)) {
      throw new BadRequestException("이미 신고한 대상입니다.");
    }

    PostReport report = new PostReport();
    report.setReporterId(reporterId);
    report.setTargetType(type);
    report.setTargetId(targetId);
    report.setReason(request.reason());
    report.setStatus(ReportStatus.PENDING);
    report.setCreatedAt(Instant.now());
    PostReport saved = postReportRepository.save(report);

    return new ModerationDtos.ReportResponse(
        saved.getId(),
        saved.getTargetType(),
        saved.getTargetId(),
        saved.getStatus().name(),
        saved.getCreatedAt()
    );
  }

  @Timed(value = "community.moderation.block", histogram = true)
  @Transactional
  public void block(String userId, String blockedUserId) {
    if (userId.equals(blockedUserId)) {
      throw new BadRequestException("자기 자신을 차단할 수 없습니다.");
    }
    if (userBlockRepository.existsByUserIdAndBlockedUserId(userId, blockedUserId)) {
      return;
    }
    UserBlock block = new UserBlock();
    block.setUserId(userId);
    block.setBlockedUserId(blockedUserId);
    userBlockRepository.save(block);
  }

  @Timed(value = "community.moderation.unblock", histogram = true)
  @Transactional
  public void unblock(String userId, String blockedUserId) {
    userBlockRepository.deleteByUserIdAndBlockedUserId(userId, blockedUserId);
  }

  @Transactional(readOnly = true)
  public ModerationDtos.BlockListResponse listBlocks(String userId) {
    List<ModerationDtos.BlockEntry> entries = userBlockRepository.findByUserId(userId).stream()
        .map(block -> new ModerationDtos.BlockEntry(block.getBlockedUserId(), block.getCreatedAt()))
        .toList();
    return new ModerationDtos.BlockListResponse(entries);
  }

  @Transactional(readOnly = true)
  public Set<String> blockedUserIds(String userId) {
    return userBlockRepository.findByUserId(userId).stream()
        .map(UserBlock::getBlockedUserId)
        .collect(Collectors.toSet());
  }

  private void validateTargetExists(ReactionTargetType type, Long targetId) {
    if (type == ReactionTargetType.POST) {
      postRepository.findByIdAndDeletedAtIsNull(targetId)
          .orElseThrow(() -> new ResourceNotFoundException("게시글을 찾을 수 없습니다."));
    } else if (type == ReactionTargetType.COMMENT) {
      commentRepository.findByIdAndDeletedAtIsNull(targetId)
          .orElseThrow(() -> new ResourceNotFoundException("댓글을 찾을 수 없습니다."));
    } else {
      throw new BadRequestException("지원하지 않는 신고 대상입니다.");
    }
  }
}

