package com.OhRyue.certpilot.community.repository;

import com.OhRyue.certpilot.community.domain.PostReport;
import com.OhRyue.certpilot.community.domain.ReactionTargetType;
import com.OhRyue.certpilot.community.domain.enums.ReportStatus;
import org.springframework.data.jpa.repository.JpaRepository;

public interface PostReportRepository extends JpaRepository<PostReport, Long> {

  boolean existsByReporterIdAndTargetTypeAndTargetId(String reporterId, ReactionTargetType targetType, Long targetId);

  long countByStatus(ReportStatus status);
}

