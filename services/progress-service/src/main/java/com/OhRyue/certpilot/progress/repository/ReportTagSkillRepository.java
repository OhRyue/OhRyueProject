package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.ReportTagSkill;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ReportTagSkillRepository extends JpaRepository<ReportTagSkill, Long> {
  List<ReportTagSkill> findTop20ByUserIdAndExamModeOrderByTotalDesc(String userId, String examMode);
}
