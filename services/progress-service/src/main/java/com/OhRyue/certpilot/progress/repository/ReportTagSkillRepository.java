package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.ReportTagSkill;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface ReportTagSkillRepository extends JpaRepository<ReportTagSkill, Long> {
  List<ReportTagSkill> findByUserIdAndExamModeOrderByTotalDesc(String userId, ExamMode examMode);
  Optional<ReportTagSkill> findByUserIdAndTagAndExamMode(String userId, String tag, ExamMode examMode);
  List<ReportTagSkill> findByUserIdOrderByAccuracyDesc(String userId);
}
