package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.Topic;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface TopicRepository extends JpaRepository<Topic, Long> {
  List<Topic> findByParentId(Long parentId);
  List<Topic> findByExamMode(ExamMode examMode);
}
