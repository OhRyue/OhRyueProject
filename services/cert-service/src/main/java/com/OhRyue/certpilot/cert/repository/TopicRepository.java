package com.OhRyue.certpilot.cert.repository;

import com.OhRyue.certpilot.cert.domain.Topic;
import com.OhRyue.certpilot.cert.domain.enums.ExamMode;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface TopicRepository extends JpaRepository<Topic, Long> {

    List<Topic> findByCertId(Long certId);

    List<Topic> findByParentId(Long parentId);

    List<Topic> findByCertIdAndExamMode(Long certId, ExamMode examMode);

    List<Topic> findByExamMode(ExamMode examMode);
    
    List<Topic> findByExamModeAndParentId(ExamMode examMode, Long parentId);

    List<Topic> findByCodeContaining(String code);

    List<Topic> findByTitleContaining(String title);
}
