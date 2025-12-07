package com.OhRyue.certpilot.cert.repository;

import com.OhRyue.certpilot.cert.domain.Topic;
import com.OhRyue.certpilot.cert.domain.enums.ExamMode;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface TopicRepository extends JpaRepository<Topic, Long> {

    List<Topic> findByCertId(Long certId);

    List<Topic> findByParentId(Long parentId);

    List<Topic> findByCertIdAndExamMode(Long certId, ExamMode examMode);

    List<Topic> findByExamMode(ExamMode examMode);
    
    List<Topic> findByExamModeAndParentId(ExamMode examMode, Long parentId);

    List<Topic> findByCodeContaining(String code);

    List<Topic> findByTitleContaining(String title);

    // 루트 노드 조회: code에 소수점이 없고(점이 없음), exam_mode로 필터링
    @Query("SELECT t FROM Topic t WHERE t.examMode = :examMode AND t.code NOT LIKE '%.%' ORDER BY t.orderNo")
    List<Topic> findRootNodesByExamMode(@Param("examMode") ExamMode examMode);
}
