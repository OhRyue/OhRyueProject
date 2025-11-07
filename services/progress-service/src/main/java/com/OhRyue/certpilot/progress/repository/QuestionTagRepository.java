package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.QuestionTag;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface QuestionTagRepository extends JpaRepository<QuestionTag, Long> {

    /**
     * 태그별 집계 (WRITTEN: OX/MCQ, PRACTICAL: SHORT/LONG)
     * question_tag.tag 를 직접 사용한다고 가정
     */
    @Query(value = """
    SELECT qt.tag as tag,
           SUM(CASE WHEN ua.correct = 1 THEN 1 ELSE 0 END) as correct_cnt,
           COUNT(*) as total_cnt
      FROM user_answer ua
      JOIN question_tag qt ON qt.question_id = ua.question_id
      JOIN question q ON q.id = ua.question_id
     WHERE ua.user_id = :userId
       AND ( (:mode = 'WRITTEN' AND q.type IN ('OX','MCQ'))
          OR (:mode = 'PRACTICAL' AND q.type IN ('SHORT','LONG')) )
     GROUP BY qt.tag
     ORDER BY total_cnt DESC
     LIMIT :limit
  """, nativeQuery = true)
    List<Object[]> aggregateByTag(
            @Param("userId") String userId,
            @Param("mode") String mode,
            @Param("limit") int limit
    );
}
