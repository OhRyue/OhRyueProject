package com.OhRyue.certpilot.question.repo;

import com.OhRyue.certpilot.question.domain.question.Question;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface QuestionRepository extends JpaRepository<Question, Long> {

  @Query(value = """
        SELECT q.* FROM question q
        JOIN question_topic qt ON qt.question_id = q.id
        WHERE qt.topic_id IN (:topicIds)
        ORDER BY q.id DESC
        LIMIT :limit
        """, nativeQuery = true)
  List<Question> findLatestByTopicIds(@Param("topicIds") List<Long> topicIds,
                                      @Param("limit") int limit);
}
