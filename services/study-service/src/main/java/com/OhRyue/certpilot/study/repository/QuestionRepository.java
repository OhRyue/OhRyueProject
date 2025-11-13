package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Collection;
import java.util.List;

public interface QuestionRepository extends JpaRepository<Question, Long> {

  List<Question> findByIdIn(Collection<Long> ids);

  List<Question> findByTopicIdAndModeAndType(Long topicId, ExamMode mode, QuestionType type);

  List<Question> findByTopicIdInAndModeAndType(Collection<Long> topicIds, ExamMode mode, QuestionType type);

  List<Question> findByModeAndTypeAndDifficulty(ExamMode mode, QuestionType type, Difficulty difficulty);

  @Query("""
      SELECT q FROM Question q
      WHERE q.topicId = :topicId
        AND q.mode = :mode
        AND q.type = :type
      ORDER BY function('RAND')
      """)
  List<Question> pickRandomByTopic(
      @Param("topicId") Long topicId,
      @Param("mode") ExamMode mode,
      @Param("type") QuestionType type,
      Pageable pageable
  );

  @Query("""
      SELECT q FROM Question q
      WHERE q.topicId IN :topicIds
        AND q.mode = :mode
        AND q.type = :type
      ORDER BY function('RAND')
      """)
  List<Question> pickRandomByTopicIn(
      @Param("topicIds") Collection<Long> topicIds,
      @Param("mode") ExamMode mode,
      @Param("type") QuestionType type,
      Pageable pageable
  );
}