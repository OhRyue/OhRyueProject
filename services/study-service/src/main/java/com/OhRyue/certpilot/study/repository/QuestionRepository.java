package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;
import java.util.List;

public interface QuestionRepository extends JpaRepository<Question, Long> {

  List<Question> findByIdIn(Collection<Long> ids);

  List<Question> findByIdInAndDifficultyInAndTypeIn(
      Collection<Long> ids,
      Collection<Difficulty> difficulties,
      Collection<QuestionType> types
  );

  /* === 미니체크/리뷰용 === */
  List<Question> findTop20ByTopicIdAndTypeOrderByIdAsc(Long topicId, QuestionType type);
  Page<Question> findByTopicIdAndType(Long topicId, QuestionType type, Pageable pageable);
  List<Question> findByTopicIdAndTypeOrderByIdAsc(Long topicId, QuestionType type);

  /* === 카테고리/난이도 퀴즈용 (이번 에러 해결 포인트) === */
  // 주어진 여러 topicId에서 특정 타입 조회
  List<Question> findByTopicIdInAndType(Collection<Long> topicIds, QuestionType type);

  // 특정 난이도 + 타입 조회
  List<Question> findByDifficultyAndType(Difficulty difficulty, QuestionType type);

  List<Question> findByTopicIdInAndDifficultyInAndType(
      Collection<Long> topicIds,
      Collection<Difficulty> difficulties,
      QuestionType type
  );
}
