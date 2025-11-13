package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.UserAnswer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.Instant;
import java.util.List;

public interface UserAnswerRepository extends JpaRepository<UserAnswer, Long> {

  List<UserAnswer> findByUserId(String userId);

  List<UserAnswer> findByUserIdAndAnsweredAtAfter(String userId, Instant answeredAt);

  List<UserAnswer> findByUserIdAndSessionId(String userId, Long sessionId);

  /* 태그별 성취: tag, correct, total */
  @Query("""
      select t.tag as tag,
             sum(case when ua.correct=true then 1 else 0 end) as correct,
             count(ua) as total
      from UserAnswer ua
      join QuestionTag t on t.questionId = ua.questionId
      where ua.userId = :userId
      group by t.tag
      order by (1.0 - (sum(case when ua.correct=true then 1 else 0 end)*1.0 / count(ua))) desc
      """)
  List<Object[]> aggregatePerTag(String userId);

  /* 특정 태그들에서 내가 틀린 문제들(재학습용) */
  @Query("""
      select ua.questionId from UserAnswer ua
      join QuestionTag t on t.questionId = ua.questionId
      where ua.userId = :userId and t.tag in :tags and ua.correct = false
      """)
  List<Long> findWrongQuestionIdsByTags(String userId, List<String> tags);
}
