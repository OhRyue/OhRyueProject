package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.QuestionTag;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Collection;
import java.util.List;

public interface QuestionTagRepository extends JpaRepository<QuestionTag, Long> {

  // 태그 하나에 속한 questionId들
  @Query("select qt.questionId from QuestionTag qt where qt.tag in :tags")
  List<Long> findQuestionIdsByTags(Collection<String> tags);

  // 단일 태그의 questionId들
  @Query("select qt.questionId from QuestionTag qt where qt.tag = :tag")
  List<Long> findQuestionIdsByTag(String tag);

  // 질문 하나의 태그들
  @Query("select qt.tag from QuestionTag qt where qt.questionId = :qid")
  List<String> findTagsByQuestionId(Long qid);

  // 여러 질문의 태그들을 한 번에 조회 (questionId -> tag 코드 목록)
  @Query("select qt.questionId, qt.tag from QuestionTag qt where qt.questionId in :questionIds")
  List<Object[]> findTagsByQuestionIds(@Param("questionIds") Collection<Long> questionIds);

  // QuestionTag 엔티티로 조회 (questionId 목록)
  List<QuestionTag> findByQuestionIdIn(Collection<Long> questionIds);
}
