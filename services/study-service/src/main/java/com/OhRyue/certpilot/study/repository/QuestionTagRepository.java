package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.QuestionTag;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

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
}
