package com.OhRyue.certpilot.question.domain.repo;

import com.OhRyue.certpilot.question.domain.QuestionTag;
import com.OhRyue.certpilot.question.domain.QuestionTagId;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface QuestionTagRepository extends JpaRepository<QuestionTag, QuestionTagId> {
    @Query("select qt.questionId from QuestionTag qt where qt.tag in :tags")
    List<Long> findQuestionIdsByTags(List<String> tags);
}
