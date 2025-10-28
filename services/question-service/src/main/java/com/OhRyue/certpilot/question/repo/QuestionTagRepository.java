package com.OhRyue.certpilot.question.repo;

import com.OhRyue.certpilot.question.domain.question.QuestionTag;
import com.OhRyue.certpilot.question.domain.question.QuestionTagId;
import org.springframework.data.jpa.repository.JpaRepository;

public interface QuestionTagRepository extends JpaRepository<QuestionTag, QuestionTagId> {}
