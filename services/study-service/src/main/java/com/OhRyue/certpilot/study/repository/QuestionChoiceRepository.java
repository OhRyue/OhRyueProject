package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.QuestionChoice;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface QuestionChoiceRepository extends JpaRepository<QuestionChoice, Long> {
  List<QuestionChoice> findByQuestionId(Long questionId);
}
