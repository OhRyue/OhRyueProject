package com.OhRyue.certpilot.report.repository;

import com.OhRyue.certpilot.report.domain.Question;
import org.springframework.data.jpa.repository.JpaRepository;

public interface QuestionRepository extends JpaRepository<Question, Long> {}
