package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.Question;
import org.springframework.data.jpa.repository.JpaRepository;

public interface QuestionRepository extends JpaRepository<Question, Long> {}
