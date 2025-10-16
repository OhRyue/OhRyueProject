package com.OhRyue.certpilot.question.domain.repo;

import com.OhRyue.certpilot.question.domain.Question;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface QuestionRepository extends JpaRepository<Question, Long> {
    List<Question> findTop50ByDifficultyBetweenOrderByIdDesc(int min, int max);
}
