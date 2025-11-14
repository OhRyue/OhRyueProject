// src/main/java/com/OhRyue/certpilot/study/repository/QuestionChoiceRepository.java
package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.QuestionChoice;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface QuestionChoiceRepository extends JpaRepository<QuestionChoice, Long> {

    // 기존 메서드가 다른 곳에서 쓰고 있으면 남겨둬도 됩니다.
    List<QuestionChoice> findByQuestionId(Long questionId);

    // MCQ 세트에서 보기 A/B/C/D 정렬해서 가져오기
    List<QuestionChoice> findByQuestionIdOrderByLabelAsc(Long questionId);

    // 정답 보기 1개 찾기
    Optional<QuestionChoice> findFirstByQuestionIdAndCorrectTrue(Long questionId);
}
