package com.OhRyue.certpilot.wrongnote.domain.repo;

import com.OhRyue.certpilot.wrongnote.domain.WrongNote;
import com.OhRyue.certpilot.wrongnote.domain.WrongNoteId;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

/**
 * 복합 PK(User, Question, Tag)로 조회하는 메서드 제공
 */
public interface WrongNoteRepository extends JpaRepository<WrongNote, WrongNoteId> {
    Optional<WrongNote> findByUserIdAndQuestionIdAndTag(Long userId, Long questionId, String tag);

    // 리포트용
    List<WrongNote> findTopByUserIdOrderByLastWrongAtDesc(Long userId);

    int countByUserIdAndTag(Long userId, String tag);

}
