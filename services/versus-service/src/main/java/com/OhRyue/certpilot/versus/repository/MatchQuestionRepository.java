package com.OhRyue.certpilot.versus.repository;

import com.OhRyue.certpilot.versus.domain.MatchQuestion;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface MatchQuestionRepository extends JpaRepository<MatchQuestion, Long> {

  List<MatchQuestion> findByRoomIdOrderByRoundNoAscOrderNoAsc(Long roomId);

  List<MatchQuestion> findByRoomIdAndRoundNo(Long roomId, Integer roundNo);

  long countByRoomId(Long roomId);

  Optional<MatchQuestion> findByRoomIdAndQuestionId(Long roomId, Long questionId);
}
 