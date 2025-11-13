package com.OhRyue.certpilot.versus.repository;

import com.OhRyue.certpilot.versus.domain.MatchAnswer;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface MatchAnswerRepository extends JpaRepository<MatchAnswer, Long> {

  List<MatchAnswer> findByRoomId(Long roomId);

  Optional<MatchAnswer> findByRoomIdAndQuestionIdAndUserId(Long roomId, Long questionId, String userId);

  long countByRoomId(Long roomId);

  long countByRoomIdAndQuestionId(Long roomId, Long questionId);

  long countByRoomIdAndRoundNo(Long roomId, Integer roundNo);

  List<MatchAnswer> findByRoomIdAndRoundNo(Long roomId, Integer roundNo);
}
 