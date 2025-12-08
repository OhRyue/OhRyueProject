package com.OhRyue.certpilot.versus.repository;

import com.OhRyue.certpilot.versus.domain.MatchAnswer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface MatchAnswerRepository extends JpaRepository<MatchAnswer, Long> {

    List<MatchAnswer> findByRoomId(Long roomId);

    Optional<MatchAnswer> findByRoomIdAndQuestionIdAndUserId(Long roomId, Long questionId, String userId);

    long countByRoomId(Long roomId);

    long countByRoomIdAndQuestionId(Long roomId, Long questionId);

    long countByRoomIdAndRoundNo(Long roomId, Integer roundNo);

    List<MatchAnswer> findByRoomIdAndRoundNo(Long roomId, Integer roundNo);

    List<MatchAnswer> findByRoomIdAndQuestionId(Long roomId, Long questionId);

    @Query("select count(distinct a.questionId) from MatchAnswer a where a.roomId = :roomId")
    long countDistinctQuestionIdByRoomId(@Param("roomId") Long roomId);

    interface AnswerAggregate {
        String getUserId();
        Long getTotalCount();
        Long getCorrectCount();
        Long getTotalTimeMs();
    }

    @Query("""
        select a.userId as userId,
               count(a) as totalCount,
               sum(case when a.correct = true then 1 else 0 end) as correctCount,
               sum(a.timeMs) as totalTimeMs
        from MatchAnswer a
        where a.roomId = :roomId
        group by a.userId
        """)
    List<AnswerAggregate> aggregateByRoomId(@Param("roomId") Long roomId);
}
 