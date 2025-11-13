package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserAnswer;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;

import java.time.Instant;
import java.util.List;

public interface UserAnswerRepository extends JpaRepository<UserAnswer, Long> {

    @Query("""
    select ua from UserAnswer ua
    where ua.userId = :userId
    """)
    List<UserAnswer> findAllByUser(@Param("userId") String userId);

    @Query("""
    select ua from UserAnswer ua
    where ua.userId = :userId
      and ua.createdAt between :from and :to
    """)
    List<UserAnswer> findByUserAndRange(@Param("userId") String userId,
                                        @Param("from") Instant from,
                                        @Param("to") Instant to);

    long countByUserId(String userId);
}
