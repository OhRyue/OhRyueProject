package com.OhRyue.certpilot.community.repository;

import com.OhRyue.certpilot.community.domain.PostViewLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.Instant;

public interface PostViewLogRepository extends JpaRepository<PostViewLog, Long> {

  @Query("""
      select count(l) from PostViewLog l
      where l.postId = :postId
        and (:userId is null or l.userId = :userId)
        and l.viewedAt >= :since
      """)
  long countRecentViews(@Param("postId") Long postId,
                        @Param("userId") String userId,
                        @Param("since") Instant since);
}


