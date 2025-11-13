package com.OhRyue.certpilot.community.repository;

import com.OhRyue.certpilot.community.domain.Reaction;
import com.OhRyue.certpilot.community.domain.ReactionTargetType;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface ReactionRepository extends JpaRepository<Reaction, Long> {

  Optional<Reaction> findByTargetTypeAndTargetIdAndUserId(ReactionTargetType targetType,
                                                          Long targetId,
                                                          String userId);

  long countByTargetTypeAndTargetId(ReactionTargetType targetType, Long targetId);
}


