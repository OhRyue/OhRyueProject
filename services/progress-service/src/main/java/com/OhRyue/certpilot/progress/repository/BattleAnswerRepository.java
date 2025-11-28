package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.BattleAnswer;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface BattleAnswerRepository extends JpaRepository<BattleAnswer, Long> {
    List<BattleAnswer> findByBattleRecordId(Long battleRecordId);
}

