package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserPointLedger;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserPointLedgerRepository extends JpaRepository<UserPointLedger, Long> {}
