package com.OhRyue.certpilot.account.repo;

import com.OhRyue.certpilot.account.domain.UserAccount;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface UserAccountRepository extends JpaRepository<UserAccount, String> {
  Optional<UserAccount> findByEmail(String email);
}
