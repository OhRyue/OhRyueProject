package com.OhRyue.certpilot.account.repo;

import com.OhRyue.certpilot.account.domain.SocialAccount;
import com.OhRyue.certpilot.account.domain.SocialProvider;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface SocialAccountRepository extends JpaRepository<SocialAccount, Long> {
    Optional<SocialAccount> findByProviderAndProviderId(SocialProvider provider, String providerId);
    boolean existsByProviderAndProviderId(SocialProvider provider, String providerId);
    boolean existsByUserId(String userId);
}
