package com.OhRyue.certpilot.account.repo;

import com.OhRyue.certpilot.account.domain.UserSettings;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserSettingsRepository extends JpaRepository<UserSettings, String> {
}
