// src/main/java/com/OhRyue/certpilot/progress/repository/UserLoadoutRepository.java
package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserLoadout;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserLoadoutRepository extends JpaRepository<UserLoadout, String> {}
