package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.BadgeCatalog;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface BadgeCatalogRepository extends JpaRepository<BadgeCatalog, Long> {

  Optional<BadgeCatalog> findByCode(String code);
}

