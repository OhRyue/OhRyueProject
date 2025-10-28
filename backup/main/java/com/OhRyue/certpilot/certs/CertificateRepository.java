package com.OhRyue.certpilot.certs;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface CertificateRepository extends JpaRepository<Certificate, Long> {

  Page<Certificate> findByFieldIgnoreCase(String field, Pageable pageable);

  Page<Certificate> findByNameContainingIgnoreCase(String name, Pageable pageable);

  List<Certificate> findTop10ByNameContainingIgnoreCase(String name);
}
