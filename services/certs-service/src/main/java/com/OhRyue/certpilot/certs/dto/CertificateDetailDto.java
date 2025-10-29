package com.OhRyue.certpilot.certs.dto;

import com.OhRyue.certpilot.certs.entity.Certificate;

public class CertificateDetailDto {
  private final Long id;
  private final String name;
  private final String field;

  public CertificateDetailDto(Long id, String name, String field) {
    this.id = id;
    this.name = name;
    this.field = field;
  }

  public static CertificateDetailDto from(Certificate c) {
    return new CertificateDetailDto(c.getId(), c.getName(), c.getField());
  }

  public Long getId() { return id; }
  public String getName() { return name; }
  public String getField() { return field; }
}

