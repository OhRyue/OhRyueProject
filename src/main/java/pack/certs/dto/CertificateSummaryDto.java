package pack.certs.dto;

import pack.certs.Certificate;

public class CertificateSummaryDto {
  private final Long id;
  private final String name;
  private final String field;

  public CertificateSummaryDto(Long id, String name, String field) {
    this.id = id;
    this.name = name;
    this.field = field;
  }

  public static CertificateSummaryDto from(Certificate c) {
    return new CertificateSummaryDto(c.getId(), c.getName(), c.getField());
  }

  public Long getId() { return id; }
  public String getName() { return name; }
  public String getField() { return field; }
}
