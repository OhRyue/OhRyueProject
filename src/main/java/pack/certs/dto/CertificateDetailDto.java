package pack.certs.dto;

import pack.certs.Certificate;

public class CertificateDetailDto {
  private final Long id;
  private final String name;
  private final String field;

  // 향후 fee, subjects 등 확장 필드 추가 예정

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
