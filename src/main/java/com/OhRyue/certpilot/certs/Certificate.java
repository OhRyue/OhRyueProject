package com.OhRyue.certpilot.certs;

import jakarta.persistence.*;

@Entity
@Table(name = "certificate")
public class Certificate {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  // 자격증명(필수)
  @Column(nullable = false, length = 100)
  private String name;

  // 분야(IT, 회계 등) - 선택
  @Column(length = 50)
  private String field;

  protected Certificate() { }                   // JPA 기본 생성자

  public Certificate(String name, String field) {
    this.name = name;
    this.field = field;
  }

  public Long getId() { return id; }
  public String getName() { return name; }
  public String getField() { return field; }

  public void setName(String name) { this.name = name; }
  public void setField(String field) { this.field = field; }
}
