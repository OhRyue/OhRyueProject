package com.OhRyue.certpilot.cert.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

@Entity
@Table(name = "cert")
@Getter
@Setter
public class CertEntity {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(nullable = false, length = 255)
  private String name;

  @Column(length = 100)
  private String level;

  @Column(length = 255)
  private String issuer;

  @Column(name = "issuer_url", length = 500)
  private String issuerUrl;

  @Column(columnDefinition = "TEXT")
  private String description;

  @Column(name = "written_fee")
  private Integer writtenFee;

  @Column(name = "practical_fee")
  private Integer practicalFee;

  @Column(name = "pass_rule_text", columnDefinition = "TEXT")
  private String passRuleText;

  @Column(name = "qnet_jm_cd", length = 20)
  private String qnetJmCd;

  @Column(name = "qnet_qualgb_cd", length = 10)
  private String qnetQualgbCd;
}

