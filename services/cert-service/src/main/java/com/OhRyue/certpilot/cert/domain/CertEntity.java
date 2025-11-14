package com.OhRyue.certpilot.cert.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "cert")
@Getter
@NoArgsConstructor
public class CertEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    // 자격증 이름 (예: 정보처리기사)
    @Column(nullable = false, length = 255)
    private String name;

    // 등급/레벨 (예: 기사, 산업기사 등) - NULL 허용
    @Column(length = 100)
    private String level;

    // 발급기관명 (예: 한국산업인력공단)
    @Column(length = 255)
    private String issuer;

    // 발급기관 URL
    @Column(name = "issuer_url", length = 500)
    private String issuerUrl;

    // 자격증 설명 (소개)
    @Lob
    @Column
    private String description;

    // 필기 응시료 (원) - NULL 가능
    @Column(name = "written_fee")
    private Integer writtenFee;

    // 실기 응시료 (원) - NULL 가능
    @Column(name = "practical_fee")
    private Integer practicalFee;

    // 합격 규정 설명 텍스트
    @Lob
    @Column(name = "pass_rule_text")
    private String passRuleText;

    // Q-Net 연동용(선택): 종목코드/구분코드
    @Column(name = "qnet_jm_cd", length = 20)
    private String qnetJmCd;

    @Column(name = "qnet_qualgb_cd", length = 10)
    private String qnetQualgbCd;
}
