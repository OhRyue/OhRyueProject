package com.OhRyue.certpilot.account.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "users") // 테이블명
@Getter
@Setter
@NoArgsConstructor
public class User {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id; // 기본키

    @Column(nullable = false, unique = true)
    private String username; // 사용자 아이디

    @Column(nullable = false)
    private String password; // 비밀번호 (암호화 저장 예정)

    @Column(nullable = false)
    private String role = "USER"; // 권한 (기본값 USER)
}