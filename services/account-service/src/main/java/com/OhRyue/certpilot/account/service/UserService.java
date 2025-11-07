package com.OhRyue.certpilot.account.service;

import com.OhRyue.certpilot.account.domain.AccountStatus;
import com.OhRyue.certpilot.account.domain.UserAccount;
import com.OhRyue.certpilot.account.repo.UserAccountRepository;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Optional;

/**
 * 최신 스키마(user_account) 기반 UserService
 * - 회원가입: PK=id(username), status=BLOCKED 로 저장 → 이메일 인증 시 ACTIVE 전환
 * - 로그인: id(username)로 조회, 상태 ACTIVE 확인, 비밀번호 검증
 *   (시드 {noop} 계정과의 호환을 위해 noop 비교 로직 포함)
 */
@Service
public class UserService {

  private final UserAccountRepository userAccountRepository;
  private final PasswordEncoder passwordEncoder;

  public UserService(UserAccountRepository userAccountRepository,
                     PasswordEncoder passwordEncoder) {
    this.userAccountRepository = userAccountRepository;
    this.passwordEncoder = passwordEncoder;
  }

  // 회원 가입
  public UserAccount register(String username, String rawPassword, String email) {
    // 1) username(id) 중복 검사
    if (userAccountRepository.findById(username).isPresent()) {
      throw new IllegalArgumentException("이미 존재하는 사용자명입니다.");
    }

    // 2) email 중복 검사
    if (userAccountRepository.findByEmail(email).isPresent()) {
      throw new IllegalArgumentException("이미 존재하는 이메일입니다.");
    }

    // 3) 비밀번호 암호화 (BCrypt)
    String encodedPassword = passwordEncoder.encode(rawPassword);

    // 4) 유저 생성 (초기에는 BLOCKED로 저장 → 이메일 인증 후 ACTIVE)
    UserAccount user = UserAccount.builder()
        .id(username)                         // PK = username
        .email(email)
        .passwordHash(encodedPassword)
        .status(AccountStatus.BLOCKED)        // 이메일 인증 전
        .createdAt(LocalDateTime.now())
        .lastLoginAt(null)
        .build();

    return userAccountRepository.save(user);
  }

  // 로그인
  public UserAccount login(String username, String rawPassword) {
    UserAccount user = userAccountRepository.findById(username)
        .orElseThrow(() -> new IllegalArgumentException("존재하지 않는 사용자입니다."));

    if (user.getStatus() != AccountStatus.ACTIVE) {
      throw new IllegalStateException("이메일 인증 후 로그인 가능합니다.");
    }

    // 시드 호환: password_hash 가 {noop}로 시작하면 평문 비교, 아니면 BCrypt 비교
    String hash = user.getPasswordHash();
    boolean matched;
    if (hash != null && hash.startsWith("{noop}")) {
      matched = rawPassword.equals(hash.substring("{noop}".length()));
    } else {
      matched = passwordEncoder.matches(rawPassword, hash);
    }

    if (!matched) {
      throw new IllegalArgumentException("비밀번호가 일치하지 않습니다.");
    }

    // 마지막 로그인 시각 갱신(선택)
    user.setLastLoginAt(LocalDateTime.now());
    userAccountRepository.save(user);

    return user;
  }

  // 사용자 조회(username=id 기반)
  public Optional<UserAccount> findByUsername(String username) {
    return userAccountRepository.findById(username);
  }

  // 이메일 인증 완료 → status = ACTIVE 저장
  public void enableUser(String email) {
    UserAccount user = userAccountRepository.findByEmail(email)
        .orElseThrow(() -> new IllegalArgumentException("해당 이메일의 사용자가 없습니다."));

    user.setStatus(AccountStatus.ACTIVE);   // 인증 완료
    userAccountRepository.save(user);
  }
}
