package com.OhRyue.certpilot.account.service;

import com.OhRyue.certpilot.account.domain.AccountStatus;
import com.OhRyue.certpilot.account.domain.UserAccount;
import com.OhRyue.certpilot.account.repo.UserAccountRepository;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Locale;
import java.util.Optional;

@Service
public class UserService {

    private final UserAccountRepository userAccountRepository;
    private final PasswordEncoder passwordEncoder;

    public UserService(UserAccountRepository userAccountRepository,
                       PasswordEncoder passwordEncoder) {
        this.userAccountRepository = userAccountRepository;
        this.passwordEncoder = passwordEncoder;
    }

    /**
     * 이메일 인증이 끝난 시점에서 실제 가입 처리
     * - userId = PK
     * - status = ACTIVE 로 바로 저장
     */
    public UserAccount register(String userId, String email, String rawPassword) {
        String normalizedUserId = userId.trim();
        String normalizedEmail = normalizeEmail(email);

        if (userAccountRepository.findById(normalizedUserId).isPresent()) {
            throw new IllegalArgumentException("이미 존재하는 아이디입니다.");
        }
        if (userAccountRepository.findByEmail(normalizedEmail).isPresent()) {
            throw new IllegalArgumentException("이미 등록된 이메일입니다.");
        }

        String encodedPassword = passwordEncoder.encode(rawPassword);

        UserAccount user = UserAccount.builder()
                .id(normalizedUserId)
                .email(normalizedEmail)
                .passwordHash(encodedPassword)
                .status(AccountStatus.ACTIVE) // 이메일 인증 후 생성되므로 ACTIVE
                .createdAt(LocalDateTime.now())
                .lastLoginAt(null)
                .build();

        return userAccountRepository.save(user);
    }

    /**
     * 로그인: userId + password
     */
    public UserAccount login(String userId, String rawPassword) {
        String normalizedUserId = userId.trim();

        UserAccount user = userAccountRepository.findById(normalizedUserId)
                .orElseThrow(() -> new IllegalArgumentException("존재하지 않는 사용자입니다."));

        if (!user.isActive()) {
            throw new IllegalStateException("이메일 인증 후 로그인 가능합니다.");
        }

        String hash = user.getPasswordHash();
        boolean matched;
        if (hash != null && hash.startsWith("{noop}")) {
            // 시드 계정 호환용
            matched = rawPassword.equals(hash.substring("{noop}".length()));
        } else {
            matched = passwordEncoder.matches(rawPassword, hash);
        }

        if (!matched) {
            throw new IllegalArgumentException("비밀번호가 일치하지 않습니다.");
        }

        user.setLastLoginAt(LocalDateTime.now());
        return userAccountRepository.save(user);
    }

    public Optional<UserAccount> findById(String userId) {
        return userAccountRepository.findById(userId);
    }

    public Optional<UserAccount> findByEmail(String email) {
        return userAccountRepository.findByEmail(normalizeEmail(email));
    }

    public boolean isUserIdDuplicate(String userId) {
        if (userId == null) return false;
        return userAccountRepository.findById(userId.trim()).isPresent();
    }

    public boolean isEmailDuplicate(String email) {
        if (email == null) return false;
        return userAccountRepository.findByEmail(normalizeEmail(email)).isPresent();
    }

    /**
     * (구 플로우용) BLOCKED → ACTIVE 전환이 필요한 경우에만 사용
     */
    public void enableUser(String email) {
        UserAccount user = userAccountRepository.findByEmail(normalizeEmail(email))
                .orElseThrow(() -> new IllegalArgumentException("해당 이메일의 사용자가 없습니다."));

        user.setStatus(AccountStatus.ACTIVE);
        userAccountRepository.save(user);
    }

    /**
     * 계정 탈퇴: 비밀번호 확인 후 상태를 DELETED로 변경
     */
    public void withdraw(String userId, String rawPassword) {
        String normalizedUserId = userId.trim();

        UserAccount user = userAccountRepository.findById(normalizedUserId)
                .orElseThrow(() -> new IllegalArgumentException("존재하지 않는 사용자입니다."));

        if (user.getStatus() == AccountStatus.DELETED) {
            throw new IllegalStateException("이미 탈퇴한 계정입니다.");
        }

        // 비밀번호 확인
        String hash = user.getPasswordHash();
        boolean matched;
        if (hash != null && hash.startsWith("{noop}")) {
            // 시드 계정 호환용
            matched = rawPassword.equals(hash.substring("{noop}".length()));
        } else {
            matched = passwordEncoder.matches(rawPassword, hash);
        }

        if (!matched) {
            throw new IllegalArgumentException("비밀번호가 일치하지 않습니다.");
        }

        // 계정 상태를 DELETED로 변경
        user.setStatus(AccountStatus.DELETED);
        userAccountRepository.save(user);
    }

    private String normalizeEmail(String email) {
        return email == null ? null : email.trim().toLowerCase(Locale.ROOT);
    }
}
