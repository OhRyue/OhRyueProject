package com.OhRyue.certpilot.account.service;

import com.OhRyue.certpilot.account.domain.User;
import com.OhRyue.certpilot.account.exception.InvalidCredentialsException;
import com.OhRyue.certpilot.account.repo.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class UserService {

    private final UserRepository userRepository;
    private final BCryptPasswordEncoder passwordEncoder;

    @Autowired
    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
        this.passwordEncoder = new BCryptPasswordEncoder();
    }

    // 회원 가입
    public User register(String username, String rawPassword, String email) {
        // 1) username 중복 검사
        if (userRepository.findByUsername(username).isPresent()) {
            throw new IllegalArgumentException("이미 존재하는 사용자명입니다.");
        }

        // 2) email 중복 검사
        if (userRepository.findByEmail(email).isPresent()) {
            throw new IllegalArgumentException("이미 존재하는 이메일입니다.");
        }

        // 3) 비밀번호 암호화
        String encodedPassword = passwordEncoder.encode(rawPassword);

        // 4) 유저 생성 (enabled = false 기본값)
        User user = User.builder()
                .username(username)
                .password(encodedPassword)
                .email(email)
                .role("USER")
                .enabled(false)
                .build();

        return userRepository.save(user);
    }


    // 로그인
    public User login(String username, String rawPassword) {
        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new IllegalArgumentException("존재하지 않는 사용자입니다."));

        if (!user.isEnabled()) {
            throw new IllegalStateException("이메일 인증 후 로그인 가능합니다.");
        }

        if (!passwordEncoder.matches(rawPassword, user.getPassword())) {
            throw new IllegalArgumentException("비밀번호가 일치하지 않습니다.");
        }

        return user;
    }

    // 사용자 조회(username 기반)
    public Optional<User> findByUsername(String username) {
        return userRepository.findByUsername(username);
    }

    // 이메일 인증 완료 → enabled = true 저장
    public void enableUser(String email) {
        User user = userRepository.findByEmail(email)
                .orElseThrow(() -> new IllegalArgumentException("해당 이메일의 사용자가 없습니다."));

        user.setEnabled(true);   // 인증 완료
        userRepository.save(user);
    }


}