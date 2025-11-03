package com.OhRyue.certpilot.account.service;

import com.OhRyue.certpilot.account.domain.User;
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

    /**
     * 회원가입
     */
    public User register(String username, String rawPassword) {
        // 중복 아이디 체크
        if (userRepository.findByUsername(username).isPresent()) {
            throw new IllegalArgumentException("이미 존재하는 사용자명입니다.");
        }

        // 비밀번호 암호화
        String encodedPassword = passwordEncoder.encode(rawPassword);

        User user = new User();
        user.setUsername(username);
        user.setPassword(encodedPassword);
        user.setRole("USER");

        return userRepository.save(user);
    }

    /**
     * 로그인 (비밀번호 검증)
     */
    public User login(String username, String rawPassword) {
        Optional<User> userOpt = userRepository.findByUsername(username);
        if (userOpt.isEmpty()) {
            throw new IllegalArgumentException("존재하지 않는 사용자입니다.");
        }

        User user = userOpt.get();
        if (!passwordEncoder.matches(rawPassword, user.getPassword())) {
            throw new IllegalArgumentException("비밀번호가 올바르지 않습니다.");
        }

        return user; // 로그인 성공 시 사용자 정보 반환
    }

    /**
     * 사용자 조회 (username 기반)
     */
    public Optional<User> findByUsername(String username) {
        return userRepository.findByUsername(username);
    }
}