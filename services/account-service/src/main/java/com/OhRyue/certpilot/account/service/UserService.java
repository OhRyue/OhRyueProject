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
    public User register(String username, String rawPassword) {
        System.out.println("✅ register() 들어옴 username=" + username);

        // 1) 아이디 중복 검사
        if (userRepository.findByUsername(username).isPresent()) {
            throw new IllegalArgumentException("이미 존재하는 사용자명입니다.");
            // → GlobalExceptionHandler의 @ExceptionHandler(IllegalArgumentException)에서 처리됨
        }

        // 2) 비밀번호 암호화
        String encodedPassword = passwordEncoder.encode(rawPassword);

        // 3) 유저 엔티티 생성
        User user = new User();
        user.setUsername(username);
        user.setPassword(encodedPassword);
        user.setRole("USER");

        // 4) 저장 후 반환
        return userRepository.save(user);
    }

    // 로그인
    public User login(String username, String rawPassword) {
        return userRepository.findByUsername(username)
                .filter(user -> passwordEncoder.matches(rawPassword, user.getPassword()))
                .orElseThrow(InvalidCredentialsException::new);
    }

    // 사용자 조회(username 기반)
    public Optional<User> findByUsername(String username) {
        return userRepository.findByUsername(username);
    }
}