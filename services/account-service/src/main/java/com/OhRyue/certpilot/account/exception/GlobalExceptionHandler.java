package com.OhRyue.certpilot.account.exception;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.Map;

/*
    전역 예외 처리
    - 로그인 실패, 서버 에러
 */
@Slf4j
@RestControllerAdvice
public class GlobalExceptionHandler {
    // 로그인 실패
    @ExceptionHandler(InvalidCredentialsException.class)
    public ResponseEntity<?> handleInvalidCredentials() {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
                "error", "invalid_credentials",
                "message", "아이디 또는 비밀번호가 올바르지 않습니다"
        ));
    }

    // 서버 에러
    @ExceptionHandler(Exception.class)
    public ResponseEntity<?> handleOtherExceptions(Exception e) {
        log.error("unexpected error", e);
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(Map.of(
                "error", "internal_server_error",
                "message", "서버 에러가 발생했습니다"
        ));
    }
}
