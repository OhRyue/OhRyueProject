package com.OhRyue.certpilot.account.exception;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.Map;

/*
    전역 예외 처리
    - 컨트롤러에서 발생하는 모든 예외를 여기서 가로채, 일관된 json 형태로 클라이언트에 응답
 */
@Slf4j
@RestControllerAdvice
public class GlobalExceptionHandler {
    // 로그인 실패(아이디, 비밀번호 틀림)
    @ExceptionHandler(InvalidCredentialsException.class)
    public ResponseEntity<?> handleInvalidCredentials() {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
                "error", "invalid_credentials",
                "message", "아이디 또는 비밀번호가 올바르지 않습니다"
        ));
    }

    // 회원가입 실패(중복된 username 등 잘못된 요청일 때)
    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<?> handleIllegalArgument(IllegalArgumentException e) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(Map.of(
                "error", "invalid_request",
                "message", e.getMessage()
        ));
    }

    // 이메일 인증 안 된 사용자
    @ExceptionHandler(IllegalStateException.class)
    public ResponseEntity<?> handleIllegalState(IllegalStateException e) {
        return ResponseEntity.status(HttpStatus.FORBIDDEN).body(Map.of(
                "error", "email_not_verified",
                "message", e.getMessage()   // → "이메일 인증 후 로그인 가능합니다."
        ));
    }

    // 요청 DTO의 유효성 검사(@Valid) 실패 시 발생하는 예외 처리
    // → 예: 아이디 형식 불일치, 필수값 누락 등
    // 클라이언트에 400 Bad Request와 함께 메시지를 반환
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<?> handleValidationException(MethodArgumentNotValidException e) {
        String errorMessage = e.getBindingResult()
                .getFieldErrors()
                .stream()
                .findFirst()
                .map(err -> err.getDefaultMessage())
                .orElse("잘못된 요청입니다.");
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(Map.of(
                "error", "invalid_format",
                "message", errorMessage
        ));
    }

    // 그 외 예상하지 못한 서버 내부 에러
    @ExceptionHandler(Exception.class)
    public ResponseEntity<?> handleOtherExceptions(Exception e) {
        log.error("unexpected error", e);
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(Map.of(
                "error", "internal_server_error",
                "message", "서버 에러가 발생했습니다"
        ));
    }
}
