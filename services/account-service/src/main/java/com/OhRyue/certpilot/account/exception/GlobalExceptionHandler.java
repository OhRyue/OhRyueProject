package com.OhRyue.certpilot.account.exception;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
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

  // 온보딩 미완료 사용자
  @ExceptionHandler(OnboardingRequiredException.class)
  public ResponseEntity<?> handleOnboardingRequired(OnboardingRequiredException e) {
    return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
        "errorCode", "ONBOARDING_REQUIRED",
        "error", "onboarding_required",
        "message", e.getMessage() != null ? e.getMessage() : "온보딩이 필요한 사용자입니다."
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

  // 요청 데이터 유효성 검증 실패 (@Valid 실패)
  @ExceptionHandler({MethodArgumentNotValidException.class, BindException.class})
  public ResponseEntity<?> handleValidationException(MethodArgumentNotValidException ex) {
    Map<String, Object> body = new LinkedHashMap<>();
    body.put("error", "validation_failed");
    
    List<Map<String, String>> errors = new ArrayList<>();
    ex.getBindingResult().getFieldErrors().forEach(error -> {
      errors.add(Map.of(
          "field", error.getField(),
          "message", error.getDefaultMessage() != null ? error.getDefaultMessage() : "유효하지 않은 값입니다."
      ));
    });
    
    // 첫 번째 에러 메시지를 기본 메시지로 사용
    String defaultMessage = errors.isEmpty() 
        ? "입력값이 올바르지 않습니다." 
        : errors.get(0).get("message");
    
    body.put("message", defaultMessage);
    body.put("errors", errors);
    
    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(body);
  }

  // 그 외 예상하지 못한 서버 내부 에러
  @ExceptionHandler(Exception.class)
  public ResponseEntity<?> handleOtherExceptions(Exception e) {
    log.error("unexpected error", e);
    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(Map.of(
        "error", "internal_server_error",
        "message", "서버 에러가 발생했습니다",
        "exception", e.getClass().getName(),
        "detail", e.getMessage()
    ));
  }
}
