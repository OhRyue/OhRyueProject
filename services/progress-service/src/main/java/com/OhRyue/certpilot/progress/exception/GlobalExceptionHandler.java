package com.OhRyue.certpilot.progress.exception;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.Map;

@Slf4j
@RestControllerAdvice
public class GlobalExceptionHandler {

  // 온보딩 미완료 사용자
  @ExceptionHandler(OnboardingRequiredException.class)
  public ResponseEntity<?> handleOnboardingRequired(OnboardingRequiredException e) {
    return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
        "errorCode", "ONBOARDING_REQUIRED",
        "error", "onboarding_required",
        "message", e.getMessage() != null ? e.getMessage() : "온보딩이 필요한 사용자입니다."
    ));
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


