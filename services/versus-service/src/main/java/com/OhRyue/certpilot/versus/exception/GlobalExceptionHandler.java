package com.OhRyue.certpilot.versus.exception;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.server.ResponseStatusException;

import java.time.Instant;
import java.util.Map;

/**
 * 전역 예외 핸들러
 * - 모든 예외를 일관된 형식으로 처리
 * - 로깅 강화
 */
@Slf4j
@RestControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(ResponseStatusException.class)
    public ResponseEntity<Map<String, Object>> handleResponseStatusException(ResponseStatusException e) {
        log.warn("ResponseStatusException: status={}, reason={}", e.getStatusCode(), e.getReason(), e);
        
        Map<String, Object> body = Map.of(
            "timestamp", Instant.now().toString(),
            "status", e.getStatusCode().value(),
            "error", e.getStatusCode().toString(),
            "message", e.getReason() != null ? e.getReason() : e.getMessage(),
            "path", "N/A"
        );
        
        return ResponseEntity.status(e.getStatusCode()).body(body);
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<Map<String, Object>> handleIllegalArgumentException(IllegalArgumentException e) {
        log.warn("IllegalArgumentException: {}", e.getMessage(), e);
        
        Map<String, Object> body = Map.of(
            "timestamp", Instant.now().toString(),
            "status", HttpStatus.BAD_REQUEST.value(),
            "error", "Bad Request",
            "message", e.getMessage(),
            "path", "N/A"
        );
        
        return ResponseEntity.badRequest().body(body);
    }

    /**
     * JSON 파싱 오류 처리
     * - 사용자가 입력한 텍스트에 줄바꿈, 특수문자 등이 포함되어 발생하는 오류를 친절하게 처리
     */
    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ResponseEntity<Map<String, Object>> handleHttpMessageNotReadableException(HttpMessageNotReadableException e) {
        String message = "JSON 형식 오류입니다. 요청 본문을 확인해주세요.";
        
        // 원인 예외에서 더 자세한 정보 추출
        Throwable cause = e.getCause();
        if (cause instanceof JsonParseException) {
            JsonParseException jpe = (JsonParseException) cause;
            message = String.format("JSON 파싱 오류: %s (위치: %s)", 
                jpe.getOriginalMessage(), 
                jpe.getLocation() != null ? 
                    String.format("라인 %d, 컬럼 %d", jpe.getLocation().getLineNr(), jpe.getLocation().getColumnNr()) : 
                    "알 수 없음");
        } else if (cause instanceof JsonMappingException) {
            JsonMappingException jme = (JsonMappingException) cause;
            message = String.format("JSON 매핑 오류: %s", jme.getOriginalMessage());
        } else if (cause != null) {
            message = String.format("JSON 처리 오류: %s", cause.getMessage());
        }
        
        log.warn("JSON 파싱 오류: {}", message, e);
        
        Map<String, Object> body = Map.of(
            "timestamp", Instant.now().toString(),
            "status", HttpStatus.BAD_REQUEST.value(),
            "error", "Bad Request",
            "message", message,
            "path", "N/A",
            "hint", "텍스트 입력 시 줄바꿈이나 특수문자가 포함되어 있으면 JSON에서 이스케이프 처리(\\n, \\t 등)가 필요합니다."
        );
        
        return ResponseEntity.badRequest().body(body);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<Map<String, Object>> handleException(Exception e) {
        log.error("Unexpected exception: {}", e.getMessage(), e);
        
        Map<String, Object> body = Map.of(
            "timestamp", Instant.now().toString(),
            "status", HttpStatus.INTERNAL_SERVER_ERROR.value(),
            "error", "Internal Server Error",
            "message", e.getMessage() != null ? e.getMessage() : "An unexpected error occurred",
            "path", "N/A"
        );
        
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(body);
    }
}




