package com.OhRyue.certpilot.study.config;

import jakarta.validation.ConstraintViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.*;

import java.util.*;

@RestControllerAdvice
public class RestExceptionHandler {

  @ExceptionHandler({ MethodArgumentNotValidException.class, BindException.class })
  public ResponseEntity<?> handleBind(MethodArgumentNotValidException ex) {
    Map<String, Object> body = new LinkedHashMap<>();
    body.put("message", "유효성 검사에 실패했습니다.");
    List<Map<String,String>> errs = new ArrayList<>();
    ex.getBindingResult().getFieldErrors().forEach(e -> {
      errs.add(Map.of("field", e.getField(), "error", e.getDefaultMessage()));
    });
    body.put("errors", errs);
    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(body);
  }

  @ExceptionHandler(ConstraintViolationException.class)
  public ResponseEntity<?> handleConstraint(ConstraintViolationException ex) {
    Map<String, Object> body = new LinkedHashMap<>();
    body.put("message", "유효성 검사에 실패했습니다.");
    List<Map<String,String>> errs = new ArrayList<>();
    ex.getConstraintViolations().forEach(v -> {
      errs.add(Map.of("path", String.valueOf(v.getPropertyPath()), "error", v.getMessage()));
    });
    body.put("errors", errs);
    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(body);
  }

  @ExceptionHandler(IllegalArgumentException.class)
  public ResponseEntity<?> handleIllegal(IllegalArgumentException ex) {
    return ResponseEntity.badRequest().body(Map.of("message", ex.getMessage()));
  }

  @ExceptionHandler(NoSuchElementException.class)
  public ResponseEntity<?> handleNotFound(NoSuchElementException ex) {
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(Map.of("message", ex.getMessage()));
  }
}
