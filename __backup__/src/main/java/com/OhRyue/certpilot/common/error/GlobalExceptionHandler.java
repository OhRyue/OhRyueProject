package com.OhRyue.certpilot.common.error;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.ConstraintViolationException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.*;

import java.util.NoSuchElementException;

@RestControllerAdvice
public class GlobalExceptionHandler {

  @ExceptionHandler(NoSuchElementException.class)
  public ResponseEntity<ErrorResponse> handleNotFound(NoSuchElementException ex, HttpServletRequest req) {
    var body = ErrorResponse.of(404, "Not Found", ex.getMessage(), req);
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(body);
  }

  @ExceptionHandler({MethodArgumentNotValidException.class, ConstraintViolationException.class})
  public ResponseEntity<ErrorResponse> handleBadRequest(Exception ex, HttpServletRequest req) {
    String msg;
    if (ex instanceof MethodArgumentNotValidException manv) {
      msg = manv.getBindingResult().getFieldErrors().stream()
          .map(fe -> fe.getField() + ": " + fe.getDefaultMessage())
          .findFirst().orElse("Validation failed");
    } else {
      msg = ex.getMessage();
    }
    var body = ErrorResponse.of(400, "Bad Request", msg, req);
    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(body);
  }

  @ExceptionHandler({HttpMessageNotReadableException.class, IllegalArgumentException.class})
  public ResponseEntity<ErrorResponse> handleReadable(Exception ex, HttpServletRequest req) {
    var body = ErrorResponse.of(400, "Bad Request", ex.getMessage(), req);
    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(body);
  }

  @ExceptionHandler(DataIntegrityViolationException.class)
  public ResponseEntity<ErrorResponse> handleConflict(DataIntegrityViolationException ex, HttpServletRequest req) {
    var body = ErrorResponse.of(409, "Conflict", "데이터 무결성 위반(중복/외래키 등)", req);
    return ResponseEntity.status(HttpStatus.CONFLICT).body(body);
  }

  @ExceptionHandler(Exception.class)
  public ResponseEntity<ErrorResponse> handle500(Exception ex, HttpServletRequest req) {
    var body = ErrorResponse.of(500, "Internal Server Error", ex.getMessage(), req);
    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(body);
  }
}
