package com.OhRyue.common.web;


import jakarta.servlet.http.HttpServletRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;


@RestControllerAdvice
public class GlobalExceptionHandler {
  @ExceptionHandler(MethodArgumentNotValidException.class)
  public ResponseEntity<ErrorResponse> handleValidation(MethodArgumentNotValidException ex, HttpServletRequest req){
    return ResponseEntity.badRequest().body(
        ErrorResponse.of(400, "Bad Request", ex.getMessage(), req.getRequestURI()));
  }
  @ExceptionHandler(Exception.class)
  public ResponseEntity<ErrorResponse> handle(Exception ex, HttpServletRequest req){
    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(
        ErrorResponse.of(500, "Internal Server Error", ex.getMessage(), req.getRequestURI()));
  }
}