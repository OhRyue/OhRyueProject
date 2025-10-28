package com.OhRyue.certpilot.common.error;

import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.servlet.http.HttpServletRequest;

import java.time.LocalDateTime;

public class ErrorResponse {
  @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
  private final LocalDateTime timestamp;
  private final int status;
  private final String error;
  private final String message;
  private final String path;

  private ErrorResponse(LocalDateTime timestamp, int status, String error, String message, String path) {
    this.timestamp = timestamp;
    this.status = status;
    this.error = error;
    this.message = message;
    this.path = path;
  }

  public static ErrorResponse of(int status, String error, String message, HttpServletRequest req) {
    return new ErrorResponse(LocalDateTime.now(), status, error, message, req.getRequestURI());
  }

  public LocalDateTime getTimestamp() { return timestamp; }
  public int getStatus() { return status; }
  public String getError() { return error; }
  public String getMessage() { return message; }
  public String getPath() { return path; }
}
