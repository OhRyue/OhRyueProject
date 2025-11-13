package com.OhRyue.common.web;

import java.time.Instant;

public record ApiResponse<T>(T data, Meta meta) {

  public static <T> ApiResponse<T> success(T data, String path, String requestId) {
    return new ApiResponse<>(data, new Meta(Instant.now(), path, requestId));
  }

  public record Meta(Instant timestamp, String path, String requestId) {}
}

