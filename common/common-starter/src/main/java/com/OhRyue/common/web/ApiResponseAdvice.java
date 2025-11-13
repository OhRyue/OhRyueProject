package com.OhRyue.common.web;

import org.springframework.core.MethodParameter;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.http.server.ServletServerHttpRequest;
import org.springframework.http.server.ServletServerHttpResponse;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice;

import jakarta.servlet.http.HttpServletRequest;

@RestControllerAdvice
public class ApiResponseAdvice implements ResponseBodyAdvice<Object> {

  @Override
  public boolean supports(MethodParameter returnType, Class<? extends HttpMessageConverter<?>> converterType) {
    return true;
  }

  @Override
  public Object beforeBodyWrite(Object body,
                                MethodParameter returnType,
                                MediaType selectedContentType,
                                Class<? extends HttpMessageConverter<?>> selectedConverterType,
                                ServerHttpRequest request,
                                ServerHttpResponse response) {
    if (response instanceof ServletServerHttpResponse servletResponse) {
      HttpStatus status = HttpStatus.resolve(servletResponse.getServletResponse().getStatus());
      if (status != null && status.is1xxInformational()) {
        return body;
      }
      if (status != null && status.is3xxRedirection()) {
        return body;
      }
      if (status != null && status == HttpStatus.NO_CONTENT) {
        return body;
      }
    }

    if (body == null) {
      return wrap(null, request);
    }

    if (body instanceof ApiResponse<?> || body instanceof ErrorResponse) {
      return body;
    }

    if (body instanceof ResponseEntity<?> responseEntity) {
      Object entityBody = responseEntity.getBody();
      if (entityBody instanceof ApiResponse<?> || entityBody instanceof ErrorResponse) {
        return body;
      }
      ApiResponse<?> wrapped = wrap(entityBody, request);
      return ResponseEntity.status(responseEntity.getStatusCode())
          .headers(responseEntity.getHeaders())
          .body(wrapped);
    }

    if (body instanceof String || body instanceof byte[]) {
      return body;
    }

    return wrap(body, request);
  }

  private ApiResponse<Object> wrap(Object body, ServerHttpRequest request) {
    String path = null;
    String requestId = null;
    if (request instanceof ServletServerHttpRequest servletRequest) {
      HttpServletRequest httpServletRequest = servletRequest.getServletRequest();
      path = httpServletRequest.getRequestURI();
      requestId = httpServletRequest.getHeader("X-Request-Id");
    } else {
      path = request.getURI().getPath();
    }
    return ApiResponse.success(body, path, requestId);
  }
}

