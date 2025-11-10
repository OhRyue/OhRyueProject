package com.OhRyue.certpilot.study.config;

import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.Key;

public final class SigningKeyResolver {
  private SigningKeyResolver(){}

  public static Key hmacKey(String secret) {
    byte[] keyBytes = secret.getBytes(StandardCharsets.UTF_8);
    // HS256
    return new SecretKeySpec(keyBytes, "HmacSHA256");
  }
}
