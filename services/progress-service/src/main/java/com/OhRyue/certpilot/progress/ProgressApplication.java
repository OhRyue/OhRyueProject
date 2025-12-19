package com.OhRyue.certpilot.progress;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;

import java.time.ZoneId;

@Slf4j
@SpringBootApplication
@EnableScheduling
@EnableAsync
@EnableFeignClients(basePackages = "com.OhRyue.certpilot.progress.feign")
public class ProgressApplication {
  public static void main(String[] args) {
    // 타임존 정보 로깅
    ZoneId systemDefault = ZoneId.systemDefault();
    String jvmTimezone = System.getProperty("user.timezone", "미설정");
    String envTz = System.getenv("TZ");
    
    log.info("========================================");
    log.info("🚀 Progress Service 시작");
    log.info("   시스템 기본 타임존: {}", systemDefault);
    log.info("   JVM 타임존: {}", jvmTimezone);
    log.info("   환경변수 TZ: {}", envTz != null ? envTz : "미설정");
    log.info("   목표 타임존: Asia/Seoul");
    log.info("========================================");
    
    SpringApplication.run(ProgressApplication.class, args);
  }
}