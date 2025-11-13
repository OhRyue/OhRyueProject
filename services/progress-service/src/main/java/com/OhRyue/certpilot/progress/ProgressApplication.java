package com.OhRyue.certpilot.progress;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableScheduling
@EnableFeignClients(basePackages = "com.OhRyue.certpilot.progress.feign")
public class ProgressApplication {
  public static void main(String[] args) {
    SpringApplication.run(ProgressApplication.class, args);
  }
}