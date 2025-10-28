package com.OhRyue.certpilot.learn;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;

@SpringBootApplication
@EnableFeignClients(basePackages = "com.OhRyue.certpilot.learn.client")
public class LearnServiceApplication {
  public static void main(String[] args) {
    SpringApplication.run(LearnServiceApplication.class, args);
  }
}
