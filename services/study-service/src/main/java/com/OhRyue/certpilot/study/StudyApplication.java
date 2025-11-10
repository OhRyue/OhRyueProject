package com.OhRyue.certpilot.study;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;

@SpringBootApplication
@EnableFeignClients(basePackages = "com.OhRyue.certpilot.study.service.llm")
public class StudyApplication {
  public static void main(String[] args) {
    SpringApplication.run(StudyApplication.class, args);
  }
}
