package com.OhRyue.certpilot.account;


import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableScheduling
@EnableFeignClients(basePackages = "com.OhRyue.certpilot.account.feign")
public class AccountApplication {
  public static void main(String[] args) {
    SpringApplication.run(AccountApplication.class, args); }
}