package com.OhRyue.certpilot.versus;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;

@SpringBootApplication
@EnableCaching
public class VersusApplication {

  public static void main(String[] args) {
    SpringApplication.run(VersusApplication.class, args);
  }
}