package com.OhRyue.certpilot.calendar;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;

@EnableFeignClients
@SpringBootApplication
public class CalendarServiceApplication {
  public static void main(String[] args) {
    SpringApplication.run(CalendarServiceApplication.class, args);
  }
}

