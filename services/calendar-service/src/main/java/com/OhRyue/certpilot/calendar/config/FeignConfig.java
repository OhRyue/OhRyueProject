package com.OhRyue.certpilot.calendar.config;

import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableFeignClients(basePackages = "com.OhRyue.certpilot.calendar.client")
public class FeignConfig {
}

