package com.OhRyue.certpilot.study.config;

import com.OhRyue.certpilot.study.security.RateLimitFilter;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class FilterConfig {
  @Bean
  public FilterRegistrationBean<RateLimitFilter> rateLimitFilter() {
    FilterRegistrationBean<RateLimitFilter> reg = new FilterRegistrationBean<>();
    reg.setFilter(new RateLimitFilter());
    reg.addUrlPatterns("/api/study/*");
    reg.setOrder(10); // SecurityFilterChain 이후/이전 순서 조정 가능
    return reg;
  }
}
