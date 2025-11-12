package com.OhRyue.certpilot.study.config;

import io.netty.channel.ChannelOption;
import io.netty.handler.timeout.ReadTimeoutHandler;
import io.netty.handler.timeout.WriteTimeoutHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.netty.http.client.HttpClient;

import java.util.concurrent.TimeUnit;

@Configuration
@RequiredArgsConstructor
public class WebClientConfig {
  private final LlmProperties props;

  @Bean
  public WebClient webClient(){
    int ms = Math.max(1000, props.getTimeoutMs());
    HttpClient http = HttpClient.create()
        .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, ms)
        .doOnConnected(conn -> {
          conn.addHandlerLast(new ReadTimeoutHandler(ms, TimeUnit.MILLISECONDS));
          conn.addHandlerLast(new WriteTimeoutHandler(ms, TimeUnit.MILLISECONDS));
        });
    return WebClient.builder()
        .clientConnector(new ReactorClientHttpConnector(http))
        .build();
  }
}
