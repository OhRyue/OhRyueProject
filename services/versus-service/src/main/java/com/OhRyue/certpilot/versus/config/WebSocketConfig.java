package com.OhRyue.certpilot.versus.config;

import com.OhRyue.certpilot.versus.websocket.JwtHandshakeInterceptor;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Configuration;
import org.springframework.messaging.simp.config.MessageBrokerRegistry;
import org.springframework.web.socket.config.annotation.EnableWebSocketMessageBroker;
import org.springframework.web.socket.config.annotation.StompEndpointRegistry;
import org.springframework.web.socket.config.annotation.WebSocketMessageBrokerConfigurer;

/**
 * WebSocket (STOMP) 설정 클래스
 * 
 * STOMP (Simple Text Oriented Messaging Protocol):
 * - WebSocket 위에서 동작하는 메시징 프로토콜
 * - Pub/Sub 패턴을 쉽게 구현 가능
 * - Redis를 통한 다중 인스턴스 메시징 지원 (향후 확장)
 * 
 * 엔드포인트:
 * - /ws/versus: WebSocket 연결 엔드포인트
 *   예: ws://localhost:8085/ws/versus?token=xxx
 * 
 * 메시징 구조:
 * - Client -> Server: /app/xxx (application prefix)
 * - Server -> Client: /topic/xxx, /user/xxx (broker prefix)
 * 
 * 보안:
 * - JwtHandshakeInterceptor를 통해 Handshake 단계에서 JWT 검증
 * - 인증된 사용자만 WebSocket 연결 가능
 */
@Configuration
@EnableWebSocketMessageBroker
@RequiredArgsConstructor
public class WebSocketConfig implements WebSocketMessageBrokerConfigurer {

    private final JwtHandshakeInterceptor jwtHandshakeInterceptor;

    /**
     * STOMP 엔드포인트 등록
     * - /ws/versus: WebSocket 연결 엔드포인트
     * - SockJS 사용 가능 (폴백 지원)
     */
    @Override
    public void registerStompEndpoints(StompEndpointRegistry registry) {
        registry.addEndpoint("/ws/versus")
                .setAllowedOriginPatterns("*") // Gateway에서 CORS 처리하므로 여기서는 허용
                .addInterceptors(jwtHandshakeInterceptor) // JWT 인증 인터셉터 등록
                .withSockJS(); // SockJS 폴백 지원 (선택사항)
    }

    /**
     * 메시지 브로커 설정
     * - /topic: 모든 구독자에게 메시지 전송 (브로드캐스트)
     * - /user: 특정 사용자에게 메시지 전송 (1:1)
     * - /app: 클라이언트가 서버로 메시지를 보낼 때 사용하는 prefix
     */
    @Override
    public void configureMessageBroker(MessageBrokerRegistry registry) {
        // 메시지 브로커 활성화 (메모리 기반, 향후 Redis로 전환 가능)
        registry.enableSimpleBroker("/topic", "/user");
        
        // 클라이언트 -> 서버 메시지 prefix
        registry.setApplicationDestinationPrefixes("/app");
        
        // 사용자별 메시징 활성화
        registry.setUserDestinationPrefix("/user");
    }
}

