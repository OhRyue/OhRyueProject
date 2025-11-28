package com.OhRyue.certpilot.versus.client;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * ProgressServiceClient Fallback
 * - Circuit Breaker가 열렸을 때 사용되는 Fallback 구현
 * - 보상 지급 실패는 치명적이지 않으므로 로그만 남기고 종료
 */
@Slf4j
@Component
public class ProgressServiceClientFallback implements ProgressServiceClient {

    @Override
    public void recordVersusResult(VersusResultRequest request) {
        log.error("ProgressServiceClient.recordVersusResult fallback 호출 - roomId={}, mode={}. " +
            "보상 지급이 실패했습니다. 나중에 수동으로 처리해야 할 수 있습니다.", 
            request.roomId(), request.mode());
        // 보상 지급 실패는 치명적이지 않으므로 예외를 던지지 않음
        // TODO: 향후 비동기 재시도 큐에 추가하는 로직 구현 가능
    }
}






