package com.OhRyue.certpilot.versus.client;

import com.OhRyue.certpilot.versus.config.MonitoringConfig;
import feign.Response;
import feign.codec.ErrorDecoder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Feign Client 에러 디코더
 * - Feign Client 호출 실패 시 모니터링 기록
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class FeignClientErrorDecoder implements ErrorDecoder {

    private final MonitoringConfig monitoringConfig;
    private final ErrorDecoder defaultDecoder = new Default();

    @Override
    public Exception decode(String methodKey, Response response) {
        // 서비스 이름 추출 (예: "StudyServiceClient#generateVersusQuestions")
        String serviceName = extractServiceName(methodKey);
        String method = extractMethodName(methodKey);

        Exception exception = defaultDecoder.decode(methodKey, response);

        // 5xx 에러 또는 네트워크 에러인 경우 모니터링 기록
        if (response.status() >= 500 || exception instanceof java.io.IOException) {
            monitoringConfig.recordFeignFailure(serviceName, method, exception);
        }

        return exception;
    }

    private String extractServiceName(String methodKey) {
        if (methodKey.contains("#")) {
            return methodKey.split("#")[0].replace("Client", "");
        }
        return "Unknown";
    }

    private String extractMethodName(String methodKey) {
        if (methodKey.contains("#")) {
            return methodKey.split("#")[1];
        }
        return methodKey;
    }
}


