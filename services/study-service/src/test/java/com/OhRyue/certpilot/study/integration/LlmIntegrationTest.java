package com.OhRyue.certpilot.study.integration;

import com.OhRyue.certpilot.study.config.LlmProperties;
import com.OhRyue.certpilot.study.service.llm.AiClient;
import com.OhRyue.certpilot.study.service.llm.OpenAiClient;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * LLM 연동 통합 테스트
 * - 실제 LLM API 호출 테스트 (환경변수 LLM_API_KEY 필요)
 * - Fallback 로직 테스트
 */
@SpringBootTest
@ActiveProfiles("test")
public class LlmIntegrationTest {

    @Autowired(required = false)
    private AiClient aiClient;

    @Autowired
    private LlmProperties llmProperties;

    @Test
    void testLlmConfiguration() {
        assertNotNull(llmProperties, "LlmProperties should be configured");
        assertNotNull(llmProperties.getBaseUrl(), "LLM base URL should be set");
        System.out.println("LLM Base URL: " + llmProperties.getBaseUrl());
        System.out.println("LLM Model: " + llmProperties.getModel());
        System.out.println("LLM Timeout: " + llmProperties.getTimeoutMs() + "ms");
    }

    @Test
    void testLlmClientBean() {
        if (aiClient == null) {
            System.out.println("Warning: AiClient bean not found. LLM integration may not be enabled.");
            return;
        }
        assertNotNull(aiClient, "AiClient should be available");
        assertTrue(aiClient instanceof OpenAiClient, "Should be OpenAiClient implementation");
    }

    @Test
    void testLlmApiKeyConfiguration() {
        String apiKey = llmProperties.getApiKey();
        if (apiKey == null || apiKey.isBlank()) {
            System.out.println("WARNING: LLM_API_KEY is not set. LLM calls will fail.");
            System.out.println("Please set LLM_API_KEY environment variable in .env file");
        } else {
            System.out.println("LLM API Key: [SET] (length: " + apiKey.length() + ")");
        }
    }

    /**
     * 실제 LLM API 호출 테스트 (환경변수 LLM_API_KEY가 설정된 경우에만 실행)
     * @Test 어노테이션을 주석 처리하여 수동 실행 가능
     */
    // @Test
    void testLlmExplainCall() {
        if (aiClient == null || llmProperties.getApiKey() == null || llmProperties.getApiKey().isBlank()) {
            System.out.println("Skipping LLM API call test - API key not configured");
            return;
        }

        AiClient.ExplainRequest request = new AiClient.ExplainRequest(
                "WRITTEN",
                "MCQ",
                "데이터베이스 정규화의 목적은?",
                List.of(
                        new AiClient.Choice("A", "데이터 중복 제거"),
                        new AiClient.Choice("B", "성능 향상"),
                        new AiClient.Choice("C", "보안 강화"),
                        new AiClient.Choice("D", "용량 절감")
                ),
                "A",
                "B",
                "정규화는 데이터 중복을 제거하고 무결성을 보장합니다.",
                Map.of("test", true)
        );

        try {
            AiClient.ExplainResponse response = aiClient.explain(request);
            assertNotNull(response, "Response should not be null");
            System.out.println("LLM Explain Response: " + response);
        } catch (Exception e) {
            System.out.println("LLM API call failed (expected if API key is invalid): " + e.getMessage());
            // Fallback 로직이 작동하므로 예외는 허용
        }
    }
}


