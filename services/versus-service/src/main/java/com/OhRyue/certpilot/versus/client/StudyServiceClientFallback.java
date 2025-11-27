package com.OhRyue.certpilot.versus.client;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * StudyServiceClient Fallback
 * - Circuit Breaker가 열렸을 때 사용되는 Fallback 구현
 */
@Slf4j
@Component
public class StudyServiceClientFallback implements StudyServiceClient {

    @Override
    public List<QuestionDto> generateVersusQuestions(VersusQuestionRequest request) {
        log.error("StudyServiceClient.generateVersusQuestions fallback 호출 - Circuit Breaker가 열렸거나 서비스가 다운되었습니다.");
        throw new RuntimeException("study-service가 일시적으로 사용할 수 없습니다. 잠시 후 다시 시도해주세요.");
    }

    @Override
    public QuestionDto getQuestion(Long questionId) {
        log.error("StudyServiceClient.getQuestion fallback 호출 - questionId={}", questionId);
        throw new RuntimeException("study-service가 일시적으로 사용할 수 없습니다. 잠시 후 다시 시도해주세요.");
    }

    @Override
    public AnswerValidationResult validateAnswer(Long questionId, UserAnswerDto userAnswer) {
        log.error("StudyServiceClient.validateAnswer fallback 호출 - questionId={}", questionId);
        // Fallback: 클라이언트가 보낸 값을 그대로 사용 (부정행위 가능성 있음)
        return new AnswerValidationResult(false, "", "서버 검증이 일시적으로 불가능합니다.");
    }
}


