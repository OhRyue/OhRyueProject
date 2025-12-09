package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.StudySession;
import com.OhRyue.certpilot.study.domain.StudySessionItem;
import com.OhRyue.certpilot.study.dto.InternalDtos;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.LearningStepRepository;
import com.OhRyue.certpilot.study.repository.StudySessionItemRepository;
import com.OhRyue.certpilot.study.repository.StudySessionRepository;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class StudyInternalService {

    private final StudySessionRepository sessionRepository;
    private final StudySessionItemRepository itemRepository;
    private final QuestionRepository questionRepository;
    private final LearningStepRepository learningStepRepository;
    private final ObjectMapper objectMapper;

    private static final TypeReference<Map<String, Object>> MAP_TYPE = new TypeReference<>() {};

    /**
     * 세션 상세 조회 (내부 API용)
     * progress-service에서 호출하여 세션의 문제/답안/정답 정보를 가져옴
     */
    public InternalDtos.StudySessionDetailDto getSessionDetail(Long sessionId, String userId) {
        StudySession session = sessionRepository.findById(sessionId)
                .orElseThrow(() -> new IllegalArgumentException("Session not found: " + sessionId));

        // userId 검증
        if (!session.getUserId().equals(userId)) {
            throw new IllegalArgumentException("Session does not belong to user: " + userId);
        }

        // Micro는 MINI + MCQ 두 세션으로 나뉘므로 LearningSession 단위로 질문을 합산한다.
        List<StudySessionItem> items;
        Map<Long, String> sessionStepMap = new HashMap<>();

        if ("MICRO".equals(session.getMode()) && session.getLearningStep() != null
                && session.getLearningStep().getLearningSession() != null) {
            var learningSession = session.getLearningStep().getLearningSession();
            var steps = learningStepRepository.findByLearningSessionIdOrderByIdAsc(learningSession.getId());

            var microSessions = steps.stream()
                    .filter(step -> step.getStudySession() != null)
                    .filter(step -> {
                        String code = step.getStepCode();
                        return "MINI".equals(code) || "MCQ".equals(code) || "MICRO_MINI".equals(code) || "MICRO_MCQ".equals(code);
                    })
                    .map(step -> {
                        StudySession ss = step.getStudySession();
                        sessionStepMap.put(ss.getId(), step.getStepCode());
                        return ss;
                    })
                    .toList();

            if (!microSessions.isEmpty()) {
                items = microSessions.stream()
                        .flatMap(s -> itemRepository.findBySessionIdOrderByOrderNoAsc(s.getId()).stream())
                        .toList();
            } else {
                items = itemRepository.findBySessionIdOrderByOrderNoAsc(sessionId);
                sessionStepMap.put(sessionId, session.getLearningStep().getStepCode());
            }
        } else {
            items = itemRepository.findBySessionIdOrderByOrderNoAsc(sessionId);
            if (session.getLearningStep() != null) {
                sessionStepMap.put(sessionId, session.getLearningStep().getStepCode());
            }
        }

        // 문제 ID 목록 추출
        List<Long> questionIds = items.stream()
                .map(StudySessionItem::getQuestionId)
                .collect(Collectors.toList());

        // 문제 일괄 조회
        Map<Long, Question> questionMap = questionRepository.findByIdIn(questionIds).stream()
                .collect(Collectors.toMap(Question::getId, q -> q));

        // 문제 상세 리스트 구성 (MINI 먼저, 그 다음 MCQ 순서를 유지)
        List<InternalDtos.QuestionDetailDto> questionDetails = new ArrayList<>();
        items.stream()
                .sorted((a, b) -> {
                    String stepA = sessionStepMap.getOrDefault(a.getSessionId(), "");
                    String stepB = sessionStepMap.getOrDefault(b.getSessionId(), "");
                    int stepOrderA = ("MINI".equals(stepA) || "MICRO_MINI".equals(stepA)) ? 0 : 1;
                    int stepOrderB = ("MINI".equals(stepB) || "MICRO_MINI".equals(stepB)) ? 0 : 1;
                    if (stepOrderA != stepOrderB) return Integer.compare(stepOrderA, stepOrderB);
                    return Integer.compare(a.getOrderNo(), b.getOrderNo());
                })
                .forEachOrdered(item -> {
                    Question question = questionMap.get(item.getQuestionId());
                    if (question == null) {
                        return;
                    }

                    String myAnswer = extractMyAnswer(item.getUserAnswerJson(), question.getType());
                    String correctAnswer = extractCorrectAnswer(question);
                    Long timeTakenMs = null;
                    if (item.getAnsweredAt() != null && item.getCreatedAt() != null) {
                        timeTakenMs = item.getAnsweredAt().toEpochMilli() - item.getCreatedAt().toEpochMilli();
                    }

                    questionDetails.add(new InternalDtos.QuestionDetailDto(
                            questionDetails.size() + 1, // 재정렬된 순번
                            item.getQuestionId(),
                            question.getType().name(),
                            Optional.ofNullable(question.getStem()).orElse(""),
                            myAnswer,
                            correctAnswer,
                            Boolean.TRUE.equals(item.getCorrect()),
                            item.getAnsweredAt() != null
                                    ? item.getAnsweredAt().atZone(ZoneId.of("Asia/Seoul")).toLocalDateTime()
                                    : null,
                            timeTakenMs,
                            item.getScore()
                    ));
                });

        return new InternalDtos.StudySessionDetailDto(
                session.getId(),
                userId,
                questionDetails
        );
    }

    private String extractMyAnswer(String userAnswerJson, com.OhRyue.certpilot.study.domain.enums.QuestionType questionType) {
        if (userAnswerJson == null || userAnswerJson.isBlank()) {
            return "";
        }

        try {
            Map<String, Object> answerMap = objectMapper.readValue(userAnswerJson, MAP_TYPE);
            
            // 실기 문제: "answer" 필드
            if (answerMap.containsKey("answer")) {
                return String.valueOf(answerMap.get("answer"));
            }
            
            // 필기 문제: "label" 필드 (MCQ)
            if (answerMap.containsKey("label")) {
                return String.valueOf(answerMap.get("label"));
            }
            
            return "";
        } catch (Exception e) {
            log.warn("Failed to parse userAnswerJson: {}", userAnswerJson, e);
            return "";
        }
    }

    private String extractCorrectAnswer(Question question) {
        String answerKey = Optional.ofNullable(question.getAnswerKey()).orElse("").trim();
        
        if (!answerKey.isBlank()) {
            return answerKey;
        }
        
        return "";
    }
}



