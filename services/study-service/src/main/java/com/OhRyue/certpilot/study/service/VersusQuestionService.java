package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.QuestionChoice;
import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.VersusDtos;
import com.OhRyue.certpilot.study.repository.QuestionChoiceRepository;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Slf4j
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class VersusQuestionService {

    private final QuestionRepository questionRepository;
    private final QuestionChoiceRepository choiceRepository;
    private final ObjectMapper objectMapper;

    /**
     * Versus 모드용 문제 세트 생성
     */
    public List<VersusDtos.QuestionDto> generateVersusQuestions(VersusDtos.VersusQuestionRequest request) {
        if (request.questionTypes() == null || request.questionTypes().isEmpty()) {
            throw new ResponseStatusException(
                HttpStatus.BAD_REQUEST, "questionTypes cannot be empty");
        }

        ExamMode examMode = parseExamMode(request.examMode());
        Difficulty difficulty = parseDifficulty(request.difficulty());

        List<Question> allQuestions = new ArrayList<>();

        // 각 문제 유형별로 문제 수집
        for (VersusDtos.QuestionTypeSpec spec : request.questionTypes()) {
            QuestionType questionType = parseQuestionType(spec.type());
            int count = spec.count();

            List<Question> questions = collectQuestions(
                examMode, questionType, difficulty, request.topicId(), request.topicScope(), count
            );

            if (questions.size() < count) {
                log.warn("Requested {} questions of type {}, but only {} available",
                    count, questionType, questions.size());
            }

            allQuestions.addAll(questions);
        }

        if (allQuestions.isEmpty()) {
            throw new ResponseStatusException(
                HttpStatus.NOT_FOUND, 
                String.format("No questions found for examMode=%s, difficulty=%s, topicScope=%s",
                    examMode, difficulty, request.topicScope()));
        }

        // 랜덤 셔플
        Collections.shuffle(allQuestions);

        // 요청한 개수만큼만 반환
        int totalCount = Math.min(allQuestions.size(), request.count());
        List<Question> selected = allQuestions.stream()
            .limit(totalCount)
            .toList();

        log.info("Generated {} questions for versus mode: examMode={}, difficulty={}, types={}, requested={}",
            selected.size(), examMode, difficulty, request.questionTypes(), request.count());

        return selected.stream()
            .map(this::toQuestionDto)
            .toList();
    }

    /**
     * 문제 정보 조회
     */
    public VersusDtos.QuestionDto getQuestion(Long questionId) {
        Question question = questionRepository.findById(questionId)
            .orElseThrow(() -> new ResponseStatusException(
                HttpStatus.NOT_FOUND, "Question not found: " + questionId));

        return toQuestionDto(question);
    }

    /**
     * 정답 검증
     */
    public VersusDtos.AnswerValidationResult validateAnswer(Long questionId, VersusDtos.UserAnswerDto userAnswer) {
        Question question = questionRepository.findById(questionId)
            .orElseThrow(() -> new ResponseStatusException(
                HttpStatus.NOT_FOUND, "Question not found: " + questionId));

        String correctAnswer = getCorrectAnswer(question);
        String userAnswerText = userAnswer.answer().trim();
        boolean isCorrect = false;
        String explanation = Optional.ofNullable(question.getSolutionText()).orElse("");

        // 문제 유형별 정답 검증
        if (question.getType() == QuestionType.OX || question.getType() == QuestionType.MCQ) {
            // OX, MCQ: label 비교
            isCorrect = correctAnswer.equalsIgnoreCase(userAnswerText);
            log.debug("Answer validation (OX/MCQ): questionId={}, correctAnswer={}, userAnswer={}, isCorrect={}",
                    questionId, correctAnswer, userAnswerText, isCorrect);
        } else if (question.getType() == QuestionType.SHORT || question.getType() == QuestionType.LONG) {
            // SHORT, LONG: 텍스트 비교 (대소문자 무시, 공백 정규화)
            String normalizedCorrect = normalizeText(correctAnswer);
            String normalizedUser = normalizeText(userAnswerText);
            isCorrect = normalizedCorrect.equals(normalizedUser);
            
            log.info("Answer validation (SHORT/LONG): questionId={}, questionType={}, " +
                    "correctAnswer=[{}], userAnswer=[{}], " +
                    "normalizedCorrect=[{}], normalizedUser=[{}], isCorrect={}",
                    questionId, question.getType(),
                    correctAnswer, userAnswerText,
                    normalizedCorrect, normalizedUser, isCorrect);
            
            // 실기는 AI 채점이 필요할 수 있지만, 일단 간단한 텍스트 비교
            // 향후 AIExplanationService 활용 가능
        }

        return new VersusDtos.AnswerValidationResult(
            isCorrect,
            correctAnswer,
            explanation
        );
    }

    // ========== Private Helper Methods ==========

    private List<Question> collectQuestions(
        ExamMode examMode,
        QuestionType questionType,
        Difficulty difficulty,
        Long topicId,
        String topicScope,
        int count
    ) {
        List<Question> questions;

        if ("SPECIFIC".equals(topicScope) && topicId != null) {
            // 특정 토픽에서 선택
            questions = questionRepository.findByTopicIdAndModeAndType(topicId, examMode, questionType);
        } else {
            // 전체 범위에서 선택
            questions = questionRepository.findByModeAndTypeAndDifficulty(examMode, questionType, difficulty);
        }

        // 랜덤 셔플 후 요청 개수만큼 반환
        Collections.shuffle(questions);
        return questions.stream()
            .limit(count)
            .toList();
    }

    private String getCorrectAnswer(Question question) {
        if (question.getType() == QuestionType.OX || question.getType() == QuestionType.MCQ) {
            // MCQ/OX: QuestionChoice에서 정답 label 찾기
            return choiceRepository.findFirstByQuestionIdAndCorrectTrue(question.getId())
                .map(QuestionChoice::getLabel)
                .orElse(Optional.ofNullable(question.getAnswerKey()).orElse(""));
        } else {
            // SHORT/LONG: answerKey 직접 사용
            return Optional.ofNullable(question.getAnswerKey()).orElse("");
        }
    }

    private VersusDtos.QuestionDto toQuestionDto(Question question) {
        Map<String, Object> payloadJson = null;
        if (question.getPayloadJson() != null && !question.getPayloadJson().isBlank()) {
            try {
                payloadJson = objectMapper.readValue(
                    question.getPayloadJson(), new TypeReference<Map<String, Object>>() {});
            } catch (Exception e) {
                log.warn("Failed to parse payloadJson for question {}: {}", question.getId(), e.getMessage());
            }
        }

        return new VersusDtos.QuestionDto(
            question.getId(),
            question.getMode().name(),
            question.getType().name(),
            question.getDifficulty().name(),
            question.getStem(),
            getCorrectAnswer(question),
            question.getSolutionText(),
            payloadJson
        );
    }

    private String normalizeText(String text) {
        if (text == null) return "";
        return text.trim().toLowerCase().replaceAll("\\s+", " ");
    }

    private ExamMode parseExamMode(String mode) {
        if (mode == null || mode.isBlank()) return ExamMode.WRITTEN;
        try {
            return ExamMode.valueOf(mode.toUpperCase());
        } catch (IllegalArgumentException e) {
            log.warn("Invalid exam mode: {}, defaulting to WRITTEN", mode);
            return ExamMode.WRITTEN;
        }
    }

    private Difficulty parseDifficulty(String difficulty) {
        if (difficulty == null || difficulty.isBlank()) return Difficulty.NORMAL;
        try {
            return Difficulty.valueOf(difficulty.toUpperCase());
        } catch (IllegalArgumentException e) {
            log.warn("Invalid difficulty: {}, defaulting to NORMAL", difficulty);
            return Difficulty.NORMAL;
        }
    }

    private QuestionType parseQuestionType(String type) {
        if (type == null || type.isBlank()) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Question type is required");
        }
        try {
            return QuestionType.valueOf(type.toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new ResponseStatusException(
                HttpStatus.BAD_REQUEST, "Invalid question type: " + type);
        }
    }
}

