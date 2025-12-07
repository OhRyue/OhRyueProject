package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.QuestionChoice;
import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.VersusDtos;
import com.OhRyue.certpilot.study.repository.QuestionChoiceRepository;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.service.TopicTreeService;
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
    private final TopicTreeService topicTreeService;
    private final ObjectMapper objectMapper;
    private final AIExplanationService aiExplanationService;

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
            log.info("Answer validation (OX/MCQ): questionId={}, questionType={}, correctAnswer=[{}], userAnswer=[{}], isCorrect={}, answerKey={}",
                    questionId, question.getType(), correctAnswer, userAnswerText, isCorrect, question.getAnswerKey());
        } else if (question.getType() == QuestionType.SHORT || question.getType() == QuestionType.LONG) {
            // SHORT, LONG: AI 채점 사용 (해설 제외, 채점만)
            try {
                isCorrect = aiExplanationService.scorePracticalOnly(question, userAnswerText);
                log.info("Answer validation (SHORT/LONG with AI): questionId={}, questionType={}, userAnswer=[{}], isCorrect={}",
                        questionId, question.getType(), userAnswerText, isCorrect);
            } catch (Exception e) {
                log.warn("AI 채점 실패, 텍스트 비교로 fallback: questionId={}, error={}", questionId, e.getMessage());
                // AI 채점 실패 시 텍스트 비교로 fallback
                String normalizedCorrect = normalizeText(correctAnswer);
                String normalizedUser = normalizeText(userAnswerText);
                isCorrect = normalizedCorrect.equals(normalizedUser);
                
                log.info("Answer validation (SHORT/LONG with text comparison fallback): questionId={}, questionType={}, " +
                        "correctAnswer=[{}], userAnswer=[{}], " +
                        "normalizedCorrect=[{}], normalizedUser=[{}], isCorrect={}",
                        questionId, question.getType(),
                        correctAnswer, userAnswerText,
                        normalizedCorrect, normalizedUser, isCorrect);
            }
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
            // 특정 토픽 + 하위 토픽 전체에서 선택 (난이도 필터링 없음 - 카테고리 모드)
            // - rootTopicId = 11001 이면, 111xx, 112xx 등 모든 자식 토픽까지 포함
            var topicIds = topicTreeService.descendantsOf(topicId);
            if (topicIds.isEmpty()) {
                topicIds = java.util.Set.of(topicId);
            }
            questions = questionRepository.findByTopicIdInAndModeAndType(topicIds, examMode, questionType);
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

    /**
     * 정답 키 반환
     * 모든 문제 유형에서 answerKey를 직접 사용합니다.
     * QuestionChoice의 correct 레이블은 프론트엔드 표시용이며, 정답 판정에는 사용하지 않습니다.
     */
    private String getCorrectAnswer(Question question) {
        String answerKey = Optional.ofNullable(question.getAnswerKey()).orElse("");
        log.debug("Getting correct answer from answerKey: questionId={}, questionType={}, answerKey=[{}]",
                question.getId(), question.getType(), answerKey);
        return answerKey;
    }

    private VersusDtos.QuestionDto toQuestionDto(Question question) {
        Map<String, Object> payloadJson = new HashMap<>();
        
        // 기존 payloadJson 파싱
        if (question.getPayloadJson() != null && !question.getPayloadJson().isBlank()) {
            try {
                Map<String, Object> existingPayload = objectMapper.readValue(
                    question.getPayloadJson(), new TypeReference<Map<String, Object>>() {});
                if (existingPayload != null) {
                    payloadJson.putAll(existingPayload);
                }
            } catch (Exception e) {
                log.warn("Failed to parse payloadJson for question {}: {}", question.getId(), e.getMessage());
            }
        }
        
        // MCQ/OX 문제의 경우 선택지 정보 추가
        if (question.getType() == QuestionType.MCQ || question.getType() == QuestionType.OX) {
            List<QuestionChoice> choices = choiceRepository.findByQuestionIdOrderByLabelAsc(question.getId());
            List<Map<String, Object>> choicesList;
            
            // answerKey를 기준으로 정답 판단 (백엔드 정답 판정 기준)
            String correctAnswer = getCorrectAnswer(question);
            
            if (choices.isEmpty() && question.getType() == QuestionType.OX) {
                // OX 문제의 경우 선택지가 없으면 기본 선택지 생성
                choicesList = new ArrayList<>();
                
                Map<String, Object> oChoice = new HashMap<>();
                oChoice.put("label", "O");
                oChoice.put("content", "맞음");
                oChoice.put("correct", "O".equalsIgnoreCase(correctAnswer));
                choicesList.add(oChoice);
                
                Map<String, Object> xChoice = new HashMap<>();
                xChoice.put("label", "X");
                xChoice.put("content", "틀림");
                xChoice.put("correct", "X".equalsIgnoreCase(correctAnswer));
                choicesList.add(xChoice);
            } else {
                // MCQ 문제 또는 DB에 선택지가 있는 OX 문제
                // answerKey를 기준으로 correct 값 계산 (QuestionChoice의 correct 값 무시)
                choicesList = choices.stream()
                    .map(choice -> {
                        Map<String, Object> choiceMap = new HashMap<>();
                        choiceMap.put("label", choice.getLabel());
                        choiceMap.put("content", choice.getContent());
                        // answerKey와 label을 비교하여 correct 값 결정
                        choiceMap.put("correct", correctAnswer.equalsIgnoreCase(choice.getLabel()));
                        return choiceMap;
                    })
                    .collect(Collectors.toList());
            }
            
            payloadJson.put("choices", choicesList);
        }
        
        // payloadJson이 비어있으면 null로 설정
        Map<String, Object> finalPayloadJson = payloadJson.isEmpty() ? null : payloadJson;

        return new VersusDtos.QuestionDto(
            question.getId(),
            question.getMode().name(),
            question.getType().name(),
            question.getDifficulty().name(),
            question.getStem(),
            getCorrectAnswer(question),
            question.getSolutionText(),
            finalPayloadJson
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

