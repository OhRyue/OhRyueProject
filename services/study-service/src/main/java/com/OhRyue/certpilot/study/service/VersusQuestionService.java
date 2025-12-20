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
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;

import java.util.*;
import java.util.stream.Collectors;

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

        // ROOT_DESCENDANTS 모드: 보조학습과 동일한 균등 분배 로직 적용
        if ("ROOT_DESCENDANTS".equals(request.topicScope()) && request.topicId() != null) {
            return generateQuestionsWithRootDescendants(request, examMode, difficulty);
        }

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

        // questionTypes 상세 로깅
        String questionTypesDetail = request.questionTypes().stream()
                .map(qt -> String.format("%s:%d", qt.type(), qt.count()))
                .collect(Collectors.joining(", ", "[", "]"));
        log.info("Generated {} questions for versus mode: examMode={}, difficulty={}, topicScope={}, topicId={}, requestedCount={}, questionTypes={}, actualCount={}",
            selected.size(), examMode, difficulty, request.topicScope(), request.topicId(), request.count(), questionTypesDetail, selected.size());

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

    /**
     * ROOT_DESCENDANTS 모드: rootTopicId의 하위 토픽(children)에서 균등 분배로 문제 생성
     * 보조학습 Category 모드와 동일한 출제 규칙 적용
     */
    private List<VersusDtos.QuestionDto> generateQuestionsWithRootDescendants(
            VersusDtos.VersusQuestionRequest request,
            ExamMode examMode,
            Difficulty difficulty) {
        
        Long rootTopicId = request.topicId();
        int want = request.count();
        
        log.info("🌳 ROOT_DESCENDANTS 모드 시작: rootTopicId={}, examMode={}, count={}", rootTopicId, examMode, want);
        
        // 1. rootTopicId의 직접 자식 토픽(3레벨 leaf 토픽) 조회
        Long certId = request.certId();
        if (certId == null) {
            log.warn("[study] certId missing -> default certId=1 (멀티 자격증 지원 시 수정 필요)");
            certId = 1L; // 기본값: certId=1 (정보처리기사)
        }
        log.info("[study] ROOT_DESCENDANTS: 자식 토픽 조회 시작, rootTopicId={}, examMode={}, certId={}", rootTopicId, examMode, certId);
        
        Set<Long> childTopicIds;
        try {
            childTopicIds = topicTreeService.childrenOf(rootTopicId, examMode.name(), certId);
        } catch (ResponseStatusException e) {
            // TopicTreeService에서 던진 예외를 그대로 전파 (503/502 등)
            log.error("[study] ROOT_DESCENDANTS FAILED: cert-service 호출 실패, status={}, message={}", 
                    e.getStatusCode(), e.getMessage());
            throw e; // 503/502/500 등을 그대로 전파
        } catch (Exception e) {
            // 예상치 못한 예외
            log.error("[study] ROOT_DESCENDANTS FAILED: 예상치 못한 예외, rootTopicId={}, examMode={}, error={}", 
                    rootTopicId, examMode, e.getMessage(), e);
            throw new ResponseStatusException(
                HttpStatus.INTERNAL_SERVER_ERROR,
                String.format("Unexpected error while getting child topics. rootTopicId=%d, examMode=%s", 
                        rootTopicId, examMode),
                e);
        }
        
        if (childTopicIds == null || childTopicIds.isEmpty()) {
            // cert-service 호출은 성공했지만 빈 리스트를 반환한 경우 (데이터 없음)
            log.error("[study] ROOT_DESCENDANTS FAILED: 자식 토픽이 없습니다 (데이터 없음)");
            log.error("[study] ROOT_DESCENDANTS FAILED: rootTopicId={}, examMode={}, childTopicIds={}", 
                    rootTopicId, examMode, childTopicIds);
            log.error("[study] ROOT_DESCENDANTS FAILED: cert-service 호출은 성공했지만 빈 리스트를 반환했습니다.");
            log.error("[study] ROOT_DESCENDANTS FAILED: 가능한 원인:");
            log.error("[study]   1. DB에 실제로 자식 토픽이 없음");
            log.error("[study]   2. examMode 필터로 제외됨");
            log.error("[study]   3. certId 불일치");
            log.error("[study] ROOT_DESCENDANTS FAILED: 404 NOT_FOUND 반환 (데이터 없음)");
            
            throw new ResponseStatusException(
                HttpStatus.NOT_FOUND,
                String.format("No child topics found for rootTopicId=%d, examMode=%s. " +
                        "Please check if child topics exist in the database.", 
                        rootTopicId, examMode));
        }
        
        log.info("📋 ROOT_DESCENDANTS: childTopicIds={}, count={}", childTopicIds, childTopicIds.size());
        
        // 2. 각 문제 타입별로 문제 풀 수집
        Map<QuestionType, List<Question>> poolByType = new HashMap<>();
        for (VersusDtos.QuestionTypeSpec spec : request.questionTypes()) {
            QuestionType questionType = parseQuestionType(spec.type());
            log.info("🔍 문제 조회 시작: topicIds={}, examMode={}, questionType={}", childTopicIds, examMode, questionType);
            
            List<Question> questions = questionRepository.findByTopicIdInAndModeAndType(
                childTopicIds, examMode, questionType);
            
            log.info("📚 문제 조회 결과: questionType={}, foundCount={}, topicIds={}", 
                    questionType, questions.size(), 
                    questions.stream().map(Question::getTopicId).distinct().collect(Collectors.toList()));
            
            poolByType.put(questionType, questions);
        }
        
        // 전체 풀 크기 확인
        int totalPoolSize = poolByType.values().stream().mapToInt(List::size).sum();
        log.info("📊 전체 문제 풀 크기: total={}, byType={}", 
                totalPoolSize,
                poolByType.entrySet().stream()
                    .collect(Collectors.toMap(e -> e.getKey().name(), e -> e.getValue().size())));
        
        // 3. 모든 문제를 토픽별로 그룹화
        Map<Long, List<Question>> questionsByTopicId = new HashMap<>();
        for (List<Question> questions : poolByType.values()) {
            for (Question q : questions) {
                questionsByTopicId.computeIfAbsent(q.getTopicId(), k -> new ArrayList<>()).add(q);
            }
        }
        
        // 각 토픽의 문제를 섞기
        questionsByTopicId.values().forEach(Collections::shuffle);
        
        List<Long> topicIdsList = new ArrayList<>(questionsByTopicId.keySet());
        int topicCount = topicIdsList.size();
        
        if (topicCount == 0) {
            log.error("❌ ROOT_DESCENDANTS: 문제를 찾을 수 없습니다.");
            log.error("❌ 원인 분석:");
            log.error("   1. childTopicIds={} (자식 토픽 목록)", childTopicIds);
            log.error("   2. examMode={}, questionTypes={}", examMode, request.questionTypes());
            log.error("   3. poolByType 크기: {}", poolByType.values().stream().mapToInt(List::size).sum());
            log.error("   4. questionsByTopicId 크기: {}", questionsByTopicId.size());
            log.error("❌ 가능한 원인:");
            log.error("   - 자식 토픽에 해당 examMode/questionType의 문제가 없음");
            log.error("   - 문제 seed 데이터가 하위 토픽에 매핑되지 않음");
            log.error("   - cert-service에서 잘못된 자식 토픽을 반환함");
            
            throw new ResponseStatusException(
                HttpStatus.NOT_FOUND,
                String.format("No questions found for rootTopicId=%d, examMode=%s, childTopicIds=%s. " +
                        "Please check if questions exist for child topics.", rootTopicId, examMode, childTopicIds));
        }
        
        // 4. 각 타입별로 토픽 균등 분배
        List<Question> selectedQuestions = new ArrayList<>();
        Map<QuestionType, Integer> remainingByType = new HashMap<>();
        for (VersusDtos.QuestionTypeSpec spec : request.questionTypes()) {
            QuestionType questionType = parseQuestionType(spec.type());
            remainingByType.put(questionType, spec.count());
        }
        
        // 각 타입별로 토픽 균등 분배 적용
        for (Map.Entry<QuestionType, Integer> entry : remainingByType.entrySet()) {
            QuestionType questionType = entry.getKey();
            int typeCount = entry.getValue();
            
            // 해당 타입의 문제만 필터링
            Map<Long, List<Question>> typeQuestionsByTopic = new HashMap<>();
            for (Map.Entry<Long, List<Question>> topicEntry : questionsByTopicId.entrySet()) {
                List<Question> typeQuestions = topicEntry.getValue().stream()
                    .filter(q -> q.getType() == questionType)
                    .toList();
                if (!typeQuestions.isEmpty()) {
                    typeQuestionsByTopic.put(topicEntry.getKey(), new ArrayList<>(typeQuestions));
                }
            }
            
            if (typeQuestionsByTopic.isEmpty()) {
                log.warn("ROOT_DESCENDANTS: type={}에 해당하는 문제가 없습니다.", questionType);
                continue;
            }
            
            List<Long> typeTopicIds = new ArrayList<>(typeQuestionsByTopic.keySet());
            int typeTopicCount = typeTopicIds.size();
            
            // 기본 할당량 계산
            int basePerTopic = typeCount / typeTopicCount;
            int remainder = typeCount % typeTopicCount;
            
            Map<Long, Integer> topicQuotas = new HashMap<>();
            for (int i = 0; i < typeTopicIds.size(); i++) {
                Long topicId = typeTopicIds.get(i);
                int quota = basePerTopic + (i < remainder ? 1 : 0);
                topicQuotas.put(topicId, quota);
            }
            
            log.info("[study] ROOT_DESCENDANTS 균등 분배: type={}, typeCount={}, typeTopicCount={}, topicQuotas={}",
                questionType, typeCount, typeTopicCount, topicQuotas);
            
            // 라운드로빈 방식으로 균등 분배
            Map<Long, Integer> selectedCountByTopic = new HashMap<>();
            Map<Long, Integer> currentIndexByTopic = new HashMap<>();
            for (Long topicId : typeTopicIds) {
                selectedCountByTopic.put(topicId, 0);
                currentIndexByTopic.put(topicId, 0);
            }
            
            int selectedForType = 0;
            while (selectedForType < typeCount && selectedQuestions.size() < want) {
                boolean anySelected = false;
                
                // 각 토픽을 순회하며 1문제씩 선택 (라운드로빈)
                for (Long topicId : typeTopicIds) {
                    if (selectedForType >= typeCount) break;
                    
                    List<Question> topicQuestions = typeQuestionsByTopic.get(topicId);
                    if (topicQuestions == null || topicQuestions.isEmpty()) {
                        continue;
                    }
                    
                    int quota = topicQuotas.getOrDefault(topicId, 0);
                    int alreadySelected = selectedCountByTopic.getOrDefault(topicId, 0);
                    
                    // 할당량을 초과하지 않도록 체크
                    if (alreadySelected >= quota) {
                        continue;
                    }
                    
                    // 현재 인덱스에서 문제 선택
                    int currentIndex = currentIndexByTopic.getOrDefault(topicId, 0);
                    if (currentIndex < topicQuestions.size()) {
                        Question q = topicQuestions.get(currentIndex);
                        if (!selectedQuestions.contains(q)) {
                            selectedQuestions.add(q);
                            selectedCountByTopic.put(topicId, alreadySelected + 1);
                            selectedForType++;
                            anySelected = true;
                        }
                        currentIndexByTopic.put(topicId, currentIndex + 1);
                    }
                }
                
                // 더 이상 선택할 문제가 없으면 종료
                if (!anySelected) {
                    break;
                }
            }
            
            // 각 토픽별 선택된 문제 수 로깅
            log.info("[study] ROOT_DESCENDANTS 분배 결과: type={}, selectedByTopic={}, totalSelected={}, requested={}",
                questionType, selectedCountByTopic, selectedForType, typeCount);
        }
        
        // 5. 할당량으로 부족한 경우, 문제가 많은 토픽에서 추가 선택
        if (selectedQuestions.size() < want) {
            int remaining = want - selectedQuestions.size();
            
            // 타입별로 남은 할당량 계산
            Map<QuestionType, Integer> selectedByType = new HashMap<>();
            for (Question q : selectedQuestions) {
                selectedByType.put(q.getType(), selectedByType.getOrDefault(q.getType(), 0) + 1);
            }
            
            // 각 타입별로 부족한 만큼 추가 선택
            for (VersusDtos.QuestionTypeSpec spec : request.questionTypes()) {
                QuestionType questionType = parseQuestionType(spec.type());
                int selected = selectedByType.getOrDefault(questionType, 0);
                int needed = spec.count() - selected;
                
                if (needed <= 0 || remaining <= 0) continue;
                
                // 해당 타입의 문제를 토픽별로 그룹화
                Map<Long, List<Question>> typeQuestionsByTopic = new HashMap<>();
                for (Map.Entry<Long, List<Question>> topicEntry : questionsByTopicId.entrySet()) {
                    List<Question> typeQuestions = topicEntry.getValue().stream()
                        .filter(q -> q.getType() == questionType && !selectedQuestions.contains(q))
                        .toList();
                    if (!typeQuestions.isEmpty()) {
                        typeQuestionsByTopic.put(topicEntry.getKey(), new ArrayList<>(typeQuestions));
                    }
                }
                
                // 선택 비율이 낮은 토픽 우선으로 정렬
                Map<Long, Integer> selectedCountByTopic = new HashMap<>();
                for (Question q : selectedQuestions) {
                    if (q.getType() == questionType) {
                        selectedCountByTopic.put(q.getTopicId(), 
                            selectedCountByTopic.getOrDefault(q.getTopicId(), 0) + 1);
                    }
                }
                
                List<Map.Entry<Long, List<Question>>> sortedTopics = typeQuestionsByTopic.entrySet().stream()
                    .sorted((e1, e2) -> {
                        int size1 = e1.getValue().size();
                        int size2 = e2.getValue().size();
                        int selected1 = selectedCountByTopic.getOrDefault(e1.getKey(), 0);
                        int selected2 = selectedCountByTopic.getOrDefault(e2.getKey(), 0);
                        int ratio1 = size1 > 0 ? (selected1 * 100) / size1 : 0;
                        int ratio2 = size2 > 0 ? (selected2 * 100) / size2 : 0;
                        if (ratio1 != ratio2) {
                            return Integer.compare(ratio1, ratio2);
                        }
                        return Integer.compare(size2 - selected2, size1 - selected1);
                    })
                    .toList();
                
                for (Map.Entry<Long, List<Question>> entry : sortedTopics) {
                    if (remaining <= 0 || needed <= 0) break;
                    
                    List<Question> topicQuestions = entry.getValue();
                    for (Question q : topicQuestions) {
                        if (remaining <= 0 || needed <= 0) break;
                        if (!selectedQuestions.contains(q)) {
                            selectedQuestions.add(q);
                            remaining--;
                            needed--;
                        }
                    }
                }
            }
        }
        
        // 6. 최종적으로 부족하면 전체 풀에서 랜덤으로 추가
        if (selectedQuestions.size() < want) {
            List<Question> allPool = new ArrayList<>();
            for (List<Question> questions : poolByType.values()) {
                allPool.addAll(questions);
            }
            allPool.removeAll(selectedQuestions);
            Collections.shuffle(allPool);
            
            int additional = Math.min(want - selectedQuestions.size(), allPool.size());
            selectedQuestions.addAll(allPool.subList(0, additional));
        }
        
        // 최종 셔플
        Collections.shuffle(selectedQuestions);
        
        log.info("ROOT_DESCENDANTS: selectedQuestions={}, requested={}, topicDistribution={}",
            selectedQuestions.size(), want,
            selectedQuestions.stream()
                .collect(Collectors.groupingBy(Question::getTopicId, Collectors.counting())));
        
        return selectedQuestions.stream()
            .map(this::toQuestionDto)
            .toList();
    }

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
        
        // DUEL에서는 difficulty 필터를 사용하지 않으므로, ALL은 무시
        if ("ALL".equalsIgnoreCase(difficulty)) {
            log.debug("difficulty=ALL detected, using NORMAL (DUEL does not filter by difficulty)");
            return Difficulty.NORMAL;
        }
        
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

