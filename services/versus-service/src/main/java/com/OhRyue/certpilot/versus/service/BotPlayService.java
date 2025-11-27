package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.*;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.*;
import com.OhRyue.certpilot.versus.support.VersusBotConst;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;
import java.util.Objects;

/**
 * 봇 자동 플레이 서비스
 * - 모든 모드(DUEL, TOURNAMENT, GOLDENBELL) 지원
 * - 난이도별 봇 동작 (EASY, NORMAL, HARD)
 * - match_answer와 match_event 자동 기록
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class BotPlayService {

    private final MatchQuestionRepository questionRepository;
    private final MatchAnswerRepository answerRepository;
    private final MatchEventRepository eventRepository;
    private final MatchRoomRepository roomRepository;
    private final MatchParticipantRepository participantRepository;
    private final GoldenbellStateRepository goldenbellStateRepository;
    private final VersusService versusService;
    private final ObjectMapper objectMapper;
    private final Random random = new Random();

    // ========== DUEL 모드 ==========

    /**
     * DUEL 봇 자동 플레이 시뮬레이션 (비동기)
     */
    @Async
    public void simulateDuelBotPlayAsync(Long roomId, String botUserId) {
        try {
            Thread.sleep(500); // 트랜잭션 커밋 대기
            
            log.info("DUEL 봇 자동 플레이 시작: roomId={}, botUserId={}", roomId, botUserId);
            
            MatchRoom room = roomRepository.findById(roomId)
                    .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));
            
            // 문제 생성 (없으면)
            ensureQuestionsExist(room);
            
            List<MatchQuestion> questions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId);
            if (questions.isEmpty()) {
                log.error("문제 생성 실패: roomId={}", roomId);
                return;
            }
            
            // 생성된 문제의 questionId 로그 출력 (사용자가 확인 가능하도록)
            List<Long> questionIds = questions.stream()
                    .map(MatchQuestion::getQuestionId)
                    .toList();
            log.info("생성된 문제 ID 목록: roomId={}, questionIds={}", roomId, questionIds);
            
            // 봇 플레이 실행
            playQuestions(roomId, botUserId, questions, room.getMode());
            
            log.info("DUEL 봇 자동 플레이 완료: roomId={}, botUserId={}", roomId, botUserId);
            
        } catch (Exception e) {
            log.error("DUEL 봇 자동 플레이 실패: roomId={}, botUserId={}, error={}", 
                    roomId, botUserId, e.getMessage(), e);
        }
    }

    // ========== TOURNAMENT 모드 ==========

    /**
     * TOURNAMENT 봇 자동 플레이 시뮬레이션 (비동기)
     * - 모든 봇이 각 라운드의 문제를 자동으로 풀고 답안 제출
     */
    @Async
    public void simulateTournamentBotPlayAsync(Long roomId) {
        try {
            Thread.sleep(500);
            
            log.info("TOURNAMENT 봇 자동 플레이 시작: roomId={}", roomId);
            
            MatchRoom room = roomRepository.findById(roomId)
                    .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));
            
            // 문제 생성 (없으면)
            ensureQuestionsExist(room);
            
            // 모든 봇 참가자 찾기
            List<MatchParticipant> allParticipants = participantRepository.findByRoomId(roomId);
            List<String> botUserIds = allParticipants.stream()
                    .map(MatchParticipant::getUserId)
                    .filter(userId -> userId.startsWith("BOT_"))
                    .collect(Collectors.toList());
            
            if (botUserIds.isEmpty()) {
                log.warn("TOURNAMENT 봇이 없음: roomId={}", roomId);
                return;
            }
            
            // 라운드별로 봇 플레이
            List<MatchQuestion> allQuestions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId);
            Map<Integer, List<MatchQuestion>> questionsByRound = allQuestions.stream()
                    .collect(Collectors.groupingBy(MatchQuestion::getRoundNo));
            
            for (Map.Entry<Integer, List<MatchQuestion>> entry : questionsByRound.entrySet()) {
                int roundNo = entry.getKey();
                List<MatchQuestion> roundQuestions = entry.getValue();
                
                log.info("TOURNAMENT 라운드 {} 봇 플레이 시작: roomId={}, questions={}", 
                        roundNo, roomId, roundQuestions.size());
                
                // 활성 봇만 플레이 (탈락하지 않은 봇)
                List<String> activeBots = participantRepository.findByRoomIdAndEliminatedFalse(roomId).stream()
                        .map(MatchParticipant::getUserId)
                        .filter(userId -> userId.startsWith("BOT_"))
                        .collect(Collectors.toList());
                
                for (String botUserId : activeBots) {
                    playQuestions(roomId, botUserId, roundQuestions, MatchMode.TOURNAMENT);
                }
                
                // 라운드 종료 이벤트는 VersusService에서 처리
            }
            
            log.info("TOURNAMENT 봇 자동 플레이 완료: roomId={}", roomId);
            
        } catch (Exception e) {
            log.error("TOURNAMENT 봇 자동 플레이 실패: roomId={}, error={}", roomId, e.getMessage(), e);
        }
    }

    // ========== GOLDENBELL 모드 ==========

    /**
     * GOLDENBELL 봇 자동 플레이 시뮬레이션 (비동기)
     * - 모든 생존 봇이 각 라운드의 문제를 자동으로 풀고 답안 제출
     */
    @Async
    public void simulateGoldenbellBotPlayAsync(Long roomId) {
        try {
            Thread.sleep(500);
            
            log.info("GOLDENBELL 봇 자동 플레이 시작: roomId={}", roomId);
            
            MatchRoom room = roomRepository.findById(roomId)
                    .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));
            
            // 문제 생성 (없으면)
            ensureQuestionsExist(room);
            
            // 모든 문제를 라운드별로 그룹화
            List<MatchQuestion> allQuestions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId);
            Map<Integer, List<MatchQuestion>> questionsByRound = allQuestions.stream()
                    .collect(Collectors.groupingBy(MatchQuestion::getRoundNo));
            
            // 라운드 순서대로 처리
            List<Integer> roundNos = new ArrayList<>(questionsByRound.keySet());
            Collections.sort(roundNos);
            
            for (Integer roundNo : roundNos) {
                List<MatchQuestion> roundQuestions = questionsByRound.get(roundNo);
                if (roundQuestions == null || roundQuestions.isEmpty()) {
                    continue;
                }
                
                // 생존 봇 찾기
                List<GoldenbellState> aliveStates = goldenbellStateRepository.findByRoomId(roomId).stream()
                        .filter(GoldenbellState::isAlive)
                        .collect(Collectors.toList());
                
                List<String> aliveBotUserIds = aliveStates.stream()
                        .map(GoldenbellState::getUserId)
                        .filter(userId -> userId.startsWith("BOT_"))
                        .collect(Collectors.toList());
                
                if (aliveBotUserIds.isEmpty()) {
                    log.info("GOLDENBELL 라운드 {}: 생존 봇 없음", roundNo);
                    continue;
                }
                
                log.info("GOLDENBELL 라운드 {} 봇 플레이 시작: roomId={}, aliveBots={}, questions={}", 
                        roomId, aliveBotUserIds.size(), roundQuestions.size());
                
                // 각 문제마다 모든 생존 봇이 답안 제출
                for (MatchQuestion question : roundQuestions) {
                    // 생존 봇 목록 재조회 (오답으로 탈락한 봇 제외)
                    List<GoldenbellState> currentAliveStates = goldenbellStateRepository.findByRoomId(roomId).stream()
                            .filter(GoldenbellState::isAlive)
                            .collect(Collectors.toList());
                    
                    List<String> currentAliveBots = currentAliveStates.stream()
                            .map(GoldenbellState::getUserId)
                            .filter(userId -> userId.startsWith("BOT_"))
                            .collect(Collectors.toList());
                    
                    for (String botUserId : currentAliveBots) {
                        // 봇이 이미 답안을 제출했는지 확인
                        boolean alreadyAnswered = answerRepository
                                .findByRoomIdAndQuestionIdAndUserId(roomId, question.getQuestionId(), botUserId)
                                .isPresent();
                        
                        if (!alreadyAnswered) {
                            boolean correct = playSingleQuestion(roomId, botUserId, question, MatchMode.GOLDENBELL);
                            
                            // 오답 시 즉시 탈락 처리
                            if (!correct) {
                                goldenbellStateRepository.findByRoomIdAndUserId(roomId, botUserId)
                                        .ifPresent(state -> {
                                            state.setAlive(false);
                                            goldenbellStateRepository.save(state);
                                            
                                            saveEvent(roomId, "PLAYER_ELIMINATED", Map.of(
                                                    "userId", botUserId,
                                                    "mode", "GOLDENBELL",
                                                    "round", question.getRoundNo(),
                                                    "reason", "INCORRECT_ANSWER"
                                            ));
                                        });
                            }
                        }
                    }
                }
                
                // 라운드 종료 후 부활전 처리 (VersusService 로직 활용)
                // R4 라운드 종료 시 부활전 체크
                if (roundNo == 4) {
                    long finalAliveCount = goldenbellStateRepository.findByRoomId(roomId).stream()
                            .filter(GoldenbellState::isAlive)
                            .count();
                    
                    if (finalAliveCount <= 5) {
                        // 부활전 로직은 VersusService에서 처리되므로 여기서는 이벤트만 기록
                        saveEvent(roomId, "REVIVAL_ROUND_START", Map.of(
                                "aliveCount", finalAliveCount
                        ));
                    }
                }
            }
            
            log.info("GOLDENBELL 봇 자동 플레이 완료: roomId={}", roomId);
            
        } catch (Exception e) {
            log.error("GOLDENBELL 봇 자동 플레이 실패: roomId={}, error={}", roomId, e.getMessage(), e);
        }
    }

    // ========== 공통 메서드 ==========

    /**
     * 문제 목록 플레이 (여러 문제를 순차적으로 플레이)
     */
    private void playQuestions(Long roomId, String botUserId, List<MatchQuestion> questions, MatchMode mode) {
        VersusBotConst.BotDifficulty difficulty = VersusBotConst.extractDifficulty(botUserId);
        int botScore = 0;
        int correctCount = 0;
        int totalCount = questions.size();
        
        for (MatchQuestion q : questions) {
            try {
                // 이미 답안을 제출했는지 확인
                boolean alreadyAnswered = answerRepository
                        .findByRoomIdAndQuestionIdAndUserId(roomId, q.getQuestionId(), botUserId)
                        .isPresent();
                
                if (alreadyAnswered) {
                    log.debug("봇이 이미 답안 제출함: roomId={}, questionId={}, botUserId={}", 
                            roomId, q.getQuestionId(), botUserId);
                    continue;
                }
                
                // 문제 시작 이벤트 기록 (처음 답안 제출 시)
                recordQuestionStartIfNeeded(roomId, q.getQuestionId(), botUserId);
                
                // 난이도별 딜레이
                int delayMs = difficulty.calculateDelay(random);
                Thread.sleep(delayMs);
                
                // 난이도별 정답 결정
                boolean correct = difficulty.decideCorrect(random);
                
                // 점수 계산
                int scoreDelta = calculateScore(q, correct, delayMs);
                botScore += scoreDelta;
                
                if (correct) {
                    correctCount++;
                }
                
                // 답안 저장
                saveBotAnswer(roomId, botUserId, q, correct, delayMs, scoreDelta);
                
                // 이벤트 기록
                saveEvent(roomId, "BOT_ANSWERED", Map.of(
                        "userId", botUserId,
                        "questionId", q.getQuestionId(),
                        "roundNo", q.getRoundNo(),
                        "phase", q.getPhase().name(),
                        "isCorrect", correct,
                        "timeMs", delayMs,
                        "scoreDelta", scoreDelta,
                        "totalScore", botScore
                ));
                
                saveEvent(roomId, "SCORE_UPDATED", Map.of(
                        "userId", botUserId,
                        "totalScore", botScore,
                        "correctCount", correctCount,
                        "totalCount", totalCount
                ));
                
                log.debug("봇 답안 제출: roomId={}, questionId={}, botUserId={}, correct={}, score={}", 
                        roomId, q.getQuestionId(), botUserId, correct, botScore);
                
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.warn("봇 플레이 중단: roomId={}", roomId);
                return;
            } catch (Exception e) {
                log.error("봇 답안 제출 실패: roomId={}, questionId={}, botUserId={}, error={}", 
                        roomId, q.getQuestionId(), botUserId, e.getMessage(), e);
            }
        }
        
        // 플레이 완료 이벤트
        saveEvent(roomId, "BOT_PLAY_COMPLETED", Map.of(
                "userId", botUserId,
                "mode", mode.name(),
                "totalScore", botScore,
                "correctCount", correctCount,
                "totalCount", totalCount,
                "accuracy", totalCount > 0 ? (double) correctCount / totalCount : 0.0
        ));
    }

    /**
     * 단일 문제 플레이
     * @return 정답 여부
     */
    private boolean playSingleQuestion(Long roomId, String botUserId, MatchQuestion question, MatchMode mode) {
        try {
            VersusBotConst.BotDifficulty difficulty = VersusBotConst.extractDifficulty(botUserId);
            
            // 문제 시작 이벤트 기록 (처음 답안 제출 시)
            recordQuestionStartIfNeeded(roomId, question.getQuestionId(), botUserId);
            
            // 난이도별 딜레이
            int delayMs = difficulty.calculateDelay(random);
            Thread.sleep(delayMs);
            
            // 난이도별 정답 결정
            boolean correct = difficulty.decideCorrect(random);
            
            // 점수 계산
            int scoreDelta = calculateScore(question, correct, delayMs);
            
            // 답안 저장
            saveBotAnswer(roomId, botUserId, question, correct, delayMs, scoreDelta);
            
            // 이벤트 기록
            saveEvent(roomId, "BOT_ANSWERED", Map.of(
                    "userId", botUserId,
                    "questionId", question.getQuestionId(),
                    "roundNo", question.getRoundNo(),
                    "phase", question.getPhase().name(),
                    "isCorrect", correct,
                    "timeMs", delayMs,
                    "scoreDelta", scoreDelta
            ));
            
            return correct;
            
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return false;
        } catch (Exception e) {
            log.error("단일 문제 플레이 실패: roomId={}, questionId={}, botUserId={}, error={}", 
                    roomId, question.getQuestionId(), botUserId, e.getMessage(), e);
            return false;
        }
    }

    /**
     * 점수 계산
     */
    private int calculateScore(MatchQuestion question, boolean correct, int timeMs) {
        if (!correct) {
            return 0;
        }
        
        int baseScore = 1000;
        int timeLimitMs = question.getTimeLimitSec() * 1000;
        int speedBonus = (int) Math.round((double) (timeLimitMs - timeMs) / timeLimitMs * 200);
        speedBonus = Math.max(0, Math.min(200, speedBonus)); // 0~200 제한
        
        return baseScore + speedBonus;
    }

    /**
     * 문제가 없으면 자동 생성 (fallback 포함)
     */
    @Transactional
    private void ensureQuestionsExist(MatchRoom room) {
        List<MatchQuestion> existing = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(room.getId());
        if (!existing.isEmpty()) {
            return;
        }
        
        log.info("문제가 없어서 자동 생성 시도: roomId={}", room.getId());
        generateQuestionsForRoom(room);
        
        // 여전히 없으면 더미 문제 생성
        existing = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(room.getId());
        if (existing.isEmpty()) {
            log.warn("문제 생성 실패, 더미 문제로 대체: roomId={}", room.getId());
            createDummyQuestionsForRoom(room.getId(), room.getMode());
        }
    }

    /**
     * 방에 문제 자동 생성 (study-service 연동)
     * 실패 시 더미 문제로 fallback
     */
    @Transactional
    private void generateQuestionsForRoom(MatchRoom room) {
        try {
            log.info("문제 자동 생성 시작: roomId={}", room.getId());
            
            List<VersusDtos.QuestionInfo> questionInfos = versusService.generateQuestionsFromScope(room, room.getScopeJson());
            
            if (questionInfos.isEmpty()) {
                throw new IllegalStateException("No questions generated from study-service");
            }
            
            List<MatchQuestion> questions = questionInfos.stream()
                    .map(q -> MatchQuestion.builder()
                            .roomId(room.getId())
                            .roundNo(q.roundNo() != null ? q.roundNo() : 1)
                            .phase(q.phase() != null ? q.phase() : MatchPhase.MAIN)
                            .orderNo(q.order() != null ? q.order() : 1)
                            .questionId(q.questionId())
                            .timeLimitSec(q.timeLimitSec() != null ? q.timeLimitSec() : 10)
                            .build())
                    .toList();
            
            questionRepository.saveAll(questions);
            
            saveEvent(room.getId(), "QUESTIONS_REGISTERED", Map.of(
                    "count", questions.size(),
                    "source", "STUDY_SERVICE"
            ));
            
            log.info("문제 자동 생성 완료 (study-service): roomId={}, count={}", room.getId(), questions.size());
            
        } catch (Exception e) {
            log.warn("study-service 연동 실패, 더미 문제로 대체합니다. roomId={}, error={}", 
                    room.getId(), e.getMessage());
            createDummyQuestionsForRoom(room.getId(), room.getMode());
        }
    }

    /**
     * 더미 문제 생성 (study-service 실패 시 fallback)
     */
    @Transactional
    public void createDummyQuestionsForRoom(Long roomId, MatchMode mode) {
        log.info("더미 문제 생성 시작: roomId={}, mode={}", roomId, mode);
        
        List<MatchQuestion> dummyQuestions = new ArrayList<>();
        
        if (mode == MatchMode.DUEL) {
            // DUEL: 10문제 (라운드 1)
            for (int i = 1; i <= 10; i++) {
                dummyQuestions.add(MatchQuestion.builder()
                        .roomId(roomId)
                        .roundNo(1)
                        .phase(MatchPhase.MAIN)
                        .orderNo(i)
                        .questionId(90000L + i)
                        .timeLimitSec(10)
                        .build());
            }
        } else if (mode == MatchMode.TOURNAMENT) {
            // TOURNAMENT: 3라운드 × 3문제 = 9문제
            for (int round = 1; round <= 3; round++) {
                for (int order = 1; order <= 3; order++) {
                    dummyQuestions.add(MatchQuestion.builder()
                            .roomId(roomId)
                            .roundNo(round)
                            .phase(MatchPhase.MAIN)
                            .orderNo(order)
                            .questionId(90000L + (round - 1) * 3 + order)
                            .timeLimitSec(10)
                            .build());
                }
            }
        } else if (mode == MatchMode.GOLDENBELL) {
            // GOLDENBELL: 8문제 (R1~R6: MAIN, R7~R8: FINAL)
            for (int i = 1; i <= 6; i++) {
                dummyQuestions.add(MatchQuestion.builder()
                        .roomId(roomId)
                        .roundNo(i)
                        .phase(MatchPhase.MAIN)
                        .orderNo(1)
                        .questionId(90000L + i)
                        .timeLimitSec(10)
                        .build());
            }
            // FINAL 라운드
            dummyQuestions.add(MatchQuestion.builder()
                    .roomId(roomId)
                    .roundNo(7)
                    .phase(MatchPhase.FINAL)
                    .orderNo(1)
                    .questionId(90007L)
                    .timeLimitSec(10)
                    .build());
            dummyQuestions.add(MatchQuestion.builder()
                    .roomId(roomId)
                    .roundNo(8)
                    .phase(MatchPhase.FINAL)
                    .orderNo(2)
                    .questionId(90008L)
                    .timeLimitSec(10)
                    .build());
        }
        
        if (!dummyQuestions.isEmpty()) {
            questionRepository.saveAll(dummyQuestions);
            
            saveEvent(roomId, "QUESTIONS_REGISTERED", Map.of(
                    "count", dummyQuestions.size(),
                    "source", "DUMMY_FALLBACK"
            ));
            
            log.info("더미 문제 생성 완료: roomId={}, count={}", roomId, dummyQuestions.size());
        }
    }

    /**
     * 봇 답안 저장
     */
    @Transactional
    protected void saveBotAnswer(Long roomId,
                                 String botUserId,
                                 MatchQuestion q,
                                 boolean correct,
                                 int timeMs,
                                 int scoreDelta) {
        MatchAnswer answer = MatchAnswer.builder()
                .roomId(roomId)
                .roundNo(q.getRoundNo())
                .phase(q.getPhase())
                .questionId(q.getQuestionId())
                .userId(botUserId)
                .submittedAt(Instant.now())
                .correct(correct)
                .timeMs(timeMs)
                .scoreDelta(scoreDelta)
                .build();
        
        answerRepository.save(answer);
    }

    /**
     * 문제 시작 이벤트 기록 (중복 방지)
     */
    private void recordQuestionStartIfNeeded(Long roomId, Long questionId, String userId) {
        // 이미 QUESTION_STARTED 이벤트가 있는지 확인
        List<MatchEvent> existingEvents = eventRepository.findByRoomIdAndEventType(roomId, "QUESTION_STARTED");
        boolean alreadyRecorded = existingEvents.stream()
                .anyMatch(event -> {
                    try {
                        Map<String, Object> payload = objectMapper.readValue(
                                event.getPayloadJson(), 
                                new com.fasterxml.jackson.core.type.TypeReference<Map<String, Object>>() {}
                        );
                        return Objects.equals(payload.get("questionId"), questionId) &&
                               Objects.equals(payload.get("userId"), userId);
                    } catch (Exception e) {
                        return false;
                    }
                });
        
        if (!alreadyRecorded) {
            saveEvent(roomId, "QUESTION_STARTED", Map.of(
                    "questionId", questionId,
                    "userId", userId,
                    "startedAt", Instant.now().toString()
            ));
        }
    }

    /**
     * 이벤트 저장
     */
    private void saveEvent(Long roomId, String eventType, Map<String, Object> payload) {
        try {
            String payloadJson = payload == null || payload.isEmpty()
                    ? null
                    : objectMapper.writeValueAsString(payload);
            
            MatchEvent event = MatchEvent.builder()
                    .roomId(roomId)
                    .eventType(eventType)
                    .payloadJson(payloadJson)
                    .build();
            
            eventRepository.save(event);
        } catch (Exception e) {
            log.warn("이벤트 저장 실패: roomId={}, eventType={}, error={}", 
                    roomId, eventType, e.getMessage());
        }
    }
}
