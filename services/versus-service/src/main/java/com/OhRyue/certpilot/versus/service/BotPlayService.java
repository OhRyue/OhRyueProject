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
import java.util.Arrays;

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
    private final com.OhRyue.certpilot.versus.client.StudyServiceClient studyServiceClient;
    private final ObjectMapper objectMapper;
    private final Random random = new Random();

    // ========== DUEL 모드 ==========

    /**
     * DUEL 봇 자동 플레이 시뮬레이션 (비동기)
     * @param jwtToken JWT 토큰 (study-service 호출용)
     */
    @Async("taskExecutor")
    public void simulateDuelBotPlayAsync(Long roomId, String botUserId, String jwtToken) {
        try {
            // JWT 토큰을 ThreadLocal에 저장 (Feign Client에서 사용)
            if (jwtToken != null && !jwtToken.isBlank()) {
                com.OhRyue.certpilot.versus.config.AsyncConfig.setJwtToken(jwtToken);
            }

            try {
                Thread.sleep(500); // 트랜잭션 커밋 대기
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.warn("DUEL 봇 시작 대기 중 인터럽트 발생: roomId={}, botUserId={}", roomId, botUserId);
                return;
            }
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
            
            // 첫 번째 문제 시작 이벤트 기록 (봇전의 경우 자동으로 시작되므로)
            if (!questions.isEmpty() && room.getMode() == MatchMode.DUEL) {
                MatchQuestion firstQuestion = questions.get(0);
                // 첫 번째 문제의 시작 시점을 기록 (모든 참가자 공통)
                saveEvent(roomId, "QUESTION_STARTED", Map.of(
                        "questionId", firstQuestion.getQuestionId(),
                        "roundNo", firstQuestion.getRoundNo(),
                        "phase", firstQuestion.getPhase().name(),
                        "startedAt", Instant.now().toString(),
                        "allParticipants", true // 모든 참가자 공통 시작
                ));
                log.info("봇전 첫 번째 문제 시작 이벤트 기록: roomId={}, questionId={}, roundNo={}",
                        roomId, firstQuestion.getQuestionId(), firstQuestion.getRoundNo());
            }
            
            // 이벤트 기반 봇 플레이: 각 문제가 시작될 때만 답안 제출
            playQuestionsEventDriven(roomId, botUserId, questions, room.getMode());
            
            log.info("DUEL 봇 자동 플레이 완료: roomId={}, botUserId={}", roomId, botUserId);
            
        } finally {
            // JWT 토큰 정리
            com.OhRyue.certpilot.versus.config.AsyncConfig.clearJwtToken();
        }
    }

    // ========== TOURNAMENT 모드 ==========

    /**
     * TOURNAMENT 봇 자동 플레이 시뮬레이션 (비동기)
     * - 모든 봇이 각 라운드의 문제를 자동으로 풀고 답안 제출
     * @param jwtToken JWT 토큰 (study-service 호출용)
     */
    @Async("taskExecutor")
    public void simulateTournamentBotPlayAsync(Long roomId, String jwtToken) {
        try {
            // JWT 토큰을 ThreadLocal에 저장 (Feign Client에서 사용)
            if (jwtToken != null && !jwtToken.isBlank()) {
                com.OhRyue.certpilot.versus.config.AsyncConfig.setJwtToken(jwtToken);
            }

            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.warn("TOURNAMENT 봇 시작 대기 중 인터럽트 발생: roomId={}", roomId);
                return;
            }
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
                    // 이벤트 기반 봇 플레이: 각 문제가 시작될 때만 답안 제출
                    playQuestionsEventDriven(roomId, botUserId, roundQuestions, MatchMode.TOURNAMENT);
                }
                
                // 라운드 종료 이벤트는 VersusService에서 처리
            }
            
            log.info("TOURNAMENT 봇 자동 플레이 완료: roomId={}", roomId);
            
        } finally {
            // JWT 토큰 정리
            com.OhRyue.certpilot.versus.config.AsyncConfig.clearJwtToken();
        }
    }

    // ========== GOLDENBELL 모드 ==========

    /**
     * GOLDENBELL 봇 자동 플레이 시뮬레이션 (비동기)
     * - 이벤트 기반: 각 문제가 시작될 때만 답안 제출
     * - QUESTION_STARTED 이벤트를 기다렸다가 답 제출
     * @param jwtToken JWT 토큰 (study-service 호출용)
     */
    @Async("taskExecutor")
    public void simulateGoldenbellBotPlayAsync(Long roomId, String jwtToken) {
        try {
            // JWT 토큰을 ThreadLocal에 저장 (Feign Client에서 사용)
            if (jwtToken != null && !jwtToken.isBlank()) {
                com.OhRyue.certpilot.versus.config.AsyncConfig.setJwtToken(jwtToken);
            }

            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.warn("GOLDENBELL 봇 시작 대기 중 인터럽트 발생: roomId={}", roomId);
                return;
            }
            log.info("GOLDENBELL 봇 자동 플레이 시작: roomId={}", roomId);
            
            MatchRoom room = roomRepository.findById(roomId)
                    .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));
            
            // 문제 생성 (없으면)
            ensureQuestionsExist(room);
            
            // 모든 문제를 순서대로 가져오기
            List<MatchQuestion> allQuestions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId);
            
            if (allQuestions.isEmpty()) {
                log.warn("GOLDENBELL 문제가 없음: roomId={}", roomId);
                return;
            }
            
            // 첫 번째 문제 시작 이벤트 기록 (골든벨 봇전의 경우 자동으로 시작되므로)
            if (!allQuestions.isEmpty()) {
                MatchQuestion firstQuestion = allQuestions.get(0);
                // 첫 번째 문제의 시작 시점을 기록 (모든 참가자 공통)
                saveEvent(roomId, "QUESTION_STARTED", Map.of(
                        "questionId", firstQuestion.getQuestionId(),
                        "roundNo", firstQuestion.getRoundNo(),
                        "phase", firstQuestion.getPhase().name(),
                        "startedAt", Instant.now().toString(),
                        "allParticipants", true // 모든 참가자 공통 시작
                ));
                log.info("골든벨 봇전 첫 번째 문제 시작 이벤트 기록: roomId={}, questionId={}, roundNo={}",
                        roomId, firstQuestion.getQuestionId(), firstQuestion.getRoundNo());
            }
            
            // 각 문제에 대해 이벤트를 기다리며 봇이 답 제출
            for (MatchQuestion question : allQuestions) {
                // 문제가 시작될 때까지 대기 (QUESTION_STARTED 이벤트 확인)
                Instant questionStartTime = waitForQuestionStart(roomId, question.getQuestionId());
                
                if (questionStartTime == null) {
                    log.warn("문제 시작 이벤트를 기다리는 중 중단: roomId={}, questionId={}", 
                            roomId, question.getQuestionId());
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
                    log.info("GOLDENBELL 문제 {}: 생존 봇 없음", question.getQuestionId());
                    continue;
                }
                
                log.info("GOLDENBELL 문제 {} 봇 플레이 시작: roomId={}, aliveBots={}", 
                        question.getQuestionId(), roomId, aliveBotUserIds.size());
                
                // 모든 생존 봇이 답안 제출
                for (String botUserId : aliveBotUserIds) {
                    // REVIVAL phase 문제는 부활한 봇도 참여 가능 (이미 답안 제출했어도 재참여)
                    // 다른 phase는 이미 답안 제출했으면 스킵
                    boolean isRevivalPhase = question.getPhase() == MatchPhase.REVIVAL;
                    boolean alreadyAnswered = answerRepository
                            .findByRoomIdAndQuestionIdAndUserId(roomId, question.getQuestionId(), botUserId)
                            .isPresent();
                    
                    // REVIVAL phase가 아니고 이미 답안을 제출했다면 스킵
                    if (!isRevivalPhase && alreadyAnswered) {
                        continue;
                    }
                    
                    // REVIVAL phase이고 이미 답안을 제출했다면, 부활한 봇인지 확인
                    if (isRevivalPhase && alreadyAnswered) {
                        GoldenbellState state = goldenbellStateRepository.findByRoomIdAndUserId(roomId, botUserId)
                                .orElse(null);
                        // 부활한 봇이 아니면 스킵 (이미 처리한 문제)
                        if (state == null || !state.isRevived()) {
                            continue;
                        }
                        // 부활한 봇이면 기존 답안 삭제하고 재참여
                        answerRepository.findByRoomIdAndQuestionIdAndUserId(roomId, question.getQuestionId(), botUserId)
                                .ifPresent(answerRepository::delete);
                        log.info("부활한 봇이 REVIVAL 문제 재참여: roomId={}, questionId={}, botUserId={}", 
                                roomId, question.getQuestionId(), botUserId);
                    }
                    
                    // 문제 시작 시간을 기준으로 딜레이 계산
                    boolean correct = playSingleQuestionWithStartTime(
                            roomId, botUserId, question, MatchMode.GOLDENBELL, questionStartTime);
                    
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
                                            "phase", question.getPhase().name(),
                                            "reason", "INCORRECT_ANSWER"
                                    ));
                                });
                    }
                }
            }
            
            log.info("GOLDENBELL 봇 자동 플레이 완료: roomId={}", roomId);
            
        } catch (Exception e) {
            log.error("GOLDENBELL 봇 자동 플레이 실패: roomId={}, error={}", roomId, e.getMessage(), e);
        } finally {
            // JWT 토큰 정리
            com.OhRyue.certpilot.versus.config.AsyncConfig.clearJwtToken();
        }
    }
    
    /**
     * 문제 시작 이벤트를 기다림 (최대 60초)
     * @return 문제 시작 시간, 없으면 null
     */
    private Instant waitForQuestionStart(Long roomId, Long questionId) {
        int maxWaitSeconds = 60;
        int pollIntervalMs = 200; // 200ms마다 확인
        int maxPolls = (maxWaitSeconds * 1000) / pollIntervalMs;
        
        for (int i = 0; i < maxPolls; i++) {
            try {
                // QUESTION_STARTED 이벤트 확인
                List<MatchEvent> startEvents = eventRepository.findByRoomIdAndEventType(roomId, "QUESTION_STARTED");
                for (MatchEvent event : startEvents) {
                    try {
                        if (event.getPayloadJson() == null) continue;
                        Map<String, Object> payload = objectMapper.readValue(
                                event.getPayloadJson(), 
                                new com.fasterxml.jackson.core.type.TypeReference<Map<String, Object>>() {}
                        );
                        Object qId = payload.get("questionId");
                        if (qId != null && questionId.equals(Long.valueOf(qId.toString()))) {
                            // 문제 시작 시간 반환
                            String startedAtStr = (String) payload.get("startedAt");
                            if (startedAtStr != null) {
                                return Instant.parse(startedAtStr);
                            }
                            // startedAt이 없으면 이벤트 생성 시간 사용
                            return event.getCreatedAt();
                        }
                    } catch (Exception e) {
                        // 무시하고 계속
                    }
                }
                
                // 이벤트가 없으면 잠시 대기 후 다시 확인
                Thread.sleep(pollIntervalMs);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                return null;
            }
        }
        
        log.warn("문제 시작 이벤트를 찾지 못함: roomId={}, questionId={}", roomId, questionId);
        return null;
    }
    
    /**
     * 문제 시작 시간을 기준으로 딜레이를 계산하여 답안 제출
     */
    private boolean playSingleQuestionWithStartTime(Long roomId, String botUserId, MatchQuestion question, 
                                                     MatchMode mode, Instant questionStartTime) {
        try {
            VersusBotConst.BotDifficulty difficulty = VersusBotConst.extractDifficulty(botUserId);
            
            // 난이도별 딜레이 계산
            int delayMs = difficulty.calculateDelay(random);
            
            // 문제 시작 시간부터 딜레이만큼 대기
            Instant now = Instant.now();
            long elapsedMs = java.time.Duration.between(questionStartTime, now).toMillis();
            long remainingDelay = delayMs - elapsedMs;
            
            if (remainingDelay > 0) {
                Thread.sleep(remainingDelay);
            }
            
            // 실제 제출 시간 계산
            Instant submitTime = Instant.now();
            long actualTimeMs = java.time.Duration.between(questionStartTime, submitTime).toMillis();
            
            // 난이도별 정답 결정
            boolean correct = difficulty.decideCorrect(random);
            
            // 점수 계산
            int scoreDelta = calculateScore(question, correct, (int) actualTimeMs);
            
            log.info("봇 답안 제출: roomId={}, botUserId={}, questionId={}, correct={}, timeMs={}, scoreDelta={}, difficulty={}", 
                    roomId, botUserId, question.getQuestionId(), correct, actualTimeMs, scoreDelta, difficulty.getCode());
            
            // 답안 저장
            saveBotAnswer(roomId, botUserId, question, correct, (int) actualTimeMs, scoreDelta);
            
            // 저장 후 검증: 실제 저장된 값 확인
            MatchAnswer savedAnswer = answerRepository.findByRoomIdAndQuestionIdAndUserId(roomId, question.getQuestionId(), botUserId)
                    .orElse(null);
            if (savedAnswer != null) {
                log.info("봇 답안 저장 확인: roomId={}, botUserId={}, questionId={}, savedCorrect={}, savedScoreDelta={}", 
                        roomId, botUserId, question.getQuestionId(), savedAnswer.isCorrect(), savedAnswer.getScoreDelta());
            } else {
                log.error("봇 답안 저장 실패: roomId={}, botUserId={}, questionId={}", 
                        roomId, botUserId, question.getQuestionId());
            }
            
            // 이벤트 기록
            saveEvent(roomId, "BOT_ANSWERED", Map.of(
                    "userId", botUserId,
                    "questionId", question.getQuestionId(),
                    "roundNo", question.getRoundNo(),
                    "phase", question.getPhase().name(),
                    "isCorrect", correct,
                    "timeMs", actualTimeMs,
                    "scoreDelta", scoreDelta
            ));
            
            // DUEL 모드인 경우 진행 상태 확인 및 다음 문제 시작
            // 봇이 답안을 제출한 후에도 handleDuelProgress가 호출되어야 다음 문제로 넘어감
            if (mode == MatchMode.DUEL) {
                try {
                    // 스코어보드 계산 후 진행 상태 확인
                    com.OhRyue.certpilot.versus.domain.MatchRoom room = roomRepository.findById(roomId)
                            .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));
                    VersusDtos.ScoreBoardResp scoreboard = versusService.computeScoreboard(room);
                    // handleModeAfterAnswer를 호출하여 다음 문제로 진행
                    versusService.handleModeAfterAnswer(room, question, scoreboard);
                    log.debug("봇 답안 제출 후 진행 상태 확인 완료: roomId={}, questionId={}, botUserId={}", 
                            roomId, question.getQuestionId(), botUserId);
                } catch (Exception e) {
                    log.error("봇 답안 제출 후 진행 상태 확인 실패: roomId={}, questionId={}, botUserId={}, error={}", 
                            roomId, question.getQuestionId(), botUserId, e.getMessage(), e);
                }
            }
            
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

    // ========== 공통 메서드 ==========

    /**
     * 이벤트 기반 문제 플레이 (DUEL, TOURNAMENT 모드용)
     * - 각 문제가 시작될 때까지 기다렸다가 답안 제출
     * - QUESTION_STARTED 이벤트를 기다림
     */
    private void playQuestionsEventDriven(Long roomId, String botUserId, List<MatchQuestion> questions, MatchMode mode) {
        log.info("이벤트 기반 봇 플레이 시작: roomId={}, botUserId={}, mode={}, questions={}", 
                roomId, botUserId, mode, questions.size());
        
        for (MatchQuestion question : questions) {
            try {
                // 이미 답안을 제출했는지 확인
                boolean alreadyAnswered = answerRepository
                        .findByRoomIdAndQuestionIdAndUserId(roomId, question.getQuestionId(), botUserId)
                        .isPresent();
                
                if (alreadyAnswered) {
                    log.debug("봇이 이미 답안 제출함: roomId={}, questionId={}, botUserId={}", 
                            roomId, question.getQuestionId(), botUserId);
                    continue;
                }
                
                // 문제가 시작될 때까지 대기 (QUESTION_STARTED 이벤트 확인)
                Instant questionStartTime = waitForQuestionStart(roomId, question.getQuestionId());
                
                if (questionStartTime == null) {
                    log.warn("문제 시작 이벤트를 기다리는 중 중단: roomId={}, questionId={}", 
                            roomId, question.getQuestionId());
                    continue;
                }
                
                log.info("문제 {} 봇 플레이 시작: roomId={}, botUserId={}", 
                        question.getQuestionId(), roomId, botUserId);
                
                // 문제 시작 시간을 기준으로 답안 제출
                playSingleQuestionWithStartTime(roomId, botUserId, question, mode, questionStartTime);
                
            } catch (Exception e) {
                log.error("문제 플레이 중 오류: roomId={}, questionId={}, botUserId={}, error={}", 
                        roomId, question.getQuestionId(), botUserId, e.getMessage(), e);
            }
        }
        
        log.info("이벤트 기반 봇 플레이 완료: roomId={}, botUserId={}", roomId, botUserId);
        
        // 봇이 모든 문제에 답안을 제출한 후 매치 완료 체크 (DUEL 모드만)
        if (mode == MatchMode.DUEL) {
            try {
                boolean completed = versusService.checkAndCompleteMatchIfNeeded(roomId);
                if (completed) {
                    log.info("봇 플레이 완료 후 매치 완료 처리됨: roomId={}, botUserId={}", roomId, botUserId);
                }
            } catch (Exception e) {
                log.error("매치 완료 체크 중 오류: roomId={}, botUserId={}, error={}", 
                        roomId, botUserId, e.getMessage(), e);
            }
        }
    }

    /**
     * 문제 목록 플레이 (여러 문제를 순차적으로 플레이)
     * @deprecated DUEL/TOURNAMENT는 playQuestionsEventDriven 사용 권장
     */
    @Deprecated
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
            log.info("문제 자동 생성 시작: roomId={}, scopeJson={}", room.getId(), room.getScopeJson());
            
            if (room.getScopeJson() == null || room.getScopeJson().isBlank()) {
                log.error("scopeJson이 null이거나 비어있음: roomId={}, scopeJson={}", room.getId(), room.getScopeJson());
                throw new IllegalStateException("scopeJson is null or empty for roomId=" + room.getId());
            }
            
            log.info("study-service 호출 시도: roomId={}, scopeJson={}", room.getId(), room.getScopeJson());
            List<VersusDtos.QuestionInfo> questionInfos = versusService.generateQuestionsFromScope(room, room.getScopeJson());
            
            if (questionInfos.isEmpty()) {
                log.error("study-service에서 문제를 받지 못함: roomId={}, scopeJson={}", room.getId(), room.getScopeJson());
                throw new IllegalStateException("No questions generated from study-service");
            }
            
            log.info("study-service에서 문제 받음: roomId={}, count={}, questionIds={}", 
                    room.getId(), questionInfos.size(), 
                    questionInfos.stream().map(q -> q.questionId()).toList());
            
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
            
            log.info("문제 자동 생성 완료 (study-service): roomId={}, count={}, questionIds={}", 
                    room.getId(), questions.size(), 
                    questions.stream().map(q -> q.getQuestionId()).toList());
            
        } catch (org.springframework.web.server.ResponseStatusException e) {
            // ResponseStatusException은 HTTP 상태 코드가 포함된 예외
            // 원인 예외 확인 (UnknownHostException 등)
            Throwable cause = e.getCause();
            if (cause != null && cause.getCause() instanceof java.net.UnknownHostException) {
                java.net.UnknownHostException uhe = (java.net.UnknownHostException) cause.getCause();
                log.error("study-service 호스트를 찾을 수 없음 (UnknownHostException): roomId={}, scopeJson={}, host={}", 
                        room.getId(), room.getScopeJson(), uhe.getMessage());
                log.error("study-service가 실행 중이지 않거나 Eureka에 등록되지 않았을 수 있습니다. 네트워크 설정을 확인하세요.");
            }
            log.error("study-service 연동 실패 (ResponseStatusException): roomId={}, scopeJson={}, status={}, reason={}", 
                    room.getId(), room.getScopeJson(), e.getStatusCode(), e.getReason());
            log.error("예외 상세 정보: ", e);
            log.warn("더미 문제 생성 시작: roomId={}, mode={} (study-service 연동 실패로 인한 fallback)", room.getId(), room.getMode());
            createDummyQuestionsForRoom(room.getId(), room.getMode());
        } catch (feign.RetryableException e) {
            // Feign RetryableException (네트워크 오류, 호스트를 찾을 수 없음 등)
            Throwable cause = e.getCause();
            if (cause instanceof java.net.UnknownHostException) {
                java.net.UnknownHostException uhe = (java.net.UnknownHostException) cause;
                log.error("study-service 호스트를 찾을 수 없음 (UnknownHostException): roomId={}, scopeJson={}, host={}", 
                        room.getId(), room.getScopeJson(), uhe.getMessage());
                log.error("study-service가 실행 중이지 않거나 Eureka에 등록되지 않았을 수 있습니다. 네트워크 설정을 확인하세요.");
            }
            log.error("study-service 연동 실패 (RetryableException): roomId={}, scopeJson={}, message={}", 
                    room.getId(), room.getScopeJson(), e.getMessage());
            log.error("예외 상세 정보: ", e);
            log.warn("더미 문제 생성 시작: roomId={}, mode={} (study-service 연동 실패로 인한 fallback)", room.getId(), room.getMode());
            createDummyQuestionsForRoom(room.getId(), room.getMode());
        } catch (feign.FeignException e) {
            // Feign Client 예외 (네트워크 오류, 서비스 미응답 등)
            log.error("study-service 연동 실패 (FeignException): roomId={}, scopeJson={}, status={}, message={}, content={}", 
                    room.getId(), room.getScopeJson(), e.status(), e.getMessage(), 
                    e.contentUTF8() != null ? e.contentUTF8().substring(0, Math.min(500, e.contentUTF8().length())) : "null");
            log.error("예외 상세 정보: ", e);
            log.warn("더미 문제 생성 시작: roomId={}, mode={} (study-service 연동 실패로 인한 fallback)", room.getId(), room.getMode());
            createDummyQuestionsForRoom(room.getId(), room.getMode());
        } catch (Exception e) {
            // 기타 예외
            Throwable cause = e.getCause();
            if (cause instanceof java.net.UnknownHostException) {
                java.net.UnknownHostException uhe = (java.net.UnknownHostException) cause;
                log.error("study-service 호스트를 찾을 수 없음 (UnknownHostException): roomId={}, scopeJson={}, host={}", 
                        room.getId(), room.getScopeJson(), uhe.getMessage());
                log.error("study-service가 실행 중이지 않거나 Eureka에 등록되지 않았을 수 있습니다. 네트워크 설정을 확인하세요.");
            }
            log.error("study-service 연동 실패 (기타 예외): roomId={}, scopeJson={}, errorClass={}, errorMessage={}", 
                    room.getId(), room.getScopeJson(), e.getClass().getName(), e.getMessage());
            log.error("예외 상세 정보: ", e);
            if (cause != null) {
                log.error("원인 예외: causeClass={}, causeMessage={}", cause.getClass().getName(), cause.getMessage());
            }
            log.warn("더미 문제 생성 시작: roomId={}, mode={} (study-service 연동 실패로 인한 fallback)", room.getId(), room.getMode());
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
            // GOLDENBELL: 라운드 1(OX 2), 라운드 2(MCQ 2), 라운드 3(부활전 MCQ 1), 라운드 4(FINAL SHORT 1 + LONG 1)
            // 라운드 1: OX 2문제 (order 1, 2)
            dummyQuestions.add(MatchQuestion.builder()
                    .roomId(roomId)
                    .roundNo(1)
                    .phase(MatchPhase.MAIN)
                    .orderNo(1)
                    .questionId(90001L)
                    .timeLimitSec(10)
                    .build());
            dummyQuestions.add(MatchQuestion.builder()
                    .roomId(roomId)
                    .roundNo(1)
                    .phase(MatchPhase.MAIN)
                    .orderNo(2)
                    .questionId(90002L)
                    .timeLimitSec(10)
                    .build());
            // 라운드 2: MCQ 2문제 (order 1, 2)
            dummyQuestions.add(MatchQuestion.builder()
                    .roomId(roomId)
                    .roundNo(2)
                    .phase(MatchPhase.MAIN)
                    .orderNo(1)
                    .questionId(90003L)
                    .timeLimitSec(10)
                    .build());
            dummyQuestions.add(MatchQuestion.builder()
                    .roomId(roomId)
                    .roundNo(2)
                    .phase(MatchPhase.MAIN)
                    .orderNo(2)
                    .questionId(90004L)
                    .timeLimitSec(10)
                    .build());
            // 라운드 3: MCQ 1문제 (REVIVAL) - 패자부활전 (15초)
            dummyQuestions.add(MatchQuestion.builder()
                    .roomId(roomId)
                    .roundNo(3)
                    .phase(MatchPhase.REVIVAL)
                    .orderNo(1)
                    .questionId(90005L)
                    .timeLimitSec(15)
                    .build());
            // 라운드 4: SHORT 1문제 + LONG 1문제 (FINAL, order 1, 2) (30초)
            dummyQuestions.add(MatchQuestion.builder()
                    .roomId(roomId)
                    .roundNo(4)
                    .phase(MatchPhase.FINAL)
                    .orderNo(1)
                    .questionId(90006L)
                    .timeLimitSec(30)
                    .build());
            dummyQuestions.add(MatchQuestion.builder()
                    .roomId(roomId)
                    .roundNo(4)
                    .phase(MatchPhase.FINAL)
                    .orderNo(2)
                    .questionId(90007L)
                    .timeLimitSec(30)
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
        // 단답식/서술형을 위한 답안 텍스트 생성
        String userAnswerText = generateBotAnswerText(q.getQuestionId(), correct);
        
        // 기존 답안이 있으면 업데이트, 없으면 생성
        MatchAnswer answer = answerRepository.findByRoomIdAndQuestionIdAndUserId(roomId, q.getQuestionId(), botUserId)
                .orElse(MatchAnswer.builder()
                        .roomId(roomId)
                        .questionId(q.getQuestionId())
                        .userId(botUserId)
                        .build());
        
        answer.setRoundNo(q.getRoundNo());
        answer.setPhase(q.getPhase());
        answer.setSubmittedAt(Instant.now());
        answer.setCorrect(correct);
        answer.setTimeMs(timeMs);
        answer.setScoreDelta(scoreDelta);
        answer.setUserAnswer(userAnswerText);
        
        answerRepository.save(answer);
        answerRepository.flush(); // 즉시 DB에 반영
        
        log.debug("봇 답안 저장 완료: roomId={}, botUserId={}, questionId={}, correct={}, scoreDelta={}, userAnswer={}", 
                roomId, botUserId, q.getQuestionId(), correct, scoreDelta, userAnswerText);
    }

    /**
     * 봇 답안 텍스트 생성 (단답식/서술형용)
     * OX/MCQ는 label을 반환하고, SHORT/LONG은 텍스트를 생성
     */
    private String generateBotAnswerText(Long questionId, boolean correct) {
        try {
            // study-service에서 문제 정보 가져오기
            com.OhRyue.certpilot.versus.client.StudyServiceClient.QuestionDto questionDto = 
                    studyServiceClient.getQuestion(questionId);
            
            if (questionDto == null) {
                // 문제 정보를 가져올 수 없으면 더미 답안 생성
                return correct ? "정답" : "오답";
            }
            
            String questionType = questionDto.type(); // OX, MCQ, SHORT, LONG
            String correctAnswer = questionDto.answerKey(); // 정답 키
            
            // OX/MCQ는 label 반환
            if ("OX".equals(questionType) || "MCQ".equals(questionType)) {
                if (correct && correctAnswer != null) {
                    // 정답인 경우 정답 키 반환
                    return correctAnswer;
                } else {
                    // 오답인 경우 반드시 정답이 아닌 답 선택
                    if ("OX".equals(questionType)) {
                        // OX: 정답이 "O"면 "X", 정답이 "X"면 "O"
                        if (correctAnswer != null && correctAnswer.trim().equalsIgnoreCase("O")) {
                            return "X";
                        } else if (correctAnswer != null && correctAnswer.trim().equalsIgnoreCase("X")) {
                            return "O";
                        } else {
                            // 정답 키를 알 수 없으면 랜덤
                            return random.nextBoolean() ? "O" : "X";
                        }
                    } else {
                        // MCQ: 정답이 아닌 선택지 중 랜덤 선택
                        String[] allLabels = {"A", "B", "C", "D", "E"};
                        if (correctAnswer != null) {
                            String correctLabel = correctAnswer.trim().toUpperCase();
                            List<String> wrongLabels = Arrays.stream(allLabels)
                                    .filter(label -> !label.equals(correctLabel))
                                    .collect(Collectors.toList());
                            if (!wrongLabels.isEmpty()) {
                                return wrongLabels.get(random.nextInt(wrongLabels.size()));
                            }
                        }
                        // 정답 키를 알 수 없으면 랜덤
                        return allLabels[random.nextInt(allLabels.length)];
                    }
                }
            }
            
            // SHORT/LONG은 텍스트 생성
            if ("SHORT".equals(questionType) || "LONG".equals(questionType)) {
                if (correct && questionDto.answerKey() != null) {
                    // 정답인 경우 정답 키 사용 (일부만 사용하여 자연스럽게)
                    String answerKey = questionDto.answerKey();
                    // 정답이 너무 길면 일부만 사용
                    if (answerKey.length() > 50 && "SHORT".equals(questionType)) {
                        return answerKey.substring(0, Math.min(30, answerKey.length())) + "...";
                    }
                    return answerKey;
                } else {
                    // 오답인 경우 랜덤 텍스트 생성
                    String[] wrongAnswers = {
                        "데이터베이스", "정규화", "인덱스", "트랜잭션", "무결성",
                        "쿼리", "스키마", "뷰", "프로시저", "트리거",
                        "관계형", "NoSQL", "분산", "복제", "백업"
                    };
                    String base = wrongAnswers[random.nextInt(wrongAnswers.length)];
                    if ("LONG".equals(questionType)) {
                        // 서술형은 더 긴 텍스트
                        return base + "에 대한 설명입니다. " + 
                               wrongAnswers[random.nextInt(wrongAnswers.length)] + 
                               "와 관련이 있습니다.";
                    }
                    return base;
                }
            }
            
            return correct ? "정답" : "오답";
            
        } catch (Exception e) {
            log.warn("봇 답안 텍스트 생성 실패: questionId={}, error={}", questionId, e.getMessage());
            // 실패 시 기본값
            return correct ? "정답" : "오답";
        }
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
