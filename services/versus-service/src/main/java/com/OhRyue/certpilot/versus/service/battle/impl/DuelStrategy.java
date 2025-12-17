package com.OhRyue.certpilot.versus.service.battle.impl;

import com.OhRyue.certpilot.versus.domain.*;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.*;
import com.OhRyue.certpilot.versus.service.RealtimeEventService;
import com.OhRyue.certpilot.versus.service.VersusService;
import com.OhRyue.certpilot.versus.service.battle.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

/**
 * DUEL 모드 전략 구현
 * 
 * 기존 DuelQuestionFinishService와 DuelMatchFinishService의 로직을
 * Strategy 패턴으로 마이그레이션한 구현입니다.
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class DuelStrategy implements BattleModeStrategy {

    private static final int REQUIRED_PLAYERS = 2;
    private static final int TOTAL_QUESTIONS = 10;
    private static final String EVENT_QUESTION_STARTED = "QUESTION_STARTED";
    private static final String EVENT_QUESTION_TIMEOUT_HANDLED = "QUESTION_TIMEOUT_HANDLED";
    private static final String EVENT_QUESTION_FINISHED = "QUESTION_FINISHED";
    private static final int QUESTION_INTERMISSION_SEC = 5;

    private final MatchQuestionRepository questionRepository;
    private final MatchAnswerRepository answerRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchEventRepository eventRepository;
    private final VersusService versusService;
    private final RealtimeEventService realtimeEventService;
    private final ObjectMapper objectMapper;
    private final DuelMatchFinishServiceAdapter matchFinishService; // 기존 DuelMatchFinishService 래퍼

    @Override
    public MatchMode mode() {
        return MatchMode.DUEL;
    }

    @Override
    public int requiredPlayers() {
        return REQUIRED_PLAYERS;
    }

    @Override
    public QuestionPlan buildQuestionPlan(MatchRoom room) {
        // TODO: 기존 VersusService의 문제 생성 로직 마이그레이션
        // 현재는 스캐폴딩만 제공
        return QuestionPlan.builder()
                .totalQuestions(TOTAL_QUESTIONS)
                .totalRounds(1)
                .questions(List.of())
                .build();
    }

    @Override
    public ScoreResult score(MatchQuestion question, com.OhRyue.certpilot.versus.service.battle.MatchAnswer answer, ScorePolicy policy) {
        // 기존 VersusService.evaluateScore() 로직 적용
        int limitMs = Math.max(1, Optional.ofNullable(question.getTimeLimitSec()).orElse(10)) * 1000;
        int timeMs = Math.max(0, answer.getTimeMs());
        
        // 시간 초과 시 정답도 오답 처리
        boolean correct = answer.isCorrect() && timeMs <= limitMs;
        int cappedTime = Math.min(timeMs, limitMs);
        int scoreDelta = 0;
        
        if (correct && policy.isSpeedBonusEnabled()) {
            // 속도 보너스 계산 (DUEL/TOURNAMENT는 속도 보너스 있음)
            double speedRatio = (double) (limitMs - cappedTime) / limitMs;
            int bonus = (int) Math.round(speedRatio * 50); // SPEED_BONUS_MAX = 50
            scoreDelta = 100 + bonus; // BASE_SCORE = 100
        } else if (correct) {
            // GOLDENBELL은 속도 보너스 없음
            scoreDelta = 1;
        }
        
        return ScoreResult.builder()
                .correct(correct)
                .scoreDelta(scoreDelta)
                .timeMs(cappedTime)
                .build();
    }

    @Override
    public ProgressDecision decideNext(MatchRoom room, String currentStepKey, VersusDtos.ScoreBoardResp scoreboard) {
        Long roomId = room.getId();
        StepKey stepKey = parseStepKey(currentStepKey);
        
        // 현재 문제 조회
        MatchQuestion currentQuestion = getCurrentQuestion(room, currentStepKey);
        
        // 다음 문제 찾기
        Optional<MatchQuestion> nextQuestion = findNextQuestion(roomId, currentQuestion);
        boolean matchCompleted = checkAllQuestionsAnswered(roomId);

        if (matchCompleted) {
            return ProgressDecision.matchCompleted();
        } else if (nextQuestion.isPresent()) {
            String nextStepKey = buildStepKey(nextQuestion.get());
            return ProgressDecision.nextQuestion(nextStepKey);
        } else {
            return ProgressDecision.none();
        }
    }

    @Override
    public EliminationDecision eliminate(MatchRoom room, String stepKey, VersusDtos.ScoreBoardResp scoreboard) {
        // DUEL은 탈락 없음
        return EliminationDecision.none();
    }

    @Override
    public StepKey parseStepKey(String stepKey) {
        // DUEL: stepKey = questionId (문자열)
        try {
            Long questionId = Long.parseLong(stepKey);
            return StepKey.builder()
                    .questionId(questionId)
                    .original(stepKey)
                    .build();
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid DUEL stepKey: " + stepKey);
        }
    }

    @Override
    public String buildStepKey(MatchQuestion question) {
        // DUEL: stepKey = questionId (문자열)
        return String.valueOf(question.getQuestionId());
    }

    @Override
    public MatchQuestion getCurrentQuestion(MatchRoom room, String stepKey) {
        StepKey parsed = parseStepKey(stepKey);
        return questionRepository.findByRoomIdAndQuestionId(room.getId(), parsed.getQuestionId())
                .orElseThrow(() -> new IllegalStateException("Question not found: " + parsed.getQuestionId()));
    }

    @Override
    public boolean canFinishQuestion(MatchRoom room, String stepKey) {
        // DUEL: 모든 참가자가 답안 제출 완료 또는 타임아웃
        Long roomId = room.getId();
        StepKey parsed = parseStepKey(stepKey);
        
        long totalParticipants = participantRepository.countByRoomId(roomId);
        long answeredParticipants = answerRepository.countByRoomIdAndQuestionId(roomId, parsed.getQuestionId());
        
        return answeredParticipants >= totalParticipants;
    }

    @Override
    public QuestionFinishResult finishQuestion(MatchRoom room, String stepKey, 
                                               BattleEngineService.FinishReason reason, 
                                               String triggeredByUserId) {
        Long roomId = room.getId();
        MatchQuestion question = getCurrentQuestion(room, stepKey);

        // 1. 미제출 유저 자동 오답 처리
        if (reason == BattleEngineService.FinishReason.TIMEOUT) {
            processUnansweredUsers(roomId, question);
        }

        // 2. 스코어보드 계산
        VersusDtos.ScoreBoardResp scoreboard = versusService.computeScoreboard(room);

        // 3. 질문 종료 이벤트 기록 (기존 DuelQuestionFinishService 로직)
        recordQuestionFinished(roomId, question.getQuestionId(), reason, triggeredByUserId);

        // 4. 다음 진행 판단
        ProgressDecision decision = decideNext(room, stepKey, scoreboard);

        if (decision.isMatchCompleted()) {
            // 매치 종료 처리
            try {
                matchFinishService.finishMatch(
                        roomId,
                        BattleEngineService.FinishMatchReason.LAST_QUESTION_DONE
                );
                return QuestionFinishResult.completed(Optional.empty(), true);
            } catch (Exception e) {
                log.error("Failed to finish match: roomId={}, stepKey={}, error={}",
                        roomId, stepKey, e.getMessage(), e);
                throw e;
            }
        } else if (decision.getNextStepKey().isPresent()) {
            // 다음 문제로 이동
            String nextStepKey = decision.getNextStepKey().get();
            Optional<MatchQuestion> nextQuestionOpt = findNextQuestion(roomId, question);
            if (nextQuestionOpt.isPresent()) {
                startNextQuestion(roomId, nextQuestionOpt.get());
            }
            return QuestionFinishResult.completed(Optional.of(nextStepKey), false);
        } else {
            return QuestionFinishResult.completed(Optional.empty(), false);
        }
    }

    @Override
    public MatchFinishResult finishMatch(MatchRoom room, BattleEngineService.FinishMatchReason reason) {
        // 기존 DuelMatchFinishService로 위임
        return matchFinishService.finishMatch(room.getId(), reason);
    }

    // ========== Private Helper Methods ==========

    private void processUnansweredUsers(Long roomId, MatchQuestion question) {
        Set<String> allParticipants = participantRepository.findByRoomId(roomId).stream()
                .map(MatchParticipant::getUserId)
                .collect(Collectors.toSet());

        Set<String> answeredUsers = answerRepository.findByRoomIdAndQuestionId(roomId, question.getQuestionId())
                .stream()
                .map(com.OhRyue.certpilot.versus.domain.MatchAnswer::getUserId)
                .collect(Collectors.toSet());

        Set<String> unansweredUsers = allParticipants.stream()
                .filter(u -> !answeredUsers.contains(u))
                .collect(Collectors.toSet());

        for (String userId : unansweredUsers) {
            // 이미 답안이 있으면 skip
            if (answerRepository.findByRoomIdAndQuestionIdAndUserId(roomId, question.getQuestionId(), userId).isPresent()) {
                continue;
            }

            // 타임아웃 오답 저장
            com.OhRyue.certpilot.versus.domain.MatchAnswer timeoutAnswer = com.OhRyue.certpilot.versus.domain.MatchAnswer.builder()
                    .roomId(roomId)
                    .questionId(question.getQuestionId())
                    .userId(userId)
                    .roundNo(question.getRoundNo())
                    .phase(question.getPhase())
                    .correct(false)
                    .timeMs(question.getTimeLimitSec() * 1000)
                    .scoreDelta(0)
                    .userAnswer("")
                    .build();
            answerRepository.save(timeoutAnswer);

            // ANSWER_TIMEOUT 이벤트 기록
            recordEvent(roomId, "ANSWER_TIMEOUT", Map.of(
                    "userId", userId,
                    "questionId", question.getQuestionId(),
                    "round", question.getRoundNo(),
                    "phase", question.getPhase().name(),
                    "timeLimitSec", question.getTimeLimitSec()
            ));
        }
    }

    private Optional<MatchQuestion> findNextQuestion(Long roomId, MatchQuestion currentQuestion) {
        List<MatchQuestion> allQuestions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId);
        
        for (int i = 0; i < allQuestions.size(); i++) {
            if (allQuestions.get(i).getQuestionId().equals(currentQuestion.getQuestionId())) {
                if (i + 1 < allQuestions.size()) {
                    return Optional.of(allQuestions.get(i + 1));
                }
                break;
            }
        }
        
        return Optional.empty();
    }

    private boolean checkAllQuestionsAnswered(Long roomId) {
        long participantCount = participantRepository.countByRoomId(roomId);
        List<MatchQuestion> allQuestions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId);
        if (allQuestions.isEmpty()) {
            return false;
        }
        
        for (MatchQuestion question : allQuestions) {
            long answeredCount = answerRepository.countByRoomIdAndQuestionId(roomId, question.getQuestionId());
            if (answeredCount < participantCount) {
                return false;
            }
        }
        
        return true;
    }

    private void startNextQuestion(Long roomId, MatchQuestion nextQuestion) {
        // 쉬는 시간 시작 이벤트 기록
        Instant intermissionStart = Instant.now();
        recordEvent(roomId, "INTERMISSION_STARTED", Map.of(
                "nextQuestionId", nextQuestion.getQuestionId(),
                "nextRoundNo", nextQuestion.getRoundNo(),
                "nextPhase", nextQuestion.getPhase().name(),
                "durationSec", QUESTION_INTERMISSION_SEC,
                "startedAt", intermissionStart.toString(),
                "questionStartAt", intermissionStart.plusSeconds(QUESTION_INTERMISSION_SEC).toString()
        ));

        // 다음 문제 시작 시간 계산
        Instant questionStartTime = intermissionStart.plusSeconds(QUESTION_INTERMISSION_SEC);

        // 다음 문제 시작 이벤트 기록
        recordEvent(roomId, EVENT_QUESTION_STARTED, Map.of(
                "questionId", nextQuestion.getQuestionId(),
                "roundNo", nextQuestion.getRoundNo(),
                "phase", nextQuestion.getPhase().name(),
                "startedAt", questionStartTime.toString(),
                "allParticipants", true
        ));

        // ROUND_COMPLETED 이벤트 기록
        recordEvent(roomId, "ROUND_COMPLETED", Map.of(
                "mode", "DUEL",
                "round", nextQuestion.getRoundNo(),
                "phase", nextQuestion.getPhase().name()
        ));
    }

    private void recordQuestionFinished(Long roomId, Long questionId, BattleEngineService.FinishReason reason, String triggeredByUserId) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("questionId", questionId);
        payload.put("reason", reason.name());
        payload.put("finishedAt", Instant.now().toString());
        if (triggeredByUserId != null) {
            payload.put("triggeredByUserId", triggeredByUserId);
        }

        recordEvent(roomId, EVENT_QUESTION_FINISHED, payload);

        // QUESTION_TIMEOUT_HANDLED 이벤트도 기록 (기존 로직과의 호환성)
        if (reason == BattleEngineService.FinishReason.TIMEOUT) {
            recordEvent(roomId, EVENT_QUESTION_TIMEOUT_HANDLED, Map.of(
                    "questionId", questionId,
                    "handledAt", Instant.now().toString()
            ));
        }
    }

    private void recordEvent(Long roomId, String type, Map<String, Object> payload) {
        try {
            String payloadJson = payload == null || payload.isEmpty()
                    ? null
                    : objectMapper.writeValueAsString(payload);
            
            MatchEvent event = MatchEvent.builder()
                    .roomId(roomId)
                    .eventType(type)
                    .payloadJson(payloadJson)
                    .build();
            
            MatchEvent savedEvent = eventRepository.save(event);
            
            // 실시간 브로드캐스트
            realtimeEventService.broadcastEvent(savedEvent);
        } catch (com.fasterxml.jackson.core.JsonProcessingException e) {
            log.warn("Failed to serialize payload for event: roomId={}, type={}, error={}", 
                    roomId, type, e.getMessage());
            // payload 없이 이벤트 저장 시도
            try {
                MatchEvent event = MatchEvent.builder()
                        .roomId(roomId)
                        .eventType(type)
                        .payloadJson(null)
                        .build();
                MatchEvent savedEvent = eventRepository.save(event);
                realtimeEventService.broadcastEvent(savedEvent);
            } catch (Exception ex) {
                log.warn("Failed to record event without payload: roomId={}, type={}, error={}", 
                        roomId, type, ex.getMessage());
            }
        } catch (Exception e) {
            log.warn("Failed to record event: roomId={}, type={}, error={}", 
                    roomId, type, e.getMessage());
        }
    }

    /**
     * 기존 DuelMatchFinishService를 래핑하는 어댑터
     * (전이 기간 동안 사용)
     */
    @Component
    @RequiredArgsConstructor
    private static class DuelMatchFinishServiceAdapter {
        private final com.OhRyue.certpilot.versus.service.DuelMatchFinishService duelMatchFinishService;

        public MatchFinishResult finishMatch(Long roomId, BattleEngineService.FinishMatchReason reason) {
            com.OhRyue.certpilot.versus.service.DuelMatchFinishService.FinishMatchReason adaptedReason = 
                    switch (reason) {
                        case LAST_QUESTION_DONE -> com.OhRyue.certpilot.versus.service.DuelMatchFinishService.FinishMatchReason.LAST_QUESTION_DONE;
                        case PLAYER_LEFT -> com.OhRyue.certpilot.versus.service.DuelMatchFinishService.FinishMatchReason.PLAYER_LEFT;
                        case HEARTBEAT_TIMEOUT -> com.OhRyue.certpilot.versus.service.DuelMatchFinishService.FinishMatchReason.HEARTBEAT_TIMEOUT;
                    };

            com.OhRyue.certpilot.versus.service.DuelMatchFinishService.MatchFinishResult result = 
                    duelMatchFinishService.finishMatch(roomId, adaptedReason);

            return MatchFinishResult.builder()
                    .processed(result.isProcessed())
                    .skipped(!result.isProcessed() && !result.isAlreadyFinished())
                    .alreadyFinished(result.isAlreadyFinished())
                    .winner(result.getWinner())
                    .xpGranted(result.isXpGranted())
                    .build();
        }
    }
}

