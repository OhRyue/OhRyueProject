package com.OhRyue.certpilot.versus.service.battle.impl;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchPhase;
import com.OhRyue.certpilot.versus.domain.MatchQuestion;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.*;
import com.OhRyue.certpilot.versus.service.VersusService;
import com.OhRyue.certpilot.versus.service.battle.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

/**
 * GOLDENBELL 모드 전략 구현
 * 
 * 골든벨 이벤트 모드의 비즈니스 로직을 구현합니다.
 * - 자동 시작 (스케줄 기반)
 * - 최대 20명, 최소 1명
 * - WRITTEN/PRACTICAL 모드별 라운드 구성
 * - 패자부활 (생존자 ≤5명일 때)
 * - 전원 탈락 방지 (생존자 0명 시 문제 무효+재출제)
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class GoldenBellStrategy implements BattleModeStrategy {

    private static final int MIN_PLAYERS = 1;
    private static final int MAX_PLAYERS = 20;
    private static final int REVIVAL_THRESHOLD = 5; // 생존자 ≤5명일 때 부활전

    private final MatchQuestionRepository questionRepository;
    private final GoldenbellStateRepository goldenbellStateRepository;
    private final GoldenBellQuestionFinishServiceAdapter questionFinishService;
    private final GoldenBellMatchFinishServiceAdapter matchFinishService;

    @Override
    public MatchMode mode() {
        return MatchMode.GOLDENBELL;
    }

    @Override
    public int requiredPlayers() {
        return MIN_PLAYERS;
    }

    @Override
    public QuestionPlan buildQuestionPlan(MatchRoom room) {
        // TODO: 기존 VersusService의 문제 생성 로직 마이그레이션
        // WRITTEN: Round1(OX 2) → Round2(MCQ 2) → Round3(REVIVAL 1) → Round4(HARD 2)
        // PRACTICAL: Round1(SHORT 2) → Round2(SHORT 2) → Round3(REVIVAL 1) → Round4(FINAL 2)
        return QuestionPlan.builder()
                .totalQuestions(7) // WRITTEN/PRACTICAL 모두 7문제
                .totalRounds(4)
                .questions(List.of())
                .build();
    }

    @Override
    public ScoreResult score(MatchQuestion question, MatchAnswer answer, ScorePolicy policy) {
        // TODO: 기존 VersusService.evaluateScore() 로직 마이그레이션
        // GOLDENBELL은 속도 보너스 없음, 정답 여부만 중요
        return ScoreResult.builder()
                .correct(answer.isCorrect())
                .scoreDelta(answer.isCorrect() ? 1 : 0)
                .timeMs(answer.getTimeMs())
                .build();
    }

    @Override
    public ProgressDecision decideNext(MatchRoom room, String currentStepKey, VersusDtos.ScoreBoardResp scoreboard) {
        StepKey stepKey = parseStepKey(currentStepKey);
        
        int currentRound = stepKey.getRoundNo();
        int currentOrder = stepKey.getOrderNo();
        String currentPhase = stepKey.getPhase();

        // REVIVAL 페이즈는 1문제만
        if (MatchPhase.REVIVAL.name().equals(currentPhase)) {
            // REVIVAL 완료 후 다음 라운드로
            if (currentRound < 4) {
                String nextStepKey = String.format("%d:1:MAIN", currentRound + 1);
                return ProgressDecision.nextQuestion(nextStepKey);
            } else {
                // 마지막 라운드 완료 → 매치 종료
                return ProgressDecision.matchCompleted();
            }
        }

        // 라운드별 문제 수 확인
        int questionsInRound = getQuestionsInRound(currentRound, currentPhase);
        
        if (currentOrder < questionsInRound) {
            // 같은 라운드의 다음 문제
            String nextStepKey = String.format("%d:%d:%s", currentRound, currentOrder + 1, currentPhase);
            return ProgressDecision.nextQuestion(nextStepKey);
        } else {
            // 라운드의 마지막 문제 완료
            // 생존자 수 확인하여 REVIVAL 필요 여부 판단
            long aliveCount = goldenbellStateRepository.findByRoomId(room.getId()).stream()
                    .filter(state -> state.isAlive())
                    .count();

            if (aliveCount <= REVIVAL_THRESHOLD && currentRound <= 2) {
                // REVIVAL 페이즈로 진행
                String nextStepKey = String.format("%d:1:REVIVAL", currentRound + 1);
                return ProgressDecision.nextQuestion(nextStepKey);
            } else if (currentRound < 4) {
                // 다음 라운드로
                String nextStepKey = String.format("%d:1:MAIN", currentRound + 1);
                return ProgressDecision.nextQuestion(nextStepKey);
            } else {
                // 마지막 라운드 완료 → 매치 종료
                return ProgressDecision.matchCompleted();
            }
        }
    }

    @Override
    public EliminationDecision eliminate(MatchRoom room, String stepKey, VersusDtos.ScoreBoardResp scoreboard) {
        // GOLDENBELL: 오답/타임아웃 시 즉시 탈락
        // 이 로직은 finishQuestion에서 처리됨
        return EliminationDecision.none();
    }

    @Override
    public StepKey parseStepKey(String stepKey) {
        // GOLDENBELL: stepKey = "{round}:{order}:{phase}"
        try {
            String[] parts = stepKey.split(":");
            if (parts.length != 3) {
                throw new IllegalArgumentException("Invalid GOLDENBELL stepKey format: " + stepKey);
            }
            int roundNo = Integer.parseInt(parts[0]);
            int orderNo = Integer.parseInt(parts[1]);
            String phase = parts[2];
            
            return StepKey.builder()
                    .roundNo(roundNo)
                    .orderNo(orderNo)
                    .phase(phase)
                    .original(stepKey)
                    .build();
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid GOLDENBELL stepKey: " + stepKey, e);
        }
    }

    @Override
    public String buildStepKey(MatchQuestion question) {
        // GOLDENBELL: stepKey = "{round}:{order}:{phase}"
        String phase = question.getPhase() != null ? question.getPhase().name() : "MAIN";
        return String.format("%d:%d:%s", question.getRoundNo(), question.getOrderNo(), phase);
    }

    @Override
    public MatchQuestion getCurrentQuestion(MatchRoom room, String stepKey) {
        StepKey parsed = parseStepKey(stepKey);
        List<MatchQuestion> questions = questionRepository.findByRoomIdAndRoundNo(
                room.getId(), parsed.getRoundNo());
        return questions.stream()
                .filter(q -> q.getOrderNo().equals(parsed.getOrderNo()) &&
                           (q.getPhase() != null && q.getPhase().name().equals(parsed.getPhase())))
                .findFirst()
                .orElseThrow(() -> new IllegalStateException(
                        "Question not found: roomId=" + room.getId() + ", round=" + 
                        parsed.getRoundNo() + ", order=" + parsed.getOrderNo() + ", phase=" + parsed.getPhase()));
    }

    @Override
    public boolean canFinishQuestion(MatchRoom room, String stepKey) {
        // GOLDENBELL: 타임아웃 시 즉시 종료 (모든 답변을 기다리지 않음)
        return true;
    }

    @Override
    public QuestionFinishResult finishQuestion(MatchRoom room, String stepKey, 
                                               com.OhRyue.certpilot.versus.service.battle.BattleEngineService.FinishReason reason, 
                                               String triggeredByUserId) {
        // GoldenBellQuestionFinishService로 위임
        return questionFinishService.finishQuestion(room.getId(), stepKey, reason, triggeredByUserId);
    }

    @Override
    public MatchFinishResult finishMatch(MatchRoom room, 
                                         com.OhRyue.certpilot.versus.service.battle.BattleEngineService.FinishMatchReason reason) {
        // GoldenBellMatchFinishService로 위임
        return matchFinishService.finishMatch(room.getId(), reason);
    }

    // ========== Private Helper Methods ==========

    /**
     * 라운드별 문제 수 반환
     */
    private int getQuestionsInRound(int round, String phase) {
        if (MatchPhase.REVIVAL.name().equals(phase)) {
            return 1; // REVIVAL은 항상 1문제
        }
        
        switch (round) {
            case 1:
            case 2:
            case 4:
                return 2; // Round1, 2, 4는 각 2문제
            case 3:
                return 1; // Round3는 REVIVAL 1문제
            default:
                return 0;
        }
    }

    /**
     * 기존 GoldenBellQuestionFinishService를 래핑하는 어댑터
     */
    @Component
    @RequiredArgsConstructor
    private static class GoldenBellQuestionFinishServiceAdapter {
        private final GoldenBellQuestionFinishService questionFinishService;

        public QuestionFinishResult finishQuestion(Long roomId, String stepKey,
                                                   com.OhRyue.certpilot.versus.service.battle.BattleEngineService.FinishReason reason,
                                                   String triggeredByUserId) {
            GoldenBellQuestionFinishService.FinishReason adaptedReason = 
                    switch (reason) {
                        case SUBMIT -> GoldenBellQuestionFinishService.FinishReason.SUBMIT;
                        case TIMEOUT -> GoldenBellQuestionFinishService.FinishReason.TIMEOUT;
                    };

            GoldenBellQuestionFinishService.QuestionFinishResult result = 
                    questionFinishService.finishQuestion(roomId, stepKey, adaptedReason, triggeredByUserId);

            return QuestionFinishResult.builder()
                    .processed(result.isProcessed())
                    .skipped(result.isSkipped())
                    .alreadyFinished(result.isAlreadyFinished())
                    .nextStepKey(Optional.ofNullable(result.getNextStepKey()))
                    .matchCompleted(result.isMatchCompleted())
                    .build();
        }
    }


    /**
     * 기존 GoldenBellMatchFinishService를 래핑하는 어댑터
     */
    @Component
    @RequiredArgsConstructor
    private static class GoldenBellMatchFinishServiceAdapter {
        private final com.OhRyue.certpilot.versus.service.battle.impl.GoldenBellMatchFinishService matchFinishService;

        public MatchFinishResult finishMatch(Long roomId,
                                            com.OhRyue.certpilot.versus.service.battle.BattleEngineService.FinishMatchReason reason) {
            com.OhRyue.certpilot.versus.service.battle.impl.GoldenBellMatchFinishService.FinishMatchReason adaptedReason = 
                    switch (reason) {
                        case LAST_QUESTION_DONE -> com.OhRyue.certpilot.versus.service.battle.impl.GoldenBellMatchFinishService.FinishMatchReason.LAST_QUESTION_DONE;
                        case PLAYER_LEFT -> com.OhRyue.certpilot.versus.service.battle.impl.GoldenBellMatchFinishService.FinishMatchReason.PLAYER_LEFT;
                        case HEARTBEAT_TIMEOUT -> com.OhRyue.certpilot.versus.service.battle.impl.GoldenBellMatchFinishService.FinishMatchReason.HEARTBEAT_TIMEOUT;
                    };

            com.OhRyue.certpilot.versus.service.battle.impl.GoldenBellMatchFinishService.MatchFinishResult result = 
                    matchFinishService.finishMatch(roomId, adaptedReason);

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

