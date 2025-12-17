package com.OhRyue.certpilot.versus.service.battle.impl;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchParticipant;
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
 * TOURNAMENT 모드 전략 구현
 * 
 * 8인 토너먼트 모드의 비즈니스 로직을 구현합니다.
 * - 3라운드 × 3문제
 * - 라운드 종료 시 하위 탈락 (8→4→2→1)
 * - 우승자 1명만 보상 지급
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class TournamentStrategy implements BattleModeStrategy {

    private static final int REQUIRED_PLAYERS = 8;
    private static final int TOTAL_ROUNDS = 3;
    private static final int QUESTIONS_PER_ROUND = 3;

    private final MatchQuestionRepository questionRepository;
    private final MatchParticipantRepository participantRepository;
    private final VersusService versusService;
    private final TournamentRoundFinishServiceAdapter roundFinishService;
    private final TournamentMatchFinishServiceAdapter matchFinishService;

    @Override
    public MatchMode mode() {
        return MatchMode.TOURNAMENT;
    }

    @Override
    public int requiredPlayers() {
        return REQUIRED_PLAYERS;
    }

    @Override
    public QuestionPlan buildQuestionPlan(MatchRoom room) {
        // TODO: 기존 VersusService의 문제 생성 로직 마이그레이션
        // 3라운드 × 3문제 = 총 9문제
        return QuestionPlan.builder()
                .totalQuestions(TOTAL_ROUNDS * QUESTIONS_PER_ROUND)
                .totalRounds(TOTAL_ROUNDS)
                .questions(List.of())
                .build();
    }

    @Override
    public ScoreResult score(MatchQuestion question, MatchAnswer answer, ScorePolicy policy) {
        // TODO: 기존 VersusService.evaluateScore() 로직 마이그레이션
        return ScoreResult.builder()
                .correct(answer.isCorrect())
                .scoreDelta(answer.getScoreDelta())
                .timeMs(answer.getTimeMs())
                .build();
    }

    @Override
    public ProgressDecision decideNext(MatchRoom room, String currentStepKey, VersusDtos.ScoreBoardResp scoreboard) {
        StepKey stepKey = parseStepKey(currentStepKey);
        
        int currentRound = stepKey.getRoundNo();
        int currentOrder = stepKey.getOrderNo();

        // 같은 라운드의 다음 문제가 있는지 확인
        if (currentOrder < QUESTIONS_PER_ROUND) {
            // 같은 라운드의 다음 문제
            String nextStepKey = String.format("%d:%d", currentRound, currentOrder + 1);
            return ProgressDecision.nextQuestion(nextStepKey);
        } else {
            // 라운드의 마지막 문제 완료 → 라운드 종료 처리
            return ProgressDecision.builder()
                    .nextStepKey(Optional.empty())
                    .matchCompleted(false)
                    .roundCompleted(true)
                    .stateChanged(true)
                    .build();
        }
    }

    @Override
    public EliminationDecision eliminate(MatchRoom room, String stepKey, VersusDtos.ScoreBoardResp scoreboard) {
        StepKey parsed = parseStepKey(stepKey);
        int round = parsed.getRoundNo();

        // 라운드별 탈락 규칙: 8→4→2→1
        int[] survivorsPerRound = {0, 4, 2, 1}; // 인덱스 0은 사용 안 함, 1라운드 종료 후=4명, 2라운드 종료 후=2명, 3라운드 종료 후=1명
        
        if (round < 1 || round > TOTAL_ROUNDS) {
            return EliminationDecision.none();
        }

        // 현재 라운드 종료 후 생존자 수
        int targetSurvivors = survivorsPerRound[round];

        // 활성 참가자 조회
        List<MatchParticipant> activeParticipants = participantRepository.findByRoomIdAndEliminatedFalse(room.getId());
        
        if (activeParticipants.size() <= targetSurvivors) {
            // 이미 목표 생존자 수 이하이면 탈락 없음
            return EliminationDecision.none();
        }

        // 점수 기준 정렬 (내림차순)
        // TODO: 실제 점수 계산 로직 적용
        // 현재는 스코어보드에서 점수 가져오기
        List<String> sortedUserIds = scoreboard.items().stream()
                .sorted((a, b) -> Integer.compare(b.score(), a.score()))
                .map(VersusDtos.ScoreBoardItem::userId)
                .toList();

        // 하위 탈락 대상 선정
        List<String> eliminatedUserIds = sortedUserIds.subList(targetSurvivors, sortedUserIds.size());

        return EliminationDecision.eliminate(eliminatedUserIds, "ROUND_ELIMINATION");
    }

    @Override
    public StepKey parseStepKey(String stepKey) {
        // TOURNAMENT: stepKey = "{round}:{order}"
        try {
            String[] parts = stepKey.split(":");
            if (parts.length != 2) {
                throw new IllegalArgumentException("Invalid TOURNAMENT stepKey format: " + stepKey);
            }
            int roundNo = Integer.parseInt(parts[0]);
            int orderNo = Integer.parseInt(parts[1]);
            
            return StepKey.builder()
                    .roundNo(roundNo)
                    .orderNo(orderNo)
                    .original(stepKey)
                    .build();
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid TOURNAMENT stepKey: " + stepKey, e);
        }
    }

    @Override
    public String buildStepKey(MatchQuestion question) {
        // TOURNAMENT: stepKey = "{round}:{order}"
        return String.format("%d:%d", question.getRoundNo(), question.getOrderNo());
    }

    @Override
    public MatchQuestion getCurrentQuestion(MatchRoom room, String stepKey) {
        StepKey parsed = parseStepKey(stepKey);
        List<MatchQuestion> questions = questionRepository.findByRoomIdAndRoundNo(
                room.getId(), parsed.getRoundNo());
        return questions.stream()
                .filter(q -> q.getOrderNo().equals(parsed.getOrderNo()))
                .findFirst()
                .orElseThrow(() -> new IllegalStateException(
                        "Question not found: roomId=" + room.getId() + ", round=" + 
                        parsed.getRoundNo() + ", order=" + parsed.getOrderNo()));
    }

    @Override
    public boolean canFinishQuestion(MatchRoom room, String stepKey) {
        // TOURNAMENT: 타임아웃 시 즉시 종료 (모든 답변을 기다리지 않음)
        // 제한시간 종료 시 finishQuestion(..., TIMEOUT)이 호출되면 종료 가능
        return true; // 타임아웃 기반이므로 항상 종료 가능
    }

    @Override
    public QuestionFinishResult finishQuestion(MatchRoom room, String stepKey, 
                                               com.OhRyue.certpilot.versus.service.battle.BattleEngineService.FinishReason reason, 
                                               String triggeredByUserId) {
        Long roomId = room.getId();
        MatchQuestion question = getCurrentQuestion(room, stepKey);

        // 1. 미제출 유저 자동 오답 처리
        if (reason == com.OhRyue.certpilot.versus.service.battle.BattleEngineService.FinishReason.TIMEOUT) {
            processUnansweredUsers(roomId, question);
        }

        // 2. 스코어보드 계산
        VersusDtos.ScoreBoardResp scoreboard = versusService.computeScoreboard(room);

        // 3. 다음 진행 판단
        ProgressDecision decision = decideNext(room, stepKey, scoreboard);

        if (decision.isRoundCompleted()) {
            // 라운드 종료 처리
            try {
                roundFinishService.finishRound(roomId, question.getRoundNo());
                
                // 다음 라운드 또는 매치 종료 판단
                if (question.getRoundNo() >= TOTAL_ROUNDS) {
                    // 마지막 라운드 종료 → 매치 종료
                    matchFinishService.finishMatch(
                            roomId,
                            com.OhRyue.certpilot.versus.service.battle.BattleEngineService.FinishMatchReason.LAST_QUESTION_DONE
                    );
                    return QuestionFinishResult.completed(Optional.empty(), true);
                } else {
                    // 다음 라운드 시작
                    String nextStepKey = String.format("%d:1", question.getRoundNo() + 1);
                    startNextQuestion(roomId, nextStepKey);
                    return QuestionFinishResult.completed(Optional.of(nextStepKey), false);
                }
            } catch (Exception e) {
                log.error("Failed to finish round: roomId={}, round={}, error={}",
                        roomId, question.getRoundNo(), e.getMessage(), e);
                throw e;
            }
        } else if (decision.getNextStepKey().isPresent()) {
            // 같은 라운드의 다음 문제로 이동
            String nextStepKey = decision.getNextStepKey().get();
            startNextQuestion(roomId, nextStepKey);
            return QuestionFinishResult.completed(Optional.of(nextStepKey), false);
        } else {
            return QuestionFinishResult.completed(Optional.empty(), false);
        }
    }

    @Override
    public MatchFinishResult finishMatch(MatchRoom room, 
                                         com.OhRyue.certpilot.versus.service.battle.BattleEngineService.FinishMatchReason reason) {
        // TournamentMatchFinishService로 위임
        return matchFinishService.finishMatch(room.getId(), reason);
    }

    // ========== Private Helper Methods ==========

    private void processUnansweredUsers(Long roomId, MatchQuestion question) {
        // TODO: 미제출 유저 자동 오답 처리
        // 활성 참가자 중 답안을 제출하지 않은 유저에게 자동 오답 저장
    }

    private void startNextQuestion(Long roomId, String nextStepKey) {
        // TODO: 다음 문제 시작
        // QUESTION_STARTED 이벤트 기록 및 브로드캐스트
    }

    /**
     * 기존 TournamentRoundFinishService를 래핑하는 어댑터
     */
    @Component
    @RequiredArgsConstructor
    private static class TournamentRoundFinishServiceAdapter {
        private final TournamentRoundFinishService roundFinishService;

        public void finishRound(Long roomId, int round) {
            roundFinishService.finishRound(roomId, round);
        }
    }

    /**
     * 기존 TournamentMatchFinishService를 래핑하는 어댑터
     */
    @Component
    @RequiredArgsConstructor
    private static class TournamentMatchFinishServiceAdapter {
        private final TournamentMatchFinishService matchFinishService;

        public MatchFinishResult finishMatch(Long roomId, 
                                            com.OhRyue.certpilot.versus.service.battle.BattleEngineService.FinishMatchReason reason) {
            TournamentMatchFinishService.FinishMatchReason adaptedReason = 
                    switch (reason) {
                        case LAST_QUESTION_DONE -> TournamentMatchFinishService.FinishMatchReason.LAST_QUESTION_DONE;
                        case PLAYER_LEFT -> TournamentMatchFinishService.FinishMatchReason.PLAYER_LEFT;
                        case HEARTBEAT_TIMEOUT -> TournamentMatchFinishService.FinishMatchReason.HEARTBEAT_TIMEOUT;
                    };

            TournamentMatchFinishService.MatchFinishResult result = 
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

