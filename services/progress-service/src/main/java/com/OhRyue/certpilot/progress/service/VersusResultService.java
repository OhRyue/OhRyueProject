package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.BattleAnswer;
import com.OhRyue.certpilot.progress.domain.BattleRecord;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.dto.VersusDtos;
import com.OhRyue.certpilot.progress.repository.BattleAnswerRepository;
import com.OhRyue.certpilot.progress.repository.BattleRecordRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class VersusResultService {

    private final XpService xpService;
    private final BadgeService badgeService;
    private final RankService rankService;
    private final BattleRecordRepository battleRecordRepository;
    private final BattleAnswerRepository battleAnswerRepository;

    // 모드별 XP 보상 (배틀: 경험치를 조금만 지급)
    private static final int XP_DUEL_WIN = 30;
    private static final int XP_DUEL_PARTICIPATE = 5;
    private static final int XP_TOURNAMENT_WIN = 100;
    private static final int XP_TOURNAMENT_PARTICIPATE = 10;
    private static final int XP_GOLDENBELL_WIN = 200;
    private static final int XP_GOLDENBELL_PARTICIPATE = 20;

    @Transactional
    public VersusDtos.VersusResultResponse recordVersusResult(VersusDtos.VersusResultRequest request) {
        if (request.participants() == null || request.participants().isEmpty()) {
            log.warn("No participants in versus result for roomId={}", request.roomId());
            return new VersusDtos.VersusResultResponse(
                request.mode(),
                request.roomId(),
                List.of()
            );
        }

        log.info("Recording versus result: mode={}, roomId={}, winner={}, participants={}, questionCount={}, durationMs={}",
            request.mode(), request.roomId(), request.winner(), request.participants().size(),
            request.questionCount(), request.durationMs());

        String mode = request.mode().toUpperCase();
        boolean isBotMatch = Boolean.TRUE.equals(request.botMatch());
        // (mode, roomId, userId) 단위로 idempotent 처리
        String refIdPrefix = mode + "-" + request.roomId() + "-";

        List<VersusDtos.XpResult> xpResults = new ArrayList<>();
        int successCount = 0;
        int failCount = 0;

        // ExamMode 파싱
        ExamMode examMode = null;
        if (request.examMode() != null && !request.examMode().isBlank()) {
            try {
                examMode = ExamMode.valueOf(request.examMode().toUpperCase());
            } catch (IllegalArgumentException e) {
                log.warn("Invalid examMode: {}, skipping", request.examMode());
            }
        }

        // 각 참가자에게 보상 지급 및 배틀 기록 저장
        for (VersusDtos.ParticipantResult participant : request.participants()) {
            try {
                VersusDtos.XpResult xpResult = processParticipantReward(
                    mode,
                    request.winner(),
                    participant,
                    refIdPrefix + participant.userId()
                );
                xpResults.add(xpResult);
                
                // 배틀 기록 저장
                saveBattleRecord(request, participant, examMode);
                
                // 배지 체크 (대전 결과 반영)
                boolean isWinner = participant.userId().equals(request.winner());
                badgeService.updateSkillCounterOnVersusResult(participant.userId(), mode, isWinner);
                
                successCount++;
            } catch (Exception e) {
                log.error("Failed to process reward for participant {} in room {}: {} - Cause: {}",
                    participant.userId(), request.roomId(), e.getMessage(), 
                    e.getCause() != null ? e.getCause().getMessage() : "N/A", e);
                failCount++;
                // 개별 참가자 실패해도 다른 참가자 처리는 계속
                // 실패한 참가자는 XP 결과에 포함하지 않음
            }
        }

        log.info("Completed versus result recording for roomId={}: success={}, failed={}, xpResults={}",
            request.roomId(), successCount, failCount, xpResults.size());

        return new VersusDtos.VersusResultResponse(
            request.mode(),
            request.roomId(),
            xpResults
        );
    }
    
    private void saveBattleRecord(
        VersusDtos.VersusResultRequest request,
        VersusDtos.ParticipantResult participant,
        ExamMode examMode
    ) {
        try {
            // 배틀 기록 저장
            BattleRecord battleRecord = BattleRecord.builder()
                .userId(participant.userId())
                .roomId(request.roomId())
                .mode(request.mode())
                .examMode(examMode)
                .score(participant.score())
                .rank(participant.rank())
                .correctCount(participant.correctCount())
                .totalCount(participant.totalCount())
                .totalTimeMs(participant.totalTimeMs())
                .isWinner(participant.userId().equals(request.winner()))
                .completedAt(Instant.now())
                .build();
            
            log.debug("Saving battle record: userId={}, roomId={}, mode={}, rank={}, score={}, correctCount={}, totalCount={}",
                participant.userId(), request.roomId(), request.mode(), participant.rank(), 
                participant.score(), participant.correctCount(), participant.totalCount());
            
            battleRecord = battleRecordRepository.save(battleRecord);
            log.debug("Battle record saved with id={}", battleRecord.getId());
            
            // 개별 답안 저장
            if (participant.answers() != null && !participant.answers().isEmpty()) {
                List<BattleAnswer> answers = new ArrayList<>();
                for (VersusDtos.AnswerDetail answerDetail : participant.answers()) {
                    BattleAnswer answer = BattleAnswer.builder()
                        .battleRecord(battleRecord)
                        .questionId(answerDetail.questionId())
                        .userAnswer(answerDetail.userAnswer())
                        .isCorrect(answerDetail.isCorrect())
                        .timeMs(answerDetail.timeMs())
                        .scoreDelta(answerDetail.scoreDelta())
                        .roundNo(answerDetail.roundNo())
                        .phase(answerDetail.phase())
                        .submittedAt(Instant.now())
                        .build();
                    answers.add(answer);
                }
                battleAnswerRepository.saveAll(answers);
                log.debug("Saved {} answers for battle record {} (user: {}, room: {})",
                    answers.size(), battleRecord.getId(), participant.userId(), request.roomId());
            }
            
            log.info("Saved battle record for user {} in room {} (mode: {}, rank: {})",
                participant.userId(), request.roomId(), request.mode(), participant.rank());
        } catch (Exception e) {
            log.error("Failed to save battle record for user {} in room {}: {} - Cause: {} - StackTrace: {}",
                participant.userId(), request.roomId(), e.getMessage(), 
                e.getCause() != null ? e.getCause().getMessage() : "N/A",
                e.getClass().getName(), e);
            // 배틀 기록 저장 실패는 치명적이지 않으므로 예외를 던지지 않음
            // 하지만 로그는 상세하게 기록
        }
    }

    private VersusDtos.XpResult processParticipantReward(
        String mode,
        String winner,
        VersusDtos.ParticipantResult participant,
        String refId
    ) {
        String userId = participant.userId();
        if (userId == null || userId.isBlank()) {
            log.warn("Invalid userId in participant result, skipping");
            throw new IllegalArgumentException("Invalid userId: " + userId);
        }

        boolean isWinner = userId.equals(winner);
        String reason = buildXpReason(mode, isWinner);

        boolean isBotUser = userId.startsWith("BOT_");
        // 1. XP 지급 전 지갑 상태 확인 (봇도 총 XP 표시용으로 조회)
        UserXpWallet walletBefore = xpService.getWallet(userId);
        long xpBefore = walletBefore.getXpTotal();
        int levelBefore = walletBefore.getLevel();

        if (isBotUser) {
            log.info("Skipping XP for bot user: userId={}, mode={}, roomIdPrefix={}", userId, mode, refId);
            return new VersusDtos.XpResult(userId, 0, reason, xpBefore, false);
        }

        // 2. XP 지급
        int xpDelta = calculateXpReward(mode, isWinner);
        if (xpDelta > 0) {
            try {
                UserXpWallet walletAfter = xpService.addXp(userId, xpDelta, XpReason.BATTLE, refId);
                long xpAfter = walletAfter.getXpTotal();
                int levelAfter = walletAfter.getLevel();
                boolean leveledUp = levelAfter > levelBefore;
                
                log.info("Awarded {} XP to user {} for {} mode (winner: {}, rank: {}). Before: {} XP (Lv{}), After: {} XP (Lv{}), LeveledUp: {}", 
                    xpDelta, userId, mode, isWinner, participant.rank(), 
                    xpBefore, levelBefore, xpAfter, levelAfter, leveledUp);
                
                // 3. 랭킹 재계산
                try {
                    rankService.recomputeForUser(userId);
                } catch (Exception e) {
                    log.warn("Failed to recompute rank for user {}: {}", userId, e.getMessage(), e);
                    // 랭킹 재계산 실패는 치명적이지 않으므로 계속 진행
                }

                // 4. 뱃지 평가 (배틀 관련 뱃지 체크)
                try {
                    badgeService.evaluate(userId);
                } catch (Exception e) {
                    log.warn("Failed to evaluate badges for user {}: {}", userId, e.getMessage(), e);
                    // 뱃지 평가 실패는 치명적이지 않으므로 계속 진행
                }

                return new VersusDtos.XpResult(
                    userId,
                    xpDelta,
                    reason,
                    xpAfter,
                    leveledUp
                );
            } catch (Exception e) {
                log.error("Failed to award XP to user {}: {} - Cause: {} - StackTrace: {}", 
                    userId, e.getMessage(), 
                    e.getCause() != null ? e.getCause().getMessage() : "N/A",
                    e.getClass().getName(), e);
                throw e; // XP 지급 실패는 재시도 필요할 수 있으므로 예외 전파
            }
        } else {
            log.warn("XP reward is 0 for user {} in {} mode (winner: {}, rank: {})", 
                userId, mode, isWinner, participant.rank());
            return new VersusDtos.XpResult(
                userId,
                0,
                reason,
                xpBefore,
                false
            );
        }
    }

    private String buildXpReason(String mode, boolean isWinner) {
        return switch (mode) {
            case "DUEL" -> isWinner ? "DUEL_WIN" : "DUEL_PARTICIPATE";
            case "TOURNAMENT" -> isWinner ? "TOURNAMENT_WIN" : "TOURNAMENT_PARTICIPATE";
            case "GOLDENBELL" -> isWinner ? "GOLDENBELL_WIN" : "GOLDENBELL_PARTICIPATE";
            default -> "BATTLE_PARTICIPATE";
        };
    }

    private int calculateXpReward(String mode, boolean isWinner) {
        return switch (mode) {
            case "DUEL" -> isWinner ? XP_DUEL_WIN : XP_DUEL_PARTICIPATE;
            case "TOURNAMENT" -> isWinner ? XP_TOURNAMENT_WIN : XP_TOURNAMENT_PARTICIPATE;
            case "GOLDENBELL" -> isWinner ? XP_GOLDENBELL_WIN : XP_GOLDENBELL_PARTICIPATE;
            default -> 0;
        };
    }
}

