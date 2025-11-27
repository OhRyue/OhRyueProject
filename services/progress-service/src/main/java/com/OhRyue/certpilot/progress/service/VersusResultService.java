package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.dto.VersusDtos;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@RequiredArgsConstructor
public class VersusResultService {

    private final XpService xpService;
    private final BadgeService badgeService;
    private final RankService rankService;

    // 모드별 XP 보상
    private static final int XP_DUEL_WIN = 100;
    private static final int XP_DUEL_PARTICIPATE = 50;
    private static final int XP_TOURNAMENT_WIN = 500;
    private static final int XP_TOURNAMENT_PARTICIPATE = 100;
    private static final int XP_GOLDENBELL_WIN = 1000;
    private static final int XP_GOLDENBELL_PARTICIPATE = 100;

    @Transactional
    public void recordVersusResult(VersusDtos.VersusResultRequest request) {
        if (request.participants() == null || request.participants().isEmpty()) {
            log.warn("No participants in versus result for roomId={}", request.roomId());
            return;
        }

        log.info("Recording versus result: mode={}, roomId={}, winner={}, participants={}, questionCount={}, durationMs={}",
            request.mode(), request.roomId(), request.winner(), request.participants().size(),
            request.questionCount(), request.durationMs());

        String mode = request.mode().toUpperCase();
        String refId = "versus:" + request.roomId() + ":" + mode;

        int successCount = 0;
        int failCount = 0;

        // 각 참가자에게 보상 지급
        for (VersusDtos.ParticipantResult participant : request.participants()) {
            try {
                processParticipantReward(mode, request.winner(), participant, refId);
                successCount++;
            } catch (Exception e) {
                log.error("Failed to process reward for participant {} in room {}: {}", 
                    participant.userId(), request.roomId(), e.getMessage(), e);
                failCount++;
                // 개별 참가자 실패해도 다른 참가자 처리는 계속
            }
        }

        log.info("Completed versus result recording for roomId={}: success={}, failed={}",
            request.roomId(), successCount, failCount);
    }

    private void processParticipantReward(
        String mode,
        String winner,
        VersusDtos.ParticipantResult participant,
        String refId
    ) {
        String userId = participant.userId();
        if (userId == null || userId.isBlank()) {
            log.warn("Invalid userId in participant result, skipping");
            return;
        }

        boolean isWinner = userId.equals(winner);

        // 1. XP 지급
        int xpDelta = calculateXpReward(mode, isWinner);
        if (xpDelta > 0) {
            try {
                xpService.addXp(userId, xpDelta, XpReason.BATTLE, refId);
                log.debug("Awarded {} XP to user {} for {} mode (winner: {}, rank: {})", 
                    xpDelta, userId, mode, isWinner, participant.rank());
            } catch (Exception e) {
                log.error("Failed to award XP to user {}: {}", userId, e.getMessage());
                throw e; // XP 지급 실패는 재시도 필요할 수 있으므로 예외 전파
            }
        }

        // 2. 랭킹 재계산
        try {
            rankService.recomputeForUser(userId);
        } catch (Exception e) {
            log.warn("Failed to recompute rank for user {}: {}", userId, e.getMessage());
            // 랭킹 재계산 실패는 치명적이지 않으므로 계속 진행
        }

        // 3. 뱃지 평가 (배틀 관련 뱃지 체크)
        try {
            badgeService.evaluate(userId);
        } catch (Exception e) {
            log.warn("Failed to evaluate badges for user {}: {}", userId, e.getMessage());
            // 뱃지 평가 실패는 치명적이지 않으므로 계속 진행
        }
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

