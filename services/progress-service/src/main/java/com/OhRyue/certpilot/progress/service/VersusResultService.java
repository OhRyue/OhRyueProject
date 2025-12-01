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
    public void recordVersusResult(VersusDtos.VersusResultRequest request) {
        if (request.participants() == null || request.participants().isEmpty()) {
            log.warn("No participants in versus result for roomId={}", request.roomId());
            return;
        }

        log.info("Recording versus result: mode={}, roomId={}, winner={}, participants={}, questionCount={}, durationMs={}",
            request.mode(), request.roomId(), request.winner(), request.participants().size(),
            request.questionCount(), request.durationMs());

        String mode = request.mode().toUpperCase();
        // Assist/Duel/Tournament/Goldenbell은 매번 지급되므로 refId를 null로 설정
        String refId = null;

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
                processParticipantReward(mode, request.winner(), participant, refId);
                
                // 배틀 기록 저장
                saveBattleRecord(request, participant, examMode);
                
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
            
            battleRecord = battleRecordRepository.save(battleRecord);
            
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
            log.error("Failed to save battle record for user {} in room {}: {}",
                participant.userId(), request.roomId(), e.getMessage(), e);
            // 배틀 기록 저장 실패는 치명적이지 않으므로 예외를 던지지 않음
        }
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
                UserXpWallet wallet = xpService.addXp(userId, xpDelta, XpReason.BATTLE, refId);
                log.info("Awarded {} XP to user {} for {} mode (winner: {}, rank: {}). New total: {} XP, level: {}", 
                    xpDelta, userId, mode, isWinner, participant.rank(), wallet.getXpTotal(), wallet.getLevel());
            } catch (Exception e) {
                log.error("Failed to award XP to user {}: {}", userId, e.getMessage(), e);
                throw e; // XP 지급 실패는 재시도 필요할 수 있으므로 예외 전파
            }
        } else {
            log.warn("XP reward is 0 for user {} in {} mode (winner: {}, rank: {})", 
                userId, mode, isWinner, participant.rank());
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

