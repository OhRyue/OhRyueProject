package com.OhRyue.certpilot.progress.service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.WeekFields;
import java.util.List;
import java.util.Locale;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.domain.ReportTagSkill;
import com.OhRyue.certpilot.progress.domain.ReportWeekly;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import com.OhRyue.certpilot.progress.domain.enums.QuestionType;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.dto.HookDtos.StudySubmitReq;
import com.OhRyue.certpilot.progress.repository.ReportDailyRepository;
import com.OhRyue.certpilot.progress.repository.ReportTagSkillRepository;
import com.OhRyue.certpilot.progress.repository.ReportWeeklyRepository;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@RequiredArgsConstructor
public class StudyActivityService {

    private static final ZoneId KST = ZoneId.of("Asia/Seoul");
    private static final int DEFAULT_TIME_PER_PROBLEM_SEC = 60;

    private final ReportDailyRepository dailyRepository;
    private final ReportWeeklyRepository weeklyRepository;
    private final ReportTagSkillRepository tagSkillRepository;
    private final XpService xpService;
    private final StreakService streakService;
    private final RankService rankService;
    private final BadgeService badgeService;

    @Transactional
    public void ingest(StudySubmitReq payload) {
        ExamMode examMode = parseExamMode(payload.examMode());
        QuestionType questionType = parseQuestionType(payload.questionType());
        boolean isCorrect = resolveCorrectness(payload.correct(), payload.score(), examMode);
        int xpDelta = 0;
        int timeSpent = DEFAULT_TIME_PER_PROBLEM_SEC;

        // XP Reason & refId
        XpReason reason = mapReason(payload.source());
        String refId = buildRefId(payload, examMode, questionType);

        // 보조학습(ASSIST): 매번 지급되도록 refId를 null로 설정, 정답일 때만 5 XP
        if (reason == XpReason.ASSIST) {
            refId = null; // ASSIST는 매번 지급되므로 idempotency 체크 비활성화
            xpDelta = computeXpDelta(examMode, questionType, isCorrect);
            if (xpDelta > 0) {
                UserXpWallet walletBefore = xpService.getWallet(payload.userId());
                UserXpWallet walletAfter = xpService.addXp(payload.userId(), xpDelta, reason, refId);
                long actualXpGained = walletAfter.getXpTotal() - walletBefore.getXpTotal();
                log.info(
                    "Assist answer - XP granted: userId={}, correct={}, xpGained={}, newTotal={}, level={}",
                    payload.userId(), isCorrect, actualXpGained, walletAfter.getXpTotal(), walletAfter.getLevel()
                );
            }
        }

        // XP & Rank (XP는 (userId, reason, refId) 기준으로 한 번만 지급)
        rankService.recomputeForUser(payload.userId());

        // Streak
        streakService.tickToday(payload.userId());

        // Daily report (문항 단위로 계속 누적)
        LocalDate today = LocalDate.now(KST);
        ReportDaily daily = dailyRepository.findByUserIdAndDate(payload.userId(), today)
            .orElse(ReportDaily.builder()
                .userId(payload.userId())
                .date(today)
                .solvedCount(0)
                .correctCount(0)
                .timeSpentSec(0)
                .accuracy(BigDecimal.ZERO)
                .xpGained(0)
                .build());
        daily.setSolvedCount(daily.getSolvedCount() + 1);
        if (isCorrect) {
            daily.setCorrectCount(daily.getCorrectCount() + 1);
        }
        daily.setTimeSpentSec(daily.getTimeSpentSec() + timeSpent);
        daily.setXpGained(daily.getXpGained() + xpDelta);
        daily.setAccuracy(calculateAccuracy(daily.getCorrectCount(), daily.getSolvedCount()));
        dailyRepository.save(daily);

        // Weekly report (마찬가지로 문항 단위)
        String weekIso = isoWeek(today);
        ReportWeekly weekly = weeklyRepository.findByUserIdAndWeekIso(payload.userId(), weekIso)
            .orElse(ReportWeekly.builder()
                .userId(payload.userId())
                .weekIso(weekIso)
                .solvedCount(0)
                .correctCount(0)
                .timeSpentSec(0)
                .accuracy(BigDecimal.ZERO)
                .xpGained(0)
                .build());
        weekly.setSolvedCount(weekly.getSolvedCount() + 1);
        if (isCorrect) {
            weekly.setCorrectCount(weekly.getCorrectCount() + 1);
        }
        weekly.setTimeSpentSec(weekly.getTimeSpentSec() + timeSpent);
        weekly.setXpGained(weekly.getXpGained() + xpDelta);
        weekly.setAccuracy(calculateAccuracy(weekly.getCorrectCount(), weekly.getSolvedCount()));
        weeklyRepository.save(weekly);

        // Tag skill (문항 단위, 태그별 능력치 누적)
        List<String> tags = payload.tags() == null ? List.of() : payload.tags().stream()
            .filter(tag -> tag != null && !tag.isBlank())
            .distinct()
            .toList();
        if (!tags.isEmpty()) {
            for (String tag : tags) {
                ReportTagSkill skill = tagSkillRepository
                    .findByUserIdAndTagAndExamMode(payload.userId(), tag, examMode)
                    .orElse(ReportTagSkill.builder()
                        .userId(payload.userId())
                        .tag(tag)
                        .examMode(examMode)
                        .correct(0)
                        .total(0)
                        .accuracy(BigDecimal.ZERO)
                        .build());
                skill.setTotal(skill.getTotal() + 1);
                if (isCorrect) {
                    skill.setCorrect(skill.getCorrect() + 1);
                }
                skill.setAccuracy(calculateAccuracy(skill.getCorrect(), skill.getTotal()));
                tagSkillRepository.save(skill);
            }
        }
        
        // 정답률 배지 체크 (report_daily의 accuracy 사용)
        // 일일 리포트가 업데이트된 후 정답률 80% 이상인지 확인
        try {
            ReportDaily updatedDaily = dailyRepository.findByUserIdAndDate(payload.userId(), today)
                .orElse(null);
            if (updatedDaily != null && updatedDaily.getAccuracy() != null) {
                double accuracy = updatedDaily.getAccuracy().doubleValue();
                if (accuracy >= 80.0) {
                    // 정답률 80% 이상이면 skill counter 업데이트 및 배지 체크
                    badgeService.updateSkillCounterOnStudyComplete(
                        payload.userId(), 
                        payload.examMode(), 
                        null,  // flowType은 없음 (개별 문제 제출)
                        accuracy
                    );
                }
            }
        } catch (Exception e) {
            // 배지 체크 실패는 치명적이지 않으므로 로깅만
            log.warn("Failed to check accuracy badge for user {}: {}", payload.userId(), e.getMessage());
        }
    }

    /* ====================== 내부 유틸 ====================== */

    private ExamMode parseExamMode(String mode) {
        try {
            return ExamMode.valueOf(mode.toUpperCase(Locale.ROOT));
        } catch (Exception ex) {
            return ExamMode.WRITTEN;
        }
    }

    private QuestionType parseQuestionType(String type) {
        try {
            return QuestionType.valueOf(type.toUpperCase(Locale.ROOT));
        } catch (Exception ex) {
            return QuestionType.MCQ;
        }
    }

    private boolean resolveCorrectness(Boolean correctFlag, Integer score, ExamMode examMode) {
        if (correctFlag != null) {
            return Boolean.TRUE.equals(correctFlag);
        }
        if (examMode == ExamMode.PRACTICAL && score != null) {
            return score >= 60;
        }
        return false;
    }

    private int computeXpDelta(ExamMode mode, QuestionType type, boolean correct) {
        // 보조학습: 정답만 경험치 지급, 오답은 0 XP
        if (!correct) {
            return 0;
        }
        return 5; // 정답 5 XP
    }

    /**
     * source 문자열을 XpReason 으로 매핑
     * - MICRO / REVIEW / ASSIST / BATTLE / ETC
     */
    private XpReason mapReason(String source) {
        if (source == null || source.isBlank()) return XpReason.ETC;
        String upper = source.toUpperCase(Locale.ROOT);
        if (upper.contains("MICRO")) return XpReason.MICRO;
        if (upper.contains("REVIEW")) return XpReason.REVIEW;
        if (upper.contains("ASSIST")) return XpReason.ASSIST;
        if (upper.contains("BATTLE") || upper.contains("VERSUS")) return XpReason.BATTLE;
        return XpReason.ETC;
    }

    /**
     * refId 생성 규칙 (XP idempotency용)
     *
     * 예시:
     *  study:ohryue:WRITTEN:MICRO:MCQ:2025-11-17
     */
    private String buildRefId(StudySubmitReq payload, ExamMode mode, QuestionType type) {
        String flow = normalizeFlowFromSource(payload.source());
        LocalDate today = LocalDate.now(KST);
        return "study:"
            + payload.userId() + ":"
            + mode.name().toUpperCase(Locale.ROOT) + ":"
            + flow + ":"
            + type.name().toUpperCase(Locale.ROOT) + ":"
            + today;
    }

    private String normalizeFlowFromSource(String source) {
        if (source == null || source.isBlank()) {
            return "ETC";
        }
        String upper = source.toUpperCase(Locale.ROOT);
        if (upper.contains("MICRO")) return "MICRO";
        if (upper.contains("REVIEW")) return "REVIEW";
        if (upper.contains("ASSIST")) return "ASSIST";
        if (upper.contains("BATTLE") || upper.contains("VERSUS")) return "BATTLE";
        return "ETC";
    }

    private BigDecimal calculateAccuracy(int correct, int total) {
        if (total == 0) return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        double ratio = (double) correct / total * 100.0;
        return BigDecimal.valueOf(ratio).setScale(2, RoundingMode.HALF_UP);
    }

    private String isoWeek(LocalDate date) {
        WeekFields wf = WeekFields.ISO;
        int week = date.get(wf.weekOfWeekBasedYear());
        int year = date.get(wf.weekBasedYear());
        return String.format("%d-W%02d", year, week);
    }
}
