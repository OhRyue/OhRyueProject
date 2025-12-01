package com.OhRyue.certpilot.progress.service;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.WeekFields;
import java.util.Locale;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.domain.ReportWeekly;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.dto.StudyFlowCompleteReq;
import com.OhRyue.certpilot.progress.repository.ReportDailyRepository;
import com.OhRyue.certpilot.progress.repository.ReportWeeklyRepository;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@RequiredArgsConstructor
public class StudyFlowService {

    private static final ZoneId KST = ZoneId.of("Asia/Seoul");

    private final XpService xpService;
    private final ReportDailyRepository dailyRepository;
    private final ReportWeeklyRepository weeklyRepository;
    private final RankService rankService;

    @Transactional
    public void handleFlowComplete(StudyFlowCompleteReq req) {
        ExamMode mode = parseExamMode(req.examMode());
        String flowType = normalizeFlowType(req.flowType());

        // 1) 이번 플로우에 줄 XP 양 정의
        int xpDelta = computeFlowXp(mode, flowType);

        // 2) refId = (user, mode, flowType, topicId) → 이 조합에 대해 최초 1번만 XP
        String refId = buildRefId(req.userId(), mode, flowType, req.topicId());

        // 3) 이유 (reason) 도 flowType 기준
        XpReason reason = switch (flowType) {
            case "MICRO" -> XpReason.MICRO;
            case "REVIEW" -> XpReason.REVIEW;
            default -> XpReason.ETC;
        };

        // 4) XP 지급 (idempotent)
        UserXpWallet walletBefore = xpService.getWallet(req.userId());
        UserXpWallet walletAfter = xpService.addXp(req.userId(), xpDelta, reason, refId);

        long actualXpGained = walletAfter.getXpTotal() - walletBefore.getXpTotal();
        if (actualXpGained > 0) {
            log.info(
                "Flow complete - XP granted: userId={}, flowType={}, examMode={}, topicId={}, expectedXp={}, actualXp={}, newTotal={}, level={}",
                req.userId(), flowType, mode, req.topicId(), xpDelta, actualXpGained, walletAfter.getXpTotal(), walletAfter.getLevel()
            );
        } else {
            log.info(
                "Flow complete - XP already granted (idempotent): userId={}, flowType={}, examMode={}, topicId={}, refId={}",
                req.userId(), flowType, mode, req.topicId(), refId
            );
        }

        rankService.recomputeForUser(req.userId());

        // 5) Daily / Weekly 리포트에도 xpGained 누적 (문항 카운트는 ingest에서 처리)
        LocalDate today = LocalDate.now(KST);

        // daily
        ReportDaily daily = dailyRepository.findByUserIdAndDate(req.userId(), today)
            .orElse(ReportDaily.builder()
                .userId(req.userId())
                .date(today)
                .solvedCount(0)
                .correctCount(0)
                .timeSpentSec(0)
                .accuracy(java.math.BigDecimal.ZERO)
                .xpGained(0)
                .build());
        daily.setXpGained(daily.getXpGained() + xpDelta);
        dailyRepository.save(daily);

        // weekly
        String weekIso = isoWeek(today);
        ReportWeekly weekly = weeklyRepository.findByUserIdAndWeekIso(req.userId(), weekIso)
            .orElse(ReportWeekly.builder()
                .userId(req.userId())
                .weekIso(weekIso)
                .solvedCount(0)
                .correctCount(0)
                .timeSpentSec(0)
                .accuracy(java.math.BigDecimal.ZERO)
                .xpGained(0)
                .build());
        weekly.setXpGained(weekly.getXpGained() + xpDelta);
        weeklyRepository.save(weekly);
    }

    /* ================= 내부 유틸 ================= */

    private ExamMode parseExamMode(String mode) {
        try {
            return ExamMode.valueOf(mode.toUpperCase(Locale.ROOT));
        } catch (Exception ex) {
            return ExamMode.WRITTEN;
        }
    }

    private String normalizeFlowType(String flowType) {
        if (flowType == null || flowType.isBlank()) return "ETC";
        return flowType.toUpperCase(Locale.ROOT);
    }

    /**
     * 플로우별 XP 양 정의
     * 메인학습(MICRO/REVIEW)은 최초 1회만 경험치 지급(완전 정답 시)
     */
    private int computeFlowXp(ExamMode mode, String flowType) {
        // 메인학습 MICRO: 필기 150 XP, 실기 200 XP
        if ("MICRO".equals(flowType)) {
            return (mode == ExamMode.PRACTICAL) ? 200 : 150;
        }
        // 메인학습 REVIEW: 필기 200 XP, 실기 250 XP
        if ("REVIEW".equals(flowType)) {
            return (mode == ExamMode.PRACTICAL) ? 250 : 200;
        }
        return 0;
    }

    private String buildRefId(String userId, ExamMode mode, String flowType, Long topicId) {
        return "flow:"
            + userId + ":"
            + mode.name().toUpperCase(Locale.ROOT) + ":"
            + flowType + ":"
            + (topicId == null ? "unknown" : topicId);
    }

    private String isoWeek(LocalDate date) {
        WeekFields wf = WeekFields.ISO;
        int week = date.get(wf.weekOfWeekBasedYear());
        int year = date.get(wf.weekBasedYear());
        return String.format("%d-W%02d", year, week);
    }
}
