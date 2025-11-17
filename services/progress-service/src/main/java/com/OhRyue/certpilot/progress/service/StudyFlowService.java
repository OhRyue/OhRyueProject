// src/main/java/com/OhRyue/certpilot/progress/service/StudyFlowService.java
package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.domain.ReportWeekly;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.dto.StudyFlowCompleteReq;
import com.OhRyue.certpilot.progress.repository.ReportDailyRepository;
import com.OhRyue.certpilot.progress.repository.ReportWeeklyRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.WeekFields;
import java.util.Locale;

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

        // 1) 이번 플로우에 줄 XP 양 정의 (원하시는대로 조정 가능)
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
        xpService.addXp(req.userId(), xpDelta, reason, refId);
        rankService.recomputeForUser(req.userId());

        // 5) Daily / Weekly 리포트에도 xpGained 누적 (문항 카운트는 기존 ingest에서 처리)
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
     * - 예시는 임의 값입니다. 마음에 드는 값으로 조정하셔도 됩니다.
     */
    private int computeFlowXp(ExamMode mode, String flowType) {
        // 추측 기반 예시값입니다. 원하시면 숫자만 바꾸시면 됩니다.
        if ("MICRO".equals(flowType)) {
            return (mode == ExamMode.PRACTICAL) ? 80 : 60;  // 실기 Micro 조금 더 크게
        }
        if ("REVIEW".equals(flowType)) {
            return (mode == ExamMode.PRACTICAL) ? 100 : 80; // Review는 더 크게
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
