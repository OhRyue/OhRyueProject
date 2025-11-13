package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.domain.ReportWeekly;
import com.OhRyue.certpilot.progress.domain.ReportTagSkill;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import com.OhRyue.certpilot.progress.domain.enums.QuestionType;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.dto.HookDtos.StudySubmitReq;
import com.OhRyue.certpilot.progress.repository.ReportDailyRepository;
import com.OhRyue.certpilot.progress.repository.ReportTagSkillRepository;
import com.OhRyue.certpilot.progress.repository.ReportWeeklyRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.*;
import java.time.temporal.WeekFields;
import java.util.List;
import java.util.Locale;

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

  @Transactional
  public void ingest(StudySubmitReq payload) {
    ExamMode examMode = parseExamMode(payload.examMode());
    QuestionType questionType = parseQuestionType(payload.questionType());
    boolean isCorrect = resolveCorrectness(payload.correct(), payload.score(), examMode);
    int xpDelta = computeXpDelta(examMode, questionType, isCorrect);
    int timeSpent = DEFAULT_TIME_PER_PROBLEM_SEC;

    // XP & Rank
    XpReason reason = mapReason(payload.source());
    xpService.addXp(payload.userId(), xpDelta, reason, buildRefId(questionType, examMode));
    rankService.recomputeForUser(payload.userId());

    // Streak
    streakService.tickToday(payload.userId());

    // Daily report
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

    // Weekly report
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

    // Tag skill
    List<String> tags = payload.tags() == null ? List.of() : payload.tags().stream()
        .filter(tag -> tag != null && !tag.isBlank())
        .distinct()
        .toList();
    if (!tags.isEmpty()) {
      for (String tag : tags) {
        ReportTagSkill skill = tagSkillRepository.findByUserIdAndTagAndExamMode(payload.userId(), tag, examMode)
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
  }

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
    if (mode == ExamMode.PRACTICAL && type != QuestionType.OX && !correct) {
      return 3; // 실기 오답 기본 보상
    }
    return correct ? 10 : 2;
  }

  private XpReason mapReason(String source) {
    if (source == null || source.isBlank()) return XpReason.ETC;
    String upper = source.toUpperCase(Locale.ROOT);
    if (upper.contains("MICRO")) return XpReason.MICRO;
    if (upper.contains("REVIEW")) return XpReason.REVIEW;
    if (upper.contains("ASSIST")) return XpReason.ASSIST;
    if (upper.contains("BATTLE") || upper.contains("VERSUS")) return XpReason.BATTLE;
    return XpReason.ETC;
  }

  private String buildRefId(QuestionType type, ExamMode mode) {
    return "study:" + mode.name().toLowerCase(Locale.ROOT) + ":" + type.name().toLowerCase(Locale.ROOT);
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


