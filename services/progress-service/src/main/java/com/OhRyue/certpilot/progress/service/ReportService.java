package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.domain.ReportTagSkill;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import com.OhRyue.certpilot.progress.dto.ReportDtos.*;
import com.OhRyue.certpilot.progress.feign.StudyReportClient;
import com.OhRyue.certpilot.progress.repository.BattleRecordRepository;
import com.OhRyue.certpilot.progress.repository.ReportDailyRepository;
import com.OhRyue.certpilot.progress.repository.ReportTagSkillRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.*;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ReportService {

  private static final ZoneId KST = ZoneId.of("Asia/Seoul");

  private final ReportDailyRepository dailyRepository;
  private final ReportTagSkillRepository tagSkillRepository;
  private final StudyReportClient studyReportClient;
  private final StreakService streakService;
  private final BattleRecordRepository battleRecordRepository;

  /**
   * 필기/실기 합산 리포트 개요
   */
  public Overview overview(String userId) {
    List<ReportDaily> daily = dailyRepository.findByUserId(userId);
    if (daily.isEmpty()) {
      int streak = streakService.get(userId).getCurrentDays();
      return new Overview(
          0, 0, 0.0, 0.0,
          0, 0,
          streak,
          0, 0, 0.0, 0.0, 0
      );
    }

    LocalDate today = LocalDate.now(KST);
    LocalDate weekStart = today.minusDays(6);
    LocalDate prevWeekStart = today.minusDays(13);
    LocalDate prevWeekEnd = today.minusDays(7);

    long totalSolved = daily.stream().mapToLong(ReportDaily::getSolvedCount).sum();
    long totalCorrect = daily.stream().mapToLong(ReportDaily::getCorrectCount).sum();
    double avgAcc = ratio(totalCorrect, totalSolved);

    long weekSolved = daily.stream()
        .filter(r -> !r.getDate().isBefore(weekStart))
        .mapToLong(ReportDaily::getSolvedCount)
        .sum();
    long weekCorrect = daily.stream()
        .filter(r -> !r.getDate().isBefore(weekStart))
        .mapToLong(ReportDaily::getCorrectCount)
        .sum();
    double weekAcc = ratio(weekCorrect, weekSolved);

    long prevWeekSolved = daily.stream()
        .filter(r -> !r.getDate().isBefore(prevWeekStart) && !r.getDate().isAfter(prevWeekEnd))
        .mapToLong(ReportDaily::getSolvedCount)
        .sum();
    long prevWeekCorrect = daily.stream()
        .filter(r -> !r.getDate().isBefore(prevWeekStart) && !r.getDate().isAfter(prevWeekEnd))
        .mapToLong(ReportDaily::getCorrectCount)
        .sum();
    double prevWeekAcc = ratio(prevWeekCorrect, prevWeekSolved);

    long totalMinutes = daily.stream().mapToLong(ReportDaily::getTimeSpentSec).sum() / 60;
    long weekMinutes = daily.stream()
        .filter(r -> !r.getDate().isBefore(weekStart))
        .mapToLong(ReportDaily::getTimeSpentSec)
        .sum() / 60;
    long prevWeekMinutes = daily.stream()
        .filter(r -> !r.getDate().isBefore(prevWeekStart) && !r.getDate().isAfter(prevWeekEnd))
        .mapToLong(ReportDaily::getTimeSpentSec)
        .sum() / 60;

    int streak = streakService.get(userId).getCurrentDays();

    return new Overview(
        totalSolved,
        weekSolved,
        round1(avgAcc),
        round1(weekAcc - prevWeekAcc),
        totalMinutes,
        weekMinutes,
        streak,
        prevWeekSolved,
        totalCorrect,
        round1(weekAcc),
        round1(prevWeekAcc),
        prevWeekMinutes
    );
  }

  public TagAbilityResp abilityByTag(String userId, String mode, int limit) {
    ExamMode examMode = parseMode(mode);
    List<ReportTagSkill> skills = tagSkillRepository.findByUserIdAndExamModeOrderByTotalDesc(userId, examMode);
    skills = skills.size() <= limit ? skills : skills.subList(0, limit);
    List<TagAbility> items = skills.stream()
        .map(skill -> new TagAbility(
            skill.getTag(),
            skill.getCorrect(),
            skill.getTotal(),
            skill.getAccuracy().doubleValue()
        ))
        .toList();
    List<String> weaknessTags = items.stream()
        .filter(item -> item.accuracy() < 70.0)
        .sorted(Comparator.comparing(TagAbility::accuracy))
        .map(TagAbility::tag)
        .toList();
    String message;
    if (weaknessTags.isEmpty()) {
      message = items.isEmpty()
          ? "아직 리포트를 만들기 위한 데이터가 부족합니다. 학습을 시작해 보세요!"
          : "태그별 정답률이 안정적으로 유지되고 있어요. 지금 흐름을 유지해 보세요!";
    } else {
      String highlight = String.join(", ", weaknessTags.subList(0, Math.min(3, weaknessTags.size())));
      message = highlight + " 태그 정답률이 낮습니다. 약점 보완 세트를 추천해요.";
    }
    return new TagAbilityResp(items, weaknessTags, message);
  }

  public RecentRecordsResp recentRecords(String userId, int limit) {
    // study-service에서 학습 기록 가져오기
    RecentRecordsResp studyResp = studyReportClient.recent(limit);
    List<RecentRecord> allRecords = new ArrayList<>();
    
    if (studyResp != null && studyResp.records() != null) {
      allRecords.addAll(studyResp.records());
    }
    
    // progress-service에서 배틀 기록 가져오기
    List<RecentRecord> battleRecords = battleRecordRepository
        .findRecentByUser(userId, org.springframework.data.domain.PageRequest.of(0, limit))
        .stream()
        .map(br -> {
          String type = switch (br.getMode()) {
            case "DUEL" -> "1:1 배틀";
            case "TOURNAMENT" -> "토너먼트";
            case "GOLDENBELL" -> "골든벨";
            default -> "배틀";
          };
          double accuracy = br.getTotalCount() > 0 
              ? (double) br.getCorrectCount() / br.getTotalCount() * 100.0 
              : 0.0;
          return new RecentRecord(
              br.getCompletedAt().atZone(java.time.ZoneId.of("Asia/Seoul")).toLocalDate(),
              type,
              br.getExamMode() != null ? (br.getExamMode() == ExamMode.WRITTEN ? "필기" : "실기") : "배틀",
              br.getTotalCount(),
              br.getCorrectCount(),
              accuracy
          );
        })
        .toList();
    
    allRecords.addAll(battleRecords);
    
    // 날짜 내림차순 정렬 (최신순)
    allRecords.sort(Comparator.comparing(RecentRecord::date).reversed());
    
    // limit만큼만 반환
    if (allRecords.size() > limit) {
      allRecords = allRecords.subList(0, limit);
    }
    
    return new RecentRecordsResp(allRecords);
  }

  private ExamMode parseMode(String mode) {
    try {
      return ExamMode.valueOf(mode.toUpperCase(Locale.ROOT));
    } catch (Exception e) {
      return ExamMode.WRITTEN;
    }
  }

  private double ratio(long numerator, long denominator) {
    if (denominator == 0) return 0.0;
    return (double) numerator / denominator * 100.0;
  }

  private double round1(double value) {
    return BigDecimal.valueOf(value).setScale(1, RoundingMode.HALF_UP).doubleValue();
  }
}
