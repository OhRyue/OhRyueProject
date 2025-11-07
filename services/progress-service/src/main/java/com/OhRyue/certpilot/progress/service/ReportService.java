package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.Question;
import com.OhRyue.certpilot.progress.domain.UserAnswer;
import com.OhRyue.certpilot.progress.domain.enums.QuestionType;
import com.OhRyue.certpilot.progress.dto.ReportDtos.*;
import com.OhRyue.certpilot.progress.repository.QuestionRepository;
import com.OhRyue.certpilot.progress.repository.QuestionTagRepository;
import com.OhRyue.certpilot.progress.repository.UserAnswerRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.*;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ReportService {

    private final UserAnswerRepository answerRepo;
    private final QuestionRepository questionRepo;
    private final QuestionTagRepository tagRepo;

    private static final ZoneId KST = ZoneId.of("Asia/Seoul");

    /* ============ 개요 ============ */
    public Overview overview(String userId, String mode) {
        Instant now = Instant.now();
        LocalDate today = LocalDate.now(KST);

        Instant thisWeekStart = today.minusDays(6).atStartOfDay(KST).toInstant();
        Instant thisWeekEnd   = today.plusDays(1).atStartOfDay(KST).toInstant();

        Instant prevWeekStart = today.minusDays(13).atStartOfDay(KST).toInstant();
        Instant prevWeekEnd   = today.minusDays(6).atStartOfDay(KST).toInstant();

        // 전체/주간 데이터 로드 후 모드 필터
        List<UserAnswer> all = filterByMode(answerRepo.findAllByUser(userId), mode);
        List<UserAnswer> thisWeek = filterByMode(answerRepo.findByUserAndRange(userId, thisWeekStart, thisWeekEnd), mode);
        List<UserAnswer> prevWeek = filterByMode(answerRepo.findByUserAndRange(userId, prevWeekStart, prevWeekEnd), mode);

        long totalProblems = all.size();
        long problemsThisWeek = thisWeek.size();

        double avgAcc = accuracy(all);
        double thisWeekAcc = accuracy(thisWeek);
        double prevWeekAcc = accuracy(prevWeek);
        double weekDelta = round1(thisWeekAcc - prevWeekAcc);

        long minutesAll = estimateStudyMinutes(all);
        long minutesWeek = estimateStudyMinutes(thisWeek);

        int streak = computeStreakDays(all);

        return new Overview(
                totalProblems,
                problemsThisWeek,
                avgAcc,
                weekDelta,
                minutesAll,
                minutesWeek,
                streak
        );
    }

    /* ============ 태그별 능력지수 ============ */
    public TagAbilityResp abilityByTag(String userId, String mode, int limit) {
        List<Object[]> rows = tagRepo.aggregateByTag(userId, canonicalMode(mode), limit);
        List<TagAbility> items = rows.stream().map(r -> {
            String tag = String.valueOf(r[0]);
            long correct = toLong(r[1]);
            long total = toLong(r[2]);
            double acc = total == 0 ? 0 : round1(100.0 * correct / total);
            return new TagAbility(tag, correct, total, acc);
        }).toList();
        return new TagAbilityResp(items);
    }

    /* ============ 최근 일별 결과 ============ */
    public RecentResp recentDaily(String userId, String mode, int days) {
        LocalDate today = LocalDate.now(KST);
        Instant from = today.minusDays(days - 1L).atStartOfDay(KST).toInstant();
        Instant to = today.plusDays(1).atStartOfDay(KST).toInstant();

        List<UserAnswer> inRange = filterByMode(answerRepo.findByUserAndRange(userId, from, to), mode);

        Map<LocalDate, List<UserAnswer>> byDate = inRange.stream()
                .collect(Collectors.groupingBy(
                        ua -> LocalDateTime.ofInstant(ua.getCreatedAt(), KST).toLocalDate(),
                        TreeMap::new, Collectors.toList()
                ));

        List<RecentItem> items = new ArrayList<>();
        for (Map.Entry<LocalDate, List<UserAnswer>> e : byDate.entrySet()) {
            long solved = e.getValue().size();
            long corr = e.getValue().stream().filter(UserAnswer::isCorrect).count();
            double acc = solved == 0 ? 0 : round1(100.0 * corr / solved);
            items.add(new RecentItem(e.getKey(), canonicalMode(mode), solved, corr, acc));
        }
        return new RecentResp(items);
    }

    /* ============ 내부 유틸 ============ */

    private List<UserAnswer> filterByMode(List<UserAnswer> list, String mode) {
        if (list.isEmpty()) return list;

        // Question type 로딩
        Map<Long, Question> qm = questionRepo.findAllById(
                list.stream().map(UserAnswer::getQuestionId).collect(Collectors.toSet())
        ).stream().collect(Collectors.toMap(Question::getId, it -> it));

        boolean written = "WRITTEN".equalsIgnoreCase(mode);
        return list.stream().filter(ua -> {
            Question q = qm.get(ua.getQuestionId());
            if (q == null) return false;
            QuestionType t = q.getType();
            return written
                    ? (t == QuestionType.OX || t == QuestionType.MCQ)
                    : (t == QuestionType.SHORT || t == QuestionType.LONG);
        }).toList();
    }

    private double accuracy(List<UserAnswer> xs) {
        if (xs.isEmpty()) return 0d;
        long c = xs.stream().filter(UserAnswer::isCorrect).count();
        return round1(100.0 * c / xs.size());
    }

    /** “답변 간격 5분 이내는 한 세션”으로 보고 총 학습 시간 추정(분) */
    private long estimateStudyMinutes(List<UserAnswer> xs) {
        if (xs.isEmpty()) return 0L;
        List<Instant> ts = xs.stream().map(UserAnswer::getCreatedAt).sorted().toList();
        long totalSec = 0;
        Instant sessionStart = ts.get(0);
        Instant prev = ts.get(0);
        for (int i = 1; i < ts.size(); i++) {
            Instant cur = ts.get(i);
            long gap = Duration.between(prev, cur).toSeconds();
            if (gap <= 300) { // 5분
                prev = cur;
            } else {
                totalSec += Math.max(60, Duration.between(sessionStart, prev).toSeconds());
                sessionStart = prev = cur;
            }
        }
        totalSec += Math.max(60, Duration.between(sessionStart, prev).toSeconds());
        return totalSec / 60;
    }

    private int computeStreakDays(List<UserAnswer> xs) {
        if (xs.isEmpty()) return 0;
        Set<LocalDate> days = xs.stream()
                .map(a -> LocalDateTime.ofInstant(a.getCreatedAt(), KST).toLocalDate())
                .collect(Collectors.toSet());
        int streak = 0;
        LocalDate cur = LocalDate.now(KST);
        while (days.contains(cur)) { streak++; cur = cur.minusDays(1); }
        return streak;
    }

    private static double round1(double v) { return Math.round(v * 10.0) / 10.0; }
    private static String canonicalMode(String mode) {
        return ("PRACTICAL".equalsIgnoreCase(mode)) ? "PRACTICAL" : "WRITTEN";
    }
    private static long toLong(Object o) {
        if (o instanceof Number n) return n.longValue();
        return Long.parseLong(String.valueOf(o));
    }
}
