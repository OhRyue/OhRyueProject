package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.client.CertCurriculumClient;
import com.OhRyue.certpilot.study.client.CurriculumGateway;
import com.OhRyue.certpilot.study.domain.StudySession;
import com.OhRyue.certpilot.study.domain.StudySessionItem;
import com.OhRyue.certpilot.study.domain.UserAnswer;
import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.dto.ReportDtos.ReportSummaryResp;
import com.OhRyue.certpilot.study.dto.ReportDtos.RecentDailyItem;
import com.OhRyue.certpilot.study.dto.ReportDtos.RecentResultsResp;
import com.OhRyue.certpilot.study.dto.ReportDtos.RecentRecord;
import com.OhRyue.certpilot.study.dto.ReportDtos.RecentRecordsResp;
import com.OhRyue.certpilot.study.dto.ReportDtos.ProgressCardResp;
import com.OhRyue.certpilot.study.repository.StudySessionItemRepository;
import com.OhRyue.certpilot.study.repository.StudySessionRepository;
import com.OhRyue.certpilot.study.repository.UserAnswerRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
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

    private static final ZoneId ZONE = ZoneId.of("Asia/Seoul");

    private final UserAnswerRepository userAnswerRepository;
    private final UserProgressRepository userProgressRepository;
    private final CertCurriculumClient certCurriculumClient;  // cert-service 커리큘럼 조회용 Feign
    // 최근 학습 결과를 위한 의존성
    private final StudySessionRepository studySessionRepository;
    private final StudySessionItemRepository studySessionItemRepository;
    private final CurriculumGateway curriculumGateway;
    private final ObjectMapper objectMapper;

    /* ======================= 요약 카드 ======================= */

    public ReportSummaryResp summary(String userId) {
        List<UserAnswer> allAnswers = userAnswerRepository.findByUserId(userId);

        long totalSolved = allAnswers.size();
        double avgAccuracy = percent(accuracy(allAnswers));

        LocalDate today = LocalDate.now(ZONE);
        Instant last7Start = today.minusDays(6).atStartOfDay(ZONE).toInstant();
        Instant prev7Start = today.minusDays(13).atStartOfDay(ZONE).toInstant();
        Instant prev7EndExclusive = today.minusDays(6).atStartOfDay(ZONE).toInstant();

        List<UserAnswer> last7 = allAnswers.stream()
                .filter(ans -> !ans.getAnsweredAt().isBefore(last7Start))
                .toList();

        List<UserAnswer> prev7 = allAnswers.stream()
                .filter(ans -> !ans.getAnsweredAt().isBefore(prev7Start)
                        && ans.getAnsweredAt().isBefore(prev7EndExclusive))
                .toList();

        long weeklySolved = last7.size();
        double last7Acc = percent(accuracy(last7));
        double prev7Acc = percent(accuracy(prev7));
        double delta = round2(last7Acc - prev7Acc);

        int streak = calcStreakDays(allAnswers);

        return new ReportSummaryResp(
                totalSolved,
                weeklySolved,
                round2(avgAccuracy),
                round2(last7Acc),
                round2(prev7Acc),
                delta,
                streak
        );
    }

    /* ======================= 최근 일별 결과 ======================= */

    public RecentResultsResp recentDaily(String userId, int days) {
        if (days <= 0) days = 14;

        LocalDate today = LocalDate.now(ZONE);
        LocalDate from = today.minusDays(days - 1);
        Instant fromTs = from.atStartOfDay(ZONE).toInstant();

        List<UserAnswer> recent = userAnswerRepository.findByUserIdAndAnsweredAtAfter(userId, fromTs);

        Map<LocalDate, List<UserAnswer>> byDate = recent.stream()
                .collect(Collectors.groupingBy(
                        ans -> ans.getAnsweredAt().atZone(ZONE).toLocalDate()
                ));

        // 오늘부터 역순으로 days일 생성
        List<LocalDate> dates = new ArrayList<>();
        for (int i = 0; i < days; i++) {
            dates.add(today.minusDays(i));
        }

        List<RecentDailyItem> items = new ArrayList<>();
        for (LocalDate date : dates) {
            List<UserAnswer> dayAnswers = byDate.getOrDefault(date, List.of());
            int total = dayAnswers.size();
            int correct = (int) dayAnswers.stream()
                    .filter(ans -> Boolean.TRUE.equals(ans.getCorrect()))
                    .count();
            double acc = percent(total == 0 ? 0.0 : (double) correct / total);
            items.add(new RecentDailyItem(date, correct, total, round2(acc)));
        }

        return new RecentResultsResp(items);
    }

    /* ======================= 진행 카드(자격증별) ======================= */

    public ProgressCardResp progressCard(String userId, Long certId) {
        if (certId == null) {
            return new ProgressCardResp(0, 0, 0, 0.0, null);
        }

        // 🔸 cert-service의 커리큘럼 토픽 목록을 Feign으로 조회
        CertCurriculumClient.TopicListResponse topicList =
                certCurriculumClient.listTopics(certId, null, null); // examMode/parentId 필터 없음

        List<CertCurriculumClient.TopicResponse> topics =
                (topicList != null && topicList.topics() != null)
                        ? topicList.topics()
                        : List.of();

        int totalTopics = topics.size();
        if (totalTopics == 0) {
            return new ProgressCardResp(0, 0, 0, 0.0, null);
        }

        Set<Long> topicIds = topics.stream()
                .map(CertCurriculumClient.TopicResponse::id)
                .collect(Collectors.toSet());

        List<UserProgress> progresses = userProgressRepository.findByUserId(userId);

        long completed = progresses.stream()
                .filter(progress -> topicIds.contains(progress.getTopicId()))
                .filter(progress ->
                        Optional.ofNullable(progress.getWrittenDoneCnt()).orElse(0) > 0 ||
                                Optional.ofNullable(progress.getPracticalDoneCnt()).orElse(0) > 0
                )
                .count();

        long pending = Math.max(0, totalTopics - completed);
        double completionRate = totalTopics == 0
                ? 0.0
                : Math.round(((double) completed / totalTopics) * 1000.0) / 10.0;

        String lastStudiedAt = progresses.stream()
                .filter(progress -> topicIds.contains(progress.getTopicId()))
                .map(UserProgress::getLastStudiedAt)
                .filter(Objects::nonNull)
                .max(Comparator.naturalOrder())
                .map(instant -> instant.atZone(ZONE).toOffsetDateTime().toString())
                .orElse(null);

        return new ProgressCardResp(
                totalTopics,
                (int) completed,
                (int) pending,
                completionRate,
                lastStudiedAt
        );
    }

    /* ======================= 최근 학습 결과 (세션 기반) ======================= */

    public RecentRecordsResp recentRecords(String userId, int limit) {
        if (limit <= 0) limit = 30;
        
        // 최근 완료된 세션 조회 (SUBMITTED 또는 CLOSED 상태)
        List<StudySession> sessions = studySessionRepository.findByUserIdOrderByStartedAtDesc(userId)
                .stream()
                .filter(s -> s.getFinishedAt() != null && Boolean.TRUE.equals(s.getCompleted()))
                .limit(limit * 2) // 여유있게 가져와서 필터링
                .toList();

        List<RecentRecord> records = new ArrayList<>();
        for (StudySession session : sessions) {
            if (records.size() >= limit) break;
            
            // 세션 아이템에서 정답/전체 개수 계산
            List<StudySessionItem> items = studySessionItemRepository.findBySessionIdOrderByOrderNoAsc(session.getId());
            int total = items.size();
            int correct = (int) items.stream()
                    .filter(item -> Boolean.TRUE.equals(item.getCorrect()))
                    .count();
            double accuracy = total == 0 ? 0.0 : (correct * 100.0) / total;
            
            // 세션 타입 변환 (MICRO -> Micro, REVIEW -> Review, ASSIST_* -> Assist)
            String type = mapSessionTypeToDisplay(session.getMode());
            
            // 토픽 제목 조회
            String partTitle = resolvePartTitle(session);
            
            // 날짜 (KST 기준)
            LocalDate date = session.getFinishedAt() != null
                    ? session.getFinishedAt().atZone(ZONE).toLocalDate()
                    : session.getStartedAt().atZone(ZONE).toLocalDate();
            
            records.add(new RecentRecord(date, type, partTitle, total, correct, round2(accuracy)));
        }
        
        // 날짜 최신순 정렬 (이미 startedAtDesc로 가져왔지만 한번 더)
        records.sort(Comparator.comparing(RecentRecord::date).reversed()
                .thenComparing(RecentRecord::type));
        
        return new RecentRecordsResp(records.stream().limit(limit).toList());
    }

    private String mapSessionTypeToDisplay(String mode) {
        if (mode == null) return "Unknown";
        return switch (mode.toUpperCase()) {
            case "MICRO" -> "Micro";
            case "REVIEW" -> "Review";
            case "ASSIST_CATEGORY", "ASSIST_DIFFICULTY", "ASSIST_WEAK" -> "Assist";
            default -> mode;
        };
    }

    private String resolvePartTitle(StudySession session) {
        try {
            // topicScopeJson에서 topicId 또는 rootTopicId 추출
            if (session.getTopicScopeJson() != null && !session.getTopicScopeJson().isBlank()) {
                TypeReference<Map<String, Object>> typeRef = new TypeReference<>() {};
                Map<String, Object> scope = objectMapper.readValue(session.getTopicScopeJson(), typeRef);
                Long topicId = scope.containsKey("topicId") 
                        ? ((Number) scope.get("topicId")).longValue()
                        : (scope.containsKey("rootTopicId") 
                                ? ((Number) scope.get("rootTopicId")).longValue() 
                                : null);
                
                if (topicId != null) {
                    CurriculumGateway.CurriculumConcept concept = curriculumGateway.getConceptWithTopic(topicId);
                    return concept != null ? concept.topicTitle() : "알 수 없음";
                }
            }
        } catch (Exception e) {
            // 조회 실패 시 기본값
        }
        return "알 수 없음";
    }

    /* ======================= 내부 유틸 ======================= */

    private static double accuracy(List<UserAnswer> answers) {
        if (answers.isEmpty()) return 0.0;
        long correct = answers.stream()
                .filter(ans -> Boolean.TRUE.equals(ans.getCorrect()))
                .count();
        return (double) correct / answers.size();
    }

    private static double percent(double v) {
        return v * 100.0;
    }

    private static double round2(double v) {
        return Math.round(v * 100.0) / 100.0;
    }

    private int calcStreakDays(List<UserAnswer> all) {
        if (all.isEmpty()) return 0;

        Set<LocalDate> days = all.stream()
                .map(ans -> ans.getAnsweredAt().atZone(ZONE).toLocalDate())
                .collect(Collectors.toSet());

        int streak = 0;
        LocalDate cursor = LocalDate.now(ZONE);
        while (days.contains(cursor)) {
            streak++;
            cursor = cursor.minusDays(1);
        }
        return streak;
    }
}
