package com.OhRyue.certpilot.study.service;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.OhRyue.certpilot.study.client.CertCurriculumClient;
import com.OhRyue.certpilot.study.client.CurriculumGateway;
import com.OhRyue.certpilot.study.domain.LearningSession;
import com.OhRyue.certpilot.study.domain.StudySession;
import com.OhRyue.certpilot.study.domain.StudySessionItem;
import com.OhRyue.certpilot.study.domain.UserAnswer;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.dto.ReportDtos.ProgressCardResp;
import com.OhRyue.certpilot.study.dto.ReportDtos.RecentDailyItem;
import com.OhRyue.certpilot.study.dto.ReportDtos.RecentRecord;
import com.OhRyue.certpilot.study.dto.ReportDtos.RecentRecordsResp;
import com.OhRyue.certpilot.study.dto.ReportDtos.RecentResultsResp;
import com.OhRyue.certpilot.study.dto.ReportDtos.ReportSummaryResp;
import com.OhRyue.certpilot.study.repository.LearningSessionRepository;
import com.OhRyue.certpilot.study.repository.StudySessionItemRepository;
import com.OhRyue.certpilot.study.repository.StudySessionRepository;
import com.OhRyue.certpilot.study.repository.UserAnswerRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
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
    private final LearningSessionRepository learningSessionRepository;
    private final CurriculumGateway curriculumGateway;
    private final ObjectMapper objectMapper;
    
    private static final TypeReference<Map<String, Object>> MAP_TYPE = new TypeReference<>() {};

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

    public ProgressCardResp progressCard(String userId, Long certId, ExamMode examMode) {
        if (certId == null) {
            return new ProgressCardResp(0, 0, 0, 0.0, null);
        }

        return calculateProgressCardData(userId, certId, examMode);
    }

    private ProgressCardResp calculateProgressCardData(String userId, Long certId, ExamMode examMode) {
        String modeStr = examMode.name();

        // 1. cert-service에서 해당 모드의 모든 topic 조회
        CertCurriculumClient.TopicListResponse topicList;
        try {
            topicList = certCurriculumClient.listTopics(certId, modeStr, null);
        } catch (Exception e) {
            // cert-service 호출 실패 시 빈 응답 반환
            return new ProgressCardResp(0, 0, 0, 0.0, null);
        }
        
        List<CertCurriculumClient.TopicResponse> allTopics = 
            (topicList != null && topicList.topics() != null) ? topicList.topics() : List.of();

        // 2. micro 학습 가능한 topic 필터링 (code에 점이 2개인 경우)
        List<CertCurriculumClient.TopicResponse> microTopics = allTopics.stream()
            .filter(topic -> {
                if (topic.code() == null) return false;
                long dotCount = topic.code().chars().filter(c -> c == '.').count();
                return dotCount == 2; // "1.1.1", "P.1.1" 같은 형태
            })
            .collect(Collectors.toList());

        // 3. review 학습 가능한 topic 필터링 (code에 점이 1개인 루트 토픽)
        List<CertCurriculumClient.TopicResponse> reviewTopics = allTopics.stream()
            .filter(topic -> {
                if (topic.code() == null) return false;
                long dotCount = topic.code().chars().filter(c -> c == '.').count();
                return dotCount == 1; // "1.1", "P.1" 같은 형태
            })
            .collect(Collectors.toList());

        long totalTopics = microTopics.size() + reviewTopics.size();
        if (totalTopics == 0) {
            return new ProgressCardResp(0, 0, 0, 0.0, null);
        }

        // 4. micro topic들의 ID 추출
        List<Long> microTopicIds = microTopics.stream()
            .map(CertCurriculumClient.TopicResponse::id)
            .collect(Collectors.toList());

        // 5. review topic들의 ID 추출
        List<Long> reviewTopicIds = reviewTopics.stream()
            .map(CertCurriculumClient.TopicResponse::id)
            .collect(Collectors.toList());

        // 6. micro 완료 카운트: truly_completed만 카운트
        List<StudySession> allMicroSessions = studySessionRepository
            .findByUserIdAndModeAndExamMode(userId, "MICRO", examMode);

        // topicId별로 세션 그룹화 (topic_scope_json에서 topicId 추출)
        Map<Long, List<StudySession>> sessionsByTopic = allMicroSessions.stream()
            .filter(session -> {
                try {
                    if (session.getTopicScopeJson() == null || session.getTopicScopeJson().isBlank()) {
                        return false;
                    }
                    Map<String, Object> scope = objectMapper.readValue(session.getTopicScopeJson(), MAP_TYPE);
                    Object topicIdObj = scope.get("topicId");
                    return topicIdObj != null && microTopicIds.contains(((Number) topicIdObj).longValue());
                } catch (Exception e) {
                    return false;
                }
            })
            .collect(Collectors.groupingBy(session -> {
                try {
                    Map<String, Object> scope = objectMapper.readValue(session.getTopicScopeJson(), MAP_TYPE);
                    Object topicIdObj = scope.get("topicId");
                    return topicIdObj != null ? ((Number) topicIdObj).longValue() : 0L;
                } catch (Exception e) {
                    return 0L;
                }
            }));

        // 각 micro 토픽에 대한 완료 상태 확인 (truly_completed만)
        long completedMicroCount = microTopicIds.stream()
            .filter(topicId -> {
                List<StudySession> topicSessions = sessionsByTopic.getOrDefault(topicId, List.of());
                
                if (topicSessions.isEmpty()) {
                    return false;
                }
                
                // MINI 세션(question_count=4)과 MCQ 세션(question_count=5) 찾기 (가장 최신 세션)
                StudySession miniSession = topicSessions.stream()
                    .filter(s -> s.getQuestionCount() != null && s.getQuestionCount() == 4)
                    .max((s1, s2) -> s1.getStartedAt().compareTo(s2.getStartedAt()))
                    .orElse(null);
                StudySession mcqSession = topicSessions.stream()
                    .filter(s -> s.getQuestionCount() != null && s.getQuestionCount() == 5)
                    .max((s1, s2) -> s1.getStartedAt().compareTo(s2.getStartedAt()))
                    .orElse(null);
                
                // TRULY_COMPLETED 상태인 경우만 완료로 카운트 (miniPassed && mcqPassed)
                boolean miniPassed = miniSession != null && Boolean.TRUE.equals(miniSession.getPassed());
                boolean mcqPassed = mcqSession != null && Boolean.TRUE.equals(mcqSession.getPassed());
                return miniPassed && mcqPassed;
            })
            .count();

        // 7. review 완료 카운트: truly_completed만 카운트
        List<LearningSession> reviewSessions = reviewTopicIds.isEmpty() 
            ? List.of()
            : learningSessionRepository.findByUserIdAndTopicIdInAndMode(userId, reviewTopicIds, "REVIEW");

        long completedReviewCount = reviewSessions.stream()
            .filter(session -> "DONE".equals(session.getStatus()) && Boolean.TRUE.equals(session.getTrulyCompleted()))
            .map(LearningSession::getTopicId)
            .distinct()
            .count();

        long completedTopics = completedMicroCount + completedReviewCount;
        long pendingTopics = Math.max(0, totalTopics - completedTopics);

        // 8. 비율 계산
        double completionRate = totalTopics == 0 
            ? 0.0 
            : Math.round(((double) completedTopics / totalTopics) * 10000.0) / 100.0; // 소수점 2자리

        // 9. 마지막 학습 시각 계산 (micro와 review 세션 모두 고려)
        String lastStudiedAt = null;
        Instant lastMicroAt = allMicroSessions.stream()
            .map(StudySession::getStartedAt)
            .filter(Objects::nonNull)
            .max(Comparator.naturalOrder())
            .orElse(null);
        Instant lastReviewAt = reviewSessions.stream()
            .map(LearningSession::getStartedAt)
            .filter(Objects::nonNull)
            .max(Comparator.naturalOrder())
            .orElse(null);
        
        if (lastMicroAt != null || lastReviewAt != null) {
            Instant lastAt = null;
            if (lastMicroAt != null && lastReviewAt != null) {
                lastAt = lastMicroAt.isAfter(lastReviewAt) ? lastMicroAt : lastReviewAt;
            } else if (lastMicroAt != null) {
                lastAt = lastMicroAt;
            } else {
                lastAt = lastReviewAt;
            }
            lastStudiedAt = lastAt.atZone(ZONE).toOffsetDateTime().toString();
        }

        return new ProgressCardResp(
            (int) totalTopics,
            (int) completedTopics,
            (int) pendingTopics,
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
        String mode = session.getMode();
        log.info("resolvePartTitle: sessionId={}, mode={}, topicScopeJson={}", 
            session.getId(), mode, session.getTopicScopeJson() != null ? "present" : "null");
        
        // ASSIST 모드의 경우 topicScopeJson 없이도 처리 가능
        if (mode != null && mode.startsWith("ASSIST_")) {
            if (mode.contains("DIFFICULTY")) {
                log.info("resolvePartTitle: ASSIST_DIFFICULTY mode detected, returning '난이도 학습'");
                return "난이도 학습";
            } else if (mode.contains("WEAKNESS")) {
                log.info("resolvePartTitle: ASSIST_WEAKNESS mode detected, returning '약점 보완'");
                return "약점 보완";
            } else if (mode.contains("CATEGORY")) {
                log.info("resolvePartTitle: ASSIST_CATEGORY mode detected, returning '카테고리 학습'");
                return "카테고리 학습";
            }
            log.info("resolvePartTitle: ASSIST mode detected but no specific type, returning '보조 학습'");
            return "보조 학습";
        }
        
        try {
            // topicScopeJson에서 topicId 또는 rootTopicId 추출
            if (session.getTopicScopeJson() != null && !session.getTopicScopeJson().isBlank()) {
                TypeReference<Map<String, Object>> typeRef = new TypeReference<>() {};
                Map<String, Object> scope = objectMapper.readValue(session.getTopicScopeJson(), typeRef);
                
                // 1. ASSIST 모드의 경우 scope에서 추가 정보 추출
                if (mode != null && mode.startsWith("ASSIST_")) {
                    if (scope.containsKey("difficulty")) {
                        String difficulty = String.valueOf(scope.get("difficulty"));
                        return "난이도: " + difficulty;
                    } else if (scope.containsKey("weaknessTags")) {
                        Object weaknessTagsObj = scope.get("weaknessTags");
                        if (weaknessTagsObj instanceof List && !((List<?>) weaknessTagsObj).isEmpty()) {
                            List<?> tags = (List<?>) weaknessTagsObj;
                            if (tags.size() == 1) {
                                return "약점: " + tags.get(0);
                            } else {
                                return "약점: " + tags.get(0) + " 외 " + (tags.size() - 1) + "개";
                            }
                        }
                        return "약점 보완";
                    } else if (scope.containsKey("topicIds")) {
                        // topicIds가 있으면 첫 번째 topicId로 토픽 제목 조회 시도
                        Object topicIdsObj = scope.get("topicIds");
                        if (topicIdsObj instanceof List && !((List<?>) topicIdsObj).isEmpty()) {
                            Object firstTopicId = ((List<?>) topicIdsObj).get(0);
                            if (firstTopicId instanceof Number) {
                                Long topicId = ((Number) firstTopicId).longValue();
                                try {
                                    CurriculumGateway.CurriculumConcept concept = curriculumGateway.getConceptWithTopic(topicId);
                                    if (concept != null && concept.topicTitle() != null && !concept.topicTitle().isBlank()) {
                                        return concept.topicTitle();
                                    }
                                } catch (Exception e) {
                                    // curriculumGateway 호출 실패 시 "카테고리 학습"으로 fallback
                                    log.debug("Failed to get topic title for topicId={} in ASSIST_CATEGORY: {}", topicId, e.getMessage());
                                }
                            }
                        }
                        return "카테고리 학습";
                    }
                    return "보조 학습";
                }
                
                // 2. 단일 topicId 확인 (MICRO, REVIEW 등)
                Long topicId = null;
                if (scope.containsKey("topicId")) {
                    topicId = ((Number) scope.get("topicId")).longValue();
                } else if (scope.containsKey("rootTopicId")) {
                    topicId = ((Number) scope.get("rootTopicId")).longValue();
                }
                
                if (topicId != null) {
                    try {
                        CurriculumGateway.CurriculumConcept concept = curriculumGateway.getConceptWithTopic(topicId);
                        if (concept != null && concept.topicTitle() != null && !concept.topicTitle().isBlank()) {
                            return concept.topicTitle();
                        }
                    } catch (Exception e) {
                        // curriculumGateway 호출 실패 시 로그만 남기고 fallback
                        log.debug("Failed to get topic title for topicId={}: {}", topicId, e.getMessage());
                    }
                }
            }
        } catch (Exception e) {
            // JSON 파싱 실패 등
            log.debug("Failed to parse topicScopeJson for session {}: {}", session.getId(), e.getMessage());
        }
        
        // 최종 fallback: 모드에 따라 기본값 반환
        if (mode != null && mode.startsWith("ASSIST_")) {
            if (mode.contains("DIFFICULTY")) {
                log.info("resolvePartTitle: Fallback - ASSIST_DIFFICULTY mode, returning '난이도 학습'");
                return "난이도 학습";
            } else if (mode.contains("WEAKNESS")) {
                log.info("resolvePartTitle: Fallback - ASSIST_WEAKNESS mode, returning '약점 보완'");
                return "약점 보완";
            } else if (mode.contains("CATEGORY")) {
                log.info("resolvePartTitle: Fallback - ASSIST_CATEGORY mode, returning '카테고리 학습'");
                return "카테고리 학습";
            }
            log.info("resolvePartTitle: Fallback - ASSIST mode, returning '보조 학습'");
            return "보조 학습";
        } else if ("MICRO".equals(mode)) {
            log.info("resolvePartTitle: Fallback - MICRO mode, returning 'Micro 학습'");
            return "Micro 학습";
        } else if ("REVIEW".equals(mode)) {
            log.info("resolvePartTitle: Fallback - REVIEW mode, returning 'Review 학습'");
            return "Review 학습";
        }
        
        log.warn("resolvePartTitle: Unknown mode '{}' for session {}, returning '알 수 없음'", mode, session.getId());
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
