package com.OhRyue.certpilot.progress.service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.progress.domain.ProgressActivity;
import com.OhRyue.certpilot.progress.domain.enums.ActivityGroup;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import com.OhRyue.certpilot.progress.dto.activity.ActivityDtos.*;
import com.OhRyue.certpilot.progress.dto.activity.ActivityDtos.ActivityDetailDto;
import com.OhRyue.certpilot.progress.dto.activity.ActivityDtos.ActivityDetailHeaderDto;
import com.OhRyue.certpilot.progress.dto.activity.ActivityDtos.ActivityListItemDto;
import com.OhRyue.certpilot.progress.dto.activity.ActivityDtos.ProgressActivityCreateReq;
import com.OhRyue.certpilot.progress.dto.activity.ActivityDtos.QuestionDetailDto;
import com.OhRyue.certpilot.progress.dto.activity.ActivityDtos.RecentActivityItemDto;
import com.OhRyue.certpilot.progress.dto.activity.ActivityDtos.TodaySummaryDto;
import com.OhRyue.certpilot.progress.feign.CertClient;
import com.OhRyue.certpilot.progress.feign.StudyInternalClient;
import com.OhRyue.certpilot.progress.feign.VersusInternalClient;
import com.OhRyue.certpilot.progress.feign.dto.MatchDetailDto;
import com.OhRyue.certpilot.progress.feign.dto.StudySessionDetailDto;
import com.OhRyue.certpilot.progress.repository.ProgressActivityRepository;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ActivityService {

    private static final ZoneId KST = ZoneId.of("Asia/Seoul");

    private final ProgressActivityRepository activityRepository;
    private final StudyInternalClient studyInternalClient;
    private final VersusInternalClient versusInternalClient;
    private final CertClient certClient;
    private final ObjectMapper objectMapper;

    private static final TypeReference<Map<String, Object>> MAP_TYPE = new TypeReference<>() {};

    /**
     * 오늘의 성과 요약
     */
    public TodaySummaryDto getTodaySummary(String userId) {
        LocalDate today = LocalDate.now(KST);
        LocalDateTime startOfDay = today.atStartOfDay();
        LocalDateTime endOfDay = today.atTime(LocalTime.MAX);

        List<ProgressActivity> activities = activityRepository.findByUserIdAndFinishedAtBetween(
                userId, startOfDay, endOfDay
        );

        int solvedQuestionCount = activities.stream()
                .mapToInt(ProgressActivity::getQuestionCount)
                .sum();

        long totalMinutes = activities.stream()
                .mapToLong(a -> ChronoUnit.MINUTES.between(a.getStartedAt(), a.getFinishedAt()))
                .sum();

        int totalCorrect = activities.stream()
                .mapToInt(ProgressActivity::getCorrectCount)
                .sum();

        int totalQuestions = solvedQuestionCount;
        double accuracyPct = totalQuestions > 0
                ? (double) totalCorrect / totalQuestions * 100.0
                : 0.0;

        int gainedXp = activities.stream()
                .filter(a -> a.getXpGained() != null)
                .mapToInt(ProgressActivity::getXpGained)
                .sum();

        return new TodaySummaryDto(
                solvedQuestionCount,
                (int) totalMinutes,
                Math.round(accuracyPct * 10.0) / 10.0,
                gainedXp
        );
    }

    /**
     * 최근 학습 기록 (최대 5개)
     */
    public List<RecentActivityItemDto> getRecentActivities(String userId) {
        List<ProgressActivity> activities = activityRepository.findTop5ByUserIdOrderByFinishedAtDesc(userId);

        return activities.stream()
                .map(activity -> {
                    // MAIN(MICRO/REVIEW)의 경우 topicName 채우기
                    fillTopicNameIfNeeded(activity);
                    return toRecentActivityItem(activity);
                })
                .collect(Collectors.toList());
    }

    /**
     * 전체보기 리스트 (페이지네이션)
     */
    public Page<ActivityListItemDto> getActivityList(String userId, int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        Page<ProgressActivity> activityPage = activityRepository.findByUserIdOrderByFinishedAtDesc(userId, pageable);

        return activityPage.map(activity -> {
            // MAIN(MICRO/REVIEW)의 경우 topicName 채우기
            fillTopicNameIfNeeded(activity);
            return toActivityListItem(activity);
        });
    }

    /**
     * 활동 상세보기
     * study-service 또는 versus-service에서 문제 상세 정보를 가져옴
     */
    public ActivityDetailDto getActivityDetail(String userId, Long activityId) {
        ProgressActivity activity = activityRepository.findById(activityId)
                .orElseThrow(() -> new IllegalArgumentException("Activity not found: " + activityId));

        // userId 검증
        if (!activity.getUserId().equals(userId)) {
            throw new IllegalArgumentException("Activity does not belong to user: " + userId);
        }

        // MAIN(MICRO/REVIEW)의 경우 topicName 채우기
        fillTopicNameIfNeeded(activity);

        ActivityDetailHeaderDto header = toActivityDetailHeader(activity);

        // 문제 상세 조회
        List<QuestionDetailDto> questions;
        try {
            String sourceService = activity.getSourceService();
            switch (sourceService) {
                case "study" -> {
                    // study-service에서 세션 상세 조회
                    StudySessionDetailDto sessionDetail = studyInternalClient
                            .getSessionDetail(activity.getSourceSessionId(), userId);
                    questions = sessionDetail.questions().stream()
                            .map(q -> new QuestionDetailDto(
                                    q.order(),
                                    q.questionId(),
                                    q.questionType(),
                                    q.stem(),
                                    q.myAnswer(),
                                    q.correctAnswer(),
                                    q.isCorrect(),
                                    q.answeredAt(),
                                    q.timeTakenMs(),
                                    q.score()
                            ))
                            .collect(Collectors.toList());
                }
                case "versus" -> {
                    // versus-service에서 매치 상세 조회
                    MatchDetailDto matchDetail = versusInternalClient
                            .getMatchDetail(activity.getSourceSessionId(), userId);
                    questions = matchDetail.questions().stream()
                            .map(q -> new QuestionDetailDto(
                                    q.order(),
                                    q.questionId(),
                                    q.questionType(),
                                    q.stem(),
                                    q.myAnswer(),
                                    q.correctAnswer(),
                                    q.isCorrect(),
                                    q.answeredAt(),
                                    q.timeTakenMs(),
                                    q.score()
                            ))
                            .collect(Collectors.toList());
                }
                default -> {
                    log.warn("Unknown source service: {}", sourceService);
                    questions = List.of();
                }
            }
        } catch (Exception e) {
            log.error("Failed to get activity detail from source service: activityId={}, sourceService={}, sourceSessionId={}",
                    activityId, activity.getSourceService(), activity.getSourceSessionId(), e);
            questions = List.of();
        }

        return new ActivityDetailDto(header, questions);
    }

    /**
     * 활동 생성 (내부 API용)
     * 중복 방지: sourceService와 sourceSessionId가 동일한 Activity가 이미 존재하면 생성하지 않음
     */
    @Transactional
    public ProgressActivity createActivity(ProgressActivityCreateReq req) {
        // 중복 체크: 동일한 sourceService와 sourceSessionId로 이미 Activity가 생성되었는지 확인
        if (req.sourceService() != null && req.sourceSessionId() != null) {
            List<ProgressActivity> existing = activityRepository.findBySource(
                    req.userId(), req.sourceService(), req.sourceSessionId());
            if (!existing.isEmpty()) {
                log.debug("Activity already exists for source: userId={}, sourceService={}, sourceSessionId={}, skipping creation",
                        req.userId(), req.sourceService(), req.sourceSessionId());
                return existing.get(0); // 기존 Activity 반환
            }
        }

        BigDecimal accuracy = req.accuracyPct() != null
                ? BigDecimal.valueOf(req.accuracyPct())
                : BigDecimal.ZERO;

        ProgressActivity activity = ProgressActivity.builder()
                .userId(req.userId())
                .activityGroup(req.activityGroup())
                .mainType(req.mainType())
                .mainStepType(req.mainStepType())
                .assistType(req.assistType())
                .battleType(req.battleType())
                .mode(req.mode())
                .topicId(req.topicId())
                .topicName(req.topicName())
                .weaknessTagName(req.weaknessTagName())
                .difficulty(req.difficulty())
                .questionCount(req.questionCount())
                .correctCount(req.correctCount())
                .accuracyPct(accuracy)
                .finalRank(req.finalRank())
                .xpGained(req.xpGained())
                .sourceService(req.sourceService())
                .sourceSessionId(req.sourceSessionId())
                .startedAt(req.startedAt())
                .finishedAt(req.finishedAt())
                .build();

        return activityRepository.save(activity);
    }

    // ========== Private Helper Methods ==========

    private RecentActivityItemDto toRecentActivityItem(ProgressActivity activity) {
        String displayText = buildDisplayText(activity);
        return new RecentActivityItemDto(
                activity.getId(),
                activity.getActivityGroup(),
                activity.getMainType(),
                activity.getMainStepType(),
                activity.getAssistType(),
                activity.getBattleType(),
                activity.getMode(),
                displayText,
                activity.getStartedAt(),
                activity.getFinishedAt()
        );
    }

    private ActivityListItemDto toActivityListItem(ProgressActivity activity) {
        Double accuracyPct = activity.getAccuracyPct() != null
                ? activity.getAccuracyPct().doubleValue()
                : null;

        return new ActivityListItemDto(
                activity.getId(),
                activity.getActivityGroup(),
                activity.getMainType(),
                activity.getMainStepType(),
                activity.getAssistType(),
                activity.getBattleType(),
                activity.getMode(),
                activity.getTopicName(),
                activity.getWeaknessTagName(),
                activity.getDifficulty(),
                accuracyPct,
                activity.getFinalRank(),
                activity.getXpGained(),
                activity.getFinishedAt()
        );
    }

    private ActivityDetailHeaderDto toActivityDetailHeader(ProgressActivity activity) {
        Double accuracyPct = activity.getAccuracyPct() != null
                ? activity.getAccuracyPct().doubleValue()
                : null;

        return new ActivityDetailHeaderDto(
                activity.getId(),
                activity.getActivityGroup(),
                activity.getMainType(),
                activity.getMainStepType(),
                activity.getAssistType(),
                activity.getBattleType(),
                activity.getMode(),
                activity.getTopicName(),
                activity.getWeaknessTagName(),
                activity.getDifficulty(),
                activity.getFinishedAt(),
                activity.getQuestionCount(),
                activity.getCorrectCount(),
                accuracyPct,
                activity.getFinalRank(),
                activity.getXpGained()
        );
    }

    private String buildDisplayText(ProgressActivity activity) {
        StringBuilder sb = new StringBuilder();

        switch (activity.getActivityGroup()) {
            case MAIN -> {
                sb.append("메인학습");
                if (activity.getMainType() != null) {
                    if (activity.getMainType() == com.OhRyue.certpilot.progress.domain.enums.MainType.MICRO) {
                        sb.append(" · Micro");
                        // 괄호 안의 세부 타입 제거 (MCQ, OX, SHORT 등)
                    } else if (activity.getMainType() == com.OhRyue.certpilot.progress.domain.enums.MainType.REVIEW) {
                        sb.append(" · Review");
                    }
                }
            }
            case ASSIST -> {
                sb.append("ASSIST");
                if (activity.getAssistType() != null) {
                    String assistTypeName = switch (activity.getAssistType()) {
                        case CATEGORY -> "카테고리";
                        case WEAKNESS -> "약점보완";
                        case DIFFICULTY -> "난이도";
                    };
                    sb.append(" · ").append(assistTypeName);
                }
            }
            case BATTLE -> {
                sb.append("BATTLE");
                if (activity.getBattleType() != null) {
                    String battleTypeName = switch (activity.getBattleType()) {
                        case DUEL_CATEGORY -> "1:1 카테고리";
                        case DUEL_DIFFICULTY -> "1:1 난이도";
                        case TOURNAMENT -> "토너먼트";
                        case GOLDENBELL -> "골든벨";
                    };
                    sb.append(" · ").append(battleTypeName);
                }
            }
        }

        if (activity.getMode() != null) {
            String modeName = activity.getMode() == ExamMode.WRITTEN ? "필기" : "실기";
            sb.append(" · ").append(modeName);
        }

        return sb.toString();
    }

    /**
     * MAIN(MICRO/REVIEW) Activity의 topicName을 채우는 헬퍼 메서드
     * - topicName이 이미 있으면 스킵
     * - topicId가 있으면 cert-service에서 조회
     * - 없으면 study-service의 세션 정보에서 topicScopeJson 파싱하여 topicId 추출 후 조회
     */
    private void fillTopicNameIfNeeded(ProgressActivity activity) {
        // ASSIST나 BATTLE은 이미 topicName이 정상적으로 저장되어 있음
        if (activity.getActivityGroup() != ActivityGroup.MAIN) {
            return;
        }

        // 이미 topicName이 있으면 스킵
        if (activity.getTopicName() != null && !activity.getTopicName().isBlank()) {
            return;
        }

        Long topicIdToUse = null;

        // 1. topicId가 직접 있는 경우
        if (activity.getTopicId() != null) {
            topicIdToUse = activity.getTopicId();
        } else if ("study".equals(activity.getSourceService()) && activity.getSourceSessionId() != null) {
            // 2. study-service의 세션 정보에서 topicScopeJson 파싱
            try {
                StudySessionDetailDto sessionDetail = studyInternalClient
                        .getSessionDetail(activity.getSourceSessionId(), activity.getUserId());
                
                if (sessionDetail.topicScopeJson() != null && !sessionDetail.topicScopeJson().isBlank()) {
                    Map<String, Object> scope = objectMapper.readValue(sessionDetail.topicScopeJson(), MAP_TYPE);
                    
                    // MICRO: topicId 사용
                    if (activity.getMainType() != null && 
                        activity.getMainType() == com.OhRyue.certpilot.progress.domain.enums.MainType.MICRO) {
                        Object topicIdObj = scope.get("topicId");
                        if (topicIdObj != null) {
                            topicIdToUse = ((Number) topicIdObj).longValue();
                        }
                    } 
                    // REVIEW: rootTopicId 사용
                    else if (activity.getMainType() != null && 
                             activity.getMainType() == com.OhRyue.certpilot.progress.domain.enums.MainType.REVIEW) {
                        Object rootTopicIdObj = scope.get("rootTopicId");
                        if (rootTopicIdObj != null) {
                            topicIdToUse = ((Number) rootTopicIdObj).longValue();
                        }
                    }
                }
            } catch (Exception e) {
                log.debug("Failed to get topicScopeJson from study-service: activityId={}, sessionId={}, error={}",
                        activity.getId(), activity.getSourceSessionId(), e.getMessage());
            }
        }

        // 3. topicId를 찾았으면 cert-service에서 topicName 조회
        if (topicIdToUse != null) {
            try {
                String topicJson = certClient.getTopic(topicIdToUse);
                if (topicJson != null && !topicJson.isBlank()) {
                    Map<String, Object> topicMap = objectMapper.readValue(topicJson, MAP_TYPE);
                    Object titleObj = topicMap.get("title");
                    if (titleObj != null) {
                        String topicName = titleObj.toString();
                        activity.setTopicName(topicName);
                        log.debug("Filled topicName for activityId={}, topicId={}, topicName={}",
                                activity.getId(), topicIdToUse, topicName);
                    }
                }
            } catch (Exception e) {
                log.warn("Failed to get topic name from cert-service: activityId={}, topicId={}, error={}",
                        activity.getId(), topicIdToUse, e.getMessage());
            }
        }
    }
}
