package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.client.CertCurriculumClient;
import com.OhRyue.certpilot.study.client.ProgressActivityClient;
import com.OhRyue.certpilot.study.domain.StudySession;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Map;

@Slf4j
@Service
@RequiredArgsConstructor
public class ProgressActivityService {

    private final ProgressActivityClient progressActivityClient;
    private final CertCurriculumClient certCurriculumClient;
    private final ObjectMapper objectMapper;

    private static final TypeReference<Map<String, Object>> MAP_TYPE = new TypeReference<>() {};
    private static final ZoneId KST = ZoneId.of("Asia/Seoul");

    /**
     * 세션이 완료된 경우 Activity 생성 보장 (idempotent, 비동기)
     * summary 호출 시점에 사용하는 공통 메서드
     * - MINI 스텝은 제외
     * - 이미 Activity가 있으면 생성하지 않음 (progress-service에서 중복 체크)
     * - session의 score_pct와 question_count를 사용하여 통계 계산
     */
    @Async
    public void ensureActivityForSession(StudySession session) {
        if (session == null) {
            log.warn("ensureActivityForSession: session is null");
            return;
        }
        
        // MINI 스텝은 Activity 생성 제외
        String stepCode = session.getLearningStep() != null ? session.getLearningStep().getStepCode() : null;
        boolean isMiniStep = "MINI".equals(stepCode) || "MICRO_MINI".equals(stepCode);
        if (isMiniStep) {
            log.debug("ensureActivityForSession: MINI step skipped, sessionId={}, stepCode={}", session.getId(), stepCode);
            return;
        }
        
        // 세션이 완료되지 않았으면 Activity 생성하지 않음
        if (!Boolean.TRUE.equals(session.getCompleted()) || session.getFinishedAt() == null) {
            log.debug("ensureActivityForSession: session not completed, sessionId={}, completed={}, finishedAt={}", 
                    session.getId(), session.getCompleted(), session.getFinishedAt());
            return;
        }
        
        // session의 통계 정보 사용
        int total = session.getQuestionCount() != null ? session.getQuestionCount() : 0;
        double scorePct = session.getScorePct() != null ? session.getScorePct() : 0.0;
        int correct = total > 0 && scorePct > 0 ? (int) Math.round((scorePct * total) / 100.0) : 0;
        
        if (total == 0) {
            log.warn("ensureActivityForSession: session has no questions, sessionId={}", session.getId());
            return;
        }
        
        log.info("ensureActivityForSession: creating Activity for completed session, sessionId={}, mode={}, total={}, correct={}, scorePct={}", 
                session.getId(), session.getMode(), total, correct, scorePct);
        
        // Activity 생성 (중복 체크는 createActivity 내부에서 처리)
        createActivityForSession(session, total, correct, scorePct);
    }
    
    /**
     * 세션 종료 시 ProgressActivity 생성 (비동기)
     */
    @Async
    public void createActivityForSession(StudySession session, int total, int correct, double scorePct) {
        try {
            // ActivityGroup 및 타입 결정
            String activityGroup;
            String mainType = null;
            String mainStepType = null;
            String assistType = null;
            
            String mode = session.getMode();
            if ("MICRO".equals(mode) || "REVIEW".equals(mode)) {
                activityGroup = "MAIN";
                mainType = mode.equals("MICRO") ? "MICRO" : "REVIEW";
                
                // MICRO일 때만 mainStepType 결정
                if ("MICRO".equals(mode) && session.getLearningStep() != null) {
                    String stepCode = session.getLearningStep().getStepCode();
                    if ("MINI".equals(stepCode)) {
                        mainStepType = "MINI";
                    } else if ("MCQ".equals(stepCode)) {
                        mainStepType = "MCQ";
                    } else if ("SHORT".equals(stepCode)) {
                        mainStepType = "SHORT";
                    }
                }
            } else if (mode != null && mode.startsWith("ASSIST_")) {
                activityGroup = "ASSIST";
                if (mode.contains("CATEGORY")) {
                    assistType = "CATEGORY";
                } else if (mode.contains("WEAKNESS")) {
                    assistType = "WEAKNESS";
                } else if (mode.contains("DIFFICULTY")) {
                    assistType = "DIFFICULTY";
                }
            } else {
                log.warn("Unknown session mode for activity creation: mode={}, sessionId={}", mode, session.getId());
                return;
            }

            // topicId 및 topicName 추출
            Long topicId = null;
            String topicName = null;
            String weaknessTagName = null;
            String difficulty = null;

            if (session.getTopicScopeJson() != null && !session.getTopicScopeJson().isBlank()) {
                try {
                    Map<String, Object> scope = objectMapper.readValue(session.getTopicScopeJson(), MAP_TYPE);
                    
                    // ASSIST 세션의 경우 특별 처리
                    if (activityGroup.equals("ASSIST")) {
                        // 약점 태그명 추출
                        Object weaknessTagObj = scope.get("weaknessTagName");
                        if (weaknessTagObj != null) {
                            weaknessTagName = weaknessTagObj.toString();
                        }
                        // weaknessTags 배열에서 첫 번째 태그명 추출
                        if (weaknessTagName == null) {
                            Object weaknessTagsObj = scope.get("weaknessTags");
                            if (weaknessTagsObj instanceof java.util.List && !((java.util.List<?>) weaknessTagsObj).isEmpty()) {
                                Object firstTag = ((java.util.List<?>) weaknessTagsObj).get(0);
                                if (firstTag != null) {
                                    weaknessTagName = firstTag.toString();
                                }
                            }
                        }
                        
                        // 난이도 추출
                        Object difficultyObj = scope.get("difficulty");
                        if (difficultyObj != null) {
                            difficulty = difficultyObj.toString();
                        }
                        
                        // topicIds 배열에서 첫 번째 topicId로 topicName 가져오기
                        Object topicIdsObj = scope.get("topicIds");
                        if (topicIdsObj instanceof java.util.List && !((java.util.List<?>) topicIdsObj).isEmpty()) {
                            Object firstTopicIdObj = ((java.util.List<?>) topicIdsObj).get(0);
                            if (firstTopicIdObj instanceof Number) {
                                topicId = ((Number) firstTopicIdObj).longValue();
                                try {
                                    CertCurriculumClient.TopicResponse topic = certCurriculumClient.getTopic(topicId);
                                    if (topic != null && topic.title() != null && !topic.title().isBlank()) {
                                        topicName = topic.title();
                                    }
                                } catch (Exception e) {
                                    log.debug("Failed to get topic name from cert-service: topicId={}", topicId, e);
                                }
                            }
                        }
                        
                        // topicName이 없으면 assistType에 따라 기본값 설정
                        if (topicName == null || topicName.isBlank()) {
                            if ("DIFFICULTY".equals(assistType)) {
                                topicName = "난이도 학습";
                            } else if ("WEAKNESS".equals(assistType)) {
                                topicName = "약점 보완";
                            } else if ("CATEGORY".equals(assistType)) {
                                topicName = "카테고리 학습";
                            } else {
                                topicName = "보조 학습";
                            }
                        }
                    } else {
                        // MAIN 세션의 경우 기존 로직 유지
                        Object topicIdObj = scope.get("topicId");
                        if (topicIdObj == null) {
                            topicIdObj = scope.get("rootTopicId");
                        }
                        if (topicIdObj != null) {
                            topicId = ((Number) topicIdObj).longValue();
                            // cert-service에서 topicName 가져오기
                            try {
                                CertCurriculumClient.TopicResponse topic = certCurriculumClient.getTopic(topicId);
                                if (topic != null) {
                                    topicName = topic.title();
                                }
                            } catch (Exception e) {
                                log.warn("Failed to get topic name from cert-service: topicId={}", topicId, e);
                            }
                        }
                    }
                } catch (Exception e) {
                    log.warn("Failed to parse topicScopeJson: {}", session.getTopicScopeJson(), e);
                }
            }
            
            // ASSIST 세션인데 topicName이 여전히 없으면 기본값 설정
            if (activityGroup.equals("ASSIST") && (topicName == null || topicName.isBlank())) {
                if ("DIFFICULTY".equals(assistType)) {
                    topicName = "난이도 학습";
                } else if ("WEAKNESS".equals(assistType)) {
                    topicName = "약점 보완";
                } else if ("CATEGORY".equals(assistType)) {
                    topicName = "카테고리 학습";
                } else {
                    topicName = "보조 학습";
                }
            }

            // XP는 이미 지급되었을 수 있으므로, summary에서 가져오거나 0으로 설정
            // (실제로는 XP 지급 로직과 연동 필요)
            Integer xpGained = 0; // TODO: 실제 XP 지급량과 연동

            // 시간 변환
            LocalDateTime startedAt = session.getStartedAt() != null
                    ? session.getStartedAt().atZone(KST).toLocalDateTime()
                    : LocalDateTime.now();
            LocalDateTime finishedAt = session.getFinishedAt() != null
                    ? session.getFinishedAt().atZone(KST).toLocalDateTime()
                    : LocalDateTime.now();

            ProgressActivityClient.ProgressActivityCreateReq req = new ProgressActivityClient.ProgressActivityCreateReq(
                    session.getUserId(),
                    activityGroup,
                    mainType,
                    mainStepType,
                    assistType,
                    null, // battleType
                    session.getExamMode().name(),
                    topicId,
                    topicName,
                    weaknessTagName,
                    difficulty,
                    total,
                    correct,
                    scorePct,
                    null, // finalRank
                    xpGained,
                    "study",
                    session.getId(),
                    startedAt,
                    finishedAt
            );

            progressActivityClient.createActivity(req);
            log.info("ProgressActivity created for session: sessionId={}, userId={}, mode={}", 
                    session.getId(), session.getUserId(), mode);
        } catch (Exception e) {
            log.error("Failed to create ProgressActivity for session: sessionId={}", session.getId(), e);
            // 실패해도 세션 종료는 계속 진행
        }
    }
}



