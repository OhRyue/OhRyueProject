package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.client.CertCurriculumClient;
import com.OhRyue.certpilot.study.client.ProgressActivityClient;
import com.OhRyue.certpilot.study.domain.StudySession;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Map;
import java.util.Optional;

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
     * 세션 종료 시 ProgressActivity 생성 (비동기)
     */
    @Async
    public void createActivityForSession(StudySession session, int total, int correct, double scorePct) {
        try {
            // ActivityGroup 및 타입 결정
            String activityGroup;
            String mainType = null;
            String assistType = null;
            
            String mode = session.getMode();
            if ("MICRO".equals(mode) || "REVIEW".equals(mode)) {
                activityGroup = "MAIN";
                mainType = mode.equals("MICRO") ? "MICRO" : "REVIEW";
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
                    Object topicIdObj = scope.get("topicId");
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
                    // 약점 태그명 추출
                    Object weaknessTagObj = scope.get("weaknessTagName");
                    if (weaknessTagObj != null) {
                        weaknessTagName = weaknessTagObj.toString();
                    }
                    // 난이도 추출
                    Object difficultyObj = scope.get("difficulty");
                    if (difficultyObj != null) {
                        difficulty = difficultyObj.toString();
                    }
                } catch (Exception e) {
                    log.warn("Failed to parse topicScopeJson: {}", session.getTopicScopeJson(), e);
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



