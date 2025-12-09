package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.client.CertCurriculumClient;
import com.OhRyue.certpilot.study.domain.LearningSession;
import com.OhRyue.certpilot.study.domain.StudySession;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.dto.TopicProgressDtos.*;
import com.OhRyue.certpilot.study.repository.LearningSessionRepository;
import com.OhRyue.certpilot.study.repository.StudySessionRepository;
import com.OhRyue.common.auth.AuthUserUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class TopicProgressService {

  private final LearningSessionRepository learningSessionRepository;
  private final StudySessionRepository studySessionRepository;
  private final CertCurriculumClient certCurriculumClient;
  private final ObjectMapper objectMapper;
  
  private static final TypeReference<Map<String, Object>> MAP_TYPE = new TypeReference<>() {};

  @Transactional(readOnly = true)
  public BatchTopicStatusResp getBatchTopicMicroStatus(List<Long> topicIds, ExamMode examMode) {
    String userId = AuthUserUtil.getCurrentUserId();

    // StudySession 조회: mode="MICRO", examMode
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
            return topicIdObj != null && topicIds.contains(((Number) topicIdObj).longValue());
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

    // 각 토픽에 대한 상태 결정 (passed 필드 기반)
    List<TopicMicroStatus> statuses = topicIds.stream()
        .map(topicId -> {
          List<StudySession> topicSessions = sessionsByTopic.getOrDefault(topicId, List.of());
          
          if (topicSessions.isEmpty()) {
            // 시작 안함
            return new TopicMicroStatus(topicId, "NOT_STARTED", false);
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
          
          // 이어서 하기 가능 여부: OPEN 또는 SUBMITTED 상태의 세션이 있고, 아직 완료되지 않은 경우
          boolean hasInProgress = topicSessions.stream()
              .anyMatch(s -> "OPEN".equals(s.getStatus()) || 
                           ("SUBMITTED".equals(s.getStatus()) && !Boolean.TRUE.equals(s.getPassed())));
          
          // 완료 상태 결정 (passed 필드 기반)
          String status;
          boolean miniPassed = miniSession != null && Boolean.TRUE.equals(miniSession.getPassed());
          boolean mcqPassed = mcqSession != null && Boolean.TRUE.equals(mcqSession.getPassed());
          
          if (miniPassed && mcqPassed) {
            // MINI와 MCQ 모두 통과 (진정한 완료)
            status = "TRULY_COMPLETED";
          } else if (miniSession != null || mcqSession != null) {
            // 일부 세션만 존재하거나 하나라도 통과하지 못함 (일반 완료 또는 진행 중)
            status = "COMPLETED";
          } else {
            // 세션이 없음
            status = "NOT_STARTED";
          }
          
          // resumable은 IN_PROGRESS 세션 존재 여부로만 결정 (완료 상태와 독립적)
          return new TopicMicroStatus(topicId, status, hasInProgress);
        })
        .collect(Collectors.toList());

    return new BatchTopicStatusResp(statuses);
  }

  @Transactional(readOnly = true)
  public MicroLearningStatsResp getMicroLearningStats(ExamMode examMode) {
    String userId = AuthUserUtil.getCurrentUserId();
    String modeStr = examMode.name();

    // 1. cert-service에서 모든 topic 조회
    CertCurriculumClient.TopicListResponse topicList = certCurriculumClient.listTopics(null, modeStr, null);
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

    long totalCount = microTopics.size() + reviewTopics.size();
    if (totalCount == 0) {
      return new MicroLearningStatsResp(0L, 0L, 0.0);
    }

    // 4. micro topic들의 ID 추출
    List<Long> microTopicIds = microTopics.stream()
        .map(CertCurriculumClient.TopicResponse::id)
        .collect(Collectors.toList());

    // 5. review topic들의 ID 추출
    List<Long> reviewTopicIds = reviewTopics.stream()
        .map(CertCurriculumClient.TopicResponse::id)
        .collect(Collectors.toList());

    // 6. micro 완료 카운트: getBatchTopicMicroStatus와 동일한 로직 사용
    // StudySession을 조회하여 passed 필드 기반으로 완료 상태 판단
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

    // 각 micro 토픽에 대한 완료 상태 확인 (getBatchTopicMicroStatus와 동일한 로직)
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
          
          // 완료 상태 결정 (passed 필드 기반)
          boolean miniPassed = miniSession != null && Boolean.TRUE.equals(miniSession.getPassed());
          boolean mcqPassed = mcqSession != null && Boolean.TRUE.equals(mcqSession.getPassed());
          
          // TRULY_COMPLETED 상태인 경우만 완료로 카운트 (miniPassed && mcqPassed)
          return miniPassed && mcqPassed;
        })
        .count();

    // 7. review 완료 카운트: getBatchTopicReviewStatus와 동일한 로직 사용
    // StudySession의 completed=1 AND passed=1인 것만 카운트 (TRULY_COMPLETED만 포함)
    List<StudySession> allReviewSessions = studySessionRepository
        .findByUserIdAndModeAndExamMode(userId, "REVIEW", examMode);

    // reviewTopicIds에 해당하는 세션만 필터링 (topic_scope_json에서 rootTopicId 추출)
    Map<Long, List<StudySession>> reviewSessionsByRootTopic = allReviewSessions.stream()
        .filter(session -> {
          try {
            if (session.getTopicScopeJson() == null || session.getTopicScopeJson().isBlank()) {
              return false;
            }
            Map<String, Object> scope = objectMapper.readValue(session.getTopicScopeJson(), MAP_TYPE);
            Object rootTopicIdObj = scope.get("rootTopicId");
            return rootTopicIdObj != null && reviewTopicIds.contains(((Number) rootTopicIdObj).longValue());
          } catch (Exception e) {
            return false;
          }
        })
        .collect(Collectors.groupingBy(session -> {
          try {
            Map<String, Object> scope = objectMapper.readValue(session.getTopicScopeJson(), MAP_TYPE);
            Object rootTopicIdObj = scope.get("rootTopicId");
            return rootTopicIdObj != null ? ((Number) rootTopicIdObj).longValue() : 0L;
          } catch (Exception e) {
            return 0L;
          }
        }));

    // 각 rootTopicId에 대해 TRULY_COMPLETED 상태인지 확인
    long completedReviewCount = reviewTopicIds.stream()
        .filter(rootTopicId -> {
          List<StudySession> topicSessions = reviewSessionsByRootTopic.getOrDefault(rootTopicId, List.of());
          
          if (topicSessions.isEmpty()) {
            return false;
          }
          
          // 가장 최근 완료된 세션 찾기
          StudySession latestCompletedSession = topicSessions.stream()
              .filter(s -> Boolean.TRUE.equals(s.getCompleted()) && 
                          ("SUBMITTED".equals(s.getStatus()) || "CLOSED".equals(s.getStatus())))
              .max((s1, s2) -> s1.getStartedAt().compareTo(s2.getStartedAt()))
              .orElse(null);
          
          // completed=1 AND passed=1 → TRULY_COMPLETED
          return latestCompletedSession != null && Boolean.TRUE.equals(latestCompletedSession.getPassed());
        })
        .count();

    long completedCount = completedMicroCount + completedReviewCount;

    // 8. 비율 계산
    double completionRate = totalCount == 0 
        ? 0.0 
        : Math.round(((double) completedCount / totalCount) * 10000.0) / 100.0; // 소수점 2자리

    return new MicroLearningStatsResp(totalCount, completedCount, completionRate);
  }

  @Transactional(readOnly = true)
  public BatchTopicReviewStatusResp getBatchTopicReviewStatus(List<Long> rootTopicIds, ExamMode examMode) {
    String userId = AuthUserUtil.getCurrentUserId();
    String modeStr = "REVIEW";  // Review 모드는 항상 "REVIEW" 문자열

    // StudySession 조회: mode="REVIEW", examMode
    List<StudySession> allReviewSessions = studySessionRepository
        .findByUserIdAndModeAndExamMode(userId, modeStr, examMode);

    // rootTopicId별로 세션 그룹화 (topic_scope_json에서 rootTopicId 추출)
    Map<Long, List<StudySession>> sessionsByRootTopic = allReviewSessions.stream()
        .filter(session -> {
          try {
            if (session.getTopicScopeJson() == null || session.getTopicScopeJson().isBlank()) {
              return false;
            }
            Map<String, Object> scope = objectMapper.readValue(session.getTopicScopeJson(), MAP_TYPE);
            Object rootTopicIdObj = scope.get("rootTopicId");
            return rootTopicIdObj != null && rootTopicIds.contains(((Number) rootTopicIdObj).longValue());
          } catch (Exception e) {
            return false;
          }
        })
        .collect(Collectors.groupingBy(session -> {
          try {
            Map<String, Object> scope = objectMapper.readValue(session.getTopicScopeJson(), MAP_TYPE);
            Object rootTopicIdObj = scope.get("rootTopicId");
            return rootTopicIdObj != null ? ((Number) rootTopicIdObj).longValue() : 0L;
          } catch (Exception e) {
            return 0L;
          }
        }));

    // 각 rootTopicId에 대한 상태 결정
    // 완료 상태(status)와 이어서 하기 가능 여부(resumable)는 독립적으로 결정
    List<TopicReviewStatus> statuses = rootTopicIds.stream()
        .map(rootTopicId -> {
          List<StudySession> topicSessions = sessionsByRootTopic.getOrDefault(rootTopicId, List.of());
          
          if (topicSessions.isEmpty()) {
            // 시작 안함
            return new TopicReviewStatus(rootTopicId, "NOT_STARTED", false);
          }
          
          // 가장 최근 세션 찾기 (완료된 세션 우선, 그 다음 진행 중 세션)
          StudySession latestCompletedSession = topicSessions.stream()
              .filter(s -> Boolean.TRUE.equals(s.getCompleted()) && 
                          ("SUBMITTED".equals(s.getStatus()) || "CLOSED".equals(s.getStatus())))
              .max((s1, s2) -> s1.getStartedAt().compareTo(s2.getStartedAt()))
              .orElse(null);
          
          // 이어서 하기 가능 여부: 진행 중인 세션 존재 여부
          // OPEN 상태이거나, SUBMITTED 상태이지만 completed=false인 경우
          boolean hasInProgress = topicSessions.stream()
              .anyMatch(s -> "OPEN".equals(s.getStatus()) || 
                           ("SUBMITTED".equals(s.getStatus()) && !Boolean.TRUE.equals(s.getCompleted())));
          
          // 완료 상태 결정 (completed와 passed 필드 기반)
          String status;
          if (latestCompletedSession == null) {
            // 완료된 세션이 없으면
            if (hasInProgress) {
              // 진행 중인 세션이 있으면
              status = "IN_PROGRESS";
            } else {
              // 진행 중인 세션도 없으면 시작 안함
              status = "NOT_STARTED";
            }
          } else if (Boolean.TRUE.equals(latestCompletedSession.getPassed())) {
            // completed=1 AND passed=1 → TRULY_COMPLETED
            status = "TRULY_COMPLETED";
          } else {
            // completed=1 AND passed=0 → COMPLETED
            status = "COMPLETED";
          }
          
          // resumable은 진행 중인 세션 존재 여부로만 결정 (완료 상태와 독립적)
          return new TopicReviewStatus(rootTopicId, status, hasInProgress);
        })
        .collect(Collectors.toList());

    return new BatchTopicReviewStatusResp(statuses);
  }
}

