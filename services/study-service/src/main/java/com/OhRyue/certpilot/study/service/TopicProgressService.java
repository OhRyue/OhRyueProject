package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.client.CertCurriculumClient;
import com.OhRyue.certpilot.study.domain.LearningSession;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.dto.TopicProgressDtos.*;
import com.OhRyue.certpilot.study.repository.LearningSessionRepository;
import com.OhRyue.common.auth.AuthUserUtil;
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
  private final CertCurriculumClient certCurriculumClient;

  @Transactional(readOnly = true)
  public BatchTopicStatusResp getBatchTopicMicroStatus(List<Long> topicIds, ExamMode examMode) {
    String userId = AuthUserUtil.getCurrentUserId();
    String modeStr = examMode.name();

    // 여러 토픽의 학습 세션을 한 번에 조회
    List<LearningSession> allSessions = learningSessionRepository
        .findByUserIdAndTopicIdInAndMode(userId, topicIds, modeStr);

    // topicId별로 세션 그룹화
    Map<Long, List<LearningSession>> sessionsByTopic = allSessions.stream()
        .collect(Collectors.groupingBy(LearningSession::getTopicId));

    // 각 토픽에 대한 상태 결정
    List<TopicMicroStatus> statuses = topicIds.stream()
        .map(topicId -> {
          List<LearningSession> topicSessions = sessionsByTopic.getOrDefault(topicId, List.of());
          
          if (topicSessions.isEmpty()) {
            // 시작 안함
            return new TopicMicroStatus(topicId, "NOT_STARTED", false);
          }
          
          // 완료된 세션 중 가장 최근 것 찾기 (DONE 상태)
          LearningSession completedSession = topicSessions.stream()
              .filter(s -> "DONE".equals(s.getStatus()))
              .max((s1, s2) -> s1.getUpdatedAt().compareTo(s2.getUpdatedAt()))
              .orElse(null);
          
          // 진행 중인 세션 확인 (IN_PROGRESS 상태)
          boolean hasInProgress = topicSessions.stream()
              .anyMatch(s -> "IN_PROGRESS".equals(s.getStatus()));
          
          // 완료 상태 결정
          String status;
          if (completedSession == null) {
            // 완료된 세션이 없으면 시작 안함
            status = "NOT_STARTED";
          } else if (Boolean.TRUE.equals(completedSession.getTrulyCompleted())) {
            // 진정한 완료 (MCQ 완료)
            status = "TRULY_COMPLETED";
          } else {
            // 일반 완료 (전체 과정 완료)
            status = "COMPLETED";
          }
          
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

    long totalCount = microTopics.size();
    if (totalCount == 0) {
      return new MicroLearningStatsResp(0L, 0L, 0.0);
    }

    // 3. micro topic들의 ID 추출
    List<Long> microTopicIds = microTopics.stream()
        .map(CertCurriculumClient.TopicResponse::id)
        .collect(Collectors.toList());

    // 4. 해당 topic들의 LearningSession 조회
    List<LearningSession> sessions = learningSessionRepository
        .findByUserIdAndTopicIdInAndMode(userId, microTopicIds, modeStr);

    // 5. 진정한 완료(trulyCompleted = true)한 토픽 개수 계산
    // 한 번이라도 완료한 토픽을 카운트 (중복 카운트 방지)
    java.util.Set<Long> completedTopicIds = sessions.stream()
        .filter(session -> Boolean.TRUE.equals(session.getTrulyCompleted()))
        .map(LearningSession::getTopicId)
        .collect(Collectors.toSet());

    long completedCount = completedTopicIds.size();

    // 6. 비율 계산
    double completionRate = totalCount == 0 
        ? 0.0 
        : Math.round(((double) completedCount / totalCount) * 10000.0) / 100.0; // 소수점 2자리

    return new MicroLearningStatsResp(totalCount, completedCount, completionRate);
  }
}

