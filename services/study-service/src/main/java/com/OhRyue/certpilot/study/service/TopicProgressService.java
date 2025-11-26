package com.OhRyue.certpilot.study.service;

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

  @Transactional(readOnly = true)
  public BatchTopicStatusResp getBatchTopicMicroStatus(List<Long> topicIds, ExamMode examMode) {
    String userId = AuthUserUtil.getCurrentUserId();
    String modeStr = examMode.name();

    // 여러 토픽의 학습 세션을 한 번에 조회
    List<LearningSession> sessions = learningSessionRepository
        .findByUserIdAndTopicIdInAndMode(userId, topicIds, modeStr);

    // topicId -> LearningSession 매핑 (가장 최근 세션만 사용)
    Map<Long, LearningSession> sessionMap = sessions.stream()
        .collect(Collectors.toMap(
            LearningSession::getTopicId,
            session -> session,
            (existing, replacement) -> 
                existing.getUpdatedAt().isAfter(replacement.getUpdatedAt()) ? existing : replacement
        ));

    // 각 토픽에 대한 상태 결정
    List<TopicMicroStatus> statuses = topicIds.stream()
        .map(topicId -> {
          LearningSession session = sessionMap.get(topicId);
          
          if (session == null) {
            // 시작 안함
            return new TopicMicroStatus(topicId, "NOT_STARTED");
          } else if (Boolean.TRUE.equals(session.getTrulyCompleted())) {
            // 진정한 완료 (MCQ 완료)
            return new TopicMicroStatus(topicId, "TRULY_COMPLETED");
          } else if ("DONE".equals(session.getStatus())) {
            // 일반 완료 (전체 과정 완료)
            return new TopicMicroStatus(topicId, "COMPLETED");
          } else {
            // 진행 중
            return new TopicMicroStatus(topicId, "IN_PROGRESS");
          }
        })
        .collect(Collectors.toList());

    return new BatchTopicStatusResp(statuses);
  }
}

