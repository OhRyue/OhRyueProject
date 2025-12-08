package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.dto.TopicProgressDtos.BatchTopicStatusResp;
import com.OhRyue.certpilot.study.dto.TopicProgressDtos.BatchTopicReviewStatusResp;
import com.OhRyue.certpilot.study.dto.TopicProgressDtos.MicroLearningStatsResp;
import com.OhRyue.certpilot.study.service.TopicProgressService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Topic Progress(토픽 학습 진행 상태)")
@RestController
@RequestMapping("/api/study/topic-progress")
@RequiredArgsConstructor
public class TopicProgressController {

  private final TopicProgressService topicProgressService;

  @Operation(
      summary = "여러 토픽의 micro 학습 완료 상태 조회",
      description = """
          여러 토픽에 대한 micro 학습 완료 상태를 일괄 조회합니다.
          - topicIds: 조회할 토픽 ID 목록 (쉼표로 구분 또는 List)
          - mode: WRITTEN 또는 PRACTICAL
          - 응답의 status 값:
            - NOT_STARTED: 시작 안함
            - IN_PROGRESS: 진행 중
            - COMPLETED: 완료함 (전체 과정 완료)
            - TRULY_COMPLETED: 진정한 완료 (MCQ 완료)
          """
  )
  @GetMapping("/micro-status")
  public BatchTopicStatusResp getBatchMicroStatus(
      @RequestParam List<Long> topicIds,
      @RequestParam ExamMode mode) {
    return topicProgressService.getBatchTopicMicroStatus(topicIds, mode);
  }

  @Operation(
      summary = "Micro + Review 학습 통계 조회",
      description = """
          모든 micro와 review 학습에 대한 통계를 조회합니다.
          - mode: WRITTEN(필기) 또는 PRACTICAL(실기) - 필수
          - totalCount: 전체 학습 개수 (micro: code에 점이 2개 + review: code에 점이 1개)
          - completedCount: 진정한 완료(TRULY_COMPLETED)한 학습 개수
            * micro: MINI와 MCQ 모두 통과(passed=true)
            * review: status=DONE이고 trulyCompleted=true
          - completionRate: 완료 비율(%)
          """
  )
  @GetMapping("/micro-stats")
  public MicroLearningStatsResp getMicroStats(@RequestParam ExamMode mode) {
    return topicProgressService.getMicroLearningStats(mode);
  }

  @Operation(
      summary = "여러 토픽의 review 총정리 완료 상태 조회",
      description = """
          여러 rootTopicId에 대한 review 총정리 완료 상태를 일괄 조회합니다.
          - rootTopicIds: 조회할 루트 토픽 ID 목록 (쉼표로 구분 또는 List)
          - mode: WRITTEN 또는 PRACTICAL (현재는 사용하지 않지만 향후 확장 가능)
          - 응답의 status 값:
            - NOT_STARTED: 시작 안함
            - COMPLETED: 완료함 (전체 과정 완료했지만 문제를 틀림)
            - TRULY_COMPLETED: 진정한 완료 (모든 문제를 맞춤)
          """
  )
  @GetMapping("/review-status")
  public BatchTopicReviewStatusResp getBatchReviewStatus(
      @RequestParam List<Long> rootTopicIds,
      @RequestParam ExamMode mode) {
    return topicProgressService.getBatchTopicReviewStatus(rootTopicIds, mode);
  }
}

