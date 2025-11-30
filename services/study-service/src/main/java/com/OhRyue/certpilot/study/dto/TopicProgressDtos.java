package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.util.List;

public class TopicProgressDtos {

  @Schema(description = "토픽별 micro 학습 완료 상태")
  public record TopicMicroStatus(
      @Schema(description = "토픽 ID") Long topicId,
      @Schema(description = "학습 상태: NOT_STARTED(시작 안함), COMPLETED(완료), TRULY_COMPLETED(진정한 완료)") 
      String status,
      @Schema(description = "이어서 진행하기 가능 여부 (IN_PROGRESS 세션이 있으면 true)") 
      Boolean resumable
  ) {}

  @Schema(description = "여러 토픽의 micro 학습 완료 상태 조회 응답")
  public record BatchTopicStatusResp(
      @Schema(description = "토픽별 상태 목록") List<TopicMicroStatus> statuses
  ) {}

  @Schema(description = "Micro 학습 통계 응답")
  public record MicroLearningStatsResp(
      @Schema(description = "전체 micro 학습 개수") Long totalCount,
      @Schema(description = "진정한 완료한 micro 학습 개수") Long completedCount,
      @Schema(description = "완료 비율(%)") Double completionRate
  ) {}

  @Schema(description = "토픽별 review 총정리 완료 상태")
  public record TopicReviewStatus(
      @Schema(description = "루트 토픽 ID") Long rootTopicId,
      @Schema(description = "학습 상태: NOT_STARTED(시작 안함), COMPLETED(완료), TRULY_COMPLETED(진정한 완료)") 
      String status,
      @Schema(description = "이어서 진행하기 가능 여부 (IN_PROGRESS 세션이 있으면 true)") 
      Boolean resumable
  ) {}

  @Schema(description = "여러 토픽의 review 총정리 완료 상태 조회 응답")
  public record BatchTopicReviewStatusResp(
      @Schema(description = "토픽별 상태 목록") List<TopicReviewStatus> statuses
  ) {}
}

