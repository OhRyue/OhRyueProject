package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.util.List;

public class TopicProgressDtos {

  @Schema(description = "토픽별 micro 학습 완료 상태")
  public record TopicMicroStatus(
      @Schema(description = "토픽 ID") Long topicId,
      @Schema(description = "학습 상태: NOT_STARTED(시작 안함), IN_PROGRESS(진행 중), COMPLETED(완료), TRULY_COMPLETED(진정한 완료)") 
      String status
  ) {}

  @Schema(description = "여러 토픽의 micro 학습 완료 상태 조회 응답")
  public record BatchTopicStatusResp(
      @Schema(description = "토픽별 상태 목록") List<TopicMicroStatus> statuses
  ) {}
}

