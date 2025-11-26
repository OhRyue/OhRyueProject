package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.dto.TopicProgressDtos.BatchTopicStatusResp;
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
}

