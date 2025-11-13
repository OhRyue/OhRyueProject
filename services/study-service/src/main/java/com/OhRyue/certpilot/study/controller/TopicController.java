package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.domain.Topic;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.repository.TopicRepository;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Topic(학습 토픽)")
@RestController
@RequestMapping("/api/study/topics")
@RequiredArgsConstructor
public class TopicController {

  private final TopicRepository repo;

  /* -------- 토픽 목록 -------- */
  @Operation(summary = "토픽 목록 조회", description = "시험 모드/부모 토픽 ID 조건으로 필터링 가능합니다.")
  @GetMapping
  public List<Topic> list(@RequestParam(required = false) ExamMode mode,
                          @RequestParam(required = false, defaultValue = "-1") Long parentId) {
    if (mode != null && parentId != -1) {
      return repo.findByExamMode(mode).stream()
          .filter(t -> parentId == -1 || (t.getParentId()!=null && t.getParentId().equals(parentId)))
          .toList();
    }
    if (mode != null) return repo.findByExamMode(mode);
    if (parentId != -1) return repo.findByParentId(parentId);
    return repo.findAll();
  }

  /* -------- 토픽 검색 -------- */
  @Operation(summary = "토픽 코드/제목 검색")
  @GetMapping("/search")
  public List<Topic> search(@RequestParam(required = false) String code,
                            @RequestParam(required = false) String title) {
    return repo.findAll().stream()
        .filter(t -> code == null || (t.getCode()!=null && t.getCode().contains(code)))
        .filter(t -> title == null || (t.getTitle()!=null && t.getTitle().contains(title)))
        .toList();
  }
}
