package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.domain.Topic;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.repository.TopicRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/study/topics")
@RequiredArgsConstructor
public class TopicController {

  private final TopicRepository repo;

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

  @GetMapping("/search")
  public List<Topic> search(@RequestParam(required = false) String code,
                            @RequestParam(required = false) String title) {
    return repo.findAll().stream()
        .filter(t -> code == null || (t.getCode()!=null && t.getCode().contains(code)))
        .filter(t -> title == null || (t.getTitle()!=null && t.getTitle().contains(title)))
        .toList();
  }
}
