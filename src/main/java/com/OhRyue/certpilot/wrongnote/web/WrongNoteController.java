package com.OhRyue.certpilot.wrongnote.web;

import com.OhRyue.certpilot.wrongnote.domain.WrongNote;
import com.OhRyue.certpilot.wrongnote.domain.WrongNoteStatus;
import com.OhRyue.certpilot.wrongnote.domain.repo.WrongNoteRepository;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name="WrongNote", description = "오답노트 조회/상태 갱신")
@RestController
@RequestMapping("/wrong-notes")
@RequiredArgsConstructor
public class WrongNoteController {

  private final WrongNoteRepository repo;

  @Operation(summary="오답노트 목록(최근순)", description="userId 필수. tag/status 선택. page/size 제공.")
  @GetMapping
  public Page<WrongNote> list(@RequestParam Long userId,
                              @RequestParam(required = false) String tag,
                              @RequestParam(required = false) WrongNoteStatus status,
                              @RequestParam(defaultValue = "0") int page,
                              @RequestParam(defaultValue = "20") int size) {
    // 간단 구현: findAll 후 in-memory 필터 + 정렬 (데이터 커지면 쿼리 메서드로 전환)
    Pageable pageable = PageRequest.of(page, Math.min(size, 100), Sort.by(Sort.Order.desc("lastWrongAt")));
    List<WrongNote> filtered = repo.findAll().stream()
        .filter(w -> w.getUserId().equals(userId))
        .filter(w -> tag == null || w.getTag().equalsIgnoreCase(tag))
        .filter(w -> status == null || w.getStatus() == status)
        .sorted((a,b) -> {
          if (a.getLastWrongAt() == null && b.getLastWrongAt() == null) return 0;
          if (a.getLastWrongAt() == null) return 1;
          if (b.getLastWrongAt() == null) return -1;
          return b.getLastWrongAt().compareTo(a.getLastWrongAt());
        })
        .toList();
    int start = Math.min((int) pageable.getOffset(), filtered.size());
    int end = Math.min(start + pageable.getPageSize(), filtered.size());
    return new PageImpl<>(filtered.subList(start, end), pageable, filtered.size());
  }

  @Operation(summary="오답노트 상태 변경", description="todo/reviewing/cleared 로 상태 전환")
  @PatchMapping("/status")
  public void updateStatus(@RequestParam Long userId,
                           @RequestParam Long questionId,
                           @RequestParam String tag,
                           @RequestParam WrongNoteStatus status) {
    var found = repo.findByUserIdAndQuestionIdAndTag(userId, questionId, tag)
        .orElseThrow(() -> new IllegalArgumentException("wrong_note not found"));
    found.setStatus(status);
    repo.save(found);
  }
}
