package com.OhRyue.certpilot.wrongnote.web;

import com.OhRyue.certpilot.wrongnote.entity.WrongNote;
import com.OhRyue.certpilot.wrongnote.repository.WrongNoteRepository;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name="WrongNote")
@RestController
@RequestMapping("/api/wrongnote")
@RequiredArgsConstructor
public class WrongNoteController {

  private final WrongNoteRepository repo;

  @Operation(summary="사용자 오답 최근순")
  @GetMapping
  public List<WrongNote> list(@RequestParam Long userId) {
    return repo.findAll().stream()
        .filter(w -> w.getUserId().equals(userId))
        .sorted((a,b) -> {
          if (a.getLastWrongAt() == null && b.getLastWrongAt() == null) return 0;
          if (a.getLastWrongAt() == null) return 1;
          if (b.getLastWrongAt() == null) return -1;
          return b.getLastWrongAt().compareTo(a.getLastWrongAt());
        })
        .toList();
  }
}

