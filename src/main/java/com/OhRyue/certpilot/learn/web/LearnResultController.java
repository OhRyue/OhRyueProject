package com.OhRyue.certpilot.learn.web;

import com.OhRyue.certpilot.learnresult.domain.repo.MicroResultRepository;
import com.OhRyue.certpilot.learnresult.domain.repo.ReviewResultRepository;
import com.OhRyue.certpilot.learn.web.dto.RecentResultsDto;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "LearnResults", description = "최근 학습 결과 조회")
@RestController
@RequestMapping("/learn/results")
@RequiredArgsConstructor
public class LearnResultController {

  private final MicroResultRepository microRepo;
  private final ReviewResultRepository reviewRepo;

  @Operation(summary = "최근 결과(각 20개)")
  @GetMapping("/recent")
  public RecentResultsDto recent(@RequestParam Long userId) {
    var micros = microRepo.findByUserIdOrderByCreatedAtDesc(userId).stream()
        .map(r -> new RecentResultsDto.Micro(r.getId(), r.getTopicId(), r.getConceptId(), r.getScore(), r.getTotal(), r.getCreatedAt()))
        .toList();

    var reviews = reviewRepo.findByUserIdOrderByCreatedAtDesc(userId).stream()
        .map(r -> new RecentResultsDto.Review(r.getId(), r.getDetailTopicId(), r.getScore(), r.getTotal(), r.getCreatedAt()))
        .toList();

    return new RecentResultsDto(micros, reviews);
  }
}
