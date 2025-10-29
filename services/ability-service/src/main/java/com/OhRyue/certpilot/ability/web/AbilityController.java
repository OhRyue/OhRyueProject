package com.OhRyue.certpilot.ability.web;

import com.OhRyue.certpilot.ability.entity.AbilityProfile;
import com.OhRyue.certpilot.ability.entity.AbilityProfileId;
import com.OhRyue.certpilot.ability.repository.AbilityProfileRepository;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name="Ability", description="태그별 능력치(EMA) 조회")
@RestController
@RequestMapping("/api/ability")
@RequiredArgsConstructor
public class AbilityController {

  private final AbilityProfileRepository repo;

  @Operation(summary="사용자 최근 능력치 10개")
  @GetMapping("/recent")
  public List<AbilityProfile> recent(@RequestParam Long userId) {
    return repo.findByUserIdOrderByEmaCorrectAsc(userId);
  }

  @Operation(summary="특정 태그 능력치")
  @GetMapping("/by-tag")
  public AbilityProfile byTag(@RequestParam Long userId, @RequestParam String tag) {
    return repo.findById(new AbilityProfileId(userId, tag))
        .orElseGet(() -> new AbilityProfile(userId, tag));
  }
}

