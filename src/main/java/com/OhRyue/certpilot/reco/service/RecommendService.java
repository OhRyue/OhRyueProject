package com.OhRyue.certpilot.reco.service;

import com.OhRyue.certpilot.ability.domain.AbilityProfile;
import com.OhRyue.certpilot.ability.domain.repo.AbilityProfileRepository;
import com.OhRyue.certpilot.quiz.service.QuizQuickService;
import com.OhRyue.certpilot.quiz.web.dto.QuickSessionDto;
import com.OhRyue.certpilot.wrongnote.domain.WrongNote;
import com.OhRyue.certpilot.wrongnote.domain.repo.WrongNoteRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecommendService {

  private final AbilityProfileRepository abilityRepo;
  private final WrongNoteRepository wrongRepo;
  private final QuizQuickService quickService;

  private static final double EMA_WEAK_THRESHOLD = 0.5;

  @Transactional(readOnly = true)
  public RecoResult recommendNext(Long userId, int count, boolean withSession) {
    // 1) EMA 낮은 태그
    List<AbilityProfile> all = abilityRepo.findByUserId(userId);
    List<String> weak = all.stream()
        .filter(a -> a.getEmaCorrect().doubleValue() <= EMA_WEAK_THRESHOLD)
        .map(AbilityProfile::getTag)
        .toList();

    // 2) 최근 오답 태그 보충
    List<WrongNote> wrongs = wrongRepo.findAll().stream()
        .filter(w -> w.getUserId().equals(userId))
        .sorted((a,b) -> {
          if (a.getLastWrongAt()==null && b.getLastWrongAt()==null) return 0;
          if (a.getLastWrongAt()==null) return 1;
          if (b.getLastWrongAt()==null) return -1;
          return b.getLastWrongAt().compareTo(a.getLastWrongAt());
        }).toList();
    List<String> wrongTags = wrongs.stream().map(WrongNote::getTag).toList();

    // 3) unique 순서 유지
    LinkedHashSet<String> candidates = new LinkedHashSet<>();
    candidates.addAll(weak);
    candidates.addAll(wrongTags);

    // 4) 부족하면 recent ability 태그로 채움
    List<String> recentAbilityTags = all.stream().map(AbilityProfile::getTag).toList();
    candidates.addAll(recentAbilityTags);

    // 5) 상위 N 추출
    List<String> pickedTags = candidates.stream().limit(Math.max(1, Math.min(count, 5))).toList();

    QuickSessionDto session = null;
    if (withSession && !pickedTags.isEmpty()) {
      session = quickService.createSession(pickedTags, 5); // 5문항 기본
    }

    return new RecoResult(pickedTags, session);
  }

  // DTO(내부)
  public record RecoResult(List<String> tags, QuickSessionDto session) {}
}
