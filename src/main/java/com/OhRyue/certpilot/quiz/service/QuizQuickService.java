package com.OhRyue.certpilot.quiz.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.ability.domain.AbilityProfile;
import com.OhRyue.certpilot.ability.domain.AbilityProfileId;
import com.OhRyue.certpilot.ability.domain.repo.AbilityProfileRepository;
import com.OhRyue.certpilot.question.domain.Question;
import com.OhRyue.certpilot.question.domain.repo.QuestionRepository;
import com.OhRyue.certpilot.question.domain.repo.QuestionTagRepository;
import com.OhRyue.certpilot.quiz.web.dto.QuickGradeRequest;
import com.OhRyue.certpilot.quiz.web.dto.QuickGradeResult;
import com.OhRyue.certpilot.quiz.web.dto.QuickSessionDto;
import com.OhRyue.certpilot.wrongnote.domain.WrongNote;
import com.OhRyue.certpilot.wrongnote.domain.WrongNoteStatus;
import com.OhRyue.certpilot.wrongnote.domain.repo.WrongNoteRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class QuizQuickService {

  private final QuestionRepository qRepo;
  private final QuestionTagRepository qtRepo;
  private final WrongNoteRepository wrongRepo;
  private final AbilityProfileRepository abilityRepo; // ⭐ 추가

  private final ObjectMapper om = new ObjectMapper();

  /** 태그 기반 3~5문항 세션 구성 */
  @Transactional(readOnly = true)
  public QuickSessionDto createSession(List<String> tags, int count) {
    List<Question> pool;

    if (tags == null || tags.isEmpty()) {
      pool = qRepo.findTop50ByDifficultyBetweenOrderByIdDesc(1, 3);
    } else {
      var ids = new HashSet<>(qtRepo.findQuestionIdsByTags(normalize(tags)));
      pool = ids.isEmpty()
          ? qRepo.findTop50ByDifficultyBetweenOrderByIdDesc(1, 3)
          : qRepo.findAllById(ids);

      if (pool.size() < count) {
        var fill = qRepo.findTop50ByDifficultyBetweenOrderByIdDesc(1, 3);
        pool = mergeUnique(pool, fill);
      }
    }

    var picked = pool.stream().limit(Math.max(1, Math.min(count, 10))).toList();

    var items = picked.stream().map(q ->
        new QuickSessionDto.Item(
            q.getId(),
            q.getStem(),
            readStrList(q.getChoicesJson()),
            q.getAnswerIdx(), // MVP: UI에서 바로 정답 확인 가능(추후 숨김 처리)
            q.getExp()
        )
    ).toList();

    String sessionId = "qq-" + System.currentTimeMillis();
    return new QuickSessionDto(sessionId, items);
  }

  /** 채점 + wrong_note upsert + ability_profile(EMA) 갱신 */
  @Transactional
  public QuickGradeResult grade(QuickGradeRequest req) {
    var ids = req.answers().stream().map(QuickGradeRequest.Answer::id).toList();

    // qMap: id -> Question
    Map<Long, Question> qMap = qRepo.findAllById(ids).stream()
        .collect(Collectors.toMap(
            Question::getId,
            q -> q,
            (a, b) -> a,
            LinkedHashMap::new
        ));

    int total = req.answers().size();
    int score = 0;
    List<Long> wrongIds = new ArrayList<>();
    Map<Long, String> explanations = new LinkedHashMap<>();

    for (var a : req.answers()) {
      var q = qMap.get(a.id());
      if (q == null) continue;

      // 공통: 태그 결정 (meta_json.tags[0] or "general")
      String useTag = resolveTagFromMetaOrDefault(q.getMetaJson());

      if (Objects.equals(q.getAnswerIdx(), a.choiceIdx())) {
        // ===== 정답 분기 =====
        score++;

        // 2) ability_profile upsert (정답 true)
        if (req.userId() != null) {
          AbilityProfileId apId = new AbilityProfileId(req.userId(), useTag);
          AbilityProfile ap = abilityRepo.findById(apId)
              .orElseGet(() -> new AbilityProfile(req.userId(), useTag));
          ap.applyResult(true /*정답*/, 0.3);
          abilityRepo.save(ap);
        }

      } else {
        // ===== 오답 분기 =====
        wrongIds.add(q.getId());
        explanations.put(q.getId(), Optional.ofNullable(q.getExp()).orElse(""));

        // 1) wrong_note upsert
        if (req.userId() != null) {
          WrongNote wn = wrongRepo.findByUserIdAndQuestionIdAndTag(req.userId(), q.getId(), useTag)
              .orElseGet(() -> new WrongNote(req.userId(), q.getId(), useTag));
          wn.markWrongOnce();                 // wrong_count++, first/last 갱신
          wn.setStatus(WrongNoteStatus.todo); // 기본 상태 유지
          wrongRepo.save(wn);
        }

        // 2) ability_profile upsert (오답 false)
        if (req.userId() != null) {
          AbilityProfileId apId = new AbilityProfileId(req.userId(), useTag);
          AbilityProfile ap = abilityRepo.findById(apId)
              .orElseGet(() -> new AbilityProfile(req.userId(), useTag));
          ap.applyResult(false /*오답*/, 0.3);
          abilityRepo.save(ap);
        }
      }
    }

    return new QuickGradeResult(score, total, wrongIds, explanations);
  }

  // ===== helpers =====
  private List<String> readStrList(String json) {
    if (json == null || json.isBlank()) return List.of();
    try { return om.readValue(json, new TypeReference<>(){}); }
    catch (Exception e) { return List.of(); }
  }

  private List<String> normalize(List<String> tags) {
    return tags.stream().filter(Objects::nonNull).map(s -> s.trim().toLowerCase()).toList();
  }

  private List<Question> mergeUnique(List<Question> a, List<Question> b) {
    Map<Long, Question> m = new LinkedHashMap<>();
    for (var q : a) m.put(q.getId(), q);
    for (var q : b) m.putIfAbsent(q.getId(), q);
    return new ArrayList<>(m.values());
  }

  /** meta_json에서 "tags" 배열의 첫 값을 사용, 없으면 "general" */
  private String resolveTagFromMetaOrDefault(String metaJson) {
    if (metaJson != null && !metaJson.isBlank()) {
      try {
        var node = om.readTree(metaJson);
        var tagsNode = node.get("tags");
        if (tagsNode != null && tagsNode.isArray() && tagsNode.size() > 0) {
          return tagsNode.get(0).asText("general");
        }
      } catch (Exception ignore) {}
    }
    return "general";
  }
}
