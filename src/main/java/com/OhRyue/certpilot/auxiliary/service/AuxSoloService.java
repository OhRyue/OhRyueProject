package com.OhRyue.certpilot.auxiliary.service;

import com.OhRyue.certpilot.auxiliary.web.dto.*;
import com.OhRyue.certpilot.ai.AiExplainService;
import com.OhRyue.certpilot.ability.domain.AbilityProfile;
import com.OhRyue.certpilot.ability.domain.AbilityProfileId;
import com.OhRyue.certpilot.ability.domain.repo.AbilityProfileRepository;
import com.OhRyue.certpilot.question.domain.Question;
import com.OhRyue.certpilot.question.domain.repo.QuestionRepository;
import com.OhRyue.certpilot.question.domain.repo.QuestionTagRepository;
import com.OhRyue.certpilot.wrongnote.domain.WrongNote;
import com.OhRyue.certpilot.wrongnote.domain.WrongNoteStatus;
import com.OhRyue.certpilot.wrongnote.domain.repo.WrongNoteRepository;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/** 보조학습(혼자풀기) 3종 모드: 카테고리/난이도/약점 */
@Service
@RequiredArgsConstructor
public class AuxSoloService {

    private final QuestionRepository qRepo;
    private final QuestionTagRepository qtRepo;
    private final WrongNoteRepository wrongRepo;
    private final AbilityProfileRepository abilityRepo;
    private final AiExplainService aiExplain;

    private final ObjectMapper om = new ObjectMapper();

    // =========================
    // 1) 시작: 카테고리(Topic) 지정
    // =========================
    @Transactional(readOnly = true)
    public AuxSoloStartDto startByCategory(AuxSoloCategoryStartRequest req) {
        int count = optionalClamp(req.count(), 10, 1, 50);
        List<Long> topicIds = Optional.ofNullable(req.topicIds()).orElse(List.of());
        if (topicIds.isEmpty()) {
            return emptySession();
        }
        List<Question> pool = qRepo.findLatestByTopicIds(topicIds, count);
        return toStartDto(pool);
    }

    // =========================
    // 2) 시작: 난이도 범위
    // =========================
    @Transactional(readOnly = true)
    public AuxSoloStartDto startByDifficulty(AuxSoloDifficultyStartRequest req) {
        int min = Optional.ofNullable(req.minDifficulty()).orElse(1);
        int max = Optional.ofNullable(req.maxDifficulty()).orElse(3);
        if (min > max) { int t = min; min = max; max = t; }

        int count = optionalClamp(req.count(), 10, 1, 50);

        // 기존 저장소 메서드 재사용(Top50) 후 상위 count 제한
        List<Question> pool = qRepo.findTopByDifficultyBetweenOrderByIdDesc(min, max)
                .stream().limit(count).toList();

        return toStartDto(pool);
    }

    // =========================
    // 3) 시작: 약점 보완(EMA/오답 태그)
    // =========================
    @Transactional(readOnly = true)
    public AuxSoloStartDto startByWeak(AuxSoloWeakStartRequest req) {
        int count = optionalClamp(req.count(), 10, 1, 50);

        // 3-1) EMA 하위 태그 우선
        List<String> weakTags = new ArrayList<>();
        if (req.userId() != null) {
            // 간단한 휴리스틱: ema_correct 오름차순 → 상위 3개 태그
            var emalist = abilityRepo.findByUserIdOrderByEmaCorrectAsc(req.userId());
            emalist.stream().limit(3).forEach(ap -> weakTags.add(ap.getTag()));
        }

        // 3-2) EMA 없으면 최근 오답 태그로 보완
        if (weakTags.isEmpty() && req.userId() != null) {
            var wns = wrongRepo.findTopByUserIdOrderByLastWrongAtDesc(req.userId());
            wns.stream().map(WrongNote::getTag).distinct().limit(3).forEach(weakTags::add);
        }

        List<Question> pool;
        if (!weakTags.isEmpty()) {
            var qids = new HashSet<>(qtRepo.findQuestionIdsByTags(normalize(weakTags)));
            pool = qids.isEmpty() ? List.of() : qRepo.findAllById(qids).stream()
                    .sorted(Comparator.comparingLong(Question::getId).reversed())
                    .limit(count)
                    .toList();
        } else {
            // 태그 근거가 전혀 없으면 쉬운 문제로 워밍업
            pool = qRepo.findTopByDifficultyBetweenOrderByIdDesc(1, 2).stream()
                    .limit(count)
                    .toList();
        }

        return toStartDto(pool);
    }

    // =========================
    // 4) 채점: 오답노트/EMA 갱신 + 문항별 AI 해설
    // =========================
    @Transactional
    public AuxSoloGradeResult grade(AuxSoloGradeRequest req) {
        var answers = Optional.ofNullable(req.answers()).orElse(List.of());
        var ids = answers.stream().map(AuxSoloGradeRequest.Answer::id).toList();

        Map<Long, Question> qMap = qRepo.findAllById(ids).stream()
                .collect(Collectors.toMap(Question::getId, q -> q, (a,b)->a, LinkedHashMap::new));

        int total = answers.size();
        int score = 0;
        List<Long> wrongIds = new ArrayList<>();
        Map<Long, String> explanations = new LinkedHashMap<>();

        for (var a : answers) {
            var q = qMap.get(a.id());
            if (q == null) continue;

            boolean ok = Objects.equals(q.getAnswerIdx(), a.chosenIdx());
            if (ok) score++; else wrongIds.add(q.getId());

            // 태그(EMA/오답용) — meta_json.tags[0] or "general"
            String tag = resolveTagFromMetaOrDefault(q.getMetaJson());

            if (req.userId() != null) {
                // 오답노트 upsert
                if (!ok) {
                    var wn = wrongRepo.findByUserIdAndQuestionIdAndTag(req.userId(), q.getId(), tag)
                            .orElseGet(() -> new WrongNote(req.userId(), q.getId(), tag));
                    wn.markWrongOnce();
                    wn.setStatus(WrongNoteStatus.todo);
                    wrongRepo.save(wn);
                }
                // 능력치 EMA 갱신
                var apId = new AbilityProfileId(req.userId(), tag);
                var ap = abilityRepo.findById(apId).orElseGet(() -> new AbilityProfile(req.userId(), tag));
                ap.applyResult(ok, 0.3); // smoothing=0.3 (가중치는 후속 튜닝)
                abilityRepo.save(ap);
            }

            // 개인화 해설(AI → 실패 시 DB 해설 폴백)
            String dbExp = Optional.ofNullable(q.getExp()).orElse("");
            String ai = aiExplain.explainForQuestion(q, a.chosenIdx(), dbExp);
            explanations.put(q.getId(), ai);
        }

        return new AuxSoloGradeResult(score, total, wrongIds, explanations);
    }

    // ===== 헬퍼 =====
    private AuxSoloStartDto toStartDto(List<Question> pool) {
        var items = pool.stream().map(q ->
                new AuxSoloStartDto.Item(q.getId(), q.getStem(), readStrList(q.getChoicesJson()))
        ).toList();
        String sessionId = "aux-" + System.currentTimeMillis();
        return new AuxSoloStartDto(sessionId, items);
    }

    private AuxSoloStartDto emptySession() {
        return new AuxSoloStartDto("aux-" + System.currentTimeMillis(), List.of());
    }

    private int optionalClamp(Integer v, int def, int min, int max) {
        int x = Optional.ofNullable(v).orElse(def);
        if (x < min) x = min;
        if (x > max) x = max;
        return x;
    }

    private List<String> normalize(List<String> tags) {
        return tags.stream().filter(Objects::nonNull).map(s -> s.trim().toLowerCase()).toList();
    }

    private List<String> readStrList(String json) {
        if (json == null || json.isBlank()) return List.of();
        try { return om.readValue(json, new TypeReference<List<String>>() {}); }
        catch (Exception e) { return List.of(); }
    }

    /** meta_json.tags[0] or "general" */
    private String resolveTagFromMetaOrDefault(String metaJson) {
        if (metaJson != null && !metaJson.isBlank()) {
            try {
                var node = om.readTree(metaJson);
                var tags = node.get("tags");
                if (tags != null && tags.isArray() && tags.size() > 0) {
                    return tags.get(0).asText("general");
                }
            } catch (Exception ignore) {}
        }
        return "general";
    }
}
