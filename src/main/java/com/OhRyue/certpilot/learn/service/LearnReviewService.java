package com.OhRyue.certpilot.learn.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.ai.AiExplainService;
import com.OhRyue.certpilot.learn.web.dto.LearnReviewSubmitRequest;
import com.OhRyue.certpilot.learn.web.dto.LearnReviewSubmitResult;
import com.OhRyue.certpilot.question.domain.Question;
import com.OhRyue.certpilot.question.domain.repo.QuestionRepository;

// ★ ability_profile 업데이트를 위해 추가
import com.OhRyue.certpilot.ability.domain.AbilityProfile;
import com.OhRyue.certpilot.ability.domain.AbilityProfileId;
import com.OhRyue.certpilot.ability.domain.repo.AbilityProfileRepository;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class LearnReviewService {

    private final QuestionRepository qRepo;
    private final AiExplainService aiExplain;
    private final AbilityProfileRepository abilityRepo; // ★ 추가

    private final ObjectMapper om = new ObjectMapper();

    /**
     * 세부항목 Review 채점 + 문항별 AI 해설 + 최종 요약 + 능력치 갱신
     */
    @Transactional
    public LearnReviewSubmitResult submitReview(LearnReviewSubmitRequest req) {
        var answers = Optional.ofNullable(req.answers()).orElse(List.of());

        var ids = answers.stream().map(LearnReviewSubmitRequest.Item::questionId).toList();
        Map<Long, Question> qMap = qRepo.findAllById(ids).stream()
                .collect(Collectors.toMap(Question::getId, q -> q, (a, b) -> a, LinkedHashMap::new));

        int total = answers.size();
        int score = 0;

        Map<Long, Boolean> correctness = new LinkedHashMap<>();
        Map<Long, String> explanations = new LinkedHashMap<>();
        List<String> weakTags = new ArrayList<>();

        for (var a : answers) {
            var q = qMap.get(a.questionId());
            if (q == null) continue;

            boolean ok = Objects.equals(q.getAnswerIdx(), a.chosenIdx());
            correctness.put(q.getId(), ok);
            if (ok) score++; else {
                String tag = extractFirstTag(q.getMetaJson());
                if (tag != null) weakTags.add(tag);
            }

            // 문항별 AI 해설 (폴백 포함)
            String dbExp = Optional.ofNullable(q.getExp()).orElse("");
            String ai = aiExplain.explainForQuestion(q, a.chosenIdx(), dbExp);
            explanations.put(q.getId(), ai);

            // ★ 능력치(EMA) 갱신
            if (req.userId() != null) {
                String tag = resolveTagFromMetaOrDefault(q.getMetaJson());
                AbilityProfileId apId = new AbilityProfileId(req.userId(), tag);
                AbilityProfile ap = abilityRepo.findById(apId).orElseGet(() -> new AbilityProfile(req.userId(), tag));
                ap.applyResult(ok, 0.3 /*알파*/);
                abilityRepo.save(ap);
            }
        }

        String aiSummary;
        try {
            aiSummary = aiExplain.summarizeReview(correctness, weakTags);
        } catch (Exception e) {
            aiSummary = "[요약(폴백)] 총 " + total + "문항 중 정답 " + score + "개. "
                    + (weakTags.isEmpty() ? "반복 실수 없음" : "반복 실수 태그: " + String.join(", ", weakTags));
        }

        return new LearnReviewSubmitResult(score, total, correctness, explanations, aiSummary);
    }

    private String extractFirstTag(String metaJson) {
        if (metaJson == null || metaJson.isBlank()) return null;
        try {
            var node = om.readTree(metaJson);
            var tags = node.get("tags");
            if (tags != null && tags.isArray() && tags.size() > 0) {
                return tags.get(0).asText(null);
            }
        } catch (Exception ignore) {}
        return null;
    }

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
