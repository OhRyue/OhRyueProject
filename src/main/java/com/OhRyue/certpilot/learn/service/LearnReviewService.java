package com.OhRyue.certpilot.learn.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.ai.AiExplainService;
import com.OhRyue.certpilot.learn.web.dto.LearnReviewSubmitRequest;
import com.OhRyue.certpilot.learn.web.dto.LearnReviewSubmitResult;
import com.OhRyue.certpilot.question.domain.Question;
import com.OhRyue.certpilot.question.domain.repo.QuestionRepository;
import com.OhRyue.certpilot.wrongnote.domain.WrongNote;
import com.OhRyue.certpilot.wrongnote.domain.WrongNoteStatus;
import com.OhRyue.certpilot.wrongnote.domain.repo.WrongNoteRepository;
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
    private final WrongNoteRepository wrongRepo;
    private final AbilityProfileRepository abilityRepo;
    private final AiExplainService aiExplain;

    /** meta_json 파싱용 */
    private final ObjectMapper om = new ObjectMapper();

    /**
     * 세부항목 Review 제출/채점
     * - 정오표 + 문항별 AI 해설 + AI 요약
     * - 오답노트/능력치(EMA) 갱신
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
            if (ok) {
                score++;
            } else {
                weakTags.add(extractFirstTag(q.getMetaJson(), "general"));
            }

            // EMA/오답노트 갱신
            if (req.userId() != null) {
                String tag = extractFirstTag(q.getMetaJson(), "general");

                // 능력치(EMA)
                AbilityProfileId apId = new AbilityProfileId(req.userId(), tag);
                AbilityProfile ap = abilityRepo.findById(apId).orElseGet(() -> new AbilityProfile(req.userId(), tag));
                ap.applyResult(ok, 0.3);
                abilityRepo.save(ap);

                // 오답노트
                if (!ok) {
                    WrongNote wn = wrongRepo.findByUserIdAndQuestionIdAndTag(req.userId(), q.getId(), tag)
                            .orElseGet(() -> new WrongNote(req.userId(), q.getId(), tag));
                    wn.markWrongOnce();
                    wn.setStatus(WrongNoteStatus.todo);
                    wrongRepo.save(wn);
                }
            }

            String dbExp = Optional.ofNullable(q.getExp()).orElse("");
            explanations.put(q.getId(), aiExplain.explainForQuestion(q, a.chosenIdx(), dbExp));
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

    /** meta_json.tags[0] 추출(없으면 defaultTag) */
    private String extractFirstTag(String metaJson, String defaultTag) {
        if (metaJson == null || metaJson.isBlank()) return defaultTag;
        try {
            var node = om.readTree(metaJson);
            var tags = node.get("tags");
            if (tags != null && tags.isArray() && tags.size() > 0) {
                return tags.get(0).asText(defaultTag);
            }
        } catch (Exception ignore) {}
        return defaultTag;
    }
}
