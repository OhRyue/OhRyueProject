package com.OhRyue.certpilot.learn.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.ai.AiExplainService;
import com.OhRyue.certpilot.common.error.NotFoundException;
import com.OhRyue.certpilot.concept.domain.Concept;
import com.OhRyue.certpilot.concept.domain.ConceptCheck;
import com.OhRyue.certpilot.concept.domain.repo.ConceptCheckRepository;
import com.OhRyue.certpilot.concept.domain.repo.ConceptRepository;
import com.OhRyue.certpilot.curriculum.domain.Topic;
import com.OhRyue.certpilot.curriculum.domain.repo.TopicRepository;
import com.OhRyue.certpilot.learn.domain.LearnSession;
import com.OhRyue.certpilot.learn.domain.repo.LearnSessionRepository;
import com.OhRyue.certpilot.learn.web.dto.*;
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
public class LearnFlowService {

    private final TopicRepository topicRepo;
    private final ConceptRepository conceptRepo;
    private final ConceptCheckRepository ccRepo;
    private final QuestionRepository qRepo;
    private final LearnSessionRepository learnSessionRepo;

    // 신규: 오답노트/능력치
    private final WrongNoteRepository wrongRepo;
    private final AbilityProfileRepository abilityRepo;

    private final AiExplainService aiExplain;
    private final ObjectMapper om = new ObjectMapper();

    // =========================
    // 1) Micro 시작 (세세항목)
    // =========================
    @Transactional // 세션 저장이 있으므로 readOnly=false
    public LearnMicroStartDto startMicro(LearnMicroStartRequest req) {
        Long microId = req.microTopicId();

        Topic micro = topicRepo.findByIdAndCertId(microId, req.certId())
                .orElseThrow(() -> new NotFoundException("micro topic not found: " + microId));

        Concept concept = conceptRepo.findTop1ByTopicIdOrderByIdDesc(micro.getId())
                .orElseThrow(() -> new NotFoundException("concept not linked to micro topic: " + micro.getId()));

        if (req.userId() != null) {
            learnSessionRepo.save(new LearnSession(req.userId(), concept.getId()));
        }

        // 미니체크 OX 4문항 고정(정답 숨김)
        var checks = ccRepo.findByConceptId(concept.getId()).stream()
                .limit(4)
                .map(cc -> new LearnMicroStartDto.Mini(
                        cc.getId(),
                        cc.getStem(),
                        readStrList(cc.getChoicesJson()),
                        null
                ))
                .toList();

        // 세세항목(topic) 기반 퀴즈 5
        List<Question> qPool = qRepo.findLatestByTopicIds(List.of(micro.getId()), 5);
        var quiz5 = qPool.stream()
                .map(q -> new LearnMicroStartDto.Quiz(
                        q.getId(),
                        q.getStem(),
                        readStrList(q.getChoicesJson()),
                        q.getDifficulty()
                ))
                .toList();

        return new LearnMicroStartDto(
                concept.getId(),
                concept.getTitle(),
                concept.getSummary(),
                concept.getPitfalls(),
                checks,
                quiz5
        );
    }

    // =========================
    // 2) Review 시작 (세부항목 하위 전체)
    // =========================
    @Transactional(readOnly = true)
    public LearnReviewStartDto startReview(LearnReviewStartRequest req) {
        Long detailId = req.detailTopicId();
        int wanted = Optional.ofNullable(req.count()).orElse(20);
        wanted = Math.max(1, Math.min(wanted, 100));

        Topic detail = topicRepo.findByIdAndCertId(detailId, req.certId())
                .orElseThrow(() -> new NotFoundException("detail topic not found: " + detailId));
        if (detail.getLevel() != 3) {
            throw new IllegalArgumentException("detailTopicId must be level=3 topic");
        }

        var micros = topicRepo.findByParentId(detail.getId());
        List<Long> topicScope = micros.isEmpty()
                ? List.of(detail.getId())
                : micros.stream().map(Topic::getId).toList();

        List<Question> picked = qRepo.findLatestByTopicIds(topicScope, wanted);

        var quiz = picked.stream()
                .map(q -> new LearnReviewStartDto.Quiz(
                        q.getId(), q.getStem(), readStrList(q.getChoicesJson()), q.getDifficulty()
                ))
                .toList();

        return new LearnReviewStartDto(detail.getId(), quiz.size(), quiz);
    }

    // =========================
    // 3) Micro 제출/채점
    //    - 오답노트/능력치 갱신(간이 태그 기반)
    // =========================
    @Transactional
    public LearnMicroSubmitResult submitMicro(LearnMicroSubmitRequest req) {
        var miniAnswers = Optional.ofNullable(req.miniAnswers()).orElse(List.of());
        var quizAnswers = Optional.ofNullable(req.quizAnswers()).orElse(List.of());

        int total = miniAnswers.size() + quizAnswers.size();
        int score = 0;

        // 미니체크 채점 + DB 해설
        Map<Long, ConceptCheck> ccMap = ccRepo.findAllById(
                miniAnswers.stream().map(LearnMicroSubmitRequest.MiniAnswer::id).toList()
        ).stream().collect(Collectors.toMap(ConceptCheck::getId, c -> c));

        Map<Long, String> miniExplanations = new LinkedHashMap<>();
        List<Long> wrongIds = new ArrayList<>();

        for (var a : miniAnswers) {
            var cc = ccMap.get(a.id());
            if (cc == null) continue;

            boolean correct = Objects.equals(cc.getAnswerIdx(), a.choiceIdx());
            if (correct) score++; else wrongIds.add(cc.getId());

            // 시작 때 숨겼던 해설(DB) 노출
            miniExplanations.put(cc.getId(), Optional.ofNullable(cc.getExplanation()).orElse(""));
            // 미니체크는 오답노트/능력치 갱신 대상에서 제외(개념 점검 용도) — 필요 시 여기서도 tag 기준 갱신 가능
        }

        // 퀴즈 채점 + 오답노트/능력치 + AI 해설(폴백)
        Map<Long, Question> qMap = qRepo.findAllById(
                quizAnswers.stream().map(LearnMicroSubmitRequest.QuizAnswer::id).toList()
        ).stream().collect(Collectors.toMap(Question::getId, q -> q, (a, b) -> a, LinkedHashMap::new));

        Map<Long, String> quizExplanations = new LinkedHashMap<>();

        for (var a : quizAnswers) {
            var q = qMap.get(a.id());
            if (q == null) continue;

            boolean ok = Objects.equals(q.getAnswerIdx(), a.choiceIdx());
            if (ok) score++; else wrongIds.add(q.getId());

            // 능력치/오답노트는 "간이 태그(meta_json.tags[0])" 기준
            if (req.userId() != null) {
                String useTag = extractFirstTag(q.getMetaJson(), "general");

                // EMA
                AbilityProfileId apId = new AbilityProfileId(req.userId(), useTag);
                AbilityProfile ap = abilityRepo.findById(apId).orElseGet(() -> new AbilityProfile(req.userId(), useTag));
                ap.applyResult(ok, 0.3);
                abilityRepo.save(ap);

                // 오답노트
                if (!ok) {
                    WrongNote wn = wrongRepo.findByUserIdAndQuestionIdAndTag(req.userId(), q.getId(), useTag)
                            .orElseGet(() -> new WrongNote(req.userId(), q.getId(), useTag));
                    wn.markWrongOnce();
                    wn.setStatus(WrongNoteStatus.todo);
                    wrongRepo.save(wn);
                }
            }

            String dbExp = Optional.ofNullable(q.getExp()).orElse("");
            quizExplanations.put(q.getId(), aiExplain.explainForQuestion(q, a.choiceIdx(), dbExp));
        }

        return new LearnMicroSubmitResult(score, total, wrongIds, miniExplanations, quizExplanations);
    }

    // ==== 유틸 ====
    private List<String> readStrList(String json) {
        if (json == null || json.isBlank()) return List.of();
        try {
            return om.readValue(json, new TypeReference<List<String>>() {});
        } catch (Exception e) {
            return List.of();
        }
    }

    /** meta_json.tags[0] 추출(없으면 defaultTag 반환) */
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
