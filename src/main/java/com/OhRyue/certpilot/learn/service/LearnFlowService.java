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

    // ★ 추가: 학습 데이터 축적
    private final WrongNoteRepository wrongRepo;
    private final AbilityProfileRepository abilityRepo;

    private final AiExplainService aiExplain; // (미니체크는 DB 해설, 퀴즈는 AI 해설)

    private final ObjectMapper om = new ObjectMapper();

    // =========================
    // 1) Micro: 세세항목(4레벨)
    //    개념 + 미니체크(OX 4) + 퀴즈 5
    //    (정답/해설은 "시작" 응답에 미포함)
    // =========================
    @Transactional // learn_session 저장이 있으므로 readOnly=false
    public LearnMicroStartDto startMicro(LearnMicroStartRequest req) {
        Long microId = req.microTopicId();

        // 세세항목 존재/소속 확인
        Topic micro = topicRepo.findByIdAndCertId(microId, req.certId())
                .orElseThrow(() -> new NotFoundException("micro topic not found: " + microId));

        // 세세항목에 연결된 가장 최근 개념 1건
        Concept concept = conceptRepo.findTop1ByTopicIdOrderByIdDesc(micro.getId())
                .orElseThrow(() -> new NotFoundException("concept not linked to micro topic: " + micro.getId()));

        // 학습 세션 로그(선택)
        if (req.userId() != null) {
            learnSessionRepo.save(new LearnSession(req.userId(), concept.getId()));
        }

        // 미니체크 OX 4문항 고정(정답/해설은 시작 단계에서 미노출 → answerIdx=null)
        var checks = ccRepo.findByConceptId(concept.getId()).stream()
                .limit(4)
                .map(cc -> new LearnMicroStartDto.Mini(
                        cc.getId(),
                        cc.getStem(),
                        readStrList(cc.getChoicesJson()),
                        null // 정답은 시작 단계에서 숨김
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
    // 2) Review: 세부항목(3레벨) 아래 전체 범위
    //    문제 N(기본 20) — topic 기반
    // =========================
    @Transactional(readOnly = true)
    public LearnReviewStartDto startReview(LearnReviewStartRequest req) {
        Long detailId = req.detailTopicId();
        int wanted = Optional.ofNullable(req.count()).orElse(20);
        wanted = Math.max(1, Math.min(wanted, 100)); // 안전 가드

        Topic detail = topicRepo.findByIdAndCertId(detailId, req.certId())
                .orElseThrow(() -> new NotFoundException("detail topic not found: " + detailId));
        if (detail.getLevel() != 3) {
            throw new IllegalArgumentException("detailTopicId must be level=3 topic");
        }

        // 하위 세세항목(4레벨) 목록. 없으면 detail 자체로 출제 시도.
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
    //    - 미니체크: DB explanation 사용(제출 시 노출)
    //    - 퀴즈: AI 개인화 해설(실패 시 DB exp 폴백은 AiExplainService 내부)
    //    - ★ 오답노트 / 능력치(EMA) 업데이트 (토픽/태그 기반)
    // =========================
    @Transactional
    public LearnMicroSubmitResult submitMicro(LearnMicroSubmitRequest req) {
        var miniAnswers = Optional.ofNullable(req.miniAnswers()).orElse(List.of());
        var quizAnswers = Optional.ofNullable(req.quizAnswers()).orElse(List.of());

        int total = miniAnswers.size() + quizAnswers.size();
        int score = 0;

        Map<Long, String> miniExplanations = new LinkedHashMap<>();
        Map<Long, String> quizExplanations = new LinkedHashMap<>();
        List<Long> wrongIds = new ArrayList<>();

        // ===== 미니체크 채점 + DB 해설 =====
        Map<Long, ConceptCheck> ccMap = ccRepo.findAllById(
                miniAnswers.stream().map(LearnMicroSubmitRequest.MiniAnswer::id).toList()
        ).stream().collect(Collectors.toMap(ConceptCheck::getId, c -> c));

        for (var a : miniAnswers) {
            var cc = ccMap.get(a.id());
            if (cc == null) continue;

            boolean correct = Objects.equals(cc.getAnswerIdx(), a.choiceIdx());
            if (correct) {
                score++;
            } else {
                wrongIds.add(cc.getId());
            }
            // 제출 시점에만 DB 해설 노출
            miniExplanations.put(cc.getId(), Optional.ofNullable(cc.getExplanation()).orElse(""));
        }

        // ===== 퀴즈 채점 + AI 해설(폴백 DB) + ★오답노트/능력치 =====
        Map<Long, Question> qMap = qRepo.findAllById(
                quizAnswers.stream().map(LearnMicroSubmitRequest.QuizAnswer::id).toList()
        ).stream().collect(Collectors.toMap(Question::getId, q -> q, (a, b) -> a, LinkedHashMap::new));

        for (var a : quizAnswers) {
            var q = qMap.get(a.id());
            if (q == null) continue;

            boolean correct = Objects.equals(q.getAnswerIdx(), a.choiceIdx());
            if (correct) {
                score++;
            } else {
                wrongIds.add(q.getId());
            }

            String dbExp = Optional.ofNullable(q.getExp()).orElse("");
            String ai = aiExplain.explainForQuestion(q, a.choiceIdx(), dbExp);
            quizExplanations.put(q.getId(), ai);

            // ★ 학습데이터 반영 (userId가 있을 때만)
            if (req.userId() != null) {
                String useTag = resolveTagForLearning(q.getMetaJson());
                upsertWrongAndAbility(req.userId(), q.getId(), useTag, correct);
            }
        }

        return new LearnMicroSubmitResult(score, total, wrongIds, miniExplanations, quizExplanations);
    }

    // ===== 유틸 =====
    private List<String> readStrList(String json) {
        if (json == null || json.isBlank()) return List.of();
        try {
            return om.readValue(json, new TypeReference<List<String>>() {});
        } catch (Exception e) {
            return List.of();
        }
    }

    /** meta_json.tags[0] 추출 (없으면 "general") */
    private String resolveTagForLearning(String metaJson) {
        if (metaJson != null && !metaJson.isBlank()) {
            try {
                var node = om.readTree(metaJson);
                var tags = node.get("tags");
                if (tags != null && tags.isArray() && tags.size() > 0) {
                    return tags.get(0).asText("general");
                }
            } catch (Exception ignore) { }
        }
        return "general";
    }

    /** ★ wrong_note & ability_profile upsert */
    private void upsertWrongAndAbility(Long userId, Long qid, String tag, boolean correct) {
        // wrong_note (오답일 때만 카운트/상태 갱신)
        if (!correct) {
            WrongNote wn = wrongRepo.findByUserIdAndQuestionIdAndTag(userId, qid, tag)
                    .orElseGet(() -> new WrongNote(userId, qid, tag));
            wn.markWrongOnce();
            wn.setStatus(WrongNoteStatus.todo);
            wrongRepo.save(wn);
        }
        // ability_profile (정오답 모두 EMA 반영)
        AbilityProfileId apId = new AbilityProfileId(userId, tag);
        AbilityProfile ap = abilityRepo.findById(apId).orElseGet(() -> new AbilityProfile(userId, tag));
        ap.applyResult(correct, 0.3); // 알파=0.3 기본
        abilityRepo.save(ap);
    }

    /** meta_json.tags[0] 추출 (없으면 null) — Review 약점 태그용 */
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
}
