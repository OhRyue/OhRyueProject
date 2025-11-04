package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.*;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.MicroDtos.*;
import com.OhRyue.certpilot.study.dto.ReviewDtos.*;
import com.OhRyue.certpilot.study.dto.WrongRecapDtos.*;
import com.OhRyue.certpilot.study.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.*;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class WrittenService {

    private final TopicRepository topicRepo;
    private final ConceptRepository conceptRepo;
    private final QuestionRepository questionRepo;
    private final QuestionChoiceRepository choiceRepo;
    private final UserProgressRepository progressRepo;

    private final UserAnswerRepository answerRepo;
    private final QuestionTagRepository tagRepo;
    private final AIExplanationService ai;
    private final TopicTreeService topicTree;

    /* ========================= 개념 ========================= */

    public ConceptResp loadConcept(Long topicId) {
        var topic = topicRepo.findById(topicId).orElseThrow(() -> new NoSuchElementException("Topic not found: " + topicId));
        var conceptOpt = conceptRepo.findByTopicId(topicId);

        String topicTitle = topic.getTitle();
        List<ConceptResp.Section> sections = List.of();

        if (conceptOpt.isPresent()) {
            var concept = conceptOpt.get();
            var json = concept.getBlocksJson();
            if (json != null && !json.isBlank()) {
                try {
                    com.fasterxml.jackson.databind.ObjectMapper om = new com.fasterxml.jackson.databind.ObjectMapper();
                    com.fasterxml.jackson.databind.JsonNode root = om.readTree(json);
                    var arr = root.get("sections");

                    List<ConceptResp.Section> list = new ArrayList<>();
                    if (arr != null && arr.isArray()) {
                        for (var node : arr) {
                            int orderNo = node.hasNonNull("orderNo") ? node.get("orderNo").asInt() : 0;
                            String subCode = node.hasNonNull("subCode") ? node.get("subCode").asText() : "";
                            String secTitle = node.hasNonNull("title") ? node.get("title").asText() : "";
                            int importance = node.hasNonNull("importance") ? node.get("importance").asInt() : 0;

                            List<ConceptResp.Block> blocks = new ArrayList<>();
                            var blocksNode = node.get("blocks");
                            if (blocksNode != null && blocksNode.isArray()) {
                                for (var b : blocksNode) {
                                    String type    = b.hasNonNull("type")    ? b.get("type").asText() : null;
                                    String text    = b.hasNonNull("text")    ? b.get("text").asText() : null;
                                    String url     = b.hasNonNull("url")     ? b.get("url").asText() : null;
                                    String alt     = b.hasNonNull("alt")     ? b.get("alt").asText() : null;
                                    String caption = b.hasNonNull("caption") ? b.get("caption").asText() : null;

                                    List<String> items = new ArrayList<>();
                                    List<String> headers = new ArrayList<>();
                                    List<List<String>> rows = new ArrayList<>();

                                    var itemsNode = b.get("items");
                                    if (itemsNode != null && itemsNode.isArray()) itemsNode.forEach(it -> items.add(it.asText()));
                                    var headersNode = b.get("headers");
                                    if (headersNode != null && headersNode.isArray()) headersNode.forEach(h -> headers.add(h.asText()));
                                    var rowsNode = b.get("rows");
                                    if (rowsNode != null && rowsNode.isArray()) {
                                        for (var r : rowsNode) {
                                            List<String> row = new ArrayList<>();
                                            r.forEach(cell -> row.add(cell.asText()));
                                            rows.add(row);
                                        }
                                    }
                                    blocks.add(new ConceptResp.Block(type, text, items, url, alt, caption, headers, rows));
                                }
                            }
                            list.add(new ConceptResp.Section(orderNo, subCode, secTitle, importance, blocks));
                        }
                    }
                    list.sort(Comparator.comparing(ConceptResp.Section::orderNo));
                    sections = List.copyOf(list);
                } catch (Exception ignore) {
                    sections = List.of();
                }
            }
        }
        return new ConceptResp(topicId, topicTitle, sections);
    }

    /* ========================= 미니체크(OX) ========================= */

    public MiniSet miniSet(Long topicId) {
        List<Question> pool = questionRepo.findAll().stream()
                .filter(q -> Objects.equals(q.getTopicId(), topicId))
                .filter(q -> q.getType() == QuestionType.OX)
                .sorted(Comparator.comparingLong(Question::getId))
                .collect(Collectors.toList());
        Collections.shuffle(pool);

        List<MiniQuestion> items = pool.stream()
                .limit(5)
                .map(q -> new MiniQuestion(q.getId(), nzs(q.getText())))
                .toList();

        return new MiniSet(items);
    }

    @Transactional
    public MiniSubmitResp submitMini(MiniSubmitReq req) {
        List<Long> ids = req.answers().stream().map(MiniAnswer::questionId).toList();
        Map<Long, Question> qsById = questionRepo.findAllById(ids).stream()
                .collect(Collectors.toMap(Question::getId, it -> it));

        int correctCnt = 0;
        List<MiniSubmitItem> items = new ArrayList<>();

        for (MiniAnswer a : req.answers()) {
            Question q = require(qsById.get(a.questionId()), "Question not found: " + a.questionId());

            boolean correct = Objects.equals(q.getOxAnswer(), a.answer());
            if (correct) correctCnt++;

            String baseExpl = nzs(q.getExplanation());
            String aiExpl = "";

            answerRepo.save(UserAnswer.builder()
                    .userId(req.userId())
                    .questionId(q.getId())
                    .correct(correct)
                    .answerText(String.valueOf(a.answer()))
                    .createdAt(Instant.now())
                    .build());

            items.add(new MiniSubmitItem(q.getId(), correct, baseExpl, aiExpl));
        }

        boolean passed = (correctCnt == req.answers().size());

        UserProgress p = progressRepo
                .findByUserIdAndTopicIdAndExamMode(req.userId(), req.topicId(), ExamMode.WRITTEN)
                .orElseGet(() -> UserProgress.builder()
                        .userId(req.userId())
                        .topicId(req.topicId())
                        .examMode(ExamMode.WRITTEN)
                        .build());
        p.setMiniTotal(req.answers().size());
        p.setMiniCorrect(correctCnt);
        p.setMiniPassed(passed);
        p.setUpdatedAt(Instant.now());
        progressRepo.save(p);

        return new MiniSubmitResp(req.answers().size(), correctCnt, passed, items);
    }

    /* ========================= MCQ ========================= */

    public McqSet mcqSet(Long topicId, String userId) {
        List<Question> pool = questionRepo.findAll().stream()
                .filter(q -> Objects.equals(q.getTopicId(), topicId))
                .filter(q -> q.getType() == QuestionType.MCQ)
                .sorted(Comparator.comparingLong(Question::getId))
                .collect(Collectors.toList());
        Collections.shuffle(pool);

        List<Question> picked = pool.stream().limit(4).toList();

        List<McqQuestion> dto = picked.stream().map(q -> {
            List<McqChoice> choices = choiceRepo.findByQuestionId(q.getId()).stream()
                    .sorted(Comparator.comparing(QuestionChoice::getLabel))
                    .map(c -> new McqChoice(c.getLabel(), nzs(c.getText())))
                    .toList();
            return new McqQuestion(q.getId(), nzs(q.getText()), choices, q.getImageUrl());
        }).toList();

        return new McqSet(dto);
    }

    @Transactional
    public McqSubmitResp submitMcq(McqSubmitReq req) {
        List<Long> ids = req.answers().stream().map(McqAnswer::questionId).toList();
        List<Question> qs = questionRepo.findAllById(ids);

        Map<Long, String> correctMap = new HashMap<>();
        for (Question q : qs) {
            for (QuestionChoice c : choiceRepo.findByQuestionId(q.getId())) {
                if (Boolean.TRUE.equals(c.isCorrect())) { correctMap.put(q.getId(), c.getLabel()); break; }
            }
        }

        int correctCnt = 0;
        List<McqSubmitItem> items = new ArrayList<>();

        for (McqAnswer a : req.answers()) {
            String correctLabel = correctMap.get(a.questionId());
            boolean ok = Objects.equals(correctLabel, a.label());
            if (ok) correctCnt++;

            Question q = qs.stream().filter(x -> Objects.equals(x.getId(), a.questionId()))
                    .findFirst().orElseThrow();
            String baseExpl = nzs(q.getExplanation());
            String aiExpl = ok ? "" : nzs(ai.explainWrongForMCQ(a.label(), correctLabel, q));

            answerRepo.save(UserAnswer.builder()
                    .userId(req.userId())
                    .questionId(a.questionId())
                    .correct(ok)
                    .answerText(a.label())
                    .createdAt(Instant.now())
                    .build());

            items.add(new McqSubmitItem(a.questionId(), ok, correctLabel, baseExpl, aiExpl));
        }

        UserProgress p = progressRepo
                .findByUserIdAndTopicIdAndExamMode(req.userId(), req.topicId(), ExamMode.WRITTEN)
                .orElseGet(() -> UserProgress.builder()
                        .userId(req.userId())
                        .topicId(req.topicId())
                        .examMode(ExamMode.WRITTEN)
                        .build());
        p.setMcqTotal(req.answers().size());
        p.setMcqCorrect(correctCnt);
        p.setUpdatedAt(Instant.now());
        progressRepo.save(p);

        return new McqSubmitResp(req.answers().size(), correctCnt, items);
    }

    /* ========================= 요약(필기 완료) ========================= */

    public SummaryResp summary(String userId, Long topicId) {
        UserProgress p = progressRepo.findByUserIdAndTopicIdAndExamMode(userId, topicId, ExamMode.WRITTEN)
                .orElseThrow(() -> new NoSuchElementException("진행 정보가 없습니다."));

        int miniT = nz(p.getMiniTotal());
        int miniC = nz(p.getMiniCorrect());
        boolean miniPassed = Boolean.TRUE.equals(p.isMiniPassed());
        int mcqT  = nz(p.getMcqTotal());
        int mcqC  = nz(p.getMcqCorrect());

        // 완료 판정: 상위 이동은 프런트가 막으므로 서버는 "시도 여부"로만 판단
        boolean completed = (miniT > 0 && mcqT > 0);

        int streak = computeStreakDays(userId);
        String aiSummary = ai.summarizeWrittenKorean(userId, topicId, miniT, miniC, mcqT, mcqC, completed, streak);

        return new SummaryResp(miniT, miniC, miniPassed, mcqT, mcqC, aiSummary, completed);
    }

    /* ========================= 리뷰(필기) 세트/제출 ========================= */

    public ReviewSet reviewSet(Long rootTopicId) {
        Set<Long> topicIds = topicTree.descendantIds(rootTopicId);
        List<Question> pool = questionRepo.findAll().stream()
                .filter(q -> topicIds.contains(q.getTopicId()))
                .filter(q -> q.getType() == QuestionType.MCQ)
                .collect(Collectors.toList());
        Collections.shuffle(pool);
        List<Question> pick = pool.stream().limit(20).toList();

        var items = pick.stream().map(q -> {
            var choices = choiceRepo.findByQuestionId(q.getId()).stream()
                    .sorted(Comparator.comparing(QuestionChoice::getLabel))
                    .map(c -> new ReviewQuestion.Choice(c.getLabel(), nzs(c.getText())))
                    .toList();
            return new ReviewQuestion(q.getId(), nzs(q.getText()), choices, q.getImageUrl());
        }).toList();

        return new ReviewSet(items);
    }

    @Transactional
    public McqSubmitResp reviewSubmitWritten(McqSubmitReq req) {
        return submitMcq(req);
    }

    /**
     * 틀린문제 다시보기(요약): '최근 오답'을 기준으로 동일 토픽의 항목을 모아
     * 문항/내답/정답/DB 해설을 보여준다(재풀이 아님).
     */
    public WrongRecapSet wrongRecap(Long topicId, String userId, int limit) {
        // 1) 최근 오답 로그
        List<UserAnswer> wrongLogs = answerRepo.findAll().stream()
                .filter(a -> Objects.equals(a.getUserId(), userId))
                .filter(a -> !a.isCorrect())
                .sorted(Comparator.comparing(UserAnswer::getCreatedAt).reversed())
                .toList();

        // 2) questionId 추출(중복 제거, 상한 200)
        LinkedHashSet<Long> wrongQids = new LinkedHashSet<>();
        for (UserAnswer a : wrongLogs) {
            wrongQids.add(a.getQuestionId());
            if (wrongQids.size() >= 200) break;
        }

        // 3) 해당 토픽 + 모든 타입(OX/MCQ/SHORT/LONG) 필터
        List<Question> pool = questionRepo.findAllById(wrongQids).stream()
                .filter(q -> Objects.equals(q.getTopicId(), topicId))
                .sorted(Comparator.comparing(Question::getId))
                .toList();

        List<WrongRecapSet.Item> items = new ArrayList<>();
        for (Question q : pool) {
            // 내 최근 오답 1건
            String myAns = wrongLogs.stream()
                    .filter(a -> Objects.equals(a.getQuestionId(), q.getId()))
                    .findFirst().map(UserAnswer::getAnswerText).orElse("");

            String correctAns;
            switch (q.getType()) {
                case OX -> correctAns = String.valueOf(q.getOxAnswer());
                case MCQ -> {
                    String corr = choiceRepo.findByQuestionId(q.getId()).stream()
                            .filter(c -> Boolean.TRUE.equals(c.isCorrect()))
                            .map(QuestionChoice::getLabel)
                            .findFirst().orElse("");
                    correctAns = corr;
                }
                default -> correctAns = ""; // SHORT/LONG은 정답표시 대신 해설로 안내
            }

            items.add(new WrongRecapSet.Item(
                    q.getId(), q.getType().name(), nzs(q.getText()),
                    nzs(myAns), correctAns, nzs(q.getExplanation()), q.getImageUrl()
            ));
            if (items.size() >= limit) break;
        }

        return new WrongRecapSet(items);
    }

    /* ========================= 유틸 ========================= */
    private static <T> T require(T v, String msg) { if (v == null) throw new NoSuchElementException(msg); return v; }
    private static String nzs(String s) { return (s == null) ? "" : s; }
    private static int nz(Integer v) { return v == null ? 0 : v; }

    private int computeStreakDays(String userId) {
        ZoneId KST = ZoneId.of("Asia/Seoul");
        Set<LocalDate> days = answerRepo.findAll().stream()
                .filter(a -> Objects.equals(a.getUserId(), userId))
                .map(a -> LocalDateTime.ofInstant(a.getCreatedAt(), KST).toLocalDate())
                .collect(Collectors.toSet());
        if (days.isEmpty()) return 0;

        int streak = 0;
        LocalDate cur = LocalDate.now(KST);
        while (days.contains(cur)) { streak++; cur = cur.minusDays(1); }
        return streak;
    }
}
