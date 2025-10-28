package com.OhRyue.certpilot.learn.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.OhRyue.certpilot.common.error.NotFoundException;
import com.OhRyue.certpilot.concept.domain.Concept;
import com.OhRyue.certpilot.concept.domain.repo.ConceptCheckRepository;
import com.OhRyue.certpilot.concept.domain.repo.ConceptRepository;
import com.OhRyue.certpilot.learn.domain.LearnSession;
import com.OhRyue.certpilot.learn.domain.repo.LearnSessionRepository;
import com.OhRyue.certpilot.learn.web.dto.*;
import com.OhRyue.certpilot.question.domain.Question;
import com.OhRyue.certpilot.question.domain.repo.QuestionRepository;
import com.OhRyue.certpilot.question.domain.repo.QuestionTagRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class LearnService {

  private final ConceptRepository conceptRepo;
  private final ConceptCheckRepository ccRepo;
  private final QuestionRepository qRepo;
  private final QuestionTagRepository qtRepo;

  private final ObjectMapper om = new ObjectMapper();
  private final EmaCalculator ema = new EmaCalculator(0.3);

  // ⭐ 4-2) learn_session 기록용
  private final LearnSessionRepository learnSessionRepo;

  /** 제출/채점 (MVP 라이트) */
  @Transactional
  public LearnSubmitResult submit(LearnSubmitRequest req) {
    // 1) 미니체크 채점 (정답 인덱스는 concept_check 테이블에서 조회)
    int miniTotal = Optional.ofNullable(req.miniAnswers()).map(List::size).orElse(0);
    int miniCorrect = 0;
    if (miniTotal > 0) {
      var ids = req.miniAnswers().stream().map(LearnSubmitRequest.Answer::questionId).toList();
      var checkMap = ccRepo.findAllById(ids).stream().collect(Collectors.toMap(
          c -> c.getId(), c -> c.getAnswerIdx()
      ));
      for (var a : req.miniAnswers()) {
        Integer ans = checkMap.get(a.questionId());
        if (ans != null && ans.equals(a.chosenIdx())) miniCorrect++;
      }
    }

    // 2) 문제 채점 + 약점 태그 후보 수집
    int quizTotal = Optional.ofNullable(req.quizAnswers()).map(List::size).orElse(0);
    int quizCorrect = 0;
    List<String> wrongTags = new ArrayList<>();

    if (quizTotal > 0) {
      var qids = req.quizAnswers().stream().map(LearnSubmitRequest.Answer::questionId).toList();
      var qMap = qRepo.findAllById(qids).stream().collect(Collectors.toMap(Question::getId, q -> q));

      // 태그 매핑 조회
      // 간단화: 질문 ID별 태그를 DB에서 직접 한 번에 가져오는 로직이 있으면 교체
      // 여기선 최소화: 정답 비교만 수행 (약점 산출은 추후 태그 조인 확장)
      for (var a : req.quizAnswers()) {
        var q = qMap.get(a.questionId());
        if (q != null) {
          if (Objects.equals(q.getAnswerIdx(), a.chosenIdx())) {
            quizCorrect++;
          } else {
            // 약점 태그는 meta_json이나 개념 태그를 재사용(간이)
            wrongTags.add("트랜잭션"); // MVP: 고정 태그 예시 (Flyway seed에 맞춰 임시)
          }
        }
      }
    }

    // 3) weakTags (MVP: 임시 1~2개, EMA/attempts는 더미 값)
    var weak = wrongTags.isEmpty()
        ? List.<LearnSubmitResult.WeakTag>of()
        : List.of(new LearnSubmitResult.WeakTag(wrongTags.get(0), 0.48, 3));

    // 4) nextActions 추천
    var next = new ArrayList<String>();
    if (!wrongTags.isEmpty()) {
      next.add("유사문제 3개 풀기");
      next.add("해당 태그 20분 복습 만들기");
    }

    return new LearnSubmitResult(miniCorrect, miniTotal, quizCorrect, quizTotal, weak, next);
  }

  private List<String> readStrList(String json) {
    if (json == null || json.isBlank()) return List.of();
    try { return om.readValue(json, new TypeReference<>(){}); }
    catch (Exception e) { return List.of(); }
  }

  private List<Question> mergeUnique(List<Question> a, List<Question> b) {
    Map<Long, Question> m = new LinkedHashMap<>();
    for (var q : a) m.put(q.getId(), q);
    for (var q : b) m.putIfAbsent(q.getId(), q);
    return new ArrayList<>(m.values());
   }
}