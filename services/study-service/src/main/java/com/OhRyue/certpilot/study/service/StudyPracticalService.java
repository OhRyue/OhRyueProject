package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.UserAnswer;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.PracticalDtos.*;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.UserAnswerRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class StudyPracticalService {

  private final QuestionRepository qRepo;
  private final UserAnswerRepository ansRepo;
  private final UserProgressRepository progressRepo;
  private final AIExplanationService ai;

  /* 실기 세트: SHORT/LONG 혼합, topicId 범위에서 섞어서 반환 */
  public PracticalSet practicalSet(Long topicId, Integer countOpt) {
    int count = (countOpt == null || countOpt < 1) ? 4 : Math.min(10, countOpt);

    List<Question> pool = qRepo.findAll().stream()
        .filter(q -> Objects.equals(q.getTopicId(), topicId))
        .filter(q -> q.getType() == QuestionType.SHORT || q.getType() == QuestionType.LONG)
        .sorted(Comparator.comparingLong(Question::getId))
        .collect(Collectors.toList());

    Collections.shuffle(pool);
    List<Question> pick = pool.stream().limit(count).toList();

    var items = pick.stream().map(q ->
        new PracticalQuestion(q.getId(), q.getType().name(), nzs(q.getText()), q.getImageUrl())
    ).toList();

    return new PracticalSet(items);
  }

  @Transactional
  public PracticalSubmitResp submitPractical(PracticalSubmitReq req) {
    Map<Long, Question> byId = qRepo.findAllById(req.answers().stream().map(PracticalAnswer::questionId).toList())
        .stream().collect(Collectors.toMap(Question::getId, it -> it));

    int sum = 0;
    List<PracticalSubmitItem> items = new ArrayList<>();

    for (var a : req.answers()) {
      Question q = byId.get(a.questionId());
      if (q == null) continue;

      String type = q.getType().name(); // SHORT | LONG
      var aiRes = ai.explainAndScorePractical(type, q, a.userText());

      Integer score = (aiRes.score() == null) ? 0 : aiRes.score();
      sum += score;

      // 로그: correct는 점수 기준으로 60점 이상을 PASS로 간주(가이드라인)
      boolean pass = score >= 60;

      ansRepo.save(UserAnswer.builder()
          .userId(req.userId())
          .questionId(q.getId())
          .correct(pass)
          .score(score)
          .answerText(nzs(a.userText()))
          .createdAt(Instant.now())
          .build());

      items.add(new PracticalSubmitItem(q.getId(), score, nzs(q.getExplanation()), aiRes.explanation()));
    }

    int avg = (items.isEmpty()) ? 0 : Math.round(sum * 1f / items.size());

    return new PracticalSubmitResp(items.size(), avg, items);
  }

  private static String nzs(String s){ return s==null? "": s; }
}
