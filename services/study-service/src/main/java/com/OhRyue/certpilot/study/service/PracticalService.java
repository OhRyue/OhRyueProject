package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.UserAnswer;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalAnswer;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalQuestion;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSet;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSubmitItem;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSubmitReq;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSubmitResp;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.UserAnswerRepository;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class PracticalService {

  private final QuestionRepository qRepo;
  private final UserAnswerRepository ansRepo;
  private final AIExplanationService ai;

  /** 실기 세트: SHORT/LONG 혼합, topicId 범위에서 섞어서 반환 */
  public PracticalSet practicalSet(Long topicId, Integer countOpt) {
    int count = (countOpt == null || countOpt < 1) ? 4 : Math.min(10, countOpt);

    List<Question> pool = qRepo.findAll().stream()
        .filter(q -> Objects.equals(q.getTopicId(), topicId))
        .filter(q -> q.getType() == QuestionType.SHORT || q.getType() == QuestionType.LONG)
        .sorted(Comparator.comparingLong(Question::getId))
        .collect(Collectors.toList());

    Collections.shuffle(pool);
    List<Question> pick = pool.stream().limit(count).toList();

    var items = pick.stream()
        .map(q -> new PracticalQuestion(q.getId(), q.getType().name(), nz(q.getText()), q.getImageUrl()))
        .toList();

    return new PracticalSet(items);
  }

  /** 실기 제출/채점: LLM 점수(0~100) + 맞춤 해설, score>=60이면 PASS로 보고 correct=true 로 저장 */
  public PracticalSubmitResp submitPractical(PracticalSubmitReq req) {
    Map<Long, Question> byId = qRepo.findAllById(
            req.answers().stream().map(PracticalAnswer::questionId).toList())
        .stream().collect(Collectors.toMap(Question::getId, it -> it));

    int sum = 0;
    List<PracticalSubmitItem> items = new ArrayList<>();

    for (var a : req.answers()) {
      Question q = byId.get(a.questionId());
      if (q == null) continue;

      var aiRes = ai.explainAndScorePractical(q.getType().name(), q, a.userText());
      int score = Optional.ofNullable(aiRes.score()).orElse(0);
      sum += score;

      boolean pass = score >= 60; // 정책: 60점 이상을 정답 처리

      ansRepo.save(UserAnswer.builder()
          .userId(req.userId())
          .questionId(q.getId())
          .correct(pass)
          .score(score)                 // ⚠ user_answer.score 컬럼/Flyway/엔티티 추가되어 있어야 함
          .answerText(nz(a.userText()))
          .createdAt(Instant.now())
          .build());

      items.add(new PracticalSubmitItem(q.getId(), score, nz(q.getExplanation()), aiRes.explanation()));
    }

    int total = items.size();
    int avg = (total == 0) ? 0 : Math.round(sum * 1f / total);
    return new PracticalSubmitResp(total, avg, items);
  }

  private static String nz(String s) { return (s == null) ? "" : s; }
}
