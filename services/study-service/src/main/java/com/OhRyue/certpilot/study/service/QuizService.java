package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.QuestionChoice;
import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.QuizDtos.*;
import com.OhRyue.certpilot.study.repository.QuestionChoiceRepository;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class QuizService {

  private final QuestionRepository qRepo;
  private final QuestionChoiceRepository cRepo;
  private final UserProgressRepository pRepo;

  public QuizSet categoryQuiz(CategoryStartReq req) {
    List<Question> pool = qRepo.findByTopicIdInAndType(
        req.topicIds(), QuestionType.MCQ);
    return pick(pool, req.count());
  }

  public QuizSet difficultyQuiz(DifficultyStartReq req) {
    List<Question> pool = qRepo.findByDifficultyAndType(req.difficulty(), QuestionType.MCQ);
    return pick(pool, req.count());
  }

  public QuizSet weaknessQuiz(WeaknessStartReq req) {
    // 약점: 정답률 낮은 topic 우선
    List<UserProgress> ps = pRepo.findByUserId(req.userId());
    ps.sort(Comparator.comparingDouble(this::acc).thenComparing(UserProgress::getUpdatedAt));
    List<Long> targetTopics = ps.stream().map(UserProgress::getTopicId).limit(5).toList();

    List<Question> pool = targetTopics.isEmpty()
        ? qRepo.findByDifficultyAndType(Difficulty.NORMAL, QuestionType.MCQ)
        : qRepo.findByTopicIdInAndType(targetTopics, QuestionType.MCQ);

    return pick(pool, req.count());
  }

  /* helper */
  private double acc(UserProgress p) {
    int total = Math.max(1, p.getMcqTotal());
    return (double) p.getMcqCorrect() / total; // 낮을수록 약점
  }

  private QuizSet pick(List<Question> pool, int count) {
    Collections.shuffle(pool);
    List<Question> picked = pool.stream().limit(Math.max(1, count)).toList();
    List<QuizQ> qs = picked.stream().map(q -> {
      List<QuestionChoice> cs = cRepo.findByQuestionId(q.getId());
      List<QuizQ.McqChoice> ch = cs.stream()
          .map(c -> new QuizQ.McqChoice(c.getLabel(), c.getText())).toList();
      return new QuizQ(q.getId(), q.getText(), ch);
    }).toList();
    return new QuizSet(qs);
  }
}
