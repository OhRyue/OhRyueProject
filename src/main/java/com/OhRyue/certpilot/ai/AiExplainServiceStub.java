package com.OhRyue.certpilot.ai;

import com.OhRyue.certpilot.question.domain.Question;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

/** 임시 스텁: 실제 LLM 연동 전까지 사용. 실패 시 DB 해설로 폴백. */
@Slf4j
@Service
public class AiExplainServiceStub implements AiExplainService {

  @Override
  public String explainForQuestion(Question q, Integer chosenIdx, String dbExp) {
    try {
      String prefix = (chosenIdx != null && !chosenIdx.equals(q.getAnswerIdx()))
          ? "오답 이유(요약): 선택한 보기(" + chosenIdx + ")가 핵심 개념과 불일치합니다. "
          : "정답 핵심(요약): 핵심 개념에 부합합니다. ";

      String base = (dbExp == null || dbExp.isBlank())
          ? "기본 해설이 없습니다."
          : dbExp;

      return prefix + " / 기본 해설: " + base;
    } catch (Exception e) {
      log.warn("AI explain failed, fallback to DB exp. qid={}", q.getId(), e);
      return (dbExp == null || dbExp.isBlank()) ? "해설 생성 실패" : dbExp;
    }
  }

  @Override
  public String summarizeReview(Map<Long, Boolean> correctness, List<String> weakTags) {
    int total = correctness.size();
    long ok = correctness.values().stream().filter(Boolean::booleanValue).count();
    long wrong = total - ok;
    String weak = weakTags == null || weakTags.isEmpty() ? "약점 태그 없음" : "약점 태그 예: " + String.join(", ", weakTags);

    return "리뷰 요약: 총 " + total + "문항 중 정답 " + ok + ", 오답 " + wrong + ". " + weak
        + ". 오답은 정답 근거를 다시 확인하고 유사 문항으로 보강하세요.";
  }
}
