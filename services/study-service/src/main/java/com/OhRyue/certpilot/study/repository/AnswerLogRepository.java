package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.AnswerLog;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.Instant;
import java.util.Collection;
import java.util.List;

public interface AnswerLogRepository extends JpaRepository<AnswerLog, Long> {

  // 특정 기간 내의 사용자 오답 로그
  List<AnswerLog> findByUserIdAndCorrectIsFalseAndAnsweredAtAfter(String userId, Instant after);

  // 특정 문제 집합에서의 사용자 정/오답 로그(기간 필터)
  List<AnswerLog> findByUserIdAndQuestionIdInAndAnsweredAtAfter(String userId, Collection<Long> qids, Instant after);

  // 태그 약점 통계를 위해 전체 기간 집계가 필요하면 기간 조건 제거 메서드도 유용
  List<AnswerLog> findByUserIdAndQuestionIdIn(String userId, Collection<Long> qids);

  // 리포트용
  List<AnswerLog> findByUserId(String userId);
  List<AnswerLog> findByUserIdAndAnsweredAtAfter(String userId, Instant after);

}
