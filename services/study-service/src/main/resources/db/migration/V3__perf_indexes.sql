/* 보강 인덱스 스크립트 */

/* 문제 조회: 모드/유형/난이도 조합 조회 최적화 */
CREATE INDEX ix_q_mode_type_diff
  ON question (mode, type, difficulty);

/* 문제 태그 기반 필터링 */
CREATE INDEX ix_qtag_tag_question
  ON question_tag (tag, question_id);

/* 사용자 답안: 모드/유형별 최근 풀이 */
CREATE INDEX ix_uans_user_mode_type_time
  ON user_answer (user_id, exam_mode, question_type, answered_at);

/* 세션 상태별 정렬 */
CREATE INDEX ix_ss_status_started
  ON study_session (status, started_at);

/* 세션 아이템: 응답 시각 정렬 */
CREATE INDEX ix_ssi_session_answered
  ON study_session_item (session_id, answered_at);

/* AI 로그: 사용자별 추적 */
CREATE INDEX ix_aigrade_user_question_time
  ON ai_grade_log (user_id, question_id, created_at);

CREATE INDEX ix_aiexplain_question_time
  ON ai_explain_log (question_id, created_at);
