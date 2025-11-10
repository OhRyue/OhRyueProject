/* ===================== 질문(문항) ===================== */
/* 사용 쿼리:
   - findByTopicIdAndType(...)
   - findByTopicIdAndDifficultyInAndType(...)
*/
CREATE INDEX ix_q_topic_type
  ON question (topic_id, type);

CREATE INDEX ix_q_topic_diff_type
  ON question (topic_id, difficulty, type);

/* 정답 라벨 조회 최적화 (정답 하나 고르는 쿼리 많음) */
CREATE INDEX ix_choice_q_correct
  ON question_choice (question_id, is_correct);

/* 태그 기반 추천: 태그→질문 매핑 + 커버링 개선 */
CREATE INDEX ix_qtag_tag_q
  ON question_tag (tag, question_id);

/* ===================== 유저 답안 로그 ===================== */
/* 사용 쿼리:
   - user_id & created_at 기간/최근(스프린트 그래프, streak 등)
   - user_id & question_id & created_at (최신 답안, 정오답 집계)
   - question_id & created_at (최근 오답 카운트)
*/
CREATE INDEX ix_uans_q_time
  ON user_answer (question_id, created_at);

/* 이미 스키마에 존재:
   - ix_uans_user_time (user_id, created_at)
   - ix_uans_question (question_id)
   - ix_uans_user_question_time (user_id, question_id, created_at)
*/

/* ===================== 세션/아이템 ===================== */
/* 최근 세션 재개/정렬: 사용자+상태+시작시각 */
CREATE INDEX ix_ss_user_status_started
  ON study_session (user_id, status, started_at);

/* 모드/시험모드별 조회: 사용자 + exam_mode + mode + 시작시각 */
CREATE INDEX ix_ss_user_exam_mode_started
  ON study_session (user_id, exam_mode, mode, started_at);

/* 아이템은 이미 (session_id), (question_id) 및 유니크 키가 있어 충분 */

/* ===================== AI 로그 ===================== */
/* 질문-사용자 단위의 최근 채점/설명 로그 접근 */
CREATE INDEX ix_aigrade_q_user_time
  ON ai_grade_log (question_id, user_id, created_at);

CREATE INDEX ix_aiexp_q_time
  ON ai_explain_log (question_id, created_at);

/* summary 는 session_id UNIQUE 있으므로 추가 불필요 */

/* ===================== 리포트/요약 ===================== */
/* study_summary 는 user_id 필터가 있을 수 있으나 session_id UNIQUE 로 충분.
   필요 시 다음을 사용할 수 있음 (옵션):
   -- CREATE INDEX IF NOT EXISTS ix_sum_user_time ON study_summary(user_id, session_id);
*/
