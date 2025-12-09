SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- =========================================================
-- 문제 뱅크
-- =========================================================
CREATE TABLE IF NOT EXISTS question (
  id             BIGINT AUTO_INCREMENT PRIMARY KEY,
  cert_id        BIGINT        NOT NULL,
  topic_id       BIGINT        NOT NULL,  -- 논리 FK (cert-service.topic.id), FK 제약 없음
  mode           ENUM('WRITTEN','PRACTICAL') NOT NULL,
  type           ENUM('OX','MCQ','SHORT','LONG') NOT NULL,
  difficulty     ENUM('EASY','NORMAL','HARD') NOT NULL DEFAULT 'NORMAL',
  stem           TEXT          NOT NULL,
  payload_json   JSON          NULL,
  answer_key     TEXT          NULL,
  solution_text  TEXT          NULL,
  source         VARCHAR(100)  NULL,
  image_url      VARCHAR(500)  NULL,
  created_at     TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at     TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX ix_q_topic_mode (topic_id, mode),
  INDEX ix_q_mode_type (mode, type),
  INDEX ix_q_difficulty (difficulty),
  INDEX ix_q_topic_type (topic_id, type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS question_choice (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  question_id   BIGINT        NOT NULL,
  label         CHAR(1)       NOT NULL,
  content       VARCHAR(1000) NOT NULL,
  is_correct    TINYINT(1)    NOT NULL DEFAULT 0,
  created_at    TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE KEY uq_choice (question_id, label),
  INDEX ix_choice_question (question_id),
  CONSTRAINT fk_choice_question FOREIGN KEY (question_id) REFERENCES question(id)
    ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS question_tag (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  question_id  BIGINT       NOT NULL,
  tag          VARCHAR(100) NOT NULL,
  created_at   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE KEY uq_qtag (question_id, tag),
  INDEX ix_qtag_tag (tag),
  INDEX ix_qtag_question (question_id),
  CONSTRAINT fk_qtag_question FOREIGN KEY (question_id) REFERENCES question(id)
    ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- =========================================================
-- 태그 마스터 (필기/실기 공통 태그 정의)
-- =========================================================
CREATE TABLE IF NOT EXISTS tag_master (
  code        VARCHAR(100)                            NOT NULL, -- 영어 태그 코드 (PK, 예: DB_SQL_TX, P_SQL_SELECT_JOIN)
  domain      ENUM('WRITTEN','PRACTICAL')             NOT NULL, -- 필기/실기 구분
  label_ko    VARCHAR(255)                            NOT NULL, -- 화면에 표시할 한글 라벨
  label_en    VARCHAR(255)                            NULL,     -- 필요 시 영문 라벨
  description TEXT                                    NULL,     -- 태그 설명 (선택)
  order_no    INT                                     NULL,     -- 태그 정렬용 (선택)
  created_at  TIMESTAMP                               NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (code),
  INDEX ix_tag_domain (domain, code)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- =========================================================
-- LearningSession / LearningStep (계층적 학습 세션)
-- =========================================================

-- 전체 학습 플로우
CREATE TABLE IF NOT EXISTS learning_session (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id          VARCHAR(100) NOT NULL,
  topic_id         BIGINT NOT NULL,
  mode             VARCHAR(50) NOT NULL COMMENT 'WRITTEN | PRACTICAL | REVIEW | ASSIST_PRACTICAL_DIFFICULTY | ASSIST_WRITTEN_DIFFICULTY',
  status           VARCHAR(20) NOT NULL DEFAULT 'IN_PROGRESS' COMMENT 'IN_PROGRESS | DONE',
  truly_completed  TINYINT(1) NULL DEFAULT NULL COMMENT '진정한 완료 여부 (MCQ 완료) - NULL: 미완료, 1: 완료',
  started_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX ix_ls_user_topic (user_id, topic_id),
  INDEX ix_ls_user_updated (user_id, updated_at),
  INDEX ix_ls_status (status)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 단계별 학습 스텝
CREATE TABLE IF NOT EXISTS learning_step (
  id                  BIGINT AUTO_INCREMENT PRIMARY KEY,
  learning_session_id BIGINT NOT NULL,
  step_code           VARCHAR(30) NOT NULL COMMENT 'MINI | REVIEW_WRONG | MCQ | PRACTICAL | REVIEW_WRONG2 | SUMMARY',
  status              VARCHAR(20) NOT NULL DEFAULT 'READY' COMMENT 'READY | IN_PROGRESS | COMPLETE',
  score_pct           INT NULL COMMENT '정답률(%)',
  metadata_json       JSON NULL COMMENT '단계별 메타데이터',
  created_at          TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at          TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX ix_ls_learning_session (learning_session_id),
  INDEX ix_ls_step_code (learning_session_id, step_code),
  UNIQUE KEY uq_ls_session_step (learning_session_id, step_code),
  CONSTRAINT fk_ls_learning_session FOREIGN KEY (learning_session_id) REFERENCES learning_session(id)
    ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- =========================================================
-- 학습 세션 및 진행
-- =========================================================
CREATE TABLE IF NOT EXISTS study_session (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id          VARCHAR(100) NOT NULL,
  -- mode: 기존 ENUM → VARCHAR(50)로 확장
  mode             VARCHAR(50) NOT NULL COMMENT 'MICRO | REVIEW | ASSIST_CATEGORY | ASSIST_DIFFICULTY | ASSIST_WEAK | ASSIST_PRACTICAL_DIFFICULTY | ASSIST_WRITTEN_DIFFICULTY',
  exam_mode        ENUM('WRITTEN','PRACTICAL') NOT NULL,
  topic_scope_json JSON NULL,
  question_count   INT NOT NULL,
  started_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  finished_at      TIMESTAMP NULL,
  score_pct        DECIMAL(5,2) NULL,
  summary_json     JSON NULL,
  status           ENUM('OPEN','SUBMITTED','CLOSED') NOT NULL DEFAULT 'OPEN',

  -- V13: 완료/합격/XP 플래그
  completed        TINYINT(1) NOT NULL DEFAULT 0 COMMENT '모든 문제를 풀었는지',
  passed           TINYINT(1) NOT NULL DEFAULT 0 COMMENT '모든 문제를 맞췄는지',
  xp_granted       TINYINT(1) NOT NULL DEFAULT 0 COMMENT 'XP가 이미 반영되었는지',

  -- V14: 어떤 학습 단계(learning_step)에 속하는 세션인지
  learning_step_id BIGINT NULL COMMENT '어떤 학습 단계의 문제 풀이 세션인지',

  INDEX ix_ss_user_status (user_id, status),
  INDEX ix_ss_user_started (user_id, started_at),
  INDEX ix_ss_mode (mode, exam_mode),
  INDEX ix_ss_learning_step (learning_step_id),
  CONSTRAINT fk_ss_learning_step FOREIGN KEY (learning_step_id) REFERENCES learning_step(id)
    ON DELETE SET NULL ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS study_session_item (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  session_id       BIGINT NOT NULL,
  order_no         INT NOT NULL,
  question_id      BIGINT NOT NULL,
  user_answer_json JSON NULL,
  is_correct       TINYINT(1) NULL,
  score            INT NULL,
  answered_at      TIMESTAMP NULL,
  ai_explain_json  JSON NULL,
  created_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE KEY uq_ssi_session_order (session_id, order_no),
  UNIQUE KEY uq_ssi_session_question (session_id, question_id),
  INDEX ix_ssi_session (session_id),
  INDEX ix_ssi_question (question_id),
  CONSTRAINT fk_ssi_session FOREIGN KEY (session_id) REFERENCES study_session(id)
    ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_answer (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id          VARCHAR(100) NOT NULL,
  question_id      BIGINT NOT NULL,
  exam_mode        ENUM('WRITTEN','PRACTICAL') NOT NULL,
  question_type    ENUM('OX','MCQ','SHORT','LONG') NOT NULL,
  answered_at      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  user_answer_json JSON NULL,
  is_correct       TINYINT(1) NULL,
  score            INT NULL,
  source           VARCHAR(30) NULL,
  session_id       BIGINT NULL,
  session_item_id  BIGINT NULL,
  INDEX ix_uans_user_time (user_id, answered_at),
  INDEX ix_uans_question_time (question_id, answered_at),
  INDEX ix_uans_user_question_time (user_id, question_id, answered_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_progress (
  id                   BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id              VARCHAR(100) NOT NULL,
  topic_id             BIGINT NOT NULL,  -- 논리 FK (cert.topic)
  written_done_cnt     INT NOT NULL DEFAULT 0,
  practical_done_cnt   INT NOT NULL DEFAULT 0,
  written_accuracy     DECIMAL(5,2) NOT NULL DEFAULT 0.00,
  practical_avg_score  DECIMAL(5,2) NOT NULL DEFAULT 0.00,
  last_studied_at      TIMESTAMP NULL,
  updated_at           TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  UNIQUE KEY uq_progress_user_topic (user_id, topic_id),
  INDEX ix_progress_updated (updated_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- =========================================================
-- AI 로그
-- =========================================================
CREATE TABLE IF NOT EXISTS ai_grade_log (
  id          BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id     VARCHAR(100) NOT NULL,
  question_id BIGINT NOT NULL,
  model       VARCHAR(100) NOT NULL,
  prompt_ref  VARCHAR(255) NULL,
  score       INT NULL,
  latency_ms  INT NULL,
  created_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_aigrade_user (user_id, created_at),
  INDEX ix_aigrade_question (question_id, created_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS ai_explain_log (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  session_id    BIGINT NULL,
  question_id   BIGINT NOT NULL,
  model         VARCHAR(100) NOT NULL,
  payload_json  JSON NULL,
  created_at    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_aiexplain_session (session_id),
  INDEX ix_aiexplain_question (question_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS ai_summary_log (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  session_id   BIGINT NOT NULL,
  model        VARCHAR(100) NOT NULL,
  payload_json JSON NULL,
  created_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE KEY uq_aisummary_session (session_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- =========================================================
-- 보강 인덱스
-- =========================================================

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

SET FOREIGN_KEY_CHECKS = 1;
