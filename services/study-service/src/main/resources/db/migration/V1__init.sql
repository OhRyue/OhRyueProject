SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

/* =========================================================
 * 토픽/개념
 * ========================================================= */
CREATE TABLE IF NOT EXISTS topic (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  cert_id      BIGINT        NOT NULL,
  parent_id    BIGINT        NULL,
  code         VARCHAR(50)   NOT NULL,
  title        VARCHAR(200)  NOT NULL,
  emoji        VARCHAR(10)   NULL,
  order_no     INT           NOT NULL DEFAULT 0,
  exam_mode    ENUM('WRITTEN','PRACTICAL') NOT NULL,
  created_at   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  UNIQUE KEY uq_topic (cert_id, code),
  INDEX ix_topic_parent (parent_id),
  INDEX ix_topic_mode (exam_mode)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS concept (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  topic_id      BIGINT        NOT NULL,
  sections_json JSON          NOT NULL,
  created_at    TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at    TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  UNIQUE KEY uq_concept_topic (topic_id),
  CONSTRAINT fk_concept_topic FOREIGN KEY (topic_id) REFERENCES topic(id)
    ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

/* =========================================================
 * 문제 뱅크
 * ========================================================= */
CREATE TABLE IF NOT EXISTS question (
  id             BIGINT AUTO_INCREMENT PRIMARY KEY,
  cert_id        BIGINT        NOT NULL,
  topic_id       BIGINT        NOT NULL,
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
  question_id   BIGINT       NOT NULL,
  label         CHAR(1)      NOT NULL,
  content       VARCHAR(1000) NOT NULL,
  is_correct    TINYINT(1)   NOT NULL DEFAULT 0,
  created_at    TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
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

/* =========================================================
 * 학습 세션 및 진행
 * ========================================================= */
CREATE TABLE IF NOT EXISTS study_session (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id          VARCHAR(100) NOT NULL,
  mode             ENUM('MICRO','REVIEW','ASSIST_CATEGORY','ASSIST_DIFFICULTY','ASSIST_WEAK') NOT NULL,
  exam_mode        ENUM('WRITTEN','PRACTICAL') NOT NULL,
  topic_scope_json JSON NULL,
  question_count   INT NOT NULL,
  started_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  finished_at      TIMESTAMP NULL,
  score_pct        DECIMAL(5,2) NULL,
  summary_json     JSON NULL,
  status           ENUM('OPEN','SUBMITTED','CLOSED') NOT NULL DEFAULT 'OPEN',
  INDEX ix_ss_user_status (user_id, status),
  INDEX ix_ss_user_started (user_id, started_at),
  INDEX ix_ss_mode (mode, exam_mode)
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
  topic_id             BIGINT NOT NULL,
  written_done_cnt     INT NOT NULL DEFAULT 0,
  practical_done_cnt   INT NOT NULL DEFAULT 0,
  written_accuracy     DECIMAL(5,2) NOT NULL DEFAULT 0.00,
  practical_avg_score  DECIMAL(5,2) NOT NULL DEFAULT 0.00,
  last_studied_at      TIMESTAMP NULL,
  updated_at           TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  UNIQUE KEY uq_progress_user_topic (user_id, topic_id),
  INDEX ix_progress_updated (updated_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

/* =========================================================
 * AI 로그
 * ========================================================= */
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

SET FOREIGN_KEY_CHECKS = 1;
