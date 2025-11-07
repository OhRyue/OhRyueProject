SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- 문항 뱅크
CREATE TABLE IF NOT EXISTS question (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  topic_id     BIGINT       NOT NULL, -- cert.topic 논리 FK
  type         ENUM('OX','MCQ','SHORT','LONG') NOT NULL,
  difficulty   ENUM('EASY','NORMAL','HARD') NOT NULL DEFAULT 'NORMAL',
  text         TEXT         NOT NULL,
  explanation  TEXT         NULL,
  image_url    VARCHAR(500) NULL,
  ox_answer    TINYINT(1)   NULL,
  INDEX ix_q_topic (topic_id),
  INDEX ix_q_type (type),
  INDEX ix_q_diff (difficulty)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS question_choice (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  question_id   BIGINT       NOT NULL,
  label         CHAR(1)      NOT NULL,
  text          VARCHAR(1000) NOT NULL,
  is_correct    TINYINT(1)   NOT NULL DEFAULT 0,
  UNIQUE KEY uq_choice_label (question_id, label),
  INDEX ix_choice_q (question_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS question_tag (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  question_id  BIGINT       NOT NULL,
  tag          VARCHAR(100) NOT NULL,
  INDEX ix_qtag_q (question_id),
  INDEX ix_qtag_t (tag)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 세션/진행/답안
CREATE TABLE IF NOT EXISTS study_session (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id          VARCHAR(100) NOT NULL,
  mode             ENUM('MICRO','REVIEW','ASSIST_CATEGORY','ASSIST_DIFF','ASSIST_WEAK') NOT NULL,
  exam_mode        ENUM('WRITTEN','PRACTICAL') NOT NULL,
  topic_scope_json JSON NULL,
  question_count   INT NOT NULL,
  started_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  closed_at        TIMESTAMP NULL,
  status           ENUM('OPEN','SUBMITTED','CLOSED') NOT NULL DEFAULT 'OPEN',
  INDEX ix_ss_user (user_id),
  INDEX ix_ss_mode (mode),
  INDEX ix_ss_started (started_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS study_session_item (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  session_id    BIGINT NOT NULL,
  order_no      INT NOT NULL,
  question_id   BIGINT NOT NULL,
  submitted_at  TIMESTAMP NULL,
  correct       TINYINT(1) NULL,
  score         INT NULL,
  UNIQUE KEY uq_ssi_session_order (session_id, order_no),
  UNIQUE KEY uq_ssi_session_question (session_id, question_id),
  INDEX ix_ssi_session (session_id),
  INDEX ix_ssi_question (question_id),
  CONSTRAINT fk_ssi_session FOREIGN KEY (session_id) REFERENCES study_session(id)
    ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_progress (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id       VARCHAR(100) NOT NULL,
  topic_id      BIGINT NOT NULL,
  exam_mode     ENUM('WRITTEN','PRACTICAL') NOT NULL DEFAULT 'WRITTEN',
  mini_total    INT NULL,
  mini_correct  INT NULL,
  mini_passed   TINYINT(1) NULL,
  mcq_total     INT NULL,
  mcq_correct   INT NULL,
  updated_at    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  UNIQUE KEY uq_progress (user_id, topic_id, exam_mode),
  INDEX ix_progress_user (user_id),
  INDEX ix_progress_topic (topic_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_answer (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id      VARCHAR(100)  NOT NULL,
  question_id  BIGINT        NOT NULL,
  correct      TINYINT(1)    NULL,
  score        INT           NULL,
  answer_text  VARCHAR(2000) NULL,
  created_at   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_uans_user_time (user_id, created_at),
  INDEX ix_uans_question (question_id),
  INDEX ix_uans_user_question_time (user_id, question_id, created_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS study_summary (
  id              BIGINT AUTO_INCREMENT PRIMARY KEY,
  session_id      BIGINT NOT NULL,
  user_id         VARCHAR(100) NOT NULL,
  ai_summary_text TEXT NULL,
  total           INT  NOT NULL,
  correct         INT  NOT NULL,
  accuracy        DECIMAL(5,2) NOT NULL,
  time_spent_sec  INT  NULL,
  INDEX ix_sum_user (user_id),
  UNIQUE KEY uq_sum_session (session_id),
  CONSTRAINT fk_sum_session FOREIGN KEY (session_id) REFERENCES study_session(id)
    ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- AI 로그
CREATE TABLE IF NOT EXISTS ai_grade_log (
  id          BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id     VARCHAR(100) NOT NULL,
  question_id BIGINT NOT NULL,
  model       VARCHAR(100) NOT NULL,
  prompt_ref  VARCHAR(255) NULL,
  score       INT NULL,
  latency_ms  INT NULL,
  created_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_aigrade_user (user_id),
  INDEX ix_aigrade_q (question_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS ai_explain_log (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  session_id       BIGINT NULL,
  question_id      BIGINT NOT NULL,
  model            VARCHAR(100) NOT NULL,
  explanation_text TEXT NOT NULL,
  created_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_aiexp_q (question_id),
  INDEX ix_aiexp_s (session_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS ai_summary_log (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  session_id   BIGINT NOT NULL,
  model        VARCHAR(100) NOT NULL,
  summary_text TEXT NOT NULL,
  created_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE KEY uq_aisum_session (session_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;
