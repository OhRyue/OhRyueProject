SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

CREATE TABLE IF NOT EXISTS cert (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  name          VARCHAR(255) NOT NULL,
  level         VARCHAR(100) NULL,
  issuer        VARCHAR(255) NULL,
  description   TEXT NULL,
  written_fee   INT NULL,
  practical_fee INT NULL,
  pass_rule_text TEXT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS cert_subject (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  cert_id          BIGINT NOT NULL,
  exam_mode        ENUM('WRITTEN','PRACTICAL') NOT NULL,
  name             VARCHAR(255) NOT NULL,
  total_questions  INT NULL,
  duration_minutes INT NULL,
  INDEX ix_subject_cert (cert_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS exam_round (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  cert_id          BIGINT NOT NULL,
  year             INT NOT NULL,
  round_no         INT NOT NULL,
  exam_mode_scope  ENUM('WRITTEN_ONLY','PRACTICAL_ONLY','BOTH') NOT NULL DEFAULT 'BOTH',
  INDEX ix_round_cert (cert_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS exam_schedule (
  id          BIGINT AUTO_INCREMENT PRIMARY KEY,
  round_id    BIGINT NOT NULL,
  phase       ENUM('REGISTER','WRITTEN','PRACTICAL','RESULT') NOT NULL,
  start_date  DATE NOT NULL,
  end_date    DATE NOT NULL,
  INDEX ix_sched_round (round_id),
  INDEX ix_sched_phase (phase)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 커리큘럼
CREATE TABLE IF NOT EXISTS topic (
  id         BIGINT AUTO_INCREMENT PRIMARY KEY,
  cert_id    BIGINT NOT NULL,
  parent_id  BIGINT NULL,
  code       VARCHAR(50)  NOT NULL,
  title      VARCHAR(255) NOT NULL,
  exam_mode  ENUM('WRITTEN','PRACTICAL') NOT NULL DEFAULT 'WRITTEN',
  CONSTRAINT uq_topic_code UNIQUE (code),
  INDEX ix_topic_parent (parent_id),
  INDEX ix_topic_cert (cert_id),
  INDEX ix_topic_mode (exam_mode)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS concept (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  topic_id     BIGINT NOT NULL,
  content      TEXT NULL,
  blocks_json  JSON NULL,
  UNIQUE KEY uq_concept_topic (topic_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;
