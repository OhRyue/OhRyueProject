SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- =========================
-- 자격증 마스터
-- =========================
CREATE TABLE IF NOT EXISTS cert (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  name             VARCHAR(255) NOT NULL,
  level            VARCHAR(100) NULL,
  issuer           VARCHAR(255) NULL,
  issuer_url       VARCHAR(500) NULL,
  description      TEXT NULL,
  written_fee      INT NULL,
  practical_fee    INT NULL,
  pass_rule_text   TEXT NULL,
  -- Q-Net 연동용(선택): 종목코드/구분코드 저장
  qnet_jm_cd       VARCHAR(20)  NULL,  -- 종목코드
  qnet_qualgb_cd   VARCHAR(10)  NULL,  -- 자격구분
  UNIQUE KEY uq_cert_qnet (qnet_jm_cd),        -- 종목코드가 있으면 유일
  INDEX ix_cert_qnet_gb (qnet_qualgb_cd)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- =========================
-- 과목 (필기/실기)
-- =========================
CREATE TABLE IF NOT EXISTS cert_subject (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  cert_id          BIGINT NOT NULL,
  exam_mode        ENUM('WRITTEN','PRACTICAL') NOT NULL,
  name             VARCHAR(255) NOT NULL,
  total_questions  INT NULL,
  duration_minutes INT NULL,
  subject_seq      INT NOT NULL DEFAULT 0,  -- 화면 정렬/고정 순서
  CONSTRAINT fk_subject_cert
    FOREIGN KEY (cert_id) REFERENCES cert(id)
    ON DELETE CASCADE ON UPDATE RESTRICT,
  UNIQUE KEY uq_subject_unique (cert_id, exam_mode, name),
  INDEX ix_subject_cert (cert_id),
  INDEX ix_subject_mode (exam_mode)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- =========================
-- 연/회차
-- =========================
CREATE TABLE IF NOT EXISTS exam_round (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  cert_id          BIGINT NOT NULL,
  year             INT NOT NULL,
  round_no         INT NOT NULL,
  exam_mode_scope  ENUM('WRITTEN_ONLY','PRACTICAL_ONLY','BOTH') NOT NULL DEFAULT 'BOTH',
  CONSTRAINT fk_round_cert
    FOREIGN KEY (cert_id) REFERENCES cert(id)
    ON DELETE CASCADE ON UPDATE RESTRICT,
  UNIQUE KEY uq_round (cert_id, year, round_no),
  INDEX ix_round_cert (cert_id),
  INDEX ix_round_year (year)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- =========================
-- 회차별 일정
-- =========================
CREATE TABLE IF NOT EXISTS exam_schedule (
  id          BIGINT AUTO_INCREMENT PRIMARY KEY,
  round_id    BIGINT NOT NULL,
  phase       ENUM('REGISTER','WRITTEN','PRACTICAL','RESULT') NOT NULL,
  start_date  DATE NOT NULL,
  end_date    DATE NOT NULL,
  CONSTRAINT fk_sched_round
    FOREIGN KEY (round_id) REFERENCES exam_round(id)
    ON DELETE CASCADE ON UPDATE RESTRICT,
  CONSTRAINT ck_sched_range CHECK (start_date <= end_date),
  UNIQUE KEY uq_sched (round_id, phase),
  INDEX ix_sched_round (round_id),
  INDEX ix_sched_phase (phase)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- =========================
-- 커리큘럼 트리
-- =========================
CREATE TABLE IF NOT EXISTS topic (
  id         BIGINT AUTO_INCREMENT PRIMARY KEY,
  cert_id    BIGINT NOT NULL,
  parent_id  BIGINT NULL,
  code       VARCHAR(50)  NOT NULL,      -- "1", "1.1", "1.1.1" 등
  title      VARCHAR(255) NOT NULL,
  emoji      VARCHAR(16)  NULL,
  exam_mode  ENUM('WRITTEN','PRACTICAL') NOT NULL DEFAULT 'WRITTEN',
  order_no   INT NOT NULL DEFAULT 0,     -- 같은 부모 내 정렬
  CONSTRAINT fk_topic_cert
    FOREIGN KEY (cert_id) REFERENCES cert(id)
    ON DELETE CASCADE ON UPDATE RESTRICT,
  CONSTRAINT fk_topic_parent
    FOREIGN KEY (parent_id) REFERENCES topic(id)
    ON DELETE SET NULL ON UPDATE RESTRICT,
  CONSTRAINT uq_topic_cert_code UNIQUE (cert_id, code),
  INDEX ix_topic_parent (parent_id),
  INDEX ix_topic_cert (cert_id),
  INDEX ix_topic_mode (exam_mode)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- =========================
-- 개념(리치 블록)
-- =========================
CREATE TABLE IF NOT EXISTS concept (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  topic_id      BIGINT NOT NULL,
  sections_json JSON NOT NULL,
  created_at    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
                       ON UPDATE CURRENT_TIMESTAMP,
  CONSTRAINT fk_concept_topic
    FOREIGN KEY (topic_id) REFERENCES topic(id)
    ON DELETE CASCADE ON UPDATE RESTRICT,
  UNIQUE KEY uq_concept_topic (topic_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;
