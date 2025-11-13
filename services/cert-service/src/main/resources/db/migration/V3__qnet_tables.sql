CREATE TABLE IF NOT EXISTS qualification (
  jm_cd        VARCHAR(20)  NOT NULL PRIMARY KEY,
  series_cd    VARCHAR(10)  NULL,
  jm_nm        VARCHAR(255) NULL,
  eng_jm_nm    VARCHAR(255) NULL,
  series_nm    VARCHAR(255) NULL,
  impl_nm      VARCHAR(255) NULL,
  insti_nm     VARCHAR(255) NULL,
  summary      TEXT         NULL,
  job          TEXT         NULL,
  trend        TEXT         NULL,
  career       TEXT         NULL,
  hist         TEXT         NULL,
  created_at   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS exam_schedule (
  id                BIGINT       NOT NULL AUTO_INCREMENT PRIMARY KEY,
  source            VARCHAR(20)  NOT NULL,
  impl_yy           VARCHAR(4)   NULL,
  impl_seq          VARCHAR(10)  NULL,
  qualgb_cd         VARCHAR(10)  NULL,
  qualgb_nm         VARCHAR(255) NULL,
  jm_cd             VARCHAR(20)  NULL,
  jm_nm             VARCHAR(255) NULL,
  description       TEXT         NULL,
  doc_reg_start_dt  DATE         NULL,
  doc_reg_end_dt    DATE         NULL,
  doc_exam_start_dt DATE         NULL,
  doc_exam_end_dt   DATE         NULL,
  doc_pass_dt       DATE         NULL,
  prac_reg_start_dt DATE         NULL,
  prac_reg_end_dt   DATE         NULL,
  prac_exam_start_dt DATE        NULL,
  prac_exam_end_dt   DATE        NULL,
  prac_pass_dt      DATE         NULL,
  created_at        TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at        TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX ix_exam_schedule_jm (jm_cd),
  INDEX ix_exam_schedule_yy_seq (impl_yy, impl_seq)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS open_question (
  artl_seq         BIGINT       NOT NULL PRIMARY KEY,
  title            VARCHAR(500) NULL,
  series_cd        VARCHAR(10)  NULL,
  series_nm        VARCHAR(255) NULL,
  qualgb_cd        VARCHAR(10)  NULL,
  qualgb_nm        VARCHAR(255) NULL,
  jm_cd            VARCHAR(20)  NULL,
  jm_nm            VARCHAR(255) NULL,
  reg_dttm         DATETIME     NULL,
  mod_dttm         DATETIME     NULL,
  attachments_json JSON         NULL,
  created_at       TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at       TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX ix_open_question_jm (jm_cd),
  INDEX ix_open_question_series (series_cd)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

