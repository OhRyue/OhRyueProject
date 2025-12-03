CREATE TABLE IF NOT EXISTS qnet_qualification_info (
  id                BIGINT       NOT NULL AUTO_INCREMENT PRIMARY KEY,
  jm_cd             VARCHAR(20)  NOT NULL,
  infogb            VARCHAR(50)  NULL,
  contents          TEXT         NULL,
  jmfldnm           VARCHAR(255) NULL,
  mdobligfldcd      VARCHAR(20)  NULL,
  mdobligfldnm      VARCHAR(255) NULL,
  obligfldcd        VARCHAR(20)  NULL,
  obligfldnm        VARCHAR(255) NULL,
  created_at        TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at        TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX ix_qnet_qual_info_jm (jm_cd),
  INDEX ix_qnet_qual_info_jm_infogb (jm_cd, infogb)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

