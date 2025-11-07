SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

CREATE TABLE IF NOT EXISTS user_account (
  id              VARCHAR(100) PRIMARY KEY,
  email           VARCHAR(255) NOT NULL UNIQUE,
  password_hash   VARCHAR(255) NOT NULL,
  status          ENUM('ACTIVE','BLOCKED','DELETED') NOT NULL DEFAULT 'ACTIVE',
  created_at      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  last_login_at   TIMESTAMP NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_profile (
  user_id       VARCHAR(100) PRIMARY KEY,
  nickname      VARCHAR(100) NOT NULL,
  avatar_url    VARCHAR(500) NULL,
  timezone      VARCHAR(64)  NULL,
  lang          VARCHAR(16)  NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_settings (
  user_id       VARCHAR(100) PRIMARY KEY,
  ui_prefs_json JSON NULL,
  notif_prefs_json JSON NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_goal_cert (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id          VARCHAR(100) NOT NULL,
  cert_id          BIGINT NOT NULL,
  target_exam_mode ENUM('WRITTEN','PRACTICAL') NOT NULL,
  target_round_id  BIGINT NULL,
  dday_cached      INT NULL,
  created_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_goal_user (user_id),
  INDEX ix_goal_cert (cert_id),
  INDEX ix_goal_round (target_round_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;
