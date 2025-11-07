SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- XP/레벨/연속학습/배지
CREATE TABLE IF NOT EXISTS user_xp_wallet (
  user_id        VARCHAR(100) PRIMARY KEY,
  xp_total       BIGINT NOT NULL DEFAULT 0,
  level          INT NOT NULL DEFAULT 1,
  last_levelup_at TIMESTAMP NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_xp_ledger (
  id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id   VARCHAR(100) NOT NULL,
  delta     INT NOT NULL,
  reason    ENUM('MICRO','REVIEW','ASSIST','BATTLE','DAILY_STREAK','ETC') NOT NULL,
  ref_id    VARCHAR(100) NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_xpledger_user_time (user_id, created_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_streak (
  user_id         VARCHAR(100) PRIMARY KEY,
  current_days    INT NOT NULL DEFAULT 0,
  best_days       INT NOT NULL DEFAULT 0,
  last_active_date DATE NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS badge_catalog (
  id      BIGINT AUTO_INCREMENT PRIMARY KEY,
  code    VARCHAR(100) NOT NULL UNIQUE,
  name    VARCHAR(255) NOT NULL,
  rarity  ENUM('common','rare','epic') NOT NULL DEFAULT 'common',
  rule_json JSON NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_badge (
  id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id   VARCHAR(100) NOT NULL,
  badge_id  BIGINT NOT NULL,
  earned_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_ub_user (user_id),
  INDEX ix_ub_badge (badge_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 랭킹
CREATE TABLE IF NOT EXISTS user_rank_score (
  user_id        VARCHAR(100) PRIMARY KEY,
  score          BIGINT NOT NULL DEFAULT 0,
  last_updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS leaderboard_snapshot (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  snapshot_date DATE NOT NULL,
  scope         ENUM('GLOBAL','FRIENDS') NOT NULL DEFAULT 'GLOBAL',
  payload_json  JSON NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 리포트/약점/목표
CREATE TABLE IF NOT EXISTS assist_goal_daily (
  user_id       VARCHAR(100) NOT NULL,
  date          DATE NOT NULL,
  target_count  INT NOT NULL,
  progress_count INT NOT NULL DEFAULT 0,
  updated_at    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (user_id, date)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS assist_weekly_stats (
  user_id      VARCHAR(100) NOT NULL,
  week_iso     VARCHAR(10)  NOT NULL,
  solved_count INT NOT NULL DEFAULT 0,
  avg_accuracy DECIMAL(5,2) NOT NULL DEFAULT 0.00,
  PRIMARY KEY (user_id, week_iso)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS report_daily (
  user_id        VARCHAR(100) NOT NULL,
  date           DATE NOT NULL,
  solved_count   INT NOT NULL DEFAULT 0,
  time_spent_sec INT NOT NULL DEFAULT 0,
  accuracy       DECIMAL(5,2) NOT NULL DEFAULT 0.00,
  xp_gained      INT NOT NULL DEFAULT 0,
  PRIMARY KEY (user_id, date)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS report_weekly (
  user_id        VARCHAR(100) NOT NULL,
  week_iso       VARCHAR(10)  NOT NULL,
  solved_count   INT NOT NULL DEFAULT 0,
  time_spent_sec INT NOT NULL DEFAULT 0,
  accuracy       DECIMAL(5,2) NOT NULL DEFAULT 0.00,
  xp_gained      INT NOT NULL DEFAULT 0,
  PRIMARY KEY (user_id, week_iso)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS report_tag_skill (
  id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id   VARCHAR(100) NOT NULL,
  tag       VARCHAR(100) NOT NULL,
  exam_mode ENUM('WRITTEN','PRACTICAL') NOT NULL,
  correct   INT NOT NULL DEFAULT 0,
  total     INT NOT NULL DEFAULT 0,
  accuracy  DECIMAL(5,2) NOT NULL DEFAULT 0.00,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX ix_rtag_user (user_id),
  INDEX ix_rtag_tag (tag)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 상점/포인트/인벤토리
CREATE TABLE IF NOT EXISTS user_point_wallet (
  user_id     VARCHAR(100) PRIMARY KEY,
  point_total BIGINT NOT NULL DEFAULT 0,
  updated_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_point_ledger (
  id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id   VARCHAR(100) NOT NULL,
  delta     INT NOT NULL,
  reason    ENUM('PURCHASE','REWARD','REFUND') NOT NULL,
  ref_id    VARCHAR(100) NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_ptledger_user_time (user_id, created_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS store_item (
  id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  category  ENUM('HAT','CLOTHES','ACC','BG','SPECIAL') NOT NULL,
  name      VARCHAR(255) NOT NULL,
  image_url VARCHAR(500) NULL,
  price     INT NOT NULL,
  rarity    ENUM('common','rare','epic') NOT NULL DEFAULT 'common',
  limit_per_user INT NULL,
  is_active TINYINT(1) NOT NULL DEFAULT 1
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_inventory (
  id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id   VARCHAR(100) NOT NULL,
  item_id   BIGINT NOT NULL,
  owned_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_inv_user (user_id),
  INDEX ix_inv_item (item_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS user_loadout (
  user_id     VARCHAR(100) PRIMARY KEY,
  hat_id      BIGINT NULL,
  clothes_id  BIGINT NULL,
  acc_id      BIGINT NULL,
  bg_id       BIGINT NULL,
  special_id  BIGINT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;
