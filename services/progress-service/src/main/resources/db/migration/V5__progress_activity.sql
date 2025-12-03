SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- 통합 학습 활동 로그 테이블
CREATE TABLE IF NOT EXISTS progress_activity (
  id                BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id           VARCHAR(100) NOT NULL,
  activity_group    ENUM('MAIN','ASSIST','BATTLE') NOT NULL,
  main_type         ENUM('MICRO','REVIEW') NULL,
  assist_type       ENUM('CATEGORY','WEAKNESS','DIFFICULTY') NULL,
  battle_type       ENUM('DUEL_CATEGORY','DUEL_DIFFICULTY','TOURNAMENT','GOLDENBELL') NULL,
  mode              ENUM('WRITTEN','PRACTICAL') NOT NULL,
  topic_id          BIGINT NULL,
  topic_name        VARCHAR(255) NULL,
  weakness_tag_name VARCHAR(100) NULL,
  difficulty        VARCHAR(20) NULL,
  question_count    INT NOT NULL DEFAULT 0,
  correct_count     INT NOT NULL DEFAULT 0,
  accuracy_pct      DECIMAL(5,2) NOT NULL,
  final_rank        INT NULL,
  xp_gained         INT NULL,
  source_service    VARCHAR(50) NOT NULL,
  source_session_id BIGINT NOT NULL,
  started_at        TIMESTAMP NOT NULL,
  finished_at       TIMESTAMP NOT NULL,
  created_at        TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_pa_user_finished (user_id, finished_at),
  INDEX ix_pa_user_started (user_id, started_at),
  INDEX ix_pa_group (activity_group),
  INDEX ix_pa_source (source_service, source_session_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;
