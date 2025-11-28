SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- 배틀 기록 테이블
CREATE TABLE IF NOT EXISTS battle_record (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id      VARCHAR(100) NOT NULL,
  room_id      BIGINT NOT NULL,
  mode         ENUM('DUEL','TOURNAMENT','GOLDENBELL') NOT NULL,
  exam_mode    ENUM('WRITTEN','PRACTICAL') NULL,
  score        INT NOT NULL DEFAULT 0,
  rank         INT NOT NULL,
  correct_count INT NOT NULL DEFAULT 0,
  total_count  INT NOT NULL DEFAULT 0,
  total_time_ms BIGINT NOT NULL DEFAULT 0,
  is_winner    TINYINT(1) NOT NULL DEFAULT 0,
  completed_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_br_user_time (user_id, completed_at),
  INDEX ix_br_room (room_id),
  INDEX ix_br_mode (mode)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 배틀 답안 테이블 (사용자가 제출한 개별 답안)
CREATE TABLE IF NOT EXISTS battle_answer (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  battle_record_id BIGINT NOT NULL,
  question_id  BIGINT NOT NULL,
  user_answer  TEXT NULL,
  is_correct   TINYINT(1) NOT NULL DEFAULT 0,
  time_ms      INT NOT NULL DEFAULT 0,
  score_delta  INT NOT NULL DEFAULT 0,
  round_no     INT NULL,
  phase        VARCHAR(20) NULL,
  submitted_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_ba_battle (battle_record_id),
  INDEX ix_ba_question (question_id),
  FOREIGN KEY (battle_record_id) REFERENCES battle_record(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;

