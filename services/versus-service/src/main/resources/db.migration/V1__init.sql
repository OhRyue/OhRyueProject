SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

CREATE TABLE IF NOT EXISTS match_room (
  id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  mode      ENUM('DUEL','TOURNAMENT','GOLDENBELL') NOT NULL,
  status    ENUM('WAIT','ONGOING','DONE') NOT NULL DEFAULT 'WAIT',
  scope_json JSON NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS match_participant (
  id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id   BIGINT NOT NULL,
  user_id   VARCHAR(100) NOT NULL,
  joined_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  final_score INT NULL,
  rank      INT NULL,
  INDEX ix_mp_room (room_id),
  INDEX ix_mp_user (user_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS match_question (
  id          BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id     BIGINT NOT NULL,
  order_no    INT NOT NULL,
  question_id BIGINT NOT NULL,
  time_limit_sec INT NOT NULL DEFAULT 10,
  INDEX ix_mq_room (room_id),
  INDEX ix_mq_q (question_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS match_answer (
  id          BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id     BIGINT NOT NULL,
  question_id BIGINT NOT NULL,
  user_id     VARCHAR(100) NOT NULL,
  submitted_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  is_correct  TINYINT(1) NOT NULL DEFAULT 0,
  time_ms     INT NOT NULL DEFAULT 0,
  score_delta INT NOT NULL DEFAULT 0,
  INDEX ix_ma_room (room_id),
  INDEX ix_ma_user (user_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS tournament_bracket (
  id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id   BIGINT NOT NULL,
  round_no  INT NOT NULL,
  pairing_json JSON NOT NULL,
  INDEX ix_tb_room (room_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS goldenbell_rule (
  room_id            BIGINT PRIMARY KEY,
  round_flow_json    JSON NOT NULL,
  elimination        ENUM('IMMEDIATE') NOT NULL DEFAULT 'IMMEDIATE',
  revival_rule_json  JSON NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS goldenbell_state (
  id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id   BIGINT NOT NULL,
  user_id   VARCHAR(100) NOT NULL,
  alive     TINYINT(1) NOT NULL DEFAULT 1,
  revived   TINYINT(1) NOT NULL DEFAULT 0,
  INDEX ix_gs_room (room_id),
  INDEX ix_gs_user (user_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;
