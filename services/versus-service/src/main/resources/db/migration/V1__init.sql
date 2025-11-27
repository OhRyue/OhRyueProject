SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

CREATE TABLE IF NOT EXISTS match_room (
  id         BIGINT AUTO_INCREMENT PRIMARY KEY,
  mode       ENUM('DUEL','TOURNAMENT','GOLDENBELL') NOT NULL,
  status     ENUM('WAIT','ONGOING','DONE') NOT NULL DEFAULT 'WAIT',
  scope_json JSON NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS match_participant (
  id          BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id     BIGINT NOT NULL,
  user_id     VARCHAR(100) NOT NULL,
  joined_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  final_score INT NULL,
  player_rank INT NULL,
  INDEX ix_mp_room (room_id),
  INDEX ix_mp_user (user_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 라운드/파이널/부활을 명확히 관리하기 위해 round_no/phase 추가
CREATE TABLE IF NOT EXISTS match_question (
  id             BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id        BIGINT NOT NULL,
  round_no       INT NOT NULL,                                     -- 1,2,3,4 또는 파이널 전용 번호(예: 99)
  phase          ENUM('MAIN','FINAL','REVIVAL') NOT NULL DEFAULT 'MAIN',
  order_no       INT NOT NULL,
  question_id    BIGINT NOT NULL,
  time_limit_sec INT NOT NULL DEFAULT 10,
  UNIQUE KEY uq_mq_room_round_order (room_id, round_no, order_no), -- 라운드 내 순서 고정
  INDEX ix_mq_room (room_id),
  INDEX ix_mq_round (room_id, round_no),
  INDEX ix_mq_phase (room_id, phase),
  INDEX ix_mq_q (question_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 답안에도 round_no/phase(옵션) 추가 → 최종전 합산/부활전 집계 용이
CREATE TABLE IF NOT EXISTS match_answer (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id      BIGINT NOT NULL,
  round_no     INT NULL,
  phase        ENUM('MAIN','FINAL','REVIVAL') NULL,
  question_id  BIGINT NOT NULL,
  user_id      VARCHAR(100) NOT NULL,
  submitted_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  is_correct   TINYINT(1) NOT NULL DEFAULT 0,
  time_ms      INT NOT NULL DEFAULT 0,
  score_delta  INT NOT NULL DEFAULT 0,
  UNIQUE KEY uq_ma_once (room_id, question_id, user_id),
  INDEX ix_ma_room (room_id),
  INDEX ix_ma_user (user_id),
  INDEX ix_ma_round (room_id, round_no),
  INDEX ix_ma_phase (room_id, phase)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS tournament_bracket (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id      BIGINT NOT NULL,
  round_no     INT NOT NULL,
  pairing_json JSON NOT NULL,
  UNIQUE KEY uq_tb_room_round (room_id, round_no),
  INDEX ix_tb_room (room_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS goldenbell_rule (
  room_id           BIGINT PRIMARY KEY,
  round_flow_json   JSON NOT NULL,          -- 라운드 정의(JSON 배열)
  elimination       ENUM('IMMEDIATE') NOT NULL DEFAULT 'IMMEDIATE',
  revival_rule_json JSON NULL               -- 부활 규칙(JSON)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS goldenbell_state (
  id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id   BIGINT NOT NULL,
  user_id   VARCHAR(100) NOT NULL,
  alive     TINYINT(1) NOT NULL DEFAULT 1,  -- 1=생존, 0=탈락(관전전환)
  revived   TINYINT(1) NOT NULL DEFAULT 0,  -- 부활 여부
  UNIQUE KEY uq_gs_room_user (room_id, user_id),
  INDEX ix_gs_room (room_id),
  INDEX ix_gs_user (user_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;
