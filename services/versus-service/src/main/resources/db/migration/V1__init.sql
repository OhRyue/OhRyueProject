SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- =========================================
-- V1__init_versus_schema.sql
-- - match_room / match_participant / match_question / match_answer
-- - tournament_bracket / goldenbell_rule / goldenbell_state / match_event
-- =========================================

CREATE TABLE IF NOT EXISTS match_room (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  mode             ENUM('DUEL','TOURNAMENT','GOLDENBELL') NOT NULL,
  status           ENUM('WAIT','ONGOING','DONE') NOT NULL DEFAULT 'WAIT',
  scope_json       JSON NULL,
  result_reported  TINYINT(1) NOT NULL DEFAULT 0 COMMENT 'Progress XP 결과 연동 여부',
  scheduled_at     TIMESTAMP NULL COMMENT '예약 시작 시간 (GOLDENBELL 모드용)',
  is_bot_match     BOOLEAN NOT NULL DEFAULT FALSE COMMENT '봇과의 연습 매치 여부',
  created_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX idx_scheduled_at (scheduled_at, status)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS match_participant (
  id                 BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id            BIGINT NOT NULL,
  user_id            VARCHAR(100) NOT NULL,
  joined_at          TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  final_score        INT NULL,
  player_rank        INT NULL,
  eliminated         TINYINT(1) NOT NULL DEFAULT 0 COMMENT '탈락 여부 (1=탈락, 0=생존)',
  last_heartbeat_at  TIMESTAMP NULL COMMENT '마지막 하트비트 시간 (연결 상태 추적용)',
  INDEX ix_mp_room   (room_id),
  INDEX ix_mp_user   (user_id),
  INDEX ix_mp_active (room_id, eliminated)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 라운드/파이널/부활을 명확히 관리하기 위해 round_no/phase 사용
CREATE TABLE IF NOT EXISTS match_question (
  id             BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id        BIGINT NOT NULL,
  round_no       INT NOT NULL,                                     -- 1,2,3,4 또는 파이널 전용 번호(예: 99)
  phase          ENUM('MAIN','FINAL','REVIVAL') NOT NULL DEFAULT 'MAIN',
  order_no       INT NOT NULL,
  question_id    BIGINT NOT NULL,
  time_limit_sec INT NOT NULL DEFAULT 10,
  UNIQUE KEY uq_mq_room_round_order (room_id, round_no, order_no), -- 라운드 내 순서 고정
  INDEX ix_mq_room  (room_id),
  INDEX ix_mq_round (room_id, round_no),
  INDEX ix_mq_phase (room_id, phase),
  INDEX ix_mq_q     (question_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 답안에도 round_no/phase 추가 → 최종전 합산/부활전 집계 용이
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
  user_answer  TEXT NULL COMMENT '사용자가 제출한 답안 내용 (OX/MCQ: label, SHORT/LONG: 텍스트)',
  UNIQUE KEY uq_ma_once   (room_id, question_id, user_id),
  INDEX ix_ma_room        (room_id),
  INDEX ix_ma_user        (user_id),
  INDEX ix_ma_round       (room_id, round_no),
  INDEX ix_ma_phase       (room_id, phase)
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

-- 실시간 타임라인 보존용 이벤트 테이블
CREATE TABLE IF NOT EXISTS match_event (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id      BIGINT NOT NULL,
  event_type   VARCHAR(40) NOT NULL,
  payload_json JSON NULL,
  created_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_me_room_time (room_id, created_at),
  INDEX ix_me_room_type (room_id, event_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;
