SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ------------------------------------------------------------
-- 1) match_participant에 탈락 상태 플래그 추가
-- ------------------------------------------------------------
ALTER TABLE match_participant
  ADD COLUMN eliminated TINYINT(1) NOT NULL DEFAULT 0
  AFTER player_rank;

CREATE INDEX ix_mp_active ON match_participant (room_id, eliminated);

-- ------------------------------------------------------------
-- 2) 실시간 타임라인 보존용 match_event 테이블 생성
-- ------------------------------------------------------------
CREATE TABLE IF NOT EXISTS match_event (
  id          BIGINT AUTO_INCREMENT PRIMARY KEY,
  room_id     BIGINT NOT NULL,
  event_type  VARCHAR(40) NOT NULL,
  payload_json JSON NULL,
  created_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_me_room_time (room_id, created_at),
  INDEX ix_me_room_type (room_id, event_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;

