-- GOLDENBELL 예약 시스템을 위한 scheduled_at 컬럼 추가
ALTER TABLE match_room 
ADD COLUMN scheduled_at TIMESTAMP NULL COMMENT '예약 시작 시간 (GOLDENBELL 모드용)',
ADD INDEX idx_scheduled_at (scheduled_at, status);




