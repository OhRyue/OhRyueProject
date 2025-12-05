-- match_participant 테이블에 last_heartbeat_at 컬럼 추가
-- 대기 중인 방에서 사용자의 연결 상태를 추적하기 위함
ALTER TABLE match_participant
ADD COLUMN last_heartbeat_at TIMESTAMP NULL COMMENT '마지막 하트비트 시간 (연결 상태 추적용)';

-- 기존 참가자들의 last_heartbeat_at을 joined_at으로 초기화
UPDATE match_participant
SET last_heartbeat_at = joined_at
WHERE last_heartbeat_at IS NULL;

