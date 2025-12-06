-- V7__add_is_bot_match_to_match_room.sql
-- 봇전 여부를 구별하기 위한 컬럼 추가

ALTER TABLE match_room
ADD COLUMN is_bot_match BOOLEAN NOT NULL DEFAULT FALSE COMMENT '봇과의 연습 매치 여부';

-- 기존 레코드는 모두 실제 사용자 매치이므로 false로 설정 (이미 기본값으로 처리됨)

