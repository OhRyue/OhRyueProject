SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- =========================================
-- V2__seed_versus_sample_data.sql
-- - DUEL / TOURNAMENT / GOLDENBELL 테스트 룸 및 참가자/질문/룰 시드
-- =========================================

-- 0) 방 ID 고정 (중복 실행에 안전하게 ON DUPLICATE 사용)
SET @r_duel := 1101;
SET @r_tour := 1201;
SET @r_gb   := 1301;

-- 1) match_room
INSERT INTO match_room (id, mode, status, scope_json, created_at)
VALUES
(@r_duel, 'DUEL',       'WAIT', JSON_OBJECT('examMode','WRITTEN','topicScope','ALL','difficulty','NORMAL'), NOW()),
(@r_tour, 'TOURNAMENT', 'WAIT', JSON_OBJECT('examMode','WRITTEN','size',8,'seed','random','topicScope','ALL'), NOW()),
(@r_gb,   'GOLDENBELL', 'WAIT', JSON_OBJECT('maxPlayers',20,'examMode','WRITTEN','topicScope','ALL'), NOW())
ON DUPLICATE KEY UPDATE
  scope_json = VALUES(scope_json),
  status     = VALUES(status);

-- 2) match_participant
--    - DUEL: 2명
--    - TOURNAMENT: 4명(예시)
--    - GOLDENBELL: 4명(예시)

-- DUEL 참가
INSERT INTO match_participant (room_id, user_id, joined_at)
VALUES
(@r_duel, 'ohryue', NOW()),
(@r_duel, 'user2',  NOW());

-- TOURNAMENT 참가
INSERT INTO match_participant (room_id, user_id, joined_at)
VALUES
(@r_tour, 'ohryue', NOW()),
(@r_tour, 'user2',  NOW()),
(@r_tour, 'guest',  NOW()),
(@r_tour, 'admin',  NOW());

-- GOLDENBELL 참가(예시 4명, 최대 20명까지 가능)
INSERT INTO match_participant (room_id, user_id, joined_at)
VALUES
(@r_gb, 'ohryue', NOW()),
(@r_gb, 'user2',  NOW()),
(@r_gb, 'guest',  NOW()),
(@r_gb, 'admin',  NOW());

-- 3) match_question
--    라운드/페이즈 포함 배치
--    ※ question_id는 study-service 실제 ID로 교체 필요

-- [DUEL] 3문제, 메인 페이즈
INSERT INTO match_question (room_id, round_no, phase, order_no, question_id, time_limit_sec)
VALUES
(@r_duel, 1, 'MAIN', 1, 10001, 15),
(@r_duel, 2, 'MAIN', 1, 10002, 15),
(@r_duel, 3, 'MAIN', 1, 10003, 15);

-- [TOURNAMENT] 1라운드 4문제(예시)
INSERT INTO match_question (room_id, round_no, phase, order_no, question_id, time_limit_sec)
VALUES
(@r_tour, 1, 'MAIN', 1, 11001, 12),
(@r_tour, 1, 'MAIN', 2, 11002, 12),
(@r_tour, 1, 'MAIN', 3, 11003, 12),
(@r_tour, 1, 'MAIN', 4, 11004, 12);

-- [GOLDENBELL]
-- 기획: OX×2 -> MCQ×2 -> SHORT×1 -> LONG×1 (모두 10초)
-- + FINAL (SHORT 1 + LONG 1 → 합산)
INSERT INTO match_question (room_id, round_no, phase, order_no, question_id, time_limit_sec)
VALUES
-- R1 OX
(@r_gb, 1, 'MAIN', 1, 12001, 10),
-- R2 OX
(@r_gb, 2, 'MAIN', 1, 12002, 10),
-- R3 MCQ
(@r_gb, 3, 'MAIN', 1, 12003, 10),
-- R4 MCQ
(@r_gb, 4, 'MAIN', 1, 12004, 10),
-- R5 SHORT
(@r_gb, 5, 'MAIN', 1, 12005, 10),
-- R6 LONG
(@r_gb, 6, 'MAIN', 1, 12006, 10),
-- FINAL (SHORT 1 + LONG 1 → 합산)
(@r_gb, 99, 'FINAL', 1, 12007, 10),
(@r_gb, 99, 'FINAL', 2, 12008, 10);

-- 4) tournament_bracket (토너먼트 8강 예시: 현재 4명만 참가 → BYE 포함)
INSERT INTO tournament_bracket (room_id, round_no, pairing_json)
VALUES
(
  @r_tour,
  1,
  JSON_OBJECT(
    'round', 1,
    'pairs', JSON_ARRAY(
      JSON_OBJECT('a','ohryue','b','user2'),
      JSON_OBJECT('a','guest','b','admin'),
      JSON_OBJECT('a','BYE1','b',NULL),
      JSON_OBJECT('a','BYE2','b',NULL)
    )
  )
)
ON DUPLICATE KEY UPDATE pairing_json = VALUES(pairing_json);

-- 5) goldenbell_rule (필기 골든벨 규칙 샘플)
INSERT INTO goldenbell_rule (room_id, round_flow_json, elimination, revival_rule_json)
SELECT
  @r_gb,
  JSON_ARRAY(
    -- 필기 골든벨 기본 구성 (실제로는 examMode에 따라 동적으로 생성됨)
    JSON_OBJECT('round', 1,  'type', 'OX',    'count', 2, 'limitSec', 8,  'phase', 'MAIN'),
    JSON_OBJECT('round', 2,  'type', 'MCQ',   'count', 2, 'limitSec', 12, 'phase', 'MAIN'),
    JSON_OBJECT('round', 3,  'type', 'MCQ',   'count', 1, 'limitSec', 15, 'phase', 'REVIVAL'),
    JSON_OBJECT('round', 4,  'type', 'MCQ',   'count', 2, 'limitSec', 12, 'phase', 'FINAL')
  ),
  'IMMEDIATE',
  JSON_OBJECT(
    'enabled', TRUE,
    'triggerMode', 'AFTER_QUESTION_INDEX',
    'triggerAfterIndex', 4,                 -- OX+MCQ 총 4문제 종료 후 (라운드 1~2)
    'minAlive', 5,
    'revivalPhase', 'REVIVAL',
    'mode', 'ONE_QUESTION_FASTEST',
    'revivalQuestion', JSON_OBJECT('type','MCQ','limitSec',15),
    'slots', 1,
    'spectatorExistingAlive', TRUE,
    'candidates', 'ELIMINATED_ONLY',
    'earlyZeroAlive', JSON_OBJECT(         -- 조기 전멸 보호
      'enabled', TRUE,
      'topN', 5,
      'criteria', 'best_correct_then_fastest',
      'fillIfAllWrong', TRUE,
      'phase', 'REVIVAL'
    )
  )
WHERE NOT EXISTS (SELECT 1 FROM goldenbell_rule WHERE room_id = @r_gb);

-- 6) goldenbell_state (초기 생존 표기)
INSERT INTO goldenbell_state (room_id, user_id, alive, revived)
VALUES
(@r_gb, 'ohryue', 1, 0),
(@r_gb, 'user2',  1, 0),
(@r_gb, 'guest',  1, 0),
(@r_gb, 'admin',  1, 0)
ON DUPLICATE KEY UPDATE
  alive   = VALUES(alive),
  revived = VALUES(revived);

SET FOREIGN_KEY_CHECKS = 1;
