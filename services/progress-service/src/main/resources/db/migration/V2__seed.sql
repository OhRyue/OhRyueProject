SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ---------------------------------
-- 0) 공통 변수 (사용자/날짜/주차)
-- ---------------------------------
SET @u1 := 'ohryue';
SET @u2 := 'user2';
SET @u3 := 'guest';
SET @u4 := 'admin';

-- 오늘/어제 (Asia/Seoul 기준 달력에 맞춰 고정값 사용)
SET @today := DATE('2025-11-07');
SET @yday  := DATE('2025-11-06');

-- 주간 지표용 ISO 주차(문자열; 주차 계산은 운영 배치에서 표준화 권장)
SET @week_iso := '2025-W45';

-- ---------------------------------
-- 1) XP/레벨/연속
-- ---------------------------------
INSERT INTO user_xp_wallet (user_id, xp_total, level, last_levelup_at)
SELECT @u1, 12500, 25, @yday FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_xp_wallet WHERE user_id=@u1);
INSERT INTO user_xp_wallet (user_id, xp_total, level, last_levelup_at)
SELECT @u2,  8300, 19, @yday FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_xp_wallet WHERE user_id=@u2);
INSERT INTO user_xp_wallet (user_id, xp_total, level, last_levelup_at)
SELECT @u3,   200,  2,  NULL FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_xp_wallet WHERE user_id=@u3);
INSERT INTO user_xp_wallet (user_id, xp_total, level, last_levelup_at)
SELECT @u4, 15200, 26, @yday FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_xp_wallet WHERE user_id=@u4);

-- XP 레저 (최근 활동 샘플)
INSERT INTO user_xp_ledger (user_id, delta, reason, ref_id)
SELECT @u1, 150, 'MICRO', 'ss:mic-2025-11-07-01' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_xp_ledger WHERE user_id=@u1 AND ref_id='ss:mic-2025-11-07-01');
INSERT INTO user_xp_ledger (user_id, delta, reason, ref_id)
SELECT @u1, 100, 'REVIEW', 'rv:2025-11-06-01' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_xp_ledger WHERE user_id=@u1 AND ref_id='rv:2025-11-06-01');
INSERT INTO user_xp_ledger (user_id, delta, reason, ref_id)
SELECT @u2, 220, 'ASSIST', 'as:2025-11-07-01' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_xp_ledger WHERE user_id=@u2 AND ref_id='as:2025-11-07-01');
INSERT INTO user_xp_ledger (user_id, delta, reason, ref_id)
SELECT @u4, 300, 'BATTLE', 'mt:2025-11-05-01' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_xp_ledger WHERE user_id=@u4 AND ref_id='mt:2025-11-05-01');

-- 연속학습
INSERT INTO user_streak (user_id, current_days, best_days, last_active_date)
SELECT @u1, 45, 45, @today FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_streak WHERE user_id=@u1);
INSERT INTO user_streak (user_id, current_days, best_days, last_active_date)
SELECT @u2, 12, 20, @today FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_streak WHERE user_id=@u2);
INSERT INTO user_streak (user_id, current_days, best_days, last_active_date)
SELECT @u3,  1,  3, @yday FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_streak WHERE user_id=@u3);
INSERT INTO user_streak (user_id, current_days, best_days, last_active_date)
SELECT @u4, 30, 40, @today FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_streak WHERE user_id=@u4);

-- ---------------------------------
-- 2) 배지 카탈로그/보유
-- ---------------------------------
-- 카탈로그
INSERT INTO badge_catalog (code, name, rarity, rule_json)
SELECT 'STREAK_7',    '7일 연속 학습', 'common', JSON_OBJECT('type','streak','days',7) FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM badge_catalog WHERE code='STREAK_7');
INSERT INTO badge_catalog (code, name, rarity, rule_json)
SELECT 'FIRST_100',   '100문제 달성', 'rare',   JSON_OBJECT('type','solve_count','count',100) FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM badge_catalog WHERE code='FIRST_100');
INSERT INTO badge_catalog (code, name, rarity, rule_json)
SELECT 'FIRST_WIN',   '배틀 첫 승',   'common', JSON_OBJECT('type','battle_win','count',1) FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM badge_catalog WHERE code='FIRST_WIN');
INSERT INTO badge_catalog (code, name, rarity, rule_json)
SELECT 'PERFECT_1',   '첫 만점',     'common', JSON_OBJECT('type','perfect','count',1) FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM badge_catalog WHERE code='PERFECT_1');

-- 배지ID 변수
SET @b_st7  := (SELECT id FROM badge_catalog WHERE code='STREAK_7'  LIMIT 1);
SET @b_100  := (SELECT id FROM badge_catalog WHERE code='FIRST_100' LIMIT 1);
SET @b_win1 := (SELECT id FROM badge_catalog WHERE code='FIRST_WIN' LIMIT 1);
SET @b_pf1  := (SELECT id FROM badge_catalog WHERE code='PERFECT_1' LIMIT 1);

-- 보유
INSERT INTO user_badge (user_id, badge_id)
SELECT @u1, @b_st7 FROM DUAL
WHERE @b_st7 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM user_badge WHERE user_id=@u1 AND badge_id=@b_st7);
INSERT INTO user_badge (user_id, badge_id)
SELECT @u1, @b_100 FROM DUAL
WHERE @b_100 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM user_badge WHERE user_id=@u1 AND badge_id=@b_100);
INSERT INTO user_badge (user_id, badge_id)
SELECT @u2, @b_win1 FROM DUAL
WHERE @b_win1 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM user_badge WHERE user_id=@u2 AND badge_id=@b_win1);
INSERT INTO user_badge (user_id, badge_id)
SELECT @u4, @b_pf1 FROM DUAL
WHERE @b_pf1 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM user_badge WHERE user_id=@u4 AND badge_id=@b_pf1);

-- ---------------------------------
-- 3) 랭킹/스냅샷/친구
-- ---------------------------------
-- 현재 점수(경험치 기반 가중 예시)
INSERT INTO user_rank_score (user_id, score, last_updated_at)
SELECT @u1, 12500, NOW() FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_rank_score WHERE user_id=@u1);
INSERT INTO user_rank_score (user_id, score, last_updated_at)
SELECT @u2,  8300, NOW() FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_rank_score WHERE user_id=@u2);
INSERT INTO user_rank_score (user_id, score, last_updated_at)
SELECT @u3,   200, NOW() FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_rank_score WHERE user_id=@u3);
INSERT INTO user_rank_score (user_id, score, last_updated_at)
SELECT @u4, 15200, NOW() FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_rank_score WHERE user_id=@u4);

-- 오늘자 글로벌 Top5 스냅샷(JSON) — UI 홈 카드 검증용
INSERT INTO leaderboard_snapshot (snapshot_date, scope, payload_json)
SELECT @today, 'GLOBAL',
       JSON_ARRAY(
         JSON_OBJECT('user','admin','level',26,'xp',15200,'streak',30),
         JSON_OBJECT('user','ohryue','level',25,'xp',12500,'streak',45),
         JSON_OBJECT('user','user2','level',19,'xp',8300,'streak',12),
         JSON_OBJECT('user','guest','level',2,'xp',200,'streak',1)
       )
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM leaderboard_snapshot
   WHERE snapshot_date=@today AND scope='GLOBAL'
);

-- 친구(양방향 1쌍 + 관리자와 게스트는 친구 아님)
INSERT INTO user_friend (user_id, friend_id, status)
SELECT @u1, @u2, 'ACCEPTED' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_friend WHERE user_id=@u1 AND friend_id=@u2);
INSERT INTO user_friend (user_id, friend_id, status)
SELECT @u2, @u1, 'ACCEPTED' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_friend WHERE user_id=@u2 AND friend_id=@u1);

-- ---------------------------------
-- 4) 리포트/목표/약점 (홈/리포트 화면용)
-- ---------------------------------
-- 데일리 목표
INSERT INTO assist_goal_daily (user_id, date, target_count, progress_count)
SELECT @u1, @today, 30, 18 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM assist_goal_daily WHERE user_id=@u1 AND date=@today);
INSERT INTO assist_goal_daily (user_id, date, target_count, progress_count)
SELECT @u2, @today, 20, 20 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM assist_goal_daily WHERE user_id=@u2 AND date=@today);

-- 주간 누적(예시)
INSERT INTO assist_weekly_stats (user_id, week_iso, solved_count, avg_accuracy)
SELECT @u1, @week_iso, 210, 72.35 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM assist_weekly_stats WHERE user_id=@u1 AND week_iso=@week_iso);
INSERT INTO assist_weekly_stats (user_id, week_iso, solved_count, avg_accuracy)
SELECT @u2, @week_iso, 120, 68.10 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM assist_weekly_stats WHERE user_id=@u2 AND week_iso=@week_iso);

-- 데일리 리포트(오늘/어제)
INSERT INTO report_daily (user_id, date, solved_count, time_spent_sec, accuracy, xp_gained)
SELECT @u1, @yday,  95,  3600, 65.50, 350 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM report_daily WHERE user_id=@u1 AND date=@yday);
INSERT INTO report_daily (user_id, date, solved_count, time_spent_sec, accuracy, xp_gained)
SELECT @u1, @today, 120, 5400, 71.20, 420 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM report_daily WHERE user_id=@u1 AND date=@today);

INSERT INTO report_daily (user_id, date, solved_count, time_spent_sec, accuracy, xp_gained)
SELECT @u2, @today,  80, 3000, 73.75, 260 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM report_daily WHERE user_id=@u2 AND date=@today);

-- 주간 리포트(이번 주)
INSERT INTO report_weekly (user_id, week_iso, solved_count, time_spent_sec, accuracy, xp_gained)
SELECT @u1, @week_iso,  210, 10800, 72.35, 970 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM report_weekly WHERE user_id=@u1 AND week_iso=@week_iso);
INSERT INTO report_weekly (user_id, week_iso, solved_count, time_spent_sec, accuracy, xp_gained)
SELECT @u2, @week_iso,  120,  7200, 68.10, 610 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM report_weekly WHERE user_id=@u2 AND week_iso=@week_iso);

-- 태그/스킬 리포트(필기 중심 예시)
INSERT INTO report_tag_skill (user_id, tag, exam_mode, correct, total, accuracy)
SELECT @u1, '인터페이스요구', 'WRITTEN', 35, 50, 70.00 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM report_tag_skill WHERE user_id=@u1 AND tag='인터페이스요구' AND exam_mode='WRITTEN');
INSERT INTO report_tag_skill (user_id, tag, exam_mode, correct, total, accuracy)
SELECT @u1, 'OOP',            'WRITTEN', 28, 40, 70.00 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM report_tag_skill WHERE user_id=@u1 AND tag='OOP' AND exam_mode='WRITTEN');
INSERT INTO report_tag_skill (user_id, tag, exam_mode, correct, total, accuracy)
SELECT @u2, '공통모듈',       'WRITTEN', 18, 30, 60.00 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM report_tag_skill WHERE user_id=@u2 AND tag='공통모듈' AND exam_mode='WRITTEN');

-- ---------------------------------
-- 5) 포인트/상점/인벤토리/로드아웃
-- ---------------------------------
-- 포인트 지갑
INSERT INTO user_point_wallet (user_id, point_total)
SELECT @u1,  950 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_point_wallet WHERE user_id=@u1);
INSERT INTO user_point_wallet (user_id, point_total)
SELECT @u2,  420 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_point_wallet WHERE user_id=@u2);
INSERT INTO user_point_wallet (user_id, point_total)
SELECT @u3,   50 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_point_wallet WHERE user_id=@u3);
INSERT INTO user_point_wallet (user_id, point_total)
SELECT @u4, 1350 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_point_wallet WHERE user_id=@u4);

-- 포인트 레저
INSERT INTO user_point_ledger (user_id, delta, reason, ref_id)
SELECT @u1,  +300, 'REWARD',  'rv:bonus-2025-11-06' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_point_ledger WHERE user_id=@u1 AND ref_id='rv:bonus-2025-11-06');
INSERT INTO user_point_ledger (user_id, delta, reason, ref_id)
SELECT @u1,  -200, 'PURCHASE','shop:hat:001' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_point_ledger WHERE user_id=@u1 AND ref_id='shop:hat:001');
INSERT INTO user_point_ledger (user_id, delta, reason, ref_id)
SELECT @u2,  +150, 'REWARD',  'as:goal-2025-11-07' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_point_ledger WHERE user_id=@u2 AND ref_id='as:goal-2025-11-07');

-- 상점 아이템 (카테고리별 1~2개)
INSERT INTO store_item (category, name, image_url, price, rarity, limit_per_user, is_active)
SELECT 'HAT','픽셀 비니','/img/shop/hat_beanie.png',200,'common',NULL,1 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM store_item WHERE category='HAT' AND name='픽셀 비니');
INSERT INTO store_item (category, name, image_url, price, rarity, limit_per_user, is_active)
SELECT 'CLOTHES','레트로 점퍼','/img/shop/cloth_retro.png',350,'rare',NULL,1 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM store_item WHERE category='CLOTHES' AND name='레트로 점퍼');
INSERT INTO store_item (category, name, image_url, price, rarity, limit_per_user, is_active)
SELECT 'ACC','네온 안경','/img/shop/acc_neon.png',150, 'common',NULL,1 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM store_item WHERE category='ACC' AND name='네온 안경');
INSERT INTO store_item (category, name, image_url, price, rarity, limit_per_user, is_active)
SELECT 'BG','보라 그라디언트','/img/shop/bg_violet.png',250,'common',NULL,1 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM store_item WHERE category='BG' AND name='보라 그라디언트');
INSERT INTO store_item (category, name, image_url, price, rarity, limit_per_user, is_active)
SELECT 'SPECIAL','왕관 이펙트','/img/shop/sp_crown.png',800,'epic',1,1 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM store_item WHERE category='SPECIAL' AND name='왕관 이펙트');

-- 아이템ID 변수
SET @i_hat   := (SELECT id FROM store_item WHERE name='픽셀 비니' LIMIT 1);
SET @i_cloth := (SELECT id FROM store_item WHERE name='레트로 점퍼' LIMIT 1);
SET @i_acc   := (SELECT id FROM store_item WHERE name='네온 안경' LIMIT 1);
SET @i_bg    := (SELECT id FROM store_item WHERE name='보라 그라디언트' LIMIT 1);
SET @i_sp    := (SELECT id FROM store_item WHERE name='왕관 이펙트' LIMIT 1);

-- 인벤토리 보유
INSERT INTO user_inventory (user_id, item_id)
SELECT @u1, @i_hat FROM DUAL
WHERE @i_hat IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM user_inventory WHERE user_id=@u1 AND item_id=@i_hat);
INSERT INTO user_inventory (user_id, item_id)
SELECT @u1, @i_bg FROM DUAL
WHERE @i_bg IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM user_inventory WHERE user_id=@u1 AND item_id=@i_bg);
INSERT INTO user_inventory (user_id, item_id)
SELECT @u2, @i_acc FROM DUAL
WHERE @i_acc IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM user_inventory WHERE user_id=@u2 AND item_id=@i_acc);
INSERT INTO user_inventory (user_id, item_id)
SELECT @u4, @i_sp FROM DUAL
WHERE @i_sp IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM user_inventory WHERE user_id=@u4 AND item_id=@i_sp);

-- 로드아웃(착용)
INSERT INTO user_loadout (user_id, hat_id, clothes_id, acc_id, bg_id, special_id)
SELECT @u1, @i_hat, NULL, NULL, @i_bg, NULL FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_loadout WHERE user_id=@u1);
INSERT INTO user_loadout (user_id, hat_id, clothes_id, acc_id, bg_id, special_id)
SELECT @u2, NULL, NULL, @i_acc, NULL, NULL FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_loadout WHERE user_id=@u2);
INSERT INTO user_loadout (user_id, hat_id, clothes_id, acc_id, bg_id, special_id)
SELECT @u4, NULL, NULL, NULL, NULL, @i_sp FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_loadout WHERE user_id=@u4);

SET FOREIGN_KEY_CHECKS = 1;
