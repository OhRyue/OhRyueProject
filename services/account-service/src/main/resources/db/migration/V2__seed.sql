SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ---------------------------------------------------------------------
-- 1) USERS (accounts)
--  비밀번호 인코딩 정책:
--   - 기본: {noop}P@ssw0rd!  (DelegatingPasswordEncoder 사용 시 동작)
--   - Bcrypt 사용시 예시(교체용): {bcrypt}$2a$10$CwTycUXWue0Thq9StjUM0uJ8u9cQ1qA1tq7Yz3mF8n7iN6r0U0G2a
--     ↑ 위 문자열은 예시이며 실제 프로젝트에서 회원 API로 생성하거나 안전한 해시로 교체 필요
-- ---------------------------------------------------------------------
INSERT INTO user_account (id, email, password_hash, status, created_at, last_login_at)
VALUES
  ('ohryue', 'ohryue@example.com', '{noop}P@ssw0rd!', 'ACTIVE', NOW(), NULL),
  ('user2',  'user2@example.com',  '{noop}P@ssw0rd!', 'ACTIVE', NOW(), NULL),
  ('guest',  'guest@example.com',  '{noop}guest',     'ACTIVE', NOW(), NULL),
  ('admin',  'admin@example.com',  '{noop}Admin#2025','ACTIVE', NOW(), NULL)
ON DUPLICATE KEY UPDATE
  email = VALUES(email),
  password_hash = VALUES(password_hash),
  status = VALUES(status);

-- ---------------------------------------------------------------------
-- 2) PROFILES (display identity)
--  timezone: Asia/Seoul, lang: ko-KR 기본 가정
--  skin_id: 현재 착용 중인 스킨 ID
--  구매한 스킨은 user_inventory 테이블에서 관리
-- ---------------------------------------------------------------------
INSERT INTO user_profile (user_id, nickname, skin_id, timezone, lang)
VALUES
  ('ohryue', '오류에',  1, 'Asia/Seoul', 'ko-KR'),
  ('user2',  '코딩러2', 2, 'Asia/Seoul', 'ko-KR'),
  ('guest',  '게스트',  1, 'Asia/Seoul', 'ko-KR'),
  ('admin',  '관리자',  3, 'Asia/Seoul', 'ko-KR')
ON DUPLICATE KEY UPDATE
  nickname = VALUES(nickname),
  skin_id = VALUES(skin_id),
  timezone = VALUES(timezone),
  lang = VALUES(lang);

-- ---------------------------------------------------------------------
-- 3) SETTINGS (UI / notifications)
--  ui_prefs_json 예시:
--    theme: dark|light
--    sound: 정답/오답 효과음 사용 여부
--  notif_prefs_json 예시:
--    dailyReminder: 매일 정오 알림
--    weeklyReport: 주간 리포트 메일
-- ---------------------------------------------------------------------
INSERT INTO user_settings (user_id, ui_prefs_json, notif_prefs_json)
VALUES
  (
    'ohryue',
    JSON_OBJECT(
      'theme','dark',
      'sound', JSON_OBJECT('correct', TRUE, 'wrong', TRUE)
    ),
    JSON_OBJECT(
      'dailyReminder', JSON_OBJECT('enabled', TRUE,  'hhmm','12:00'),
      'weeklyReport',  JSON_OBJECT('enabled', TRUE,  'email','ohryue@example.com')
    )
  ),
  (
    'user2',
    JSON_OBJECT(
      'theme','light',
      'sound', JSON_OBJECT('correct', TRUE, 'wrong', FALSE)
    ),
    JSON_OBJECT(
      'dailyReminder', JSON_OBJECT('enabled', FALSE, 'hhmm','12:00'),
      'weeklyReport',  JSON_OBJECT('enabled', TRUE,  'email','user2@example.com')
    )
  ),
  (
    'guest',
    JSON_OBJECT(
      'theme','light',
      'sound', JSON_OBJECT('correct', FALSE, 'wrong', FALSE)
    ),
    JSON_OBJECT(
      'dailyReminder', JSON_OBJECT('enabled', FALSE, 'hhmm','12:00'),
      'weeklyReport',  JSON_OBJECT('enabled', FALSE, 'email','guest@example.com')
    )
  ),
  (
    'admin',
    JSON_OBJECT(
      'theme','dark',
      'sound', JSON_OBJECT('correct', TRUE, 'wrong', TRUE)
    ),
    JSON_OBJECT(
      'dailyReminder', JSON_OBJECT('enabled', TRUE,  'hhmm','12:00'),
      'weeklyReport',  JSON_OBJECT('enabled', TRUE,  'email','admin@example.com')
    )
  )
ON DUPLICATE KEY UPDATE
  ui_prefs_json = VALUES(ui_prefs_json),
  notif_prefs_json = VALUES(notif_prefs_json);

-- ---------------------------------------------------------------------
-- 4) GOAL CERT (홈 D-Day 연동)
--  가정: cert-service에 cert_id=1 (정보처리기사), round_id는 아직 미정 → NULL
--  dday_cached는 스케줄 배치 또는 API 호출 시 갱신
--  사용자당 1개 Unique(uq_goal_user)
-- ---------------------------------------------------------------------
INSERT INTO user_goal_cert (user_id, cert_id, target_exam_mode, target_round_id, dday_cached, created_at)
VALUES
  ('ohryue', 1, 'PRACTICAL', NULL, NULL, NOW()),
  ('user2',  1, 'WRITTEN',   NULL, NULL, NOW()),
  ('admin',  1, 'PRACTICAL', NULL, NULL, NOW())
ON DUPLICATE KEY UPDATE
  cert_id = VALUES(cert_id),
  target_exam_mode = VALUES(target_exam_mode),
  target_round_id = VALUES(target_round_id),
  dday_cached = VALUES(dday_cached);

-- 게스트는 목표 미지정(원하면 아래 주석 해제)
-- INSERT INTO user_goal_cert (user_id, cert_id, target_exam_mode, target_round_id, dday_cached, created_at)
-- VALUES ('guest', 1, 'WRITTEN', NULL, NULL, NOW())
-- ON DUPLICATE KEY UPDATE cert_id=VALUES(cert_id);

SET FOREIGN_KEY_CHECKS = 1;
