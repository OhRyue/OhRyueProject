SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- badge_catalog 테이블에 description, category, icon_url 컬럼 추가
-- Flyway는 이미 실행된 migration을 추적하므로, 새로 생성되는 경우에만 실행됨
ALTER TABLE badge_catalog 
  ADD COLUMN description TEXT NULL,
  ADD COLUMN category VARCHAR(50) NULL,
  ADD COLUMN icon_url VARCHAR(500) NULL;

-- user_skill_counter 테이블 생성 (필기/실기 성과 카운트용)
CREATE TABLE IF NOT EXISTS user_skill_counter (
  user_id VARCHAR(100) PRIMARY KEY,
  first_study_completed TINYINT(1) NOT NULL DEFAULT 0,
  written_review_90_cnt INT NOT NULL DEFAULT 0,
  practical_micro_100_cnt INT NOT NULL DEFAULT 0,
  accuracy_80_cnt INT NOT NULL DEFAULT 0,
  duel_streak INT NOT NULL DEFAULT 0,
  tournament_wins INT NOT NULL DEFAULT 0,
  goldenbell_wins INT NOT NULL DEFAULT 0,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX ix_skill_user (user_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 10개 배지 등록
INSERT INTO badge_catalog (code, name, description, category, rarity, icon_url, rule_json)
VALUES
('FIRST_STUDY', '첫 걸음', '첫 학습 세션을 완료한 사용자에게 지급됩니다.', 'BASIC', 'common', NULL, '{"criteria": "first_study_completed=1"}'),
('CONSISTENT_3DAYS', '꾸준한 학습자', '3일 연속 학습을 달성했습니다.', 'ROUTINE', 'common', NULL, '{"criteria": "streak_current>=3"}'),
('ACCURACY_MASTER', '정답 마스터', '정답률 80% 이상을 10회 달성했습니다.', 'SKILL', 'rare', NULL, '{"criteria": "accuracy_80_cnt>=10"}'),
('WRITTEN_EXPERT', '필기 장인', '필기 REVIEW 점수 90점 이상을 5회 달성했습니다.', 'WRITTEN', 'rare', NULL, '{"criteria": "written_review_90_cnt>=5"}'),
('PRACTICAL_PERFECT', '실기 퍼펙트', '실기 MICRO에서 100점을 3회 달성했습니다.', 'PRACTICAL', 'rare', NULL, '{"criteria": "practical_micro_100_cnt>=3"}'),
('DUEL_STREAK_3', '연승 파일럿', '1:1 배틀에서 3연승을 달성했습니다.', 'VERSUS', 'epic', NULL, '{"criteria": "duel_streak>=3"}'),
('TOURNAMENT_WINNER', '토너먼트 챔피언', '8인 토너먼트에서 우승했습니다.', 'VERSUS', 'epic', NULL, '{"criteria": "tournament_wins>=1"}'),
('GOLDENBELL_WINNER', '골든벨 우승자', '골든벨 모드에서 최종 우승을 달성했습니다.', 'VERSUS', 'epic', NULL, '{"criteria": "goldenbell_wins>=1"}'),
('CONSISTENT_7DAYS', '1주 루틴 유지', '7일 연속 학습을 유지했습니다.', 'ROUTINE', 'rare', NULL, '{"criteria": "streak_current>=7"}'),
('XP_10000', 'XP 10000 달성', '누적 XP가 10000을 달성했습니다.', 'XP', 'epic', NULL, '{"criteria": "xp_total>=10000"}')
ON DUPLICATE KEY UPDATE 
  description = VALUES(description),
  category = VALUES(category),
  icon_url = VALUES(icon_url),
  rule_json = VALUES(rule_json);

SET FOREIGN_KEY_CHECKS = 1;

