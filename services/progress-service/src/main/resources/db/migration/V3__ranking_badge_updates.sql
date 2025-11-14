SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- 1) 먼저 ENUM을 넓혀서 GLOBAL과 OVERALL이 같이 존재하도록 변경
ALTER TABLE leaderboard_snapshot
  MODIFY COLUMN scope ENUM('GLOBAL','OVERALL','WEEKLY','HALL_OF_FAME','FRIENDS')
    NOT NULL DEFAULT 'GLOBAL';

-- 2) 기존 GLOBAL 값을 OVERALL로 마이그레이션
UPDATE leaderboard_snapshot
SET scope = 'OVERALL'
WHERE scope = 'GLOBAL';

-- 3) 이제 GLOBAL을 ENUM 정의에서 제거하고 최종 형태로 재정의
ALTER TABLE leaderboard_snapshot
  MODIFY COLUMN scope ENUM('OVERALL','WEEKLY','HALL_OF_FAME','FRIENDS')
    NOT NULL DEFAULT 'OVERALL';

SET FOREIGN_KEY_CHECKS = 1;
