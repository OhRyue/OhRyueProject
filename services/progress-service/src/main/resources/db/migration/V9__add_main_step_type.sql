SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- progress_activity 테이블에 main_step_type 컬럼 추가
ALTER TABLE progress_activity
ADD COLUMN main_step_type ENUM('MINI','MCQ','SHORT') NULL COMMENT 'MICRO일 때만 사용 (MINI, MCQ, SHORT)' AFTER main_type;

SET FOREIGN_KEY_CHECKS = 1;



