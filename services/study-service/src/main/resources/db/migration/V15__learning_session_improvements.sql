-- LearningSession 개선: 진정한 완료 플래그 추가 및 CONCEPT 단계 제거

-- 1. LearningSession에 truly_completed 컬럼 추가
ALTER TABLE learning_session
  ADD COLUMN truly_completed TINYINT(1) NULL DEFAULT NULL 
  COMMENT '진정한 완료 여부 (MCQ 완료) - NULL: 미완료, 1: 완료';

-- 2. 기존 데이터 중 MCQ 단계가 COMPLETE인 경우 truly_completed=true로 설정
-- (LearningStep의 step_code='MCQ'이고 status='COMPLETE'인 경우)
UPDATE learning_session ls
INNER JOIN learning_step ls2 ON ls.id = ls2.learning_session_id
SET ls.truly_completed = 1
WHERE ls2.step_code = 'MCQ' AND ls2.status = 'COMPLETE';

