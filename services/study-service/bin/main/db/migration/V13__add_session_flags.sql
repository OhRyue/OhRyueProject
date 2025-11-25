-- StudySession에 completed, passed, xp_granted 필드 추가
ALTER TABLE study_session
  ADD COLUMN completed TINYINT(1) NOT NULL DEFAULT 0 COMMENT '모든 문제를 풀었는지',
  ADD COLUMN passed TINYINT(1) NOT NULL DEFAULT 0 COMMENT '모든 문제를 맞췄는지',
  ADD COLUMN xp_granted TINYINT(1) NOT NULL DEFAULT 0 COMMENT 'XP가 이미 반영되었는지';

-- 기존 세션 중 CLOSED이고 score_pct가 100인 경우 passed=true로 설정
UPDATE study_session
SET passed = 1
WHERE status = 'CLOSED' AND score_pct >= 100.0;

-- 기존 세션 중 CLOSED인 경우 completed=true로 설정
UPDATE study_session
SET completed = 1
WHERE status = 'CLOSED';



