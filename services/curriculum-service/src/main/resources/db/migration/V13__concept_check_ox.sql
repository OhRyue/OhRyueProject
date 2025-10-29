-- 미니체크 OX 전환을 위한 컬럼 추가
ALTER TABLE concept_check
  ADD COLUMN is_ox TINYINT(1) NOT NULL DEFAULT 1,
  ADD COLUMN ox_answer TINYINT(1) NULL;  -- 1=O(true), 0=X(false)

-- answer_idx == 0 이면 O(1), 아니면 X(0)
UPDATE concept_check
   SET ox_answer = CASE WHEN answer_idx = 0 THEN 1 ELSE 0 END
 WHERE ox_answer IS NULL;
