ALTER TABLE post_category
  ADD COLUMN sort_order TINYINT NOT NULL DEFAULT 0;

-- 정렬 순서 지정
-- 순서: 자유, 질문, 꿀팁, 스터디, 후기 (전체는 0번)
UPDATE post_category
SET sort_order = CASE code
  WHEN 'ALL'    THEN 0   -- 내부용
  WHEN 'FREE'   THEN 1   -- 자유
  WHEN 'QNA'    THEN 2   -- 질문
  WHEN 'TIP'    THEN 3   -- 꿀팁
  WHEN 'STUDY'  THEN 4   -- 스터디
  WHEN 'REVIEW' THEN 5   -- 후기
  ELSE 99
END;
