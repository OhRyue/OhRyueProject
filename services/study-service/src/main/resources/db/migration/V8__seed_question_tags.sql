-- 예시 태그 시드
INSERT INTO question_tag (question_id, tag) VALUES
  (6, 'DB-정규화'),
  (7, 'RDBMS'),
  (8, '네트워크-OSI'),
  (9, '운영체제-프로세스'),
  (15, '자료구조-그래프')
ON DUPLICATE KEY UPDATE tag = VALUES(tag);
