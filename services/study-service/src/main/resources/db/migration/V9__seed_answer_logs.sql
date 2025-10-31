-- 예시 풀이 로그 시드
INSERT INTO answer_log (user_id, question_id, correct, answered_at, user_answer, mode, type) VALUES
  (1, 6, FALSE, NOW() - INTERVAL 1 DAY,  'B', 'WRITTEN', 'MCQ'),
  (1, 7, TRUE,  NOW() - INTERVAL 3 DAY,  'C', 'WRITTEN', 'MCQ'),
  (1, 8, FALSE, NOW() - INTERVAL 2 DAY,  'A', 'WRITTEN', 'MCQ'),
  (1, 9, TRUE,  NOW() - INTERVAL 10 DAY, '정의', 'PRACTICAL', 'SHORT'),
  (1, 15, FALSE, NOW() - INTERVAL 5 DAY,  'D', 'WRITTEN', 'MCQ')
ON DUPLICATE KEY UPDATE correct = VALUES(correct);
