SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_cert;

-- =========================================
-- 실기용 TOPIC 트리 (정보처리기사 PRACTICAL)
--  - Major P + Sub(P.1~P.5) + Micro(각 2개씩)
-- =========================================

/* -----------------------------------------
   Major (P, 실기 종합)
   ----------------------------------------- */
INSERT INTO topic (
  id, cert_id, parent_id,
  code,  title,           emoji, exam_mode, order_no
) VALUES
  (30001, 1, NULL, 'P',   '정보처리 실무', '🧪', 'PRACTICAL', 1)
ON DUPLICATE KEY UPDATE
  title      = VALUES(title),
  emoji      = VALUES(emoji),
  exam_mode  = VALUES(exam_mode),
  order_no   = VALUES(order_no);

/* -----------------------------------------
   Sub (P.1 ~ P.5)
   ----------------------------------------- */
INSERT INTO topic (
  id, cert_id, parent_id,
  code,    title,                         emoji, exam_mode, order_no
) VALUES
  (31001, 1, 30001, 'P.1', '요구사항 분석/모델링',    '📝', 'PRACTICAL', 1),
  (31002, 1, 30001, 'P.2', 'DB 설계/정규화',          '🗄️', 'PRACTICAL', 2),
  (31003, 1, 30001, 'P.3', 'SQL 작성/튜닝',           '💾', 'PRACTICAL', 3),
  (31004, 1, 30001, 'P.4', '트랜잭션/동시성/무결성',  '🔐', 'PRACTICAL', 4),
  (31005, 1, 30001, 'P.5', '운영/백업/장애대응',      '🛟', 'PRACTICAL', 5)
ON DUPLICATE KEY UPDATE
  title      = VALUES(title),
  emoji      = VALUES(emoji),
  exam_mode  = VALUES(exam_mode),
  order_no   = VALUES(order_no);

/* -----------------------------------------
   Micro under P.1 (요구사항 분석/모델링)
   ----------------------------------------- */
INSERT INTO topic (
  id, cert_id, parent_id,
  code,      title,                   emoji, exam_mode, order_no
) VALUES
  (31101, 1, 31001, 'P.1.1', '업무 시나리오 해석',      '📋', 'PRACTICAL', 1),
  (31102, 1, 31001, 'P.1.2', '업무 및 데이터 요구 도출', '📊', 'PRACTICAL', 2)
ON DUPLICATE KEY UPDATE
  title      = VALUES(title),
  emoji      = VALUES(emoji),
  exam_mode  = VALUES(exam_mode),
  order_no   = VALUES(order_no);

/* -----------------------------------------
   Micro under P.2 (DB 설계/정규화)
   ----------------------------------------- */
INSERT INTO topic (
  id, cert_id, parent_id,
  code,      title,                        emoji, exam_mode, order_no
) VALUES
  (31201, 1, 31002, 'P.2.1', '개념/논리/물리 모델링',          '🧬', 'PRACTICAL', 1),
  (31202, 1, 31002, 'P.2.2', '정규화 및 반정규화 적용',        '🧱', 'PRACTICAL', 2)
ON DUPLICATE KEY UPDATE
  title      = VALUES(title),
  emoji      = VALUES(emoji),
  exam_mode  = VALUES(exam_mode),
  order_no   = VALUES(order_no);

/* -----------------------------------------
   Micro under P.3 (SQL 작성/튜닝)
   ----------------------------------------- */
INSERT INTO topic (
  id, cert_id, parent_id,
  code,      title,                        emoji, exam_mode, order_no
) VALUES
  (31301, 1, 31003, 'P.3.1', 'SELECT/집계/조인 쿼리 작성',     '🔍', 'PRACTICAL', 1),
  (31302, 1, 31003, 'P.3.2', '인덱스 설계 및 쿼리 튜닝',       '⚙️', 'PRACTICAL', 2)
ON DUPLICATE KEY UPDATE
  title      = VALUES(title),
  emoji      = VALUES(emoji),
  exam_mode  = VALUES(exam_mode),
  order_no   = VALUES(order_no);

/* -----------------------------------------
   Micro under P.4 (트랜잭션/동시성/무결성)
   ----------------------------------------- */
INSERT INTO topic (
  id, cert_id, parent_id,
  code,      title,                        emoji, exam_mode, order_no
) VALUES
  (31401, 1, 31004, 'P.4.1', '트랜잭션 특성 및 격리수준',      '🧱', 'PRACTICAL', 1),
  (31402, 1, 31004, 'P.4.2', '동시성 제어 및 락 전략 설계',    '🚦', 'PRACTICAL', 2)
ON DUPLICATE KEY UPDATE
  title      = VALUES(title),
  emoji      = VALUES(emoji),
  exam_mode  = VALUES(exam_mode),
  order_no   = VALUES(order_no);

/* -----------------------------------------
   Micro under P.5 (운영/백업/장애대응)
   ----------------------------------------- */
INSERT INTO topic (
  id, cert_id, parent_id,
  code,      title,                        emoji, exam_mode, order_no
) VALUES
  (31501, 1, 31005, 'P.5.1', '백업 및 복구 전략 수립',        '💽', 'PRACTICAL', 1),
  (31502, 1, 31005, 'P.5.2', '장애 분석 및 개선안 도출',       '🚑', 'PRACTICAL', 2)
ON DUPLICATE KEY UPDATE
  title      = VALUES(title),
  emoji      = VALUES(emoji),
  exam_mode  = VALUES(exam_mode),
  order_no   = VALUES(order_no);

SET FOREIGN_KEY_CHECKS = 1;
