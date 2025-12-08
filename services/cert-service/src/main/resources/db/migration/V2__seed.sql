SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_cert;

-- =========================================
-- 1) CERT (자격증 마스터)
-- =========================================
INSERT INTO cert
  (id, name, level, issuer, issuer_url, description,
   written_fee, practical_fee, pass_rule_text,
   qnet_jm_cd, qnet_qualgb_cd)
VALUES
  (1, '정보처리기사', '기사', '한국산업인력공단', 'https://www.q-net.or.kr',
   '소프트웨어 설계·개발·DB·언어·시스템 운영 전반에 대한 전문성을 평가합니다.',
   NULL, NULL,
   '예시: 필기 과목당 40점 이상 & 평균 60점 이상, 실기 60점 이상을 합격 기준으로 가정(실제 기준은 큐넷 고시 참조).',
   '1320', 'T'),  -- qnet_jm_cd: 정보처리기사 종목코드, qnet_qualgb_cd: 국가기술자격
  (2, '컴퓨터활용능력 2급', '국가기술자격', '대한상공회의소', 'https://license.korcham.net',
   '스프레드시트 및 데이터 처리 기본 능력 평가(예시).',
   NULL, NULL,
   '예시: 필기/실기 60점 이상(실제 기준은 고시 참조).',
   NULL, NULL)
ON DUPLICATE KEY UPDATE
  name=VALUES(name),
  level=VALUES(level),
  issuer=VALUES(issuer),
  issuer_url=VALUES(issuer_url),
  description=VALUES(description),
  written_fee=VALUES(written_fee),
  practical_fee=VALUES(practical_fee),
  pass_rule_text=VALUES(pass_rule_text),
  qnet_jm_cd=VALUES(qnet_jm_cd),
  qnet_qualgb_cd=VALUES(qnet_qualgb_cd);

-- =========================================
-- 2) CERT_SUBJECT (과목)
--  - 정보처리기사: 필기 5과목 + 실기 5영역
--  - subject_seq로 정렬 보장
-- =========================================
INSERT INTO cert_subject
  (id, cert_id, exam_mode, name, total_questions, duration_minutes, subject_seq)
VALUES
  -- 정보처리기사 필기 (기존 유지)
  (1001, 1, 'WRITTEN',  '소프트웨어 설계',                 20, 120, 1),
  (1002, 1, 'WRITTEN',  '소프트웨어 개발',                 20, 120, 2),
  (1003, 1, 'WRITTEN',  '데이터베이스 구축',               20, 120, 3),
  (1004, 1, 'WRITTEN',  '프로그래밍 언어 활용',            20, 120, 4),
  (1005, 1, 'WRITTEN',  '정보시스템 운영',                 20, 120, 5),

  -- 정보처리기사 실기: 5개 영역으로 분리
  -- P.1 ~ P.5 토픽 트리(31001~31005)와 컨셉을 맞춘 이름
  (1101, 1, 'PRACTICAL', '요구사항 분석/모델링(실기)',       NULL, NULL, 1),
  (1102, 1, 'PRACTICAL', 'DB 설계/정규화(실기)',            NULL, NULL, 2),
  (1103, 1, 'PRACTICAL', 'SQL 작성/튜닝(실기)',             NULL, NULL, 3),
  (1104, 1, 'PRACTICAL', '트랜잭션/동시성/무결성(실기)',     NULL, NULL, 4),
  (1105, 1, 'PRACTICAL', '운영/백업/장애대응(실기)',        NULL, NULL, 5),

  -- 예비 자격증(컴활2급) - 기존 유지
  (2001, 2, 'WRITTEN',   '컴활2급 필기(예시)',              40, 60,  1),
  (2101, 2, 'PRACTICAL', '컴활2급 실기(예시)',              NULL, 90, 1)
ON DUPLICATE KEY UPDATE
  total_questions   = VALUES(total_questions),
  duration_minutes  = VALUES(duration_minutes),
  subject_seq       = VALUES(subject_seq);

-- =========================================
-- 3) EXAM_ROUND (연/회차) - 2025년 1·2·3회 + 2026년 1회 예시
-- =========================================
INSERT INTO exam_round
  (id, cert_id, year, round_no, exam_mode_scope)
VALUES
  (101, 1, 2025, 1, 'BOTH'),
  (102, 1, 2025, 2, 'BOTH'),
  (103, 1, 2025, 3, 'BOTH'),
  (104, 1, 2026, 1, 'BOTH'),
  -- 예비 자격증
  (201, 2, 2025, 1, 'BOTH')
ON DUPLICATE KEY UPDATE
  exam_mode_scope=VALUES(exam_mode_scope);

-- =========================================
-- 4) TOPIC (커리큘럼 트리 with 이모지)
-- - 정보처리기사 WRITTEN 트리 (1~5 + 하위 2~5.x.x까지 반영)
-- - order_no 로 정렬 보장
-- - 코드 유니크: (cert_id, code)
-- =========================================

-- -------------------------------------------------
-- Major (1~5, 필기)
-- -------------------------------------------------
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (10001, 1, NULL, '1', '소프트웨어 설계',       '🧩', 'WRITTEN', 1),
  (10002, 1, NULL, '2', '소프트웨어 개발',       '💻', 'WRITTEN', 2),
  (10003, 1, NULL, '3', '데이터베이스 구축',     '🗄️', 'WRITTEN', 3),
  (10004, 1, NULL, '4', '프로그래밍 언어 활용',  '🧠', 'WRITTEN', 4),
  (10005, 1, NULL, '5', '정보시스템 구축 관리', '🛠️', 'WRITTEN', 5)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- -------------------------------------------------
-- Sub under 1 (소프트웨어 설계 - 필기)
-- -------------------------------------------------
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (11001, 1, 10001, '1.1', '요구사항 확인',        '📝', 'WRITTEN', 1),
  (11002, 1, 10001, '1.2', '화면설계',            '🖥️', 'WRITTEN', 2),
  (11003, 1, 10001, '1.3', '애플리케이션 설계',    '🏗️', 'WRITTEN', 3),
  (11004, 1, 10001, '1.4', '인터페이스 설계',      '🔗', 'WRITTEN', 4)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 1.1
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (11101, 1, 11001, '1.1.1', '현행 시스템 분석',   '🔍', 'WRITTEN', 1),
  (11102, 1, 11001, '1.1.2', '요구사항 확인',     '✅', 'WRITTEN', 2),
  (11103, 1, 11001, '1.1.3', '분석 모델 확인',    '🧪', 'WRITTEN', 3)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 1.2
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (11201, 1, 11002, '1.2.1', 'UI 요구사항 확인', '🎯', 'WRITTEN', 1)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 1.3
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (11301, 1, 11003, '1.3.1', '공통 모듈 설계',     '🧩', 'WRITTEN', 1),
  (11302, 1, 11003, '1.3.2', '객체 지향 설계',     '🧱', 'WRITTEN', 2)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 1.4
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (11401, 1, 11004, '1.4.1', '인터페이스 요구사항 확인', '🗂️', 'WRITTEN', 1),
  (11402, 1, 11004, '1.4.2', '인터페이스 대상 식별',     '🧭', 'WRITTEN', 2),
  (11403, 1, 11004, '1.4.3', '인터페이스 상세 설계',     '🛠️', 'WRITTEN', 3)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- -------------------------------------------------
-- Sub under 2 (소프트웨어 개발 - 필기)
-- -------------------------------------------------
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (12001, 1, 10002, '2.1', '데이터 입출력 구현',         '📥', 'WRITTEN', 1),
  (12002, 1, 10002, '2.2', '통합 구현',                 '🔗', 'WRITTEN', 2),
  (12003, 1, 10002, '2.3', '제품 소프트웨어 패키징',     '📦', 'WRITTEN', 3),
  (12004, 1, 10002, '2.4', '애플리케이션 테스트 관리',   '🧪', 'WRITTEN', 4),
  (12005, 1, 10002, '2.5', '인터페이스 구현',           '🔌', 'WRITTEN', 5)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 2.1
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (12101, 1, 12001, '2.1.1', '논리 데이터 저장소 확인', '📂', 'WRITTEN', 1)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 2.2
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (12201, 1, 12002, '2.2.1', '모듈 구현',        '⚙️', 'WRITTEN', 1),
  (12202, 1, 12002, '2.2.2', '통합 구현 관리',   '🧩', 'WRITTEN', 2)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 2.3
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (12301, 1, 12003, '2.3.1', '제품 소프트웨어 패키징',   '📦', 'WRITTEN', 1),
  (12302, 1, 12003, '2.3.2', '제품 소프트웨어 메뉴얼 작성', '📖', 'WRITTEN', 2),
  (12303, 1, 12003, '2.3.3', '제품 소프트웨어 버전 관리',   '🔢', 'WRITTEN', 3)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 2.4
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (12401, 1, 12004, '2.4.1', '애플리케이션 테스트 케이스 설계', '🧪', 'WRITTEN', 1),
  (12402, 1, 12004, '2.4.2', '애플리케이션 통합 테스트',        '📱', 'WRITTEN', 2),
  (12403, 1, 12004, '2.4.3', '애플리케이션 성능 개선',          '📊', 'WRITTEN', 3)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 2.5
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (12501, 1, 12005, '2.5.1', '인터페이스 설계 확인', '🧾', 'WRITTEN', 1),
  (12502, 1, 12005, '2.5.2', '인터페이스 기능 구현', '🔌', 'WRITTEN', 2)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- -------------------------------------------------
-- Sub under 3 (데이터베이스 구축 - 필기)
-- -------------------------------------------------
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (13001, 1, 10003, '3.1', 'SQL 응용',              '🧮', 'WRITTEN', 1),
  (13002, 1, 10003, '3.2', 'SQL 활용',              '📊', 'WRITTEN', 2),
  (13003, 1, 10003, '3.3', '논리 데이터베이스 설계', '📐', 'WRITTEN', 3),
  (13004, 1, 10003, '3.4', '물리 데이터베이스 설계', '🏗️', 'WRITTEN', 4)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 3.1
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (13101, 1, 13001, '3.1.1', '절차형 SQL 작성', '🧾', 'WRITTEN', 1),
  (13102, 1, 13001, '3.1.2', '응용 SQL 작성',   '🧾', 'WRITTEN', 2)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 3.2
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (13201, 1, 13002, '3.2.1', '기본 SQL 작성', '📄', 'WRITTEN', 1),
  (13202, 1, 13002, '3.2.2', '고급 SQL 작성', '📄', 'WRITTEN', 2)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 3.3
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (13301, 1, 13003, '3.3.1', '관계 데이터베이스 모델', '🧱', 'WRITTEN', 1),
  (13302, 1, 13003, '3.3.2', '데이터 모델링 및 설계',   '📐', 'WRITTEN', 2)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 3.4
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (13401, 1, 13004, '3.4.1', '물리 요소 조사 분석',        '🔍', 'WRITTEN', 1),
  (13402, 1, 13004, '3.4.2', 'DB 물리 속성 설계',          '📦', 'WRITTEN', 2),
  (13403, 1, 13004, '3.4.3', '데이터베이스 무결성과 키',   '🔑', 'WRITTEN', 3),
  (13404, 1, 13004, '3.4.4', 'DB 반 정규화',               '🧬', 'WRITTEN', 4)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- -------------------------------------------------
-- Sub under 4 (프로그래밍 언어 활용 - 필기)
-- -------------------------------------------------
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (14001, 1, 10004, '4.1', '서버 프로그램 구현',      '🖥️', 'WRITTEN', 1),
  (14002, 1, 10004, '4.2', '프로그래밍 언어 활용',    '💻', 'WRITTEN', 2),
  (14003, 1, 10004, '4.3', '응용 SW 기초 기술 활용',  '⚙️', 'WRITTEN', 3)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 4.1
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (14101, 1, 14001, '4.1.1', '개발환경 구축',      '🧱', 'WRITTEN', 1),
  (14102, 1, 14001, '4.1.2', '서버 프로그램 구현', '🖥️', 'WRITTEN', 2),
  (14103, 1, 14001, '4.1.3', '배치 프로그램 구현', '📦', 'WRITTEN', 3)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 4.2
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (14201, 1, 14002, '4.2.1', '기본문법 활용',   '📘', 'WRITTEN', 1),
  (14202, 1, 14002, '4.2.2', '언어특성 활용',   '📗', 'WRITTEN', 2),
  (14203, 1, 14002, '4.2.3', '라이브러리 활용', '📚', 'WRITTEN', 3)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 4.3
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (14301, 1, 14003, '4.3.1', '운영체제 기초 활용', '🧠', 'WRITTEN', 1),
  (14302, 1, 14003, '4.3.2', '네트워크 기초 활용', '🌐', 'WRITTEN', 2)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- -------------------------------------------------
-- Sub under 5 (정보시스템 구축 관리 - 필기)
-- -------------------------------------------------
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (15001, 1, 10005, '5.1', '소프트웨어 개발방법론 활용',      '📚', 'WRITTEN', 1),
  (15002, 1, 10005, '5.2', 'IT 프로젝트 정보 시스템 구축관리', '🏗️', 'WRITTEN', 2),
  (15003, 1, 10005, '5.3', '소프트웨어 개발 보안 구축',        '🔒', 'WRITTEN', 3),
  (15004, 1, 10005, '5.4', '시스템 보안 구축',                  '🛡️', 'WRITTEN', 4)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 5.1
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (15101, 1, 15001, '5.1.1', '소프트웨어 개발방법론 선정',        '📌', 'WRITTEN', 1),
  (15102, 1, 15001, '5.1.2', '소프트웨어 개발방법론 테일러링',    '✂️', 'WRITTEN', 2)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 5.2
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (15201, 1, 15002, '5.2.1', '네트워크 구축관리', '🌐', 'WRITTEN', 1),
  (15202, 1, 15002, '5.2.2', 'SW 구축관리',       '💾', 'WRITTEN', 2),
  (15203, 1, 15002, '5.2.3', 'HW 구축관리',       '🖥️', 'WRITTEN', 3),
  (15204, 1, 15002, '5.2.4', 'DB 구축관리',       '🗄️', 'WRITTEN', 4)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 5.3
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (15301, 1, 15003, '5.3.1', '소프트웨어 개발 보안 설계', '🛡️', 'WRITTEN', 1),
  (15302, 1, 15003, '5.3.2', '소프트웨어 개발 보안 구현', '🔐', 'WRITTEN', 2)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

-- Micro under 5.4
INSERT INTO topic (id, cert_id, parent_id, code, title, emoji, exam_mode, order_no)
VALUES
  (15401, 1, 15004, '5.4.1', '시스템 보안 설계', '🛡️', 'WRITTEN', 1),
  (15402, 1, 15004, '5.4.2', '시스템 보안 구현', '🔐', 'WRITTEN', 2)
ON DUPLICATE KEY UPDATE
  title    = VALUES(title),
  emoji    = VALUES(emoji),
  order_no = VALUES(order_no);

SET FOREIGN_KEY_CHECKS = 1;