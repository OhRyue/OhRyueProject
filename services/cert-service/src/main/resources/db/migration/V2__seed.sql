SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- =========================================
-- 1) CERT (자격증 마스터)
-- =========================================
INSERT INTO cert
  (id, name, level, issuer, issuer_url, description, written_fee, practical_fee, pass_rule_text, qnet_jm_cd, qnet_qualgb_cd)
VALUES
  (1, '정보처리기사', '기사', '한국산업인력공단', 'https://www.q-net.or.kr',
   '소프트웨어 설계·개발·DB·언어·시스템 운영 전반에 대한 전문성을 평가합니다.',
   NULL, NULL,
   '예시: 필기 과목당 40점 이상 & 평균 60점 이상, 실기 60점 이상을 합격 기준으로 가정(실제 기준은 큐넷 고시 참조).',
   NULL, NULL),
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
--  - 정보처리기사: 필기 5과목 + 실기(종합)
--  - subject_seq로 정렬 보장
-- =========================================
INSERT INTO cert_subject
  (id, cert_id, exam_mode, name, total_questions, duration_minutes, subject_seq)
VALUES
  (1001, 1, 'WRITTEN', '소프트웨어 설계',              20, 120, 1),
  (1002, 1, 'WRITTEN', '소프트웨어 개발',              20, 120, 2),
  (1003, 1, 'WRITTEN', '데이터베이스 구축',            20, 120, 3),
  (1004, 1, 'WRITTEN', '프로그래밍 언어 활용',         20, 120, 4),
  (1005, 1, 'WRITTEN', '정보시스템 운영',              20, 120, 5),
  (1101, 1, 'PRACTICAL', '실기(종합)',                 NULL, 150, 1),
  -- 예비 자격증(간단 시드)
  (2001, 2, 'WRITTEN',  '컴활2급 필기(예시)',          40, 60,  1),
  (2101, 2, 'PRACTICAL','컴활2급 실기(예시)',          NULL, 90, 1)
ON DUPLICATE KEY UPDATE
  total_questions=VALUES(total_questions),
  duration_minutes=VALUES(duration_minutes),
  subject_seq=VALUES(subject_seq);

-- =========================================
-- 3) EXAM_ROUND (연/회차)
--  - 2025년 1·2·3회 가정
-- =========================================
INSERT INTO exam_round
  (id, cert_id, year, round_no, exam_mode_scope)
VALUES
  (101, 1, 2025, 1, 'BOTH'),
  (102, 1, 2025, 2, 'BOTH'),
  (103, 1, 2025, 3, 'BOTH'),
  -- 예비 자격증
  (201, 2, 2025, 1, 'BOTH')
ON DUPLICATE KEY UPDATE
  exam_mode_scope=VALUES(exam_mode_scope);

-- =========================================
-- 4) EXAM_SCHEDULE (회차별 일정)
--  - 예시 날짜(실제와 다를 수 있음). start<=end 보장.
-- =========================================
-- 2025-1회 (정보처리기사)
INSERT INTO exam_schedule
  (round_id, phase, start_date, end_date)
VALUES
  (101, 'REGISTER',  '2025-01-06', '2025-01-10'),
  (101, 'WRITTEN',   '2025-02-08', '2025-02-08'),
  (101, 'PRACTICAL', '2025-03-22', '2025-03-22'),
  (101, 'RESULT',    '2025-04-05', '2025-04-05')
ON DUPLICATE KEY UPDATE
  start_date=VALUES(start_date), end_date=VALUES(end_date);

-- 2025-2회 (정보처리기사)
INSERT INTO exam_schedule
  (round_id, phase, start_date, end_date)
VALUES
  (102, 'REGISTER',  '2025-04-07', '2025-04-11'),
  (102, 'WRITTEN',   '2025-05-10', '2025-05-10'),
  (102, 'PRACTICAL', '2025-06-21', '2025-06-21'),
  (102, 'RESULT',    '2025-07-05', '2025-07-05')
ON DUPLICATE KEY UPDATE
  start_date=VALUES(start_date), end_date=VALUES(end_date);

-- 2025-3회 (정보처리기사)
INSERT INTO exam_schedule
  (round_id, phase, start_date, end_date)
VALUES
  (103, 'REGISTER',  '2025-08-11', '2025-08-15'),
  (103, 'WRITTEN',   '2025-09-13', '2025-09-13'),
  (103, 'PRACTICAL', '2025-10-25', '2025-10-25'),
  (103, 'RESULT',    '2025-11-08', '2025-11-08')
ON DUPLICATE KEY UPDATE
  start_date=VALUES(start_date), end_date=VALUES(end_date);

-- 2025-1회 (컴활2급 예시)
INSERT INTO exam_schedule
  (round_id, phase, start_date, end_date)
VALUES
  (201, 'REGISTER',  '2025-01-06', '2025-01-08'),
  (201, 'WRITTEN',   '2025-02-01', '2025-02-01'),
  (201, 'PRACTICAL', '2025-02-15', '2025-02-15'),
  (201, 'RESULT',    '2025-02-22', '2025-02-22')
ON DUPLICATE KEY UPDATE
  start_date=VALUES(start_date), end_date=VALUES(end_date);

-- =========================================
-- 5) TOPIC (커리큘럼 트리 with 이모지)
--  - 정보처리기사 WRITTEN 트리(질문에서 주신 예시 기반)
--  - order_no로 정렬 보장
--  - 코드 유니크: (cert_id, code)
-- =========================================
-- Major (1~5)
INSERT INTO topic (id, cert_id, parent_id, code,  title,               emoji, exam_mode, order_no) VALUES
  (10001, 1, NULL, '1',   '소프트웨어 설계',         '🧩', 'WRITTEN', 1),
  (10002, 1, NULL, '2',   '소프트웨어 개발',         '💻', 'WRITTEN', 2),
  (10003, 1, NULL, '3',   '데이터베이스 구축',       '🗄️', 'WRITTEN', 3),
  (10004, 1, NULL, '4',   '프로그래밍 언어 활용',    '🧠', 'WRITTEN', 4),
  (10005, 1, NULL, '5',   '정보시스템 운영',         '🛠️', 'WRITTEN', 5)
ON DUPLICATE KEY UPDATE title=VALUES(title), emoji=VALUES(emoji), order_no=VALUES(order_no);

-- Sub under 1 (설계)
INSERT INTO topic (id, cert_id, parent_id, code,   title,            emoji, exam_mode, order_no) VALUES
  (11001, 1, 10001, '1.1', '요구사항 확인',       '📝', 'WRITTEN', 1),
  (11002, 1, 10001, '1.2', '화면설계',           '🖥️', 'WRITTEN', 2),
  (11003, 1, 10001, '1.3', '애플리케이션 설계',   '🏗️', 'WRITTEN', 3),
  (11004, 1, 10001, '1.4', '인터페이스 설계',     '🔗', 'WRITTEN', 4)
ON DUPLICATE KEY UPDATE title=VALUES(title), emoji=VALUES(emoji), order_no=VALUES(order_no);

-- Micro under 1.1 (요구사항 확인)
INSERT INTO topic (id, cert_id, parent_id, code,     title,              emoji, exam_mode, order_no) VALUES
  (11101, 1, 11001, '1.1.1', '현행 시스템 분석',    '🔍', 'WRITTEN', 1),
  (11102, 1, 11001, '1.1.2', '요구사항 확인',      '✅', 'WRITTEN', 2),
  (11103, 1, 11001, '1.1.3', '분석 모델 확인',     '🧪', 'WRITTEN', 3)
ON DUPLICATE KEY UPDATE title=VALUES(title), emoji=VALUES(emoji), order_no=VALUES(order_no);

-- Micro under 1.2 (화면설계)
INSERT INTO topic (id, cert_id, parent_id, code,     title,               emoji, exam_mode, order_no) VALUES
  (11201, 1, 11002, '1.2.1', 'UI 요구사항 확인',     '🎯', 'WRITTEN', 1)
ON DUPLICATE KEY UPDATE title=VALUES(title), emoji=VALUES(emoji), order_no=VALUES(order_no);

-- Micro under 1.3 (애플리케이션 설계)
INSERT INTO topic (id, cert_id, parent_id, code,     title,               emoji, exam_mode, order_no) VALUES
  (11301, 1, 11003, '1.3.1', '공통 모듈 설계',       '🧩', 'WRITTEN', 1),
  (11302, 1, 11003, '1.3.2', '객체 지향 설계',       '🧱', 'WRITTEN', 2)
ON DUPLICATE KEY UPDATE title=VALUES(title), emoji=VALUES(emoji), order_no=VALUES(order_no);

-- Micro under 1.4 (인터페이스 설계)
INSERT INTO topic (id, cert_id, parent_id, code,     title,                 emoji, exam_mode, order_no) VALUES
  (11401, 1, 11004, '1.4.1', '인터페이스 요구사항 확인', '🗂️', 'WRITTEN', 1),
  (11402, 1, 11004, '1.4.2', '인터페이스 대상 식별',     '🧭', 'WRITTEN', 2),
  (11403, 1, 11004, '1.4.3', '인터페이스 상세 설계',     '🛠️', 'WRITTEN', 3)
ON DUPLICATE KEY UPDATE title=VALUES(title), emoji=VALUES(emoji), order_no=VALUES(order_no);

-- =========================================
-- 6) CONCEPT (대표 세세항목 블록 샘플)
--  - blocks_json: 섹션 배열 + 블록들(문단/리스트/이미지/표 등) 구조 예시
-- =========================================
INSERT INTO concept (topic_id, content, blocks_json)
VALUES
  (11101,
   '현행 시스템의 아키텍처/프로세스/데이터 흐름을 파악해 요구 파생을 위한 근거를 수집합니다.',
   JSON_OBJECT(
     'sections', JSON_ARRAY(
       JSON_OBJECT(
         'orderNo', 1,
         'subCode', 'A',
         'title', '현행 시스템 분석 개요',
         'importance', 2,
         'blocks', JSON_ARRAY(
           JSON_OBJECT('type','paragraph','text','현행 시스템은 조직 내/외부 이해관계자의 업무 흐름과 기술 스택을 포함합니다.'),
           JSON_OBJECT('type','list','style','ul','items',JSON_ARRAY('조직/업무 흐름 도식화','데이터 흐름(DFD) 파악','아키텍처 레이어 식별'))
         )
       ),
       JSON_OBJECT(
         'orderNo', 2,
         'subCode', 'B',
         'title', '분석 산출물',
         'importance', 1,
         'blocks', JSON_ARRAY(
           JSON_OBJECT('type','paragraph','text','AS-IS 모델(DFD/ERD), 인터페이스 목록, 장애/성능 이슈 리스트를 확보합니다.')
         )
       )
     )
   )
  ),
  (11102,
   '요구사항은 기능/비기능으로 나누어 명세하고, 모호성을 제거합니다.',
   JSON_OBJECT(
     'sections', JSON_ARRAY(
       JSON_OBJECT(
         'orderNo', 1, 'subCode', 'A', 'title', '요구사항 유형',
         'importance', 2,
         'blocks', JSON_ARRAY(
           JSON_OBJECT('type','list','style','ol','items',JSON_ARRAY('기능 요구사항','비기능 요구사항(성능/보안/가용성/확장성)','제약 조건 및 가정'))
         )
       ),
       JSON_OBJECT(
         'orderNo', 2, 'subCode', 'B', 'title', '명세 품질',
         'importance', 1,
         'blocks', JSON_ARRAY(
           JSON_OBJECT('type','paragraph','text','검증 가능성/추적 가능성/일관성/완전성 기준을 확보합니다.')
         )
       )
     )
   )
  ),
  (11103,
   '분석 모델은 요구사항을 구조화해 이해관계자 간 의사소통을 돕습니다.',
   JSON_OBJECT(
     'sections', JSON_ARRAY(
       JSON_OBJECT(
         'orderNo', 1, 'subCode', 'A', 'title', '대표 모델',
         'importance', 2,
         'blocks', JSON_ARRAY(
           JSON_OBJECT('type','list','style','ul','items',JSON_ARRAY('UML(유스케이스/클래스/시퀀스)','DFD/ERD','상태/활동 다이어그램'))
         )
       )
     )
   )
  )
ON DUPLICATE KEY UPDATE
  content=VALUES(content),
  blocks_json=VALUES(blocks_json);

SET FOREIGN_KEY_CHECKS = 1;
