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
--  - sections_json: 섹션 배열 + 블록들(문단/리스트/이미지/표 등) 구조
-- =========================================

-- topic id 매핑
SET @topic_analysis  := 11101;  -- 1.1.1 '현행 시스템 분석'
SET @topic_oop       := 11102;  -- 1.1.2 '요구사항 확인' (예시, 실제 topic id에 맞춰 수정)
SET @topic_interface := 11103;  -- 1.1.3 '분석 모델 확인' (예시)
SET @topic_practical := 11301;  -- 2.x.x 어떤 실기 토픽 id (예시)


INSERT INTO concept (topic_id, sections_json)
SELECT @topic_analysis,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.1.1.1',
             'title', '플랫폼 기능 분석',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','(1) 플랫폼의 개념'),
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '플랫폼은 애플리케이션을 구동시키는데 필요한 소프트웨어 환경이다.',
                 '동일 플랫폼 내에서는 상호 호환이 가능하도록 만들어진 결합체를 의미한다.',
                 '공급자와 수요자 등 복수 그룹이 참여하여 각 그룹이 얻고자 하는 가치를 공정한 거래를 통해 교환할 수 있도록 구축된 환경이다.'
               )),
               JSON_OBJECT('type','heading','text','(2) 플랫폼 성능 특성 분석'),
               JSON_OBJECT('type','paragraph','text','플랫폼 성능 분석을 통해 사용자의 서비스 이용 시 속도의 적정성을 알 수 있다.'),
               JSON_OBJECT('type','heading','text','(3) 플랫폼 성능 특성 측정 항목'),
               JSON_OBJECT('type','paragraph','text','측정 항목에는 경과시간, 사용률, 응답시간, 가용성이 있다.'),
               JSON_OBJECT(
                 'type','table',
                 'caption','플랫폼 성능 특성 측정 항목',
                 'headers',JSON_ARRAY('항목','설명'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('경과시간','작업 시작~종료까지의 총 소요시간'),
                   JSON_ARRAY('사용률','자원 사용 비율'),
                   JSON_ARRAY('응답시간','요청~첫 응답까지의 시간'),
                   JSON_ARRAY('가용성','서비스 가능한 시간 비율')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/concepts/platform_metrics.png',
                 'alt','플랫폼 성능 지표 개념도',
                 'caption','성능 지표 개념도'
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.1.1.2',
             'title', '운영체제 분석',
             'importance', 1,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text','...운영체제 분석 내용...')
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_analysis);


INSERT INTO concept (topic_id, sections_json)
SELECT @topic_oop,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.1.2.1',
             'title', '요구분석 기법',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','(1) 요구수집 기법'),
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '인터뷰(Stakeholder 중심 질적정보 수집)',
                 '설문(다수의 정량적 의견 수렴)',
                 '워크숍/브레인스토밍(아이디어 확장)'
               )),
               JSON_OBJECT('type','heading','text','(2) 분석/명세 기법'),
               JSON_OBJECT(
                 'type','table',
                 'caption','요구 명세 기법 비교',
                 'headers',JSON_ARRAY('기법','설명','장점','주의점'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('자연어 명세','문장 기반 명세','친숙/빠름','모호성 위험'),
                   JSON_ARRAY('정형 명세','수학적/형식언어 기반','명확/검증 용이','난이도 높음'),
                   JSON_ARRAY('유스케이스','행위 중심 시나리오','이해 용이','비기능 요구 반영 한계')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/req/elicitation.png',
                 'alt','요구수집 방법 개요',
                 'caption','요구수집 방법 개요'
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/req/spec_comparison.png',
                 'alt','명세 기법 비교',
                 'caption','명세 기법 비교 다이어그램'
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.1.2.2',
             'title', 'UML',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text','UML은 표준 시각적 모델링 언어로 구조/행위를 표현한다.'),
               JSON_OBJECT(
                 'type','table',
                 'caption','주요 UML 다이어그램',
                 'headers',JSON_ARRAY('분류','다이어그램','핵심 목적'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('구조','클래스','정적 구조/관계'),
                   JSON_ARRAY('구조','컴포넌트','배치 단위/인터페이스'),
                   JSON_ARRAY('행위','유스케이스','액터-시스템 상호작용'),
                   JSON_ARRAY('행위','시퀀스','메시지 시간 순서'),
                   JSON_ARRAY('행위','활동','절차 흐름')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/uml/uc_sample.png',
                 'alt','유스케이스 예시',
                 'caption','유스케이스 다이어그램 예시'
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/uml/seq_sample.png',
                 'alt','시퀀스 예시',
                 'caption','시퀀스 다이어그램 예시'
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.1.2.3',
             'title', '애자일(Agile)',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','(1) 핵심 가치'),
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '개인/상호작용 > 프로세스/도구',
                 '작동하는 소프트웨어 > 포괄적 문서',
                 '고객 협력 > 계약 협상',
                 '변경 대응 > 계획 준수'
               )),
               JSON_OBJECT('type','heading','text','(2) 스크럼 핵심'),
               JSON_OBJECT(
                 'type','table',
                 'caption','스크럼 이벤트/산출물',
                 'headers',JSON_ARRAY('구분','내용','주기'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('이벤트','스프린트 기획/데일리/리뷰/회고','1~4주'),
                   JSON_ARRAY('산출물','백로그/증분','지속 갱신')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/agile/scrum_cycle.png',
                 'alt','스크럼 사이클',
                 'caption','스크럼 사이클'
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_oop);


INSERT INTO concept (topic_id, sections_json)
SELECT @topic_interface,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.1.3.1',
             'title', '모델링 기법',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text','문제 영역을 추상화해 의사소통/분석을 돕는 기법.'),
               JSON_OBJECT(
                 'type','table',
                 'caption','모델링 관점',
                 'headers',JSON_ARRAY('관점','설명','예시'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('기능','무엇을 하는가','DFD, 유스케이스'),
                   JSON_ARRAY('정적 구조','구성/관계','ERD, 클래스'),
                   JSON_ARRAY('동작','어떻게 동작하는가','상태/시퀀스/활동')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/modeling/erd.png',
                 'alt','ERD 샘플',
                 'caption','ERD 예시'
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.1.3.2',
             'title', '분석 자동화 도구',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '정적 분석(코드 규칙 위반 탐지)',
                 '성능 분석(프로파일링/부하테스트 도구)',
                 '모델 검증(일관성/제약 확인)'
               )),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/tools/static_analysis.png',
                 'alt','정적 분석 개념도',
                 'caption','정적 분석 개념도'
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.1.3.3',
             'title', '요구사항 관리 도구',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT(
                 'type','table',
                 'caption','요구 관리 주요 기능',
                 'headers',JSON_ARRAY('기능','설명'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('추적성','요구~설계~테스트 간 링크'),
                   JSON_ARRAY('버전관리','요구 변경 이력 관리'),
                   JSON_ARRAY('우선순위','가치/노력 기반 정렬')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/req/traceability.png',
                 'alt','추적성',
                 'caption','요구 추적성 개요'
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_interface);


INSERT INTO concept (topic_id, sections_json)
SELECT @topic_practical,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.2.1.1',
             'title', '정규화 개요',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text','정규화는 이상현상을 줄이고 데이터 무결성을 높이기 위한 설계 절차.'),
               JSON_OBJECT(
                 'type','table',
                 'caption','정규형 요약',
                 'headers',JSON_ARRAY('정규형','핵심 조건','설명'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('1NF','원자성','반복/다중값 제거'),
                   JSON_ARRAY('2NF','부분함수 종속 제거','기본키 전체 종속'),
                   JSON_ARRAY('3NF','이행함수 종속 제거','키가 아닌 속성 간 종속 제거')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/db/normalization.png',
                 'alt','정규화 개념도',
                 'caption','정규화 개념도'
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_practical);
