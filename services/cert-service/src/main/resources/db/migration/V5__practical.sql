SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_cert;

-- =========================================
-- 실기용 TOPIC 트리 (정보처리기사 PRACTICAL)
--  - Major P + Sub(P.1~P.5) + Micro(각 2개씩)
-- =========================================

-- Major (P, 실기 종합)
INSERT INTO topic (id, cert_id, parent_id, code,  title,                 emoji, exam_mode, order_no) VALUES
  (30001, 1, NULL, 'P',   '정보처리기사 실기(종합)',   '🧪', 'PRACTICAL', 1)
ON DUPLICATE KEY UPDATE
  title=VALUES(title),
  emoji=VALUES(emoji),
  exam_mode=VALUES(exam_mode),
  order_no=VALUES(order_no);

-- Sub (P.1 ~ P.5)
INSERT INTO topic (id, cert_id, parent_id, code,    title,                          emoji, exam_mode, order_no) VALUES
  (31001, 1, 30001, 'P.1', '요구사항 분석/모델링(실기)',    '📝', 'PRACTICAL', 1),
  (31002, 1, 30001, 'P.2', 'DB 설계/정규화(실기)',          '🗄️', 'PRACTICAL', 2),
  (31003, 1, 30001, 'P.3', 'SQL 작성/튜닝(실기)',           '💾', 'PRACTICAL', 3),
  (31004, 1, 30001, 'P.4', '트랜잭션/동시성/무결성(실기)',  '🔐', 'PRACTICAL', 4),
  (31005, 1, 30001, 'P.5', '운영/백업/장애대응(실기)',      '🛟', 'PRACTICAL', 5)
ON DUPLICATE KEY UPDATE
  title=VALUES(title),
  emoji=VALUES(emoji),
  exam_mode=VALUES(exam_mode),
  order_no=VALUES(order_no);

-- Micro under P.1 (요구사항 분석/모델링)
INSERT INTO topic (id, cert_id, parent_id, code,      title,                    emoji, exam_mode, order_no) VALUES
  (31101, 1, 31001, 'P.1.1', '업무 시나리오 해석',          '📋', 'PRACTICAL', 1),
  (31102, 1, 31001, 'P.1.2', '업무/데이터 요구 도출',       '📊', 'PRACTICAL', 2)
ON DUPLICATE KEY UPDATE
  title=VALUES(title),
  emoji=VALUES(emoji),
  exam_mode=VALUES(exam_mode),
  order_no=VALUES(order_no);

-- Micro under P.2 (DB 설계/정규화)
INSERT INTO topic (id, cert_id, parent_id, code,      title,                        emoji, exam_mode, order_no) VALUES
  (31201, 1, 31002, 'P.2.1', '개념/논리/물리 모델링',          '🧬', 'PRACTICAL', 1),
  (31202, 1, 31002, 'P.2.2', '정규화/반정규화 적용',           '🧱', 'PRACTICAL', 2)
ON DUPLICATE KEY UPDATE
  title=VALUES(title),
  emoji=VALUES(emoji),
  exam_mode=VALUES(exam_mode),
  order_no=VALUES(order_no);

-- Micro under P.3 (SQL 작성/튜닝)
INSERT INTO topic (id, cert_id, parent_id, code,      title,                        emoji, exam_mode, order_no) VALUES
  (31301, 1, 31003, 'P.3.1', 'SELECT/집계/조인 쿼리 작성',     '🔍', 'PRACTICAL', 1),
  (31302, 1, 31003, 'P.3.2', '인덱스 설계 및 쿼리 튜닝',       '⚙️', 'PRACTICAL', 2)
ON DUPLICATE KEY UPDATE
  title=VALUES(title),
  emoji=VALUES(emoji),
  exam_mode=VALUES(exam_mode),
  order_no=VALUES(order_no);

-- Micro under P.4 (트랜잭션/동시성/무결성)
INSERT INTO topic (id, cert_id, parent_id, code,      title,                        emoji, exam_mode, order_no) VALUES
  (31401, 1, 31004, 'P.4.1', '트랜잭션 특성/격리수준',          '🧱', 'PRACTICAL', 1),
  (31402, 1, 31004, 'P.4.2', '동시성 제어/락 전략 설계',       '🚦', 'PRACTICAL', 2)
ON DUPLICATE KEY UPDATE
  title=VALUES(title),
  emoji=VALUES(emoji),
  exam_mode=VALUES(exam_mode),
  order_no=VALUES(order_no);

-- Micro under P.5 (운영/백업/장애대응)
INSERT INTO topic (id, cert_id, parent_id, code,      title,                        emoji, exam_mode, order_no) VALUES
  (31501, 1, 31005, 'P.5.1', '백업/복구 전략 수립',            '💽', 'PRACTICAL', 1),
  (31502, 1, 31005, 'P.5.2', '장애 분석 및 개선안 도출',       '🚑', 'PRACTICAL', 2)
ON DUPLICATE KEY UPDATE
  title=VALUES(title),
  emoji=VALUES(emoji),
  exam_mode=VALUES(exam_mode),
  order_no=VALUES(order_no);

-- =========================================
-- CONCEPT (실기용 개념 블록)
--  - 각 Micro 토픽에 1개씩 sections_json 세팅
-- =========================================

SET @tp_req_scenario   := 31101;
SET @tp_req_data       := 31102;
SET @tp_model_levels   := 31201;
SET @tp_normalization  := 31202;
SET @tp_sql_query      := 31301;
SET @tp_sql_tuning     := 31302;
SET @tp_tx_isolation   := 31401;
SET @tp_concurrency    := 31402;
SET @tp_backup         := 31501;
SET @tp_incident       := 31502;

-- P.1.1 업무 시나리오 해석
INSERT INTO concept (topic_id, sections_json)
SELECT @tp_req_scenario,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', 'P.1.1.1',
             'title', '업무 시나리오에서 찾아야 할 것들',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text',
                 '실기 시험에서는 “주문 처리”, “게시글 등록”, “수강 신청”과 같은 업무 시나리오가 제시되며, 여기서 배우/행위/데이터를 정확히 뽑아내는 것이 출발점입니다.'
               ),
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '행위자(Actor): 고객, 관리자, 점주 등',
                 '주요 이벤트: 주문 접수, 결제 완료, 배송 지연 등',
                 '상태 변화: 주문 상태 변경, 재고 감소, 포인트 적립 등'
               ))
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', 'P.1.1.2',
             'title', '시나리오에서 ERD/유스케이스로 연결하기',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text',
                 '텍스트로 된 시나리오를 보고 유스케이스 다이어그램과 ERD로 바꿀 수 있어야 실제 설계/구현 문제를 풀 때 흔들리지 않습니다.'
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @tp_req_scenario);

-- P.1.2 업무/데이터 요구 도출
INSERT INTO concept (topic_id, sections_json)
SELECT @tp_req_data,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', 'P.1.2.1',
             'title', '업무 요구 vs 데이터 요구',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','table',
                 'caption','업무 요구와 데이터 요구 비교',
                 'headers',JSON_ARRAY('구분','내용','예시'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('업무 요구','업무 절차와 규칙','“배송 지연 시 고객에게 알림 발송”'),
                   JSON_ARRAY('데이터 요구','저장·조회해야 할 데이터','“주문ID, 배송예정일, 고객연락처 보관”')
                 )
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @tp_req_data);

-- P.2.1 개념/논리/물리 모델링
INSERT INTO concept (topic_id, sections_json)
SELECT @tp_model_levels,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', 'P.2.1.1',
             'title', '모델링 3단계',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','table',
                 'caption','개념/논리/물리 모델링 정리',
                 'headers',JSON_ARRAY('단계','질문','산출물 예시'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('개념 모델','어떤 개념(엔터티)이 있는가?','개념 ERD, 후보 엔터티 목록'),
                   JSON_ARRAY('논리 모델','어떤 속성과 관계가 필요한가?','정규화된 ERD, 식별자 정의'),
                   JSON_ARRAY('물리 모델','어떻게 저장할 것인가?','테이블/컬럼/인덱스 정의서')
                 )
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @tp_model_levels);

-- P.2.2 정규화/반정규화 적용
INSERT INTO concept (topic_id, sections_json)
SELECT @tp_normalization,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', 'P.2.2.1',
             'title', '정규화의 목적과 단계',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text',
                 '실기에서는 주어진 테이블을 보고 이상현상(삽입/갱신/삭제 이상)을 찾고, 적절히 정규화한 뒤 필요 시 반정규화를 적용하는 흐름으로 문제가 출제됩니다.'
               ),
               JSON_OBJECT('type','table',
                 'caption','정규형 요약',
                 'headers',JSON_ARRAY('정규형','핵심 조건'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('1NF','원자값, 반복그룹 제거'),
                   JSON_ARRAY('2NF','부분 함수 종속 제거'),
                   JSON_ARRAY('3NF','이행 함수 종속 제거')
                 )
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', 'P.2.2.2',
             'title', '반정규화가 필요한 순간',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '조인이 너무 많이 걸려서 성능이 허용 범위를 초과하는 경우',
                 '자주 조회되는 통계성 컬럼이 있을 때 요약 테이블을 두는 경우',
                 '시험 문제에서는 “조회 성능을 향상시키기 위한 설계 방안”으로 자주 등장'
               ))
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @tp_normalization);

-- P.3.1 SELECT/집계/조인 쿼리
INSERT INTO concept (topic_id, sections_json)
SELECT @tp_sql_query,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', 'P.3.1.1',
             'title', '실기에서 자주 나오는 SELECT 패턴',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '집계 함수 + GROUP BY + HAVING',
                 'INNER JOIN / LEFT JOIN 을 이용한 다중 테이블 조회',
                 '서브쿼리(IN/EXISTS)와 윈도우 함수(시험 난이도에 따라)'
               ))
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @tp_sql_query);

-- P.3.2 인덱스 설계/튜닝
INSERT INTO concept (topic_id, sections_json)
SELECT @tp_sql_tuning,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', 'P.3.2.1',
             'title', '인덱스 기본 개념',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','table',
                 'caption','인덱스 설계 시 체크포인트',
                 'headers',JSON_ARRAY('항목','내용'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('선행 컬럼','검색/조인/정렬 조건에 가장 많이 등장하는 컬럼'),
                   JSON_ARRAY('카디널리티','값이 다양할수록 인덱스 효율 증가'),
                   JSON_ARRAY('과도한 인덱스','DML 성능 저하 및 관리 비용 증가')
                 )
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @tp_sql_tuning);

-- P.4.1 트랜잭션 특성/격리수준
INSERT INTO concept (topic_id, sections_json)
SELECT @tp_tx_isolation,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', 'P.4.1.1',
             'title', 'ACID와 격리수준',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','table',
                 'caption','대표 격리수준과 현상',
                 'headers',JSON_ARRAY('격리수준','허용되는 이상현상'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('READ UNCOMMITTED','Dirty Read, Non-Repeatable Read, Phantom Read'),
                   JSON_ARRAY('READ COMMITTED','Non-Repeatable Read, Phantom Read'),
                   JSON_ARRAY('REPEATABLE READ','Phantom Read'),
                   JSON_ARRAY('SERIALIZABLE','없음(가장 엄격)')
                 )
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @tp_tx_isolation);

-- P.4.2 동시성 제어/락 전략
INSERT INTO concept (topic_id, sections_json)
SELECT @tp_concurrency,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', 'P.4.2.1',
             'title', '락 전략 설계 포인트',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '락 범위를 최소화해 교착상태 가능성을 줄인다.',
                 '일관된 락 획득 순서를 정의한다.',
                 '읽기 위주의 작업에는 공유락/스냅샷 격리를 활용한다.'
               ))
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @tp_concurrency);

-- P.5.1 백업/복구 전략
INSERT INTO concept (topic_id, sections_json)
SELECT @tp_backup,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', 'P.5.1.1',
             'title', '백업 유형과 전략',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','table',
                 'caption','백업 유형 비교',
                 'headers',JSON_ARRAY('유형','내용','장점','단점'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('전체 백업','DB 전체를 통째로 백업','복구가 단순','백업 시간/용량이 큼'),
                   JSON_ARRAY('증분 백업','마지막 백업 이후 변경분만 백업','백업 시간/용량 절약','복구 과정이 복잡'),
                   JSON_ARRAY('차등 백업','전체 백업 이후 변경분 누적 백업','복구 절차가 증분보다 단순','증분 대비 용량 증가')
                 )
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @tp_backup);

-- P.5.2 장애 분석 및 개선
INSERT INTO concept (topic_id, sections_json)
SELECT @tp_incident,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', 'P.5.2.1',
             'title', '장애 분석 리포트의 구성',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '장애 개요(언제, 어디서, 어떤 증상이 발생했는지)',
                 '원인 분석(직접 원인과 근본 원인 분리)',
                 '영향 범위(영향 받은 사용자/서비스/기간)',
                 '재발 방지 대책 및 개선 일정'
               ))
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @tp_incident);

SET FOREIGN_KEY_CHECKS = 1;
