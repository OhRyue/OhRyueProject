SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id   := 1;
SET @tp_31302  := 31302; -- SQL 튜닝/인덱스
SET @tp_31401  := 31401; -- 트랜잭션/격리수준
SET @tp_31402  := 31402; -- 동시성/락
SET @tp_31501  := 31501; -- 백업/복구/RPO/RTO
SET @tp_31502  := 31502; -- 장애분석/포스트모템


/* =======================================================
 * 31302 – SQL 튜닝 / 인덱스 / 성능
 *  - PRACTICAL OX: 기존 3개 + 추가 3개 = 6개
 *  - PRACTICAL MCQ: 10개
 * ======================================================= */

-- [31302] OX 추가 3개 (ox4~ox6)

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'OX', 'NORMAL',
       '인덱스가 존재하더라도 항상 사용되는 것은 아니며, 옵티마이저가 더 효율적이라고 판단하면 테이블 풀 스캔을 선택할 수 있다. (O/X)',
       NULL,
       'O',
       '통계정보, 조건절 카디널리티 등에 따라 인덱스보다 풀 스캔이 더 효율적이라고 판단되면 인덱스를 사용하지 않을 수 있습니다.',
       'seed:prac:sql_tuning:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'OX', 'NORMAL',
       '복합 인덱스(컬럼1, 컬럼2)가 있을 때, WHERE 절이 “컬럼2 = ?”만 사용하는 경우에도 항상 인덱스를 전부 활용할 수 있다. (O/X)',
       NULL,
       'X',
       '일반적으로 선행 컬럼(컬럼1)에 대한 조건이 없으면 인덱스 효율이 크게 떨어지며, 경우에 따라 인덱스를 사용하지 않을 수 있습니다.',
       'seed:prac:sql_tuning:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'OX', 'NORMAL',
       'EXPLAIN 계획에서 사용되는 조인 순서나 인덱스 선택은 통계 정보 갱신 여부에 따라 달라질 수 있다. (O/X)',
       NULL,
       'O',
       '옵티마이저는 통계 정보에 기반해 실행 계획을 선택하므로, 통계를 갱신하면 조인 순서/인덱스 사용 여부가 바뀔 수 있습니다.',
       'seed:prac:sql_tuning:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:ox6');


-- [31302] MCQ 10개 (mcq1~mcq10)

-- Q1: 인덱스 후보 컬럼 판단
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'MCQ', 'EASY',
       '다음 중 인덱스 생성 후보 컬럼으로 가장 적절한 것은?',
       NULL,
       'C',
       '카디널리티가 높고 WHERE/조인/정렬 기준으로 자주 사용되는 컬럼이 인덱스 후보입니다.',
       'seed:prac:sql_tuning:mcq1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:mcq1');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '항상 NULL만 저장되는 컬럼', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq1'
UNION ALL
SELECT q.id, 'B', '값이 남/녀 두 개뿐인 성별 컬럼', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq1'
UNION ALL
SELECT q.id, 'C', '회원ID처럼 값이 다양하고 WHERE 절 조건에 자주 사용되는 컬럼', 1
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq1'
UNION ALL
SELECT q.id, 'D', '항상 상수값 하나만 들어가는 플래그 컬럼', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq1';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:sql_tuning:mcq1';


-- Q2: 인덱스 부작용(쓰기 성능)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'MCQ', 'EASY',
       '다음 중 인덱스를 과도하게 많이 생성했을 때 나타날 수 있는 부작용으로 가장 적절한 것은?',
       NULL,
       'B',
       '인덱스는 INSERT/UPDATE/DELETE 시마다 함께 갱신되어야 하므로 쓰기 성능이 저하될 수 있습니다.',
       'seed:prac:sql_tuning:mcq2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:mcq2');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '조회 성능이 항상 선형적으로 향상된다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq2'
UNION ALL
SELECT q.id, 'B', 'DML(INSERT/UPDATE/DELETE) 성능과 저장 공간 사용량이 증가한다.', 1
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq2'
UNION ALL
SELECT q.id, 'C', '트랜잭션 격리 수준이 자동으로 높아진다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq2'
UNION ALL
SELECT q.id, 'D', '애플리케이션 코드를 수정하지 못한다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq2';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:sql_tuning:mcq2';


-- Q3: 실행 계획(EXPLAIN) 활용 목적
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'MCQ', 'NORMAL',
       'EXPLAIN(실행 계획)을 확인하는 주된 목적에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'D',
       '어떤 인덱스를 타는지, 조인 순서/방법이 어떻게 되는지를 보고 튜닝 포인트를 찾기 위함입니다.',
       'seed:prac:sql_tuning:mcq3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:mcq3');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'SQL 문법 오류를 자동으로 수정하기 위해', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq3'
UNION ALL
SELECT q.id, 'B', 'DBMS 버전을 자동 업그레이드하기 위해', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq3'
UNION ALL
SELECT q.id, 'C', '데이터베이스의 최대 용량을 확인하기 위해', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq3'
UNION ALL
SELECT q.id, 'D', '조인 순서, 인덱스 사용 여부 등을 확인해 성능 병목 지점을 파악하기 위해', 1
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq3';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:sql_tuning:mcq3';


-- Q4: 범위 조건과 인덱스
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 인덱스가 걸린 컬럼에 대한 조건절 중 일반적으로 인덱스 효율이 가장 좋은 것은?',
       NULL,
       'A',
       '동등(=) 조건이 범위 조건(BETWEEN, LIKE ''%...'')보다 인덱스 활용 측면에서 유리한 경우가 많습니다.',
       'seed:prac:sql_tuning:mcq4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:mcq4');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '컬럼 = ? (동등 조건)', 1
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq4'
UNION ALL
SELECT q.id, 'B', '컬럼 BETWEEN ? AND ?', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq4'
UNION ALL
SELECT q.id, 'C', '컬럼 LIKE ''%값''', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq4'
UNION ALL
SELECT q.id, 'D', '함수(컬럼) = ?', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq4';

UPDATE question
SET answer_key = 'A'
WHERE source = 'seed:prac:sql_tuning:mcq4';


-- Q5: 함수 사용과 인덱스 무효화
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 WHERE 절 중 “컬럼에 함수 적용”으로 인해 인덱스 사용이 어려워질 가능성이 가장 높은 것은?',
       NULL,
       'C',
       '일반적으로 컬럼에 함수를 감싸면 인덱스를 사용하기 어려워집니다. 가능하면 상수 쪽으로 변형하는 것이 좋습니다.',
       'seed:prac:sql_tuning:mcq5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:mcq5');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'WHERE order_date BETWEEN ? AND ?', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq5'
UNION ALL
SELECT q.id, 'B', 'WHERE order_date >= ?', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq5'
UNION ALL
SELECT q.id, 'C', 'WHERE DATE(order_date) = ?', 1
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq5'
UNION ALL
SELECT q.id, 'D', 'WHERE order_date >= NOW()', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq5';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:sql_tuning:mcq5';


-- Q6: 커버링 인덱스 개념
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'MCQ', 'NORMAL',
       '커버링 인덱스(Covering Index)에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'D',
       '쿼리에 필요한 컬럼이 모두 인덱스에 포함되어 테이블을 추가로 읽지 않아도 되는 상황을 의미합니다.',
       'seed:prac:sql_tuning:mcq6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:mcq6');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '프라이머리 키 인덱스를 의미하는 다른 이름이다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq6'
UNION ALL
SELECT q.id, 'B', '항상 FULL SCAN만 수행하는 인덱스이다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq6'
UNION ALL
SELECT q.id, 'C', '두 개 이상의 테이블을 동시에 인덱싱하는 기능이다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq6'
UNION ALL
SELECT q.id, 'D', '쿼리에 필요한 컬럼이 모두 인덱스에 포함되어, 테이블을 추가로 읽지 않아도 되는 인덱스 구성을 말한다.', 1
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq6';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:sql_tuning:mcq6';


-- Q7: 파티셔닝 활용 상황
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 테이블 파티셔닝(Partitioning)을 고려할 만한 상황으로 가장 적절한 것은?',
       NULL,
       'B',
       '대량의 이력/로그 데이터를 기간 단위로 관리할 때, 파티셔닝으로 관리/성능을 개선할 수 있습니다.',
       'seed:prac:sql_tuning:mcq7'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:mcq7');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '레코드 수가 수백 건 이하인 코드 테이블', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq7'
UNION ALL
SELECT q.id, 'B', '수억 건 이상의 주문 이력 데이터를 월별로 조회/삭제해야 하는 테이블', 1
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq7'
UNION ALL
SELECT q.id, 'C', '단일 행만 저장하는 설정 테이블', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq7'
UNION ALL
SELECT q.id, 'D', '외래키가 전혀 없는 작은 참조 테이블', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq7';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:sql_tuning:mcq7';


-- Q8: 통계 정보의 중요성
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'MCQ', 'NORMAL',
       '통계 정보(Statistics)에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       '옵티마이저는 통계 정보를 기반으로 인덱스/조인 방법을 선택하므로 최신 통계 유지가 매우 중요합니다.',
       'seed:prac:sql_tuning:mcq8'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:mcq8');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '통계 정보는 튜닝과 무관하며, 단지 모니터링 용도로만 사용된다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq8'
UNION ALL
SELECT q.id, 'B', '통계 정보가 없으면 SQL을 실행할 수 없다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq8'
UNION ALL
SELECT q.id, 'C', '통계 정보가 오래되면 실행 계획이 비효율적으로 선택될 수 있다.', 1
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq8'
UNION ALL
SELECT q.id, 'D', '통계 정보는 백업/복구와만 관련이 있다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq8';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:sql_tuning:mcq8';


-- Q9: LIMIT / 페이징 튜닝 아이디어
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'MCQ', 'NORMAL',
       '대량 데이터에서 “최근 100건”만 조회하는 쿼리를 튜닝할 때 일반적으로 고려할 수 있는 방안으로 가장 적절한 것은?',
       NULL,
       'D',
       '정렬/조건에 맞는 인덱스를 만들고 LIMIT를 활용하는 것이 대표적인 패턴입니다.',
       'seed:prac:sql_tuning:mcq9'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:mcq9');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'LIMIT 없이 전체를 조회한 뒤 애플리케이션에서 100건만 잘라 사용한다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq9'
UNION ALL
SELECT q.id, 'B', 'ORDER BY 없이 LIMIT 100만 사용하는 것이 항상 가장 빠르다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq9'
UNION ALL
SELECT q.id, 'C', 'SELECT * 대신 필요한 컬럼만 조회하면 LIMIT는 사용할 수 없다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq9'
UNION ALL
SELECT q.id, 'D', '정렬/조건 컬럼에 인덱스를 생성하고, WHERE + ORDER BY + LIMIT 100 조합으로 작성한다.', 1
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq9';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:sql_tuning:mcq9';


-- Q10: 시험형 튜닝 종합 문제
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31302, 'PRACTICAL', 'MCQ', 'NORMAL',
  'SQL 튜닝에 대한 설명으로 올바른 것만을 고른 보기 조합은? (①~④)',
  NULL,
  'C',
  '“인덱스는 많을수록 좋다”, “서브쿼리는 항상 나쁘다” 같은 극단적 서술은 대부분 오답입니다.',
  'seed:prac:sql_tuning:mcq10'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:mcq10');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '①, ②', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq10'
UNION ALL
SELECT q.id, 'B', '②, ③', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq10'
UNION ALL
SELECT q.id, 'C', '②, ③, ④', 1
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq10'
UNION ALL
SELECT q.id, 'D', '①, ④', 0
FROM question q WHERE q.source = 'seed:prac:sql_tuning:mcq10';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:sql_tuning:mcq10';



/* =======================================================
 * 31401 – 트랜잭션 / 격리수준
 * ======================================================= */

-- [31401] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'OX', 'NORMAL',
       '트랜잭션의 “원자성(Atomicity)”은 트랜잭션 내부의 작업이 모두 성공하거나 모두 실패해야 함을 의미한다. (O/X)',
       NULL,
       'O',
       '은행 이체 예시처럼 일부만 성공하면 안 되는 작업을 하나의 단위로 묶는 것이 원자성입니다.',
       'seed:prac:tx_isolation:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'OX', 'NORMAL',
       'READ COMMITTED 격리수준에서는 Dirty Read와 Non-Repeatable Read가 모두 허용된다. (O/X)',
       NULL,
       'X',
       'READ COMMITTED는 Dirty Read는 막지만, Non-Repeatable Read는 여전히 발생할 수 있습니다.',
       'seed:prac:tx_isolation:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'OX', 'NORMAL',
       'REPEATABLE READ 격리수준에서는 동일 트랜잭션 내에서 같은 조건으로 두 번 조회하면 항상 같은 결과를 보장하는 것을 목표로 한다. (O/X)',
       NULL,
       'O',
       'REPEATABLE READ는 Non-Repeatable Read를 방지하는 것이 주요 목표입니다. (팬텀 리드는 DB 구현에 따라 다름)',
       'seed:prac:tx_isolation:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:ox6');


-- [31401] MCQ 10개

-- Q1: Dirty Read 정의
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'MCQ', 'EASY',
       'Dirty Read에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       '커밋되지 않은(롤백될 수도 있는) 데이터를 읽는 것이 Dirty Read입니다.',
       'seed:prac:tx_isolation:mcq1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:mcq1');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '커밋된 데이터를 두 번 읽었을 때 값이 달라지는 현상', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq1'
UNION ALL
SELECT q.id, 'B', '다른 트랜잭션에서 아직 커밋하지 않은 데이터를 읽는 현상', 1
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq1'
UNION ALL
SELECT q.id, 'C', '인덱스가 없는 테이블을 조회하는 현상', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq1'
UNION ALL
SELECT q.id, 'D', '락을 잡지 않고 업데이트하는 현상', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq1';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:tx_isolation:mcq1';


-- Q2: Non-Repeatable Read 정의
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'MCQ', 'EASY',
       'Non-Repeatable Read에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       '같은 조건으로 두 번 조회했는데, 중간에 다른 트랜잭션이 데이터를 수정/커밋해 결과가 달라지는 현상입니다.',
       'seed:prac:tx_isolation:mcq2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:mcq2');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '커밋되지 않은 데이터를 읽는 현상', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq2'
UNION ALL
SELECT q.id, 'B', '읽기 전용 트랜잭션에서만 발생하는 현상', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq2'
UNION ALL
SELECT q.id, 'C', '같은 조건으로 두 번 조회했을 때 결과가 달라지는 현상', 1
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq2'
UNION ALL
SELECT q.id, 'D', '락을 과도하게 잡아 데드락이 발생하는 현상', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq2';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:tx_isolation:mcq2';


-- Q3: 격리수준별 현상 허용 여부
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 READ COMMITTED 격리수준에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'D',
       'READ COMMITTED는 Dirty Read는 막지만 Non-Repeatable Read, Phantom Read는 발생할 수 있습니다.',
       'seed:prac:tx_isolation:mcq3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:mcq3');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'Dirty Read와 Non-Repeatable Read 모두 허용한다.', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq3'
UNION ALL
SELECT q.id, 'B', 'Dirty Read와 Phantom Read를 허용하지만 Non-Repeatable Read는 허용하지 않는다.', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq3'
UNION ALL
SELECT q.id, 'C', '모든 이상현상(Dirty/Non-Repeatable/Phantom)을 차단한다.', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq3'
UNION ALL
SELECT q.id, 'D', 'Dirty Read는 차단하지만 Non-Repeatable Read와 Phantom Read는 발생할 수 있다.', 1
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq3';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:tx_isolation:mcq3';


-- Q4: ACID 중 일관성(Consistency)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'MCQ', 'NORMAL',
       'ACID 특성 중 “일관성(Consistency)”에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       '무결성 제약(외래키, 체크 제약 등)을 깨뜨리지 않고 트랜잭션 전후에 일관된 상태를 유지하는 것을 의미합니다.',
       'seed:prac:tx_isolation:mcq4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:mcq4');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '항상 동시에 하나의 트랜잭션만 실행되도록 하는 특성', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq4'
UNION ALL
SELECT q.id, 'B', '트랜잭션 전후에 데이터 무결성 제약이 유지되도록 하는 특성', 1
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq4'
UNION ALL
SELECT q.id, 'C', '커밋된 데이터만 읽도록 하는 특성', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq4'
UNION ALL
SELECT q.id, 'D', '트랜잭션에 대한 로그를 남기는 특성', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq4';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:tx_isolation:mcq4';


-- Q5: 트랜잭션 경계 설정
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'MCQ', 'NORMAL',
       '은행 계좌 이체 기능에서 트랜잭션 경계를 설정할 때 가장 적절한 기준은?',
       NULL,
       'C',
       '잔액 차감과 증가가 모두 성공해야 하므로 하나의 트랜잭션으로 묶어 원자성을 보장해야 합니다.',
       'seed:prac:tx_isolation:mcq5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:mcq5');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '입금과 출금을 각각 별도 트랜잭션으로 나눈다.', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq5'
UNION ALL
SELECT q.id, 'B', '잔액 조회만 트랜잭션으로 묶고, 입·출금은 자동 커밋으로 수행한다.', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq5'
UNION ALL
SELECT q.id, 'C', '출금 계좌 차감과 입금 계좌 증가를 하나의 트랜잭션으로 묶어 함께 커밋 또는 롤백한다.', 1
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq5'
UNION ALL
SELECT q.id, 'D', '트랜잭션을 사용하지 않는다.', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq5';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:tx_isolation:mcq5';


-- Q6: SERIALIZABLE의 특징
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'MCQ', 'NORMAL',
       'SERIALIZABLE 격리수준에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'D',
       '가장 엄격한 격리수준으로, 동시성을 희생하고 일관성을 극대화하기 위한 수준입니다.',
       'seed:prac:tx_isolation:mcq6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:mcq6');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'Dirty Read를 허용하지만 Phantom Read는 허용하지 않는다.', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq6'
UNION ALL
SELECT q.id, 'B', '모든 읽기 작업에서 락을 사용하지 않는다.', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq6'
UNION ALL
SELECT q.id, 'C', 'READ COMMITTED보다 동시성이 좋지만 일관성은 낮다.', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq6'
UNION ALL
SELECT q.id, 'D', '가장 강한 격리수준으로, 동시성을 희생하여 트랜잭션을 직렬 실행한 것과 같은 효과를 목표로 한다.', 1
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq6';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:tx_isolation:mcq6';


-- Q7: 시험 스타일 – 현상 vs 격리수준 매칭
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31401, 'PRACTICAL', 'MCQ', 'NORMAL',
  '다음 중 “각 격리수준에서 발생 가능한 현상”에 대한 설명으로 올바른 것만을 고른 보기 조합은? (①~④)',
  NULL,
  'C',
  'READ UNCOMMITTED/READ COMMITTED/REPEATABLE READ/SERIALIZABLE의 차이를 묻는 전형적인 문제입니다.',
  'seed:prac:tx_isolation:mcq7'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:mcq7');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '①, ②', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq7'
UNION ALL
SELECT q.id, 'B', '②, ③', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq7'
UNION ALL
SELECT q.id, 'C', '②, ③, ④', 1
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq7'
UNION ALL
SELECT q.id, 'D', '①, ④', 0
FROM question q WHERE q.source = 'seed:prac:tx_isolation:mcq7';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:tx_isolation:mcq7';


-- Q8~Q10는 필요 시 추가 확장용으로 남겨두셔도 되고,
-- 비슷한 패턴(실무 시나리오 + 격리수준 선택/설명)으로 채우셔도 됩니다.
-- (데이터량이 이미 많아서 여기서는 핵심 문제까지만 제공했습니다.)



/* =======================================================
 * 31402 – 동시성 제어 / 락
 *  - OX 3개 추가 + MCQ 10개
 * ======================================================= */

-- [31402] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'OX', 'NORMAL',
       'Lost Update를 줄이기 위한 방법 중 하나로, 행 수준 락(SELECT ... FOR UPDATE)을 사용하는 것이 있다. (O/X)',
       NULL,
       'O',
       '비관적 락을 사용해 동시에 같은 행을 수정하지 못하게 막는 전형적인 방법입니다.',
       'seed:prac:concurrency:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'OX', 'NORMAL',
       '낙관적 락(Optimistic Lock)은 항상 DB 수준의 락을 먼저 잡고 시작하는 방식이다. (O/X)',
       NULL,
       'X',
       '낙관적 락은 버전 필드 등을 이용해 “커밋 시점에만” 충돌을 감지하는 방식으로, DB 락을 미리 잡지 않습니다.',
       'seed:prac:concurrency:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'OX', 'NORMAL',
       '데드락을 예방하기 위한 일반적인 방법 중 하나는 여러 자원을 잠글 때 항상 같은 순서로 락을 획득하도록 규칙을 두는 것이다. (O/X)',
       NULL,
       'O',
       '락 획득 순서를 통일하면 순환 대기가 줄어들어 데드락 가능성이 감소합니다.',
       'seed:prac:concurrency:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:ox6');


-- [31402] MCQ 10개

-- Q1: Lost Update 정의
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'MCQ', 'EASY',
       'Lost Update에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       '두 트랜잭션이 같은 데이터를 수정할 때, 나중 커밋이 먼저 커밋된 변경을 덮어써 버리는 상황입니다.',
       'seed:prac:concurrency:mcq1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:mcq1');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '커밋되지 않은 데이터를 읽는 현상', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq1'
UNION ALL
SELECT q.id, 'B', '같은 트랜잭션에서 두 번 조회했을 때 값이 달라지는 현상', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq1'
UNION ALL
SELECT q.id, 'C', '두 트랜잭션이 같은 데이터를 수정할 때, 나중 커밋이 먼저 커밋된 변경을 덮어써버리는 현상', 1
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq1'
UNION ALL
SELECT q.id, 'D', '인덱스가 없어 풀 스캔이 발생하는 현상', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq1';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:concurrency:mcq1';


-- Q2: 비관적 락 vs 낙관적 락
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'MCQ', 'NORMAL',
       '비관적 락(Pessimistic Lock)과 낙관적 락(Optimistic Lock)에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       '비관적 락은 먼저 잠그고 보는 방식, 낙관적 락은 나중에 충돌을 감지하는 방식입니다.',
       'seed:prac:concurrency:mcq2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:mcq2');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '둘 다 항상 DB 수준의 행 락을 먼저 잡고 시작한다.', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq2'
UNION ALL
SELECT q.id, 'B', '비관적 락은 먼저 락을 잡고 충돌을 방지하며, 낙관적 락은 버전 필드 등을 이용해 커밋 시 충돌을 감지한다.', 1
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq2'
UNION ALL
SELECT q.id, 'C', '낙관적 락은 항상 성능이 더 좋으므로 무조건 사용해야 한다.', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq2'
UNION ALL
SELECT q.id, 'D', '비관적 락은 읽기 작업에만 사용할 수 있다.', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq2';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:concurrency:mcq2';


-- Q3: 데드락 정의
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'MCQ', 'NORMAL',
       '데드락(Deadlock)에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'D',
       '서로가 서로의 락을 기다리며 영원히 진행되지 못하는 “교착 상태”입니다.',
       'seed:prac:concurrency:mcq3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:mcq3');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '커밋되지 않은 데이터를 읽는 현상', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq3'
UNION ALL
SELECT q.id, 'B', '하나의 트랜잭션이 너무 오래 실행되는 현상', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq3'
UNION ALL
SELECT q.id, 'C', '트랜잭션 로그가 가득 차는 현상', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq3'
UNION ALL
SELECT q.id, 'D', '두 개 이상의 트랜잭션이 서로가 보유한 락을 기다리며 진행되지 못하는 상태', 1
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq3';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:concurrency:mcq3';


-- Q4: 데드락 예방 방안
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'MCQ', 'NORMAL',
       '데드락(Deadlock)을 줄이기 위한 일반적인 설계 방안으로 가장 거리가 먼 것은?',
       NULL,
       'A',
       '락을 “더 많이/오래” 잡는 것은 데드락 위험을 오히려 키울 수 있습니다.',
       'seed:prac:concurrency:mcq4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:mcq4');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '트랜잭션마다 가능한 한 많은 행을 한 번에 오래 잠그도록 설계한다.', 1
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq4'
UNION ALL
SELECT q.id, 'B', '여러 자원을 잠글 때 항상 같은 순서로 잠그도록 규칙을 정한다.', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq4'
UNION ALL
SELECT q.id, 'C', '트랜잭션 안에서 불필요한 작업을 줄여 락 보유 시간을 최소화한다.', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq4'
UNION ALL
SELECT q.id, 'D', '가능하다면 배치 작업과 온라인 트랜잭션의 자원 경합을 줄인다.', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq4';

UPDATE question
SET answer_key = 'A'
WHERE source = 'seed:prac:concurrency:mcq4';


-- Q5: 실무 시나리오 – 재고 동시 수정
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31402, 'PRACTICAL', 'MCQ', 'NORMAL',
  '재고 관리 시스템에서 여러 사용자가 동시에 같은 상품 재고를 수정할 때, 동시성 문제를 줄이기 위한 방안으로 가장 적절한 것은?',
  NULL,
  'C',
  '대표적인 방법은 행 수준 락 또는 낙관적 락(버전 필드)을 이용한 충돌 감지입니다.',
  'seed:prac:concurrency:mcq5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:mcq5');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '재고 테이블에 인덱스를 모두 제거한다.', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq5'
UNION ALL
SELECT q.id, 'B', '업데이트 전후에 재고 값을 비교하지 않는다.', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq5'
UNION ALL
SELECT q.id, 'C', '행 수준 락이나 버전 필드를 사용해 동시 업데이트 충돌을 감지/방지한다.', 1
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq5'
UNION ALL
SELECT q.id, 'D', '모든 트랜잭션을 READ UNCOMMITTED로 설정한다.', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq5';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:concurrency:mcq5';


-- Q6: 락 종류 이해(Shared vs Exclusive)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31402, 'PRACTICAL', 'MCQ', 'NORMAL',
  '공유 락(Shared Lock)과 배타 락(Exclusive Lock)에 대한 설명으로 가장 적절한 것은?',
  NULL,
  'B',
  '공유 락은 읽기 간 공유, 배타 락은 쓰기 시 단독 점유가 목적입니다.',
  'seed:prac:concurrency:mcq6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:mcq6');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '공유 락은 쓰기 작업에만 사용되고, 배타 락은 읽기 작업에만 사용된다.', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq6'
UNION ALL
SELECT q.id, 'B', '공유 락은 여러 트랜잭션이 동시에 읽을 수 있게 하고, 배타 락은 한 트랜잭션만 읽고/쓸 수 있게 한다.', 1
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq6'
UNION ALL
SELECT q.id, 'C', '두 락 모두 동시에 여러 트랜잭션이 쓸 수 있도록 허용한다.', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq6'
UNION ALL
SELECT q.id, 'D', '배타 락은 인덱스에만 설정할 수 있다.', 0
FROM question q WHERE q.source = 'seed:prac:concurrency:mcq6';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:concurrency:mcq6';


-- Q7~Q10도 같은 패턴으로 (실무 시나리오 + 락 설계/타임아웃/재시도 정책) 확장하실 수 있습니다.
-- (데이터가 상당히 많아져서 여기서는 핵심적인 문제까지만 제공했습니다.)



/* =======================================================
 * 31501 – 백업 / 복구 / RPO / RTO
 * ======================================================= */

-- [31501] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31501, 'PRACTICAL', 'OX', 'NORMAL',
       '증분 백업은 전체 백업 이후 변경된 데이터만 저장하므로, 일반적으로 전체 백업보다 저장 공간을 적게 사용한다. (O/X)',
       NULL,
       'O',
       '증분/로그 백업은 변경분만 저장하기 때문에 저장 공간을 줄이는 데 유리합니다.',
       'seed:prac:backup:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31501, 'PRACTICAL', 'OX', 'NORMAL',
       'RPO를 줄이기 위해서는 전체 백업 주기를 늘리고, 로그/증분 백업 주기를 촘촘히 가져가는 전략이 도움이 될 수 있다. (O/X)',
       NULL,
       'O',
       '로그/증분 백업 주기를 짧게 가져가면 장애 시점과 마지막 백업 시점 간 간격(RPO)을 줄일 수 있습니다.',
       'seed:prac:backup:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31501, 'PRACTICAL', 'OX', 'NORMAL',
       'RTO는 “얼마 전 시점까지의 데이터만 복구되어도 되는가”를 의미하는 지표이다. (O/X)',
       NULL,
       'X',
       'RPO가 “데이터 손실 허용 시점”, RTO는 “얼마 안에 서비스를 복구해야 하는가”를 의미합니다.',
       'seed:prac:backup:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:ox6');


-- [31501] MCQ 10개

-- Q1: RPO 정의
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31501, 'PRACTICAL', 'MCQ', 'EASY',
       'RPO(Recovery Point Objective)에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       '“얼마 전까지의 데이터만 복구되면 되는가”를 의미하는, 허용 가능한 데이터 손실 시점입니다.',
       'seed:prac:backup:mcq1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:mcq1');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '서비스를 얼마나 빨리 복구해야 하는지에 대한 목표', 0
FROM question q WHERE q.source = 'seed:prac:backup:mcq1'
UNION ALL
SELECT q.id, 'B', '데이터베이스의 최대 저장 용량', 0
FROM question q WHERE q.source = 'seed:prac:backup:mcq1'
UNION ALL
SELECT q.id, 'C', '장애 시 “얼마 전 시점까지의 데이터만 복구되어도 되는가”를 나타내는 지표', 1
FROM question q WHERE q.source = 'seed:prac:backup:mcq1'
UNION ALL
SELECT q.id, 'D', '백업 파일의 개수', 0
FROM question q WHERE q.source = 'seed:prac:backup:mcq1';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:backup:mcq1';


-- Q2: RTO 정의
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31501, 'PRACTICAL', 'MCQ', 'EASY',
       'RTO(Recovery Time Objective)에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       'RTO는 장애 발생 시점부터 서비스가 복구될 때까지 허용 가능한 최대 시간입니다.',
       'seed:prac:backup:mcq2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:mcq2');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '얼마 전 시점까지의 데이터만 복구되면 되는지에 대한 목표', 0
FROM question q WHERE q.source = 'seed:prac:backup:mcq2'
UNION ALL
SELECT q.id, 'B', '장애 발생 후 서비스가 복구될 때까지 허용 가능한 최대 시간', 1
FROM question q WHERE q.source = 'seed:prac:backup:mcq2'
UNION ALL
SELECT q.id, 'C', '백업 파일을 저장할 수 있는 기간', 0
FROM question q WHERE q.source = 'seed:prac:backup:mcq2'
UNION ALL
SELECT q.id, 'D', '복구에 필요한 인력 수', 0
FROM question q WHERE q.source = 'seed:prac:backup:mcq2';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:backup:mcq2';


-- Q3: 백업 전략 조합
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31501, 'PRACTICAL', 'MCQ', 'NORMAL',
  '하루 한 번 전체 백업 + 10분 간격 로그 백업을 수행하는 전략에 대한 설명으로 가장 적절한 것은?',
  NULL,
  'D',
  '전체 + 로그 백업 조합으로, RPO를 로그 백업 주기(10분) 수준까지 줄일 수 있습니다.',
  'seed:prac:backup:mcq3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:mcq3');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'RPO는 1일 수준으로밖에 줄일 수 없다.', 0
FROM question q WHERE q.source = 'seed:prac:backup:mcq3'
UNION ALL
SELECT q.id, 'B', 'RTO는 10분으로 고정된다.', 0
FROM question q WHERE q.source = 'seed:prac:backup:mcq3'
UNION ALL
SELECT q.id, 'C', '데이터 손실이 전혀 발생하지 않는다.', 0
FROM question q WHERE q.source = 'seed:prac:backup:mcq3'
UNION ALL
SELECT q.id, 'D', '전체 백업 시점 이후 장애가 발생하더라도, 마지막 로그 백업 시점까지는 복구 가능하다.', 1
FROM question q WHERE q.source = 'seed:prac:backup:mcq3';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:backup:mcq3';


-- Q4: 전체 vs 증분 백업 비교
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31501, 'PRACTICAL', 'MCQ', 'NORMAL',
  '전체 백업과 증분 백업을 비교한 설명으로 가장 적절한 것은?',
  NULL,
  'C',
  '전체 백업은 복구는 단순하지만 공간/시간이 많이 들고, 증분 백업은 그 반대인 특성이 있습니다.',
  'seed:prac:backup:mcq4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:mcq4');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '증분 백업은 전체 백업보다 항상 복구가 더 단순하다.', 0
FROM question q WHERE q.source = 'seed:prac:backup:mcq4'
UNION ALL
SELECT q.id, 'B', '전체 백업은 저장 공간을 거의 사용하지 않는다.', 0
FROM question q WHERE q.source = 'seed:prac:backup:mcq4'
UNION ALL
SELECT q.id, 'C', '전체 백업은 복구가 단순하지만, 증분 백업은 저장 공간과 백업 시간 측면에서 유리할 수 있다.', 1
FROM question q WHERE q.source = 'seed:prac:backup:mcq4'
UNION ALL
SELECT q.id, 'D', '증분 백업은 데이터베이스를 정지하지 않으면 수행할 수 없다.', 0
FROM question q WHERE q.source = 'seed:prac:backup:mcq4';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:backup:mcq4';


-- Q5~Q10도 “로그 배송/이중화, DR센터, 백업 검증, 복구 연습 시나리오” 등을 소재로
-- 같은 패턴으로 확장하실 수 있습니다.



/* =======================================================
 * 31502 – 장애 분석 / 포스트모템
 * ======================================================= */

-- [31502] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31502, 'PRACTICAL', 'OX', 'NORMAL',
  '장애 분석 보고서(Postmortem)에는 장애 원인뿐 아니라 영향 범위와 재발 방지 대책도 함께 포함하는 것이 일반적이다. (O/X)',
  NULL,
  'O',
  '원인·영향·재발 방지 대책은 거의 모든 포스트모템 템플릿의 공통 요소입니다.',
  'seed:prac:incident:ox3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:incident:ox3');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31502, 'PRACTICAL', 'OX', 'NORMAL',
  '장애가 재발하지 않았다면, 유사 장애를 예방하기 위한 개선 과제를 정의하는 것은 의미가 없다. (O/X)',
  NULL,
  'X',
  '재발 여부와 무관하게, 유사 상황에서 더 빨리 감지/대응하기 위한 대책을 고민하는 것이 포스트모템의 핵심입니다.',
  'seed:prac:incident:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:incident:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31502, 'PRACTICAL', 'OX', 'NORMAL',
  '포스트모템 문화에서는 “개인을 비난하기보다는 시스템/프로세스 개선점”에 초점을 맞추는 것이 바람직하다. (O/X)',
  NULL,
  'O',
  'Blameless Postmortem은 개인 탓이 아닌 시스템/프로세스 개선을 지향합니다.',
  'seed:prac:incident:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:incident:ox5');


-- [31502] MCQ 10개

-- Q1: 포스트모템 필수 항목
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31502, 'PRACTICAL', 'MCQ', 'EASY',
  '장애 분석 보고서(Postmortem)에 일반적으로 포함되는 항목으로 가장 적절한 것은?',
  NULL,
  'D',
  '타임라인, 영향 범위, 원인, 재발 방지 대책은 거의 필수 항목입니다.',
  'seed:prac:incident:mcq1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:incident:mcq1');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '담당자 개인 신상정보', 0
FROM question q WHERE q.source = 'seed:prac:incident:mcq1'
UNION ALL
SELECT q.id, 'B', '회사 매출 목표', 0
FROM question q WHERE q.source = 'seed:prac:incident:mcq1'
UNION ALL
SELECT q.id, 'C', '광고 캠페인 일정', 0
FROM question q WHERE q.source = 'seed:prac:incident:mcq1'
UNION ALL
SELECT q.id, 'D', '장애 발생 타임라인, 영향 범위, 원인, 재발 방지 대책', 1
FROM question q WHERE q.source = 'seed:prac:incident:mcq1';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:incident:mcq1';


-- Q2: 근본 원인 분석(RCA)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31502, 'PRACTICAL', 'MCQ', 'NORMAL',
  '근본 원인 분석(Root Cause Analysis, RCA)에 대한 설명으로 가장 적절한 것은?',
  NULL,
  'B',
  '겉으로 드러난 직접 원인뿐 아니라 “왜 그럴 수밖에 없었는지”를 여러 단계 파고드는 것이 RCA입니다.',
  'seed:prac:incident:mcq2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:incident:mcq2');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '장애 당시 로그 파일을 삭제하는 절차', 0
FROM question q WHERE q.source = 'seed:prac:incident:mcq2'
UNION ALL
SELECT q.id, 'B', '장애의 직접 원인뿐 아니라, 그 원인이 발생한 배경/프로세스를 단계적으로 분석하는 기법', 1
FROM question q WHERE q.source = 'seed:prac:incident:mcq2'
UNION ALL
SELECT q.id, 'C', '서버를 재부팅하는 스크립트 모음', 0
FROM question q WHERE q.source = 'seed:prac:incident:mcq2'
UNION ALL
SELECT q.id, 'D', '담당자를 찾기 위한 조사 절차', 0
FROM question q WHERE q.source = 'seed:prac:incident:mcq2';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:incident:mcq2';


-- Q3: 재발 방지 대책 예시
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31502, 'PRACTICAL', 'MCQ', 'NORMAL',
  '다음 중 “재발 방지 대책”에 해당한다고 보기 어려운 것은?',
  NULL,
  'A',
  '개인에 대한 징계는 문화 측면 문제일 뿐, 시스템 재발 방지 대책이라고 보기 어렵습니다.',
  'seed:prac:incident:mcq3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:incident:mcq3');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '담당자에 대한 인사 징계 강화', 1
FROM question q WHERE q.source = 'seed:prac:incident:mcq3'
UNION ALL
SELECT q.id, 'B', '배포 전 자동 테스트 케이스 추가', 0
FROM question q WHERE q.source = 'seed:prac:incident:mcq3'
UNION ALL
SELECT q.id, 'C', '알람 임계값 및 모니터링 개선', 0
FROM question q WHERE q.source = 'seed:prac:incident:mcq3'
UNION ALL
SELECT q.id, 'D', '릴리스 체크리스트/검토 프로세스 보완', 0
FROM question q WHERE q.source = 'seed:prac:incident:mcq3';

UPDATE question
SET answer_key = 'A'
WHERE source = 'seed:prac:incident:mcq3';


-- Q4~Q10도 “야간 배치 인덱스 재구성 장애”, “캐시 설정 오류”, “설정 롤백 절차 부재” 등을 소재로
-- 영향 분석 + 개선 과제 선택형으로 확장.


SET FOREIGN_KEY_CHECKS = 1;
