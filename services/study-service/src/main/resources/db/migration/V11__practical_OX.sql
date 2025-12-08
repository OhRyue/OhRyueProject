SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id    := 1;
SET @tp_31202   := 31202; -- 정규화/반정규화
SET @tp_31301   := 31301; -- SQL 기본/집계/조인
SET @tp_31302   := 31302; -- SQL 튜닝/인덱스/성능


/* =======================================================
 * 31202 – 정규화와 반정규화
 *  - PRACTICAL OX
 * ======================================================= */

-- [31202] OX 3개 (ox4~ox6)

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'OX', 'NORMAL',
       '함수 종속성 분석은 정규화 과정에서 이상 현상을 찾기 위한 중요한 도구이다.',
       NULL,
       'O',
       '어떤 속성이 어떤 키/속성에 종속되는지 파악해야 이상 현상(갱신/삽입/삭제 이상)을 줄일 수 있습니다.',
       'seed:prac:normalization:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'OX', 'NORMAL',
       '정규화를 지나치게 수행하면 항상 성능이 크게 저하되므로, 1정규형까지만 적용하는 것이 일반적으로 권장된다.',
       NULL,
       'X',
       '현업에서는 보통 3NF 또는 BCNF까지 적용한 뒤, 필요한 부분에 한해 반정규화를 검토합니다.',
       'seed:prac:normalization:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'OX', 'NORMAL',
       '반정규화는 주로 조회 성능 개선을 위해 의도적으로 중복/집계 데이터를 추가하는 기법이다.',
       NULL,
       'O',
       '반정규화는 무결성과 성능 사이에서 트레이드오프를 선택하는 작업입니다.',
       'seed:prac:normalization:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:ox6');


/* =======================================================
 * 31301 – SQL 기본/집계/조인
 *  - PRACTICAL OX
 * ======================================================= */

-- [31301] OX 3개 (ox4~ox6)

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'OX', 'NORMAL',
       'GROUP BY를 사용하는 쿼리에서, 집계 대상이 아닌 컬럼을 SELECT 절에 그대로 쓰면 대부분의 DBMS에서 오류가 발생하거나 예측 불가능한 값을 반환한다.',
       NULL,
       'O',
       '시험에서 자주 나오는 GROUP BY 기본 함정입니다.',
       'seed:prac:sql_query:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'OX', 'NORMAL',
       'INNER JOIN은 ON 절의 조건을 만족하는 행만 반환하며, WHERE 절의 조건과는 아무 관련이 없다.',
       NULL,
       'X',
       'ON 이후 WHERE에서 추가 필터링이 들어가므로, 전체 결과는 두 조건 조합에 의해 결정됩니다.',
       'seed:prac:sql_query:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'OX', 'NORMAL',
       'SELECT DISTINCT는 중복 행을 제거하지만, 항상 인덱스 사용을 보장하지는 않는다.',
       NULL,
       'O',
       'DISTINCT 사용 여부와 인덱스 사용 여부는 실행 계획에 따라 달라집니다.',
       'seed:prac:sql_query:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:ox6');


/* =======================================================
 * 31302 – SQL 튜닝 / 인덱스 / 성능
 *  - PRACTICAL OX
 * ======================================================= */

-- [31302] OX 3개 (ox4~ox6)

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'OX', 'NORMAL',
       '인덱스가 존재하더라도 항상 사용되는 것은 아니며, 옵티마이저가 더 효율적이라고 판단하면 테이블 풀 스캔을 선택할 수 있다.',
       NULL,
       'O',
       '통계정보, 조건절 카디널리티 등에 따라 인덱스보다 풀 스캔이 더 효율적이라고 판단되면 인덱스를 사용하지 않을 수 있습니다.',
       'seed:prac:sql_tuning:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'OX', 'NORMAL',
       '복합 인덱스(컬럼1, 컬럼2)가 있을 때, WHERE 절이 “컬럼2 = ?”만 사용하는 경우에도 항상 인덱스를 전부 활용할 수 있다.',
       NULL,
       'X',
       '일반적으로 선행 컬럼(컬럼1)에 대한 조건이 없으면 인덱스 효율이 크게 떨어지며, 경우에 따라 인덱스를 사용하지 않을 수 있습니다.',
       'seed:prac:sql_tuning:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31302, 'PRACTICAL', 'OX', 'NORMAL',
       'EXPLAIN 계획에서 사용되는 조인 순서나 인덱스 선택은 통계 정보 갱신 여부에 따라 달라질 수 있다.',
       NULL,
       'O',
       '옵티마이저는 통계 정보에 기반해 실행 계획을 선택하므로, 통계를 갱신하면 조인 순서/인덱스 사용 여부가 바뀔 수 있습니다.',
       'seed:prac:sql_tuning:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_tuning:ox6');


/* =========================================================
 * PRACTICAL question_tag 매핑 (태그 시스템 기준)
 *  - 31202: 정규화/반정규화 → 정규화
 *  - 31301: SQL 기본/집계/조인 → SQL
 *  - 31302: SQL 튜닝/인덱스/성능 → 튜닝
 *  규칙:
 *   1) 위에서 정의한 태그만 사용
 *   2) 허용되지 않은 태그는 제거
 *   3) 태그가 전혀 없는 문제는 대표 태그로 새로 매핑
 * ========================================================= */

-- 31202 정규화/반정규화
--  - 허용 태그: '정규화'
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31202
  AND qt.tag <> '정규화';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '정규화'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '정규화'
WHERE q.topic_id = @tp_31202
  AND qt.question_id IS NULL;


-- 31301 SQL 기본/집계/조인
--  - 허용 태그: 'SQL'
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31301
  AND qt.tag <> 'SQL';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, 'SQL'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = 'SQL'
WHERE q.topic_id = @tp_31301
  AND qt.question_id IS NULL;


-- 31302 SQL 튜닝/인덱스/성능
--  - 허용 태그: '튜닝'
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31302
  AND qt.tag <> '튜닝';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '튜닝'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '튜닝'
WHERE q.topic_id = @tp_31302
  AND qt.question_id IS NULL;


SET FOREIGN_KEY_CHECKS = 1;
