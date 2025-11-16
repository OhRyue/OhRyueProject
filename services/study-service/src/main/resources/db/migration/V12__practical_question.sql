SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id    := 1;
SET @tp_31202   := 31202; -- 정규화/반정규화
SET @tp_31301   := 31301; -- SQL 기본/집계/조인
SET @tp_31302   := 31302; -- SQL 튜닝/인덱스/성능


/* =======================================================
 * 31202 – 정규화와 반정규화
 *  - 목표: PRACTICAL OX 총 6개, MCQ 총 10개
 * ======================================================= */

-- [31202] OX 추가 3개 (ox4~ox6)

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'OX', 'NORMAL',
       '함수 종속성 분석은 정규화 과정에서 이상 현상을 찾기 위한 중요한 도구이다. (O/X)',
       NULL,
       'O',
       '어떤 속성이 어떤 키/속성에 종속되는지 파악해야 이상 현상(갱신/삽입/삭제 이상)을 줄일 수 있습니다.',
       'seed:prac:normalization:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'OX', 'NORMAL',
       '정규화를 지나치게 수행하면 항상 성능이 크게 저하되므로, 1정규형까지만 적용하는 것이 일반적으로 권장된다. (O/X)',
       NULL,
       'X',
       '현업에서는 보통 3NF 또는 BCNF까지 적용한 뒤, 필요한 부분에 한해 반정규화를 검토합니다.',
       'seed:prac:normalization:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'OX', 'NORMAL',
       '반정규화는 주로 조회 성능 개선을 위해 의도적으로 중복/집계 데이터를 추가하는 기법이다. (O/X)',
       NULL,
       'O',
       '반정규화는 무결성과 성능 사이에서 트레이드오프를 선택하는 작업입니다.',
       'seed:prac:normalization:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:ox6');


-- [31202] MCQ 10개 (mcq1~mcq10)

-- Q1: 이상 현상 종류
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'MCQ', 'EASY',
       '다음 중 정규화가 주로 해결하고자 하는 “이상 현상”에 해당하지 않는 것은?',
       NULL,
       'D',
       '대표적인 이상 현상은 삽입/갱신/삭제 이상입니다.',
       'seed:prac:normalization:mcq1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:mcq1');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '삽입 이상(Insertion Anomaly)', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq1'
UNION ALL
SELECT q.id, 'B', '갱신 이상(Update Anomaly)', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq1'
UNION ALL
SELECT q.id, 'C', '삭제 이상(Deletion Anomaly)', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq1'
UNION ALL
SELECT q.id, 'D', '교착 상태(Deadlock)', 1
FROM question q WHERE q.source = 'seed:prac:normalization:mcq1';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:normalization:mcq1';


-- Q2: 1NF 정의
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'MCQ', 'EASY',
       '제1정규형(1NF)에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       '“한 칸에 한 값(원자값)”이 핵심 키워드입니다.',
       'seed:prac:normalization:mcq2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:mcq2');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '기본키가 반드시 두 개 이상이어야 한다.', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq2'
UNION ALL
SELECT q.id, 'B', '각 속성이 반복 그룹 없이 원자값만을 가지도록 테이블을 설계한다.', 1
FROM question q WHERE q.source = 'seed:prac:normalization:mcq2'
UNION ALL
SELECT q.id, 'C', '모든 함수 종속성이 기본키에 직·간접적으로 종속되도록 한다.', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq2'
UNION ALL
SELECT q.id, 'D', '모든 외래키를 제거한다.', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq2';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:normalization:mcq2';


-- Q3: 2NF/부분 함수 종속
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 “부분 함수 종속(Partial Dependency)”의 예로 가장 적절한 것은?',
       NULL,
       'C',
       '복합키의 일부에만 종속된 비키 속성이 있을 때 부분 함수 종속이라고 합니다.',
       'seed:prac:normalization:mcq3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:mcq3');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '(학생ID) → (학생이름)', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq3'
UNION ALL
SELECT q.id, 'B', '(과목ID) → (과목명)', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq3'
UNION ALL
SELECT q.id, 'C', '(학생ID, 과목ID) → (학생이름)', 1
FROM question q WHERE q.source = 'seed:prac:normalization:mcq3'
UNION ALL
SELECT q.id, 'D', '(학생ID, 과목ID) → (성적)', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq3';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:normalization:mcq3';


-- Q4: 3NF/이행 함수 종속
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 “이행 함수 종속(Transitive Dependency)”가 발생하는 상황으로 가장 적절한 것은?',
       NULL,
       'D',
       '기본키 → 중간 속성 → 다른 비키 속성으로 종속이 이어지는 경우입니다.',
       'seed:prac:normalization:mcq4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:mcq4');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '(주문ID) → (주문일자)', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq4'
UNION ALL
SELECT q.id, 'B', '(상품ID) → (상품명)', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq4'
UNION ALL
SELECT q.id, 'C', '(고객ID) → (고객이름)', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq4'
UNION ALL
SELECT q.id, 'D', '(주문ID) → (고객ID) → (고객이름)', 1
FROM question q WHERE q.source = 'seed:prac:normalization:mcq4';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:normalization:mcq4';


-- Q5: 정규화 단계 선택
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 정규화 수준을 결정할 때 고려해야 할 항목으로 가장 거리가 먼 것은?',
       NULL,
       'A',
       '정규화 정도는 무결성, 성능, 조회 패턴 등을 보고 결정합니다.',
       'seed:prac:normalization:mcq5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:mcq5');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '개발자의 연차', 1
FROM question q WHERE q.source = 'seed:prac:normalization:mcq5'
UNION ALL
SELECT q.id, 'B', '데이터 갱신 빈도', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq5'
UNION ALL
SELECT q.id, 'C', '조회 패턴과 성능 요구사항', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq5'
UNION ALL
SELECT q.id, 'D', '데이터 무결성 요구 수준', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq5';

UPDATE question
SET answer_key = 'A'
WHERE source = 'seed:prac:normalization:mcq5';


-- Q6: 반정규화 후보 판단
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 “반정규화”를 고려할 후보로 가장 적절한 것은?',
       NULL,
       'C',
       '조인이 너무 많이 필요한 빈번한 통계/집계 조회는 반정규화 후보가 될 수 있습니다.',
       'seed:prac:normalization:mcq6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:mcq6');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '거의 사용되지 않는 백업용 테이블', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq6'
UNION ALL
SELECT q.id, 'B', '코드성 소규모 마스터 테이블', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq6'
UNION ALL
SELECT q.id, 'C', '매 요청마다 여러 테이블을 조인해 집계하는 대용량 통계 조회', 1
FROM question q WHERE q.source = 'seed:prac:normalization:mcq6'
UNION ALL
SELECT q.id, 'D', '정적 도움말 텍스트 테이블', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq6';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:normalization:mcq6';


-- Q7: 반정규화의 단점
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'MCQ', 'NORMAL',
       '반정규화를 적용할 때 주의해야 할 사항으로 가장 적절한 것은?',
       NULL,
       'B',
       '중복 데이터 갱신 로직을 명확히 하지 않으면 무결성 문제가 발생합니다.',
       'seed:prac:normalization:mcq7'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:mcq7');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '로그 테이블은 항상 반정규화해야 한다.', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq7'
UNION ALL
SELECT q.id, 'B', '중복된 데이터를 갱신/삭제할 때 일관성이 유지되도록 로직과 트랜잭션을 설계해야 한다.', 1
FROM question q WHERE q.source = 'seed:prac:normalization:mcq7'
UNION ALL
SELECT q.id, 'C', '정규화된 테이블은 백업이 불가능하다.', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq7'
UNION ALL
SELECT q.id, 'D', '반정규화를 하면 항상 무결성이 향상된다.', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq7';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:normalization:mcq7';


-- Q8: 정규화/반정규화 균형
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'MCQ', 'NORMAL',
       '정규화와 반정규화의 관계를 설명한 것 중 가장 적절한 것은?',
       NULL,
       'D',
       '보통 정규화를 먼저 하고, 성능/운영 이슈가 있는 곳에 한해 반정규화를 적용합니다.',
       'seed:prac:normalization:mcq8'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:mcq8');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '정규화와 반정규화는 서로 무관한 작업이다.', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq8'
UNION ALL
SELECT q.id, 'B', '처음부터 반정규화된 모델을 만들고 필요하면 정규화를 적용한다.', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq8'
UNION ALL
SELECT q.id, 'C', '정규화는 성능만, 반정규화는 무결성만을 고려한다.', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq8'
UNION ALL
SELECT q.id, 'D', '정규화로 이상을 제거한 후, 특정 성능 요구 구간에만 반정규화를 선택적으로 적용한다.', 1
FROM question q WHERE q.source = 'seed:prac:normalization:mcq8';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:normalization:mcq8';


-- Q9: 설계의 흔한 실수
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 정규화/반정규화 관점에서 “잘못된 주장”으로 가장 적절한 것은?',
       NULL,
       'A',
       '정규화 여부와 상관 없이 백업/튜닝은 모두 가능합니다.',
       'seed:prac:normalization:mcq9'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:mcq9');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '정규화를 하면 백업과 튜닝 작업을 할 수 없다.', 1
FROM question q WHERE q.source = 'seed:prac:normalization:mcq9'
UNION ALL
SELECT q.id, 'B', '정규화는 데이터 중복과 이상 현상을 줄이는 데 도움을 준다.', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq9'
UNION ALL
SELECT q.id, 'C', '반정규화는 성능 개선을 위해 의도적으로 중복을 허용할 수 있다.', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq9'
UNION ALL
SELECT q.id, 'D', '반정규화 적용 시 무결성 검증 로직을 강화해야 한다.', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq9';

UPDATE question
SET answer_key = 'A'
WHERE source = 'seed:prac:normalization:mcq9';


-- Q10: 시험 스타일 요약 문제
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31202, 'PRACTICAL', 'MCQ', 'NORMAL',
       '정규화에 대한 설명으로 가장 적절한 것을 모두 고른 보기 조합은? (①~④)',
       NULL,
       'C',
       '3NF/BCNF, 이상 제거, 반정규화와의 조합이 핵심 포인트입니다.',
       'seed:prac:normalization:mcq10'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:normalization:mcq10');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '①, ②', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq10'
UNION ALL
SELECT q.id, 'B', '②, ③', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq10'
UNION ALL
SELECT q.id, 'C', '②, ③, ④', 1
FROM question q WHERE q.source = 'seed:prac:normalization:mcq10'
UNION ALL
SELECT q.id, 'D', '①, ④', 0
FROM question q WHERE q.source = 'seed:prac:normalization:mcq10';

-- (보기 내용은 화면/해설에서 설명용으로 풀어서 사용하시면 됩니다.)
UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:normalization:mcq10';



/* =======================================================
 * 31301 – SQL 기본/집계/조인
 *  - 목표: PRACTICAL OX 총 6개, MCQ 총 10개
 * ======================================================= */

-- [31301] OX 추가 3개 (ox4~ox6)

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'OX', 'NORMAL',
       'GROUP BY를 사용하는 쿼리에서, 집계 대상이 아닌 컬럼을 SELECT 절에 그대로 쓰면 대부분의 DBMS에서 오류가 발생하거나 예측 불가능한 값을 반환한다. (O/X)',
       NULL,
       'O',
       '시험에서 자주 나오는 GROUP BY 기본 함정입니다.',
       'seed:prac:sql_query:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'OX', 'NORMAL',
       'INNER JOIN은 ON 절의 조건을 만족하는 행만 반환하며, WHERE 절의 조건과는 아무 관련이 없다. (O/X)',
       NULL,
       'X',
       'ON 이후 WHERE에서 추가 필터링이 들어가므로, 전체 결과는 두 조건 조합에 의해 결정됩니다.',
       'seed:prac:sql_query:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'OX', 'NORMAL',
       'SELECT DISTINCT는 중복 행을 제거하지만, 항상 인덱스 사용을 보장하지는 않는다. (O/X)',
       NULL,
       'O',
       'DISTINCT 사용 여부와 인덱스 사용 여부는 실행 계획에 따라 달라집니다.',
       'seed:prac:sql_query:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:ox6');


-- [31301] MCQ 10개 (mcq1~mcq10)

-- Q1: 기본 SELECT 구조
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'MCQ', 'EASY',
       '다음 중 SQL SELECT 문장의 기본 작성 순서를 가장 잘 나타낸 것은?',
       NULL,
       'B',
       '시험 스타일: FROM → WHERE → GROUP BY → HAVING → SELECT → ORDER BY(논리 순서/실제 실행 개념).',
       'seed:prac:sql_query:mcq1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:mcq1');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'SELECT → FROM → WHERE → GROUP BY → HAVING → ORDER BY', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq1'
UNION ALL
SELECT q.id, 'B', 'FROM → WHERE → GROUP BY → HAVING → SELECT → ORDER BY', 1
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq1'
UNION ALL
SELECT q.id, 'C', 'WHERE → FROM → SELECT → GROUP BY → HAVING → ORDER BY', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq1'
UNION ALL
SELECT q.id, 'D', 'GROUP BY → FROM → WHERE → SELECT → ORDER BY → HAVING', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq1';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:sql_query:mcq1';


-- Q2: 집계 + HAVING
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'MCQ', 'EASY',
       '회원별 주문 금액 합계를 구하고, 합계가 10만원 이상인 회원만 조회하려고 한다. 조건을 넣기 가장 적절한 절은?',
       NULL,
       'C',
       '집계 결과에 대한 조건은 HAVING 절에 쓰는 것이 기본입니다.',
       'seed:prac:sql_query:mcq2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:mcq2');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'SELECT', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq2'
UNION ALL
SELECT q.id, 'B', 'FROM', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq2'
UNION ALL
SELECT q.id, 'C', 'HAVING', 1
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq2'
UNION ALL
SELECT q.id, 'D', 'ORDER BY', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq2';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:sql_query:mcq2';


-- Q3: JOIN 기본 이해
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'MCQ', 'EASY',
       '상품(product)과 주문상세(order_item)를 조인하여 상품별 판매 수량을 구할 때, 기본적으로 사용하기 가장 적절한 JOIN 유형은?',
       NULL,
       'B',
       '대부분의 경우 매칭되는 행만 필요하므로 INNER JOIN이 기본입니다.',
       'seed:prac:sql_query:mcq3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:mcq3');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'CROSS JOIN', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq3'
UNION ALL
SELECT q.id, 'B', 'INNER JOIN', 1
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq3'
UNION ALL
SELECT q.id, 'C', 'FULL OUTER JOIN', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq3'
UNION ALL
SELECT q.id, 'D', 'SELF JOIN', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq3';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:sql_query:mcq3';


-- Q4: NULL 처리 (COUNT)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'MCQ', 'NORMAL',
       'COUNT 함수 사용에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'D',
       'COUNT(컬럼)은 NULL을 제외, COUNT(*)는 NULL 포함 전체 행 수를 셉니다.',
       'seed:prac:sql_query:mcq4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:mcq4');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'COUNT(*)는 NULL을 제외하고 센다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq4'
UNION ALL
SELECT q.id, 'B', 'COUNT(컬럼명)은 NULL을 포함해 센다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq4'
UNION ALL
SELECT q.id, 'C', 'COUNT(*)와 COUNT(컬럼명)는 항상 같은 값을 반환한다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq4'
UNION ALL
SELECT q.id, 'D', 'COUNT(컬럼명)는 NULL이 아닌 값만 센다.', 1
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq4';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:sql_query:mcq4';


-- Q5: 서브쿼리 vs JOIN
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 서브쿼리와 JOIN에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       '시험 스타일: “서브쿼리가 항상 나쁘다” 같은 극단적인 진술은 오답입니다.',
       'seed:prac:sql_query:mcq5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:mcq5');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '서브쿼리는 항상 JOIN보다 느리므로 사용하면 안 된다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq5'
UNION ALL
SELECT q.id, 'B', '서브쿼리와 JOIN 중 어느 쪽이 더 유리한지는 데이터 특성과 실행 계획에 따라 달라진다.', 1
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq5'
UNION ALL
SELECT q.id, 'C', 'JOIN은 최대 두 테이블까지만 가능하다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq5'
UNION ALL
SELECT q.id, 'D', '서브쿼리는 SELECT 절에서만 사용할 수 있다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq5';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:sql_query:mcq5';


-- Q6: ORDER BY와 정렬 방향
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 ORDER BY에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       '기본은 ASC(오름차순)이고, 컬럼 인덱스 번호도 사용할 수 있으나 권장되지는 않습니다.',
       'seed:prac:sql_query:mcq6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:mcq6');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'ORDER BY 절에는 상수만 사용할 수 있다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq6'
UNION ALL
SELECT q.id, 'B', '정렬 방향을 지정하지 않으면 기본은 내림차순(DESC)이다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq6'
UNION ALL
SELECT q.id, 'C', 'ORDER BY에서는 컬럼명이나 컬럼 위치 인덱스를 사용해 정렬할 수 있다.', 1
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq6'
UNION ALL
SELECT q.id, 'D', 'ORDER BY는 GROUP BY와 함께 사용할 수 없다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq6';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:sql_query:mcq6';


-- Q7: NULL 정렬
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 대부분의 DBMS에서 “오름차순 정렬(ASC)” 시 NULL 값 처리에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'D',
       '시험마다 DBMS별 차이를 언급할 수 있지만, 보통 NULL이 먼저 오거나 마지막에 오도록 옵션이 제공됩니다.',
       'seed:prac:sql_query:mcq7'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:mcq7');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '항상 맨 위에 온다(표준으로 고정).', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq7'
UNION ALL
SELECT q.id, 'B', '항상 맨 아래에 온다(표준으로 고정).', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq7'
UNION ALL
SELECT q.id, 'C', '오름차순에서는 정렬 대상이 될 수 없다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq7'
UNION ALL
SELECT q.id, 'D', 'DBMS마다 기본 위치가 다를 수 있으며, NULLS FIRST/NULLS LAST 같은 옵션으로 제어하기도 한다.', 1
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq7';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:sql_query:mcq7';


-- Q8: DISTINCT + 집계
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 DISTINCT와 집계 함수 사용에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       'COUNT(DISTINCT 컬럼)은 중복을 제거한 후 개수를 셉니다.',
       'seed:prac:sql_query:mcq8'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:mcq8');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'SUM(DISTINCT 컬럼)은 사용할 수 없다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq8'
UNION ALL
SELECT q.id, 'B', 'COUNT(DISTINCT 컬럼)은 해당 컬럼의 서로 다른 값 개수를 센다.', 1
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq8'
UNION ALL
SELECT q.id, 'C', 'DISTINCT는 집계 함수와 함께 사용할 수 없다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq8'
UNION ALL
SELECT q.id, 'D', 'DISTINCT는 WHERE 절에서만 사용할 수 있다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq8';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:sql_query:mcq8';


-- Q9: SELF JOIN 개념
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31301, 'PRACTICAL', 'MCQ', 'NORMAL',
       'SELF JOIN에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       '자기 자신을 조인하는 기법이며, 계층 구조 표현 등에 자주 사용됩니다.',
       'seed:prac:sql_query:mcq9'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:mcq9');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '항상 세 개 이상의 테이블을 필요로 하는 조인이다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq9'
UNION ALL
SELECT q.id, 'B', '두 개의 서로 다른 데이터베이스를 조인하는 방식이다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq9'
UNION ALL
SELECT q.id, 'C', '하나의 테이블을 두 번 이상 FROM/JOIN에 등장시켜 자기 자신과 조인하는 방식이다.', 1
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq9'
UNION ALL
SELECT q.id, 'D', '카테시안 곱을 의미하는 다른 이름이다.', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq9';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:sql_query:mcq9';


-- Q10: 시험형 SQL 요약 문제
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31301, 'PRACTICAL', 'MCQ', 'NORMAL',
  '다음 설명을 읽고, SQL 기본 문법에 대한 올바른 설명만을 고른 보기 조합은? (①~④)',
  NULL,
  'D',
  'FROM/WHERE/GROUP BY/HAVING/ORDER BY의 역할을 구분하는 문제가 자주 나옵니다.',
  'seed:prac:sql_query:mcq10'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:sql_query:mcq10');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '①, ②', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq10'
UNION ALL
SELECT q.id, 'B', '②, ③', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq10'
UNION ALL
SELECT q.id, 'C', '③, ④', 0
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq10'
UNION ALL
SELECT q.id, 'D', '①, ③, ④', 1
FROM question q WHERE q.source = 'seed:prac:sql_query:mcq10';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:sql_query:mcq10';

SET FOREIGN_KEY_CHECKS = 1;