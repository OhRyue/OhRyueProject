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


/* =========================================
 * PRACTICAL Short/Long 추가 시드
 *  - @tp_31202: 정규화/반정규화
 *  - @tp_31301: SQL 기본/집계/조인
 *  - @tp_31302: SQL 튜닝/인덱스/성능
 *  각 토픽당 Short 5개 + Long 3개 추가
 *   (기존 Short 1 + Long 1 포함하면 Short 6 / Long 4)
 * ========================================= */

INSERT INTO question
  (cert_id, topic_id, mode, type, difficulty,
   stem, payload_json, answer_key, solution_text, source, image_url)
VALUES
  /* =========================
   * 31202 정규화/반정규화 (PRACTICAL)
   * ========================= */
  -- SHORT (추가 5개)
  (1, @tp_31202, 'PRACTICAL', 'SHORT', 'EASY',
   '제1정규형(1NF)을 만족하지 못하는 대표적인 테이블 예를 한 문장으로 설명하세요.',
   NULL,
   '하나의 컬럼에 쉼표로 구분된 여러 값이나 반복 그룹이 들어있는 테이블',
   '실기에서 자주 나오는 예시는 “전화번호1,전화번호2”와 같이 한 칸에 여러 값을 넣는 경우입니다.',
   'seed:prac:normalization:short2', NULL),

  (1, @tp_31202, 'PRACTICAL', 'SHORT', 'NORMAL',
   '부분 함수 종속(Partial Dependency)이 무엇인지, 기본키 관점에서 한 문장으로 정의하세요.',
   NULL,
   '합성 기본키의 일부에만 종속되어 있는 비기본키 속성이 존재하는 상태',
   '예를 들어 기본키가 (학생ID, 과목ID)인데, 학생주소가 학생ID에만 종속되어 있는 경우 부분 함수 종속입니다.',
   'seed:prac:normalization:short3', NULL),

  (1, @tp_31202, 'PRACTICAL', 'SHORT', 'NORMAL',
   '이행 함수 종속(Transitive Dependency)을 제거해야 하는 이유를 한 문장으로 설명하세요.',
   NULL,
   '기본키가 아닌 속성이 다른 비키 속성에 의존하면 갱신 이상과 중복이 커지기 때문',
   '3NF에서는 비키→비키 종속을 제거해 변경 시 여러 행을 수정해야 하는 문제를 줄입니다.',
   'seed:prac:normalization:short4', NULL),

  (1, @tp_31202, 'PRACTICAL', 'SHORT', 'HARD',
   '정규화를 지나치게 수행했을 때 발생할 수 있는 단점을 두 가지 쓰세요.',
   NULL,
   'JOIN이 너무 많아지는 성능 저하, 모델이 지나치게 복잡해져 이해/유지보수가 어려워짐',
   '실기에서는 “정규화 만능주의의 함정”을 성능과 복잡도 관점에서 함께 언급하면 좋습니다.',
   'seed:prac:normalization:short5', NULL),

  (1, @tp_31202, 'PRACTICAL', 'SHORT', 'HARD',
   '반정규화가 필요한 상황의 예를 하나 들어 한 문장으로 설명하세요.',
   NULL,
   '대용량 조회에서 항상 여러 테이블을 조인해야 하고 성능 병목이 되는 경우, 일부 데이터를 중복 저장해 조회를 단순화한다',
   '반정규화는 성능 문제 해결을 위해 조인을 줄이는 방향으로 제한적으로 사용하는 최적화 기법입니다.',
   'seed:prac:normalization:short6', NULL),

  -- LONG (추가 3개)
  (1, @tp_31202, 'PRACTICAL', 'LONG', 'NORMAL',
   '학생-수강-과목 정보를 하나의 테이블(학생ID, 학생이름, 과목ID, 과목명, 학점, 성적)로 관리할 때 발생할 수 있는 갱신/삽입/삭제 이상을 각각 한 가지씩 설명하고, 이를 줄이기 위한 정규화 방안을 서술하세요.',
   NULL,
   '학생, 과목, 수강 엔터티로 분리 후 키/외래키 설계',
   '예시: 갱신 이상: 과목명이 변경될 때 여러 행을 모두 수정해야 한다. 삽입 이상: 아직 수강기록이 없는 과목은 등록할 수 없다. 삭제 이상: 마지막 수강 기록을 삭제하면 과목 자체 정보가 사라진다. 해결: 학생, 과목, 수강 테이블로 분리하고 수강 테이블에 학생ID와 과목ID를 외래키로 둔다.',
   'seed:prac:normalization:long2', NULL),

  (1, @tp_31202, 'PRACTICAL', 'LONG', 'HARD',
   '실제 운영 중인 주문 테이블에 다수의 이상현상이 발견되었다. 제1정규형, 제2정규형, 제3정규형 관점에서 어떤 점검 절차를 거쳐 테이블을 분해해야 하는지 단계별로 서술하세요.',
   NULL,
   '1NF: 반복 그룹 제거, 2NF: 부분 종속 제거, 3NF: 이행 종속 제거',
   '정답 예시: 1단계(1NF): 하나의 컬럼에 여러 값이 들어있지 않은지, 반복되는 그룹이 없는지 확인하고 테이블을 분리한다. 2단계(2NF): 합성 키를 사용하는 경우, 비키가 키의 일부에만 의존하는지 확인해 별도 테이블로 분리한다. 3단계(3NF): 비키가 다른 비키에 의존하는지 확인하여 참조 테이블로 분리한다.',
   'seed:prac:normalization:long3', NULL),

  (1, @tp_31202, 'PRACTICAL', 'LONG', 'HARD',
   '대규모 로그 데이터를 설계할 때, 처음에는 강하게 정규화된 구조로 설계했다가 이후 분석/조회 성능 문제로 부분 반정규화를 수행하는 사례가 많다. 이 과정을 예로 들어, 정규화와 반정규화의 균형을 어떻게 잡아야 하는지 서술하세요.',
   NULL,
   '먼저 정규화로 이상 제거, 이후 실제 조회 패턴에 따라 필요한 부분만 반정규화',
   '예시: 초기에는 로그 테이블을 사용자, 세션, 이벤트 타입 등으로 잘게 나누어 정규화한다. 운영 후 분석 쿼리가 항상 동일한 조인을 반복해 성능 문제가 생기면, 조회용 요약 테이블을 추가하거나 일부 속성을 중복 저장해 반정규화한다. 이때 쓰기 성능과 저장 공간, 데이터 일관성을 함께 고려해야 한다.',
   'seed:prac:normalization:long4', NULL),

  /* =========================
   * 31301 SQL 기본/집계/조인 (PRACTICAL)
   * ========================= */
  -- SHORT (추가 5개)
  (1, @tp_31301, 'PRACTICAL', 'SHORT', 'EASY',
   '집계 함수와 GROUP BY를 함께 사용할 때, SELECT 절에 어떤 컬럼만 올 수 있는지 한 문장으로 설명하세요.',
   NULL,
   'GROUP BY에 포함된 컬럼과 집계 함수가 적용된 컬럼만 올 수 있다',
   '이 규칙을 어기면 대부분의 DBMS에서 오류가 발생하거나 비결정적인 결과가 나옵니다.',
   'seed:prac:sql_query:short2', NULL),

  (1, @tp_31301, 'PRACTICAL', 'SHORT', 'NORMAL',
   'INNER JOIN과 LEFT OUTER JOIN의 차이를 “결과 집합 관점”에서 한 문장으로 설명하세요.',
   NULL,
   'INNER JOIN은 양쪽에 모두 존재하는 행만, LEFT JOIN은 왼쪽은 모두 포함하고 없는 오른쪽은 NULL로 채운다',
   '실기에서는 집합 관점(교집합 vs 왼쪽 전체)을 기준으로 설명하면 이해하기 쉽습니다.',
   'seed:prac:sql_query:short3', NULL),

  (1, @tp_31301, 'PRACTICAL', 'SHORT', 'NORMAL',
   'HAVING 절이 필요한 상황의 예를 한 문장으로 설명하세요.',
   NULL,
   '집계 결과(그룹별 합계나 개수)에 조건을 걸어야 할 때 HAVING 절을 사용한다',
   'WHERE는 개별 행 필터, HAVING은 그룹 단위 필터라는 차이를 강조하면 좋습니다.',
   'seed:prac:sql_query:short4', NULL),

  (1, @tp_31301, 'PRACTICAL', 'SHORT', 'HARD',
   '서브쿼리를 사용해야만 더 깔끔해지는 상황의 예를 한 문장으로 설명하세요.',
   NULL,
   '예를 들어 “전체 평균보다 매출이 높은 지점”처럼 집계 결과를 다시 이용해 필터링할 때 서브쿼리가 직관적이다',
   'JOIN으로도 가능하지만, 서브쿼리가 의도 전달이 더 명확한 경우가 있습니다.',
   'seed:prac:sql_query:short5', NULL),

  (1, @tp_31301, 'PRACTICAL', 'SHORT', 'HARD',
   '윈도우 함수(ROW_NUMBER 등)를 사용하면 전통적인 GROUP BY만으로는 표현하기 어려운 어떤 유형의 문제를 쉽게 풀 수 있는지 한 문장으로 설명하세요.',
   NULL,
   '그룹 내 순위, 누적 합계, 이동 평균처럼 “행 간 비교나 누적”이 필요한 문제를 쉽게 풀 수 있다',
   '실기에서 윈도우 함수는 고난도 문제에 자주 등장하므로 개념적인 장점을 알고 있어야 합니다.',
   'seed:prac:sql_query:short6', NULL),

  -- LONG (추가 3개)
  (1, @tp_31301, 'PRACTICAL', 'LONG', 'NORMAL',
   '회원/주문 테이블에서 “최근 3개월 동안 주문을 1회 이상 한 회원”만 조회하는 SQL을 설계하려고 한다. WHERE, GROUP BY, HAVING, JOIN을 어떻게 조합해야 하는지 논리적인 설계 절차를 서술하세요.',
   NULL,
   '기간 조건 WHERE, 회원-주문 JOIN, 회원 기준 GROUP BY, HAVING COUNT 조건',
   '예시: (1) 최근 3개월 조건을 WHERE 주문일자 BETWEEN... 으로 필터링한다. (2) 회원-주문을 회원ID로 JOIN한다. (3) 회원ID 기준으로 GROUP BY 한다. (4) HAVING COUNT(주문ID) >= 1 조건을 걸어 주문이 있는 회원만 남긴다. 이 절차를 서술형으로 정리하면 됩니다.',
   'seed:prac:sql_query:long2', NULL),

  (1, @tp_31301, 'PRACTICAL', 'LONG', 'HARD',
   '“최근 30일 기준 일별 신규 가입자 수, 주문 수, 매출액”을 하나의 결과 집합으로 조회하는 대시보드용 SQL을 설계하려 한다. 날짜 기준으로 묶고, 없는 날짜도 0으로 표시하기 위한 설계 아이디어를 서술하세요.',
   NULL,
   '날짜 테이블(캘린더)과의 JOIN, LEFT JOIN + COALESCE, GROUP BY 날짜',
   '정답 예시: (1) 최근 30일을 갖는 임시 캘린더 테이블(또는 With Recursive)을 만든다. (2) 가입/주문/매출 집계를 각각 날짜 기준으로 GROUP BY 한다. (3) 캘린더를 기준으로 LEFT JOIN해서 값이 없는 날짜는 COALESCE로 0을 채운다. 이렇게 하면 날짜가 비어 있는 구간도 0으로 채워진 일별 지표를 만들 수 있다.',
   'seed:prac:sql_query:long3', NULL),

  (1, @tp_31301, 'PRACTICAL', 'LONG', 'HARD',
   '결제 로그 테이블에서 “한 세션 안에서 동일 상품을 3회 이상 결제 시도했다가 모두 실패한 케이스”만 찾는 SQL을 작성하려고 한다. 세션ID, 상품ID, 결제결과, 시각 컬럼이 있을 때, GROUP BY와 HAVING, 조건 집계를 이용해 어떻게 설계할지 서술하세요.',
   NULL,
   '세션ID+상품ID 그룹, 실패 건수 집계, HAVING COUNT 조건',
   '예시: (1) 세션ID, 상품ID 기준으로 GROUP BY 한다. (2) 결제 성공/실패 여부를 CASE WHEN으로 분리해 실패 건수를 집계한다. (3) HAVING에서 실패 건수가 3 이상이고 성공 건수는 0인 그룹만 필터링한다. 이렇게 논리를 글로 잘 풀어쓰는 것이 실기 서술형 포인트입니다.',
   'seed:prac:sql_query:long4', NULL),

  /* =========================
   * 31302 SQL 튜닝/인덱스/성능 (PRACTICAL)
   * ========================= */
  -- SHORT (추가 5개)
  (1, @tp_31302, 'PRACTICAL', 'SHORT', 'EASY',
   '인덱스를 설계할 때 카디널리티가 높은 컬럼이 유리한 이유를 한 문장으로 설명하세요.',
   NULL,
   '서로 다른 값이 많을수록 불필요한 행을 더 많이 걸러낼 수 있어 선택도가 좋아지기 때문',
   '카디널리티가 낮으면 인덱스를 타더라도 실제 스캔하는 행 수가 크게 줄지 않아 효율이 떨어집니다.',
   'seed:prac:sql_tuning:short2', NULL),

   (1, @tp_31302, 'PRACTICAL', 'SHORT', 'NORMAL',
   '복합 인덱스에서 선행 컬럼의 의미를 한 문장으로 설명하세요.',
   NULL,
   '인덱스 탐색 시 가장 먼저 필터링에 사용되는 컬럼으로, WHERE 절 조건과 일치할수록 효율이 올라간다',
   '선행 컬럼이 WHERE에 없으면 후행 컬럼만으로는 인덱스를 제대로 활용하지 못할 수 있습니다.',
   'seed:prac:sql_tuning:short3', NULL),

  (1, @tp_31302, 'PRACTICAL', 'SHORT', 'NORMAL',
   '커버링 인덱스(covering index)의 장점을 한 문장으로 설명하세요.',
   NULL,
   '인덱스에 필요한 모든 컬럼이 포함되어 있어 테이블을 다시 읽지 않고도 조회를 끝낼 수 있다',
   'I/O를 줄여 성능을 크게 개선할 수 있는 대표적인 튜닝 기법입니다.',
   'seed:prac:sql_tuning:short4', NULL),

  (1, @tp_31302, 'PRACTICAL', 'SHORT', 'HARD',
   '실행 계획에서 “풀 테이블 스캔”이 항상 나쁜 것은 아닌 이유를 한 문장으로 설명하세요.',
   NULL,
   '조건에 맞는 행이 대부분이거나 테이블이 매우 작으면, 인덱스를 타는 것보다 그냥 전체를 읽는 것이 더 빠를 수 있다',
   '실기에서는 상황에 따라 최적 전략이 달라진다는 점을 강조하는 답이 좋습니다.',
   'seed:prac:sql_tuning:short5', NULL),

  (1, @tp_31302, 'PRACTICAL', 'SHORT', 'HARD',
   'DML이 매우 많은 테이블에 인덱스를 과도하게 만들었을 때 발생할 수 있는 문제를 두 가지 쓰세요.',
   NULL,
   'INSERT/UPDATE/DELETE 시 인덱스까지 함께 갱신되어 쓰기 성능 저하, 인덱스 저장 공간 증가',
   '튜닝에서는 조회 성능뿐 아니라 쓰기 작업과 저장 공간까지 함께 고려해야 합니다.',
   'seed:prac:sql_tuning:short6', NULL),

  -- LONG (추가 3개)
  (1, @tp_31302, 'PRACTICAL', 'LONG', 'NORMAL',
   '“회원ID + 주문일자 범위”로 항상 조회하는 주문 검색 화면이 있다. 이 화면의 응답 시간이 느릴 때, 인덱스 설계와 쿼리 튜닝 관점에서 어떤 개선 방안을 적용할 수 있는지 3가지 이상 서술하세요.',
   NULL,
   '복합 인덱스(member_id, order_date), 필요한 컬럼만 SELECT, LIMIT/페이징, 실행 계획 확인',
   '예시: (1) WHERE 조건에 맞춰 (member_id, order_date) 복합 인덱스를 생성한다. (2) 화면에 필요한 컬럼만 SELECT하여 폭넓은 커버링 인덱스를 활용한다. (3) 최근 N건만 보여주도록 LIMIT와 적절한 ORDER BY를 사용한다. (4) 실행 계획(EXPLAIN)을 통해 인덱스 사용 여부를 점검한다.',
   'seed:prac:sql_tuning:long2', NULL),

  (1, @tp_31302, 'PRACTICAL', 'LONG', 'HARD',
   '결제 로그 테이블이 1억 건 이상으로 커져, 배치 작업과 조회 쿼리가 모두 느려졌다. 파티셔닝, 인덱스 재설계, 아카이빙 전략을 포함해 성능을 개선하기 위한 방안을 서술하세요.',
   NULL,
   '기간별 파티셔닝, 자주 사용하는 조건/정렬 기준으로 인덱스 재설계, 오래된 데이터 아카이빙',
   '정답 예시: (1) 결제일자 기준 RANGE 파티셔닝을 적용해 최근 데이터 위주로 빠르게 조회한다. (2) 실제 사용 패턴에 맞춰 인덱스를 재설계하고 불필요한 인덱스를 제거한다. (3) 1년 이상 지난 데이터는 별도 아카이브 테이블이나 DW로 이전해 운영 테이블 크기를 줄인다.',
   'seed:prac:sql_tuning:long3', NULL),

  (1, @tp_31302, 'PRACTICAL', 'LONG', 'HARD',
   '실행 계획에서 인덱스를 사용하고 있음에도 쿼리 성능이 만족스럽지 못한 경우, 통계 정보와 힌트, 쿼리 구조 관점에서 어떤 점을 점검해야 하는지 서술하세요.',
   NULL,
   '통계 최신화, 잘못된 인덱스 선택 여부, 필요한 경우 힌트/쿼리 재구성',
   '예시: (1) 테이블과 인덱스 통계가 오래되어 옵티마이저가 비효율적인 실행 계획을 선택하고 있지 않은지 확인하고 통계를 갱신한다. (2) 여러 인덱스 중 비효율적인 인덱스를 선택하는 경우, 힌트나 쿼리 구조 변경으로 원하는 인덱스를 사용하게 한다. (3) 불필요한 서브쿼리나 중복된 JOIN을 제거해 단순한 실행 계획이 나오도록 재구성한다.',
   'seed:prac:sql_tuning:long4', NULL);


/* =========================================================
 * PRACTICAL question_tag 매핑 (태그 시스템 기준)
 *  - 31202: 정규화/반정규화 → 정규화(기본), 반정규화(선택)
 *  - 31301: SQL 기본/집계/조인 → SQL
 *  - 31302: SQL 튜닝/인덱스/성능 → 튜닝
 *  규칙:
 *   1) 위에서 정의한 태그만 사용
 *   2) 허용되지 않은 태그는 제거
 *   3) 태그가 전혀 없는 문제는 대표 태그로 새로 매핑
 * ========================================================= */

-- 31202 정규화/반정규화
--  - 허용 태그: '정규화'
--  - 기존에 다른 태그가 달려 있으면 제거
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
