SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

-- cert-service에서 정의한 실기 토픽 ID와 맞추기
SET @tp_req_scenario   := 31101; -- P.1.1 업무 시나리오 해석
SET @tp_req_data       := 31102; -- P.1.2 업무/데이터 요구
SET @tp_model_levels   := 31201; -- P.2.1 개념/논리/물리 모델링
SET @tp_normalization  := 31202; -- P.2.2 정규화/반정규화
SET @tp_sql_query      := 31301; -- P.3.1 SQL 작성
SET @tp_sql_tuning     := 31302; -- P.3.2 인덱스/튜닝
SET @tp_tx_isolation   := 31401; -- P.4.1 트랜잭션/격리
SET @tp_concurrency    := 31402; -- P.4.2 동시성/락
SET @tp_backup         := 31501; -- P.5.1 백업/복구
SET @tp_incident       := 31502; -- P.5.2 장애 분석/개선;

/* =========================================================
 * P.1.1 업무 시나리오 해석 – OX 3문항
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_req_scenario, 'PRACTICAL', 'OX', 'EASY',
       '업무 시나리오에서는 “누가(Actor)”, “무엇을(행위)”, “무엇에 대해(데이터)” 하는지를 파악하는 것이 중요하다.',
       'O',
       '실기에서 시나리오 해석의 핵심은 Actor/행위/데이터(대상)를 분리해서 보는 것입니다.',
       'seed:prac:req_scenario:ox1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:req_scenario:ox1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_req_scenario, 'PRACTICAL', 'OX', 'EASY',
       '업무 시나리오를 ERD로 변환할 때는, 먼저 화면에 표시되는 항목만 엔터티로 만들고 나머지는 모두 무시한다.',
       'X',
       '엔터티는 “화면에 보이는 항목”이 아니라 “업무에서 독립적으로 관리해야 하는 개체”를 기준으로 도출해야 합니다.',
       'seed:prac:req_scenario:ox2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:req_scenario:ox2'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_req_scenario, 'PRACTICAL', 'OX', 'NORMAL',
       '시나리오에 “배송 지연 시 고객에게 알림 발송” 요구가 있다면, 설계 시에는 이를 별도의 업무 프로세스로 분리해 표현할 수 있다.',
       'O',
       '배송 지연 알림은 “지연 감지 → 알림 생성/발송”으로 독립된 프로세스로 표현하는 것이 일반적입니다.',
       'seed:prac:req_scenario:ox3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:req_scenario:ox3'
);

/* =========================================================
 * P.1.2 업무/데이터 요구 도출 – OX 3문항
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_req_data, 'PRACTICAL', 'OX', 'EASY',
       '업무 요구는 “사용자가 하고 싶은 일”에 대한 설명이고, 데이터 요구는 이를 위해 저장·조회해야 하는 데이터를 정의한 것이다.',
       'O',
       '업무 요구(행동 중심)와 데이터 요구(저장/조회 대상)를 구분하는 것이 실기 기본입니다.',
       'seed:prac:req_data:ox1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:req_data:ox1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_req_data, 'PRACTICAL', 'OX', 'EASY',
       '데이터 요구를 도출할 때는, 업무 시나리오에 등장하지 않는 데이터라도 시스템 운영에 필요하면 포함될 수 있다.',
       'O',
       '로그/이력/통계와 같이 시나리오에 직접 쓰이지 않아도 운영·분석상 필요한 데이터 요구가 존재할 수 있습니다.',
       'seed:prac:req_data:ox2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:req_data:ox2'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_req_data, 'PRACTICAL', 'OX', 'NORMAL',
       '“배송 지연 알림 발송” 요구에서 알림 발송 시간을 저장할 필요는 없으며, 알림 발송 여부만 저장하면 충분하다.',
       'X',
       '알림 발송 시간은 장애 분석/재발 방지/고객 응대 등에 활용되므로 저장하는 것이 일반적입니다.',
       'seed:prac:req_data:ox3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:req_data:ox3'
);

/* =========================================================
 * P.2.1 개념/논리/물리 모델링 – OX 3문항
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_model_levels, 'PRACTICAL', 'OX', 'EASY',
       '개념 모델은 주로 비즈니스 용어를 사용해 엔터티와 관계를 표현하며, 속성 정의는 최소한으로만 포함될 수 있다.',
       'O',
       '개념 모델은 “무엇이 존재하고 어떻게 연결되는지”에 집중하는 단계입니다.',
       'seed:prac:model_levels:ox1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:model_levels:ox1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_model_levels, 'PRACTICAL', 'OX', 'EASY',
       '논리 모델에서는 정규화를 적용하기보다는, 성능을 고려해 먼저 반정규화를 수행하는 것이 일반적이다.',
       'X',
       '논리 모델 단계에서는 먼저 정규화를 통해 이상현상을 제거하고, 이후 물리 설계/튜닝에서 반정규화를 검토하는 것이 보통입니다.',
       'seed:prac:model_levels:ox2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:model_levels:ox2'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_model_levels, 'PRACTICAL', 'OX', 'NORMAL',
       '물리 모델에서는 특정 DBMS의 자료형, 인덱스, 파티션 전략 등이 함께 결정된다.',
       'O',
       '물리 모델은 실제 DBMS에 배포될 구체적인 구조(자료형, 인덱스, 파티션 등)를 정의하는 단계입니다.',
       'seed:prac:model_levels:ox3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:model_levels:ox3'
);

/* =========================================================
 * P.2.2 정규화/반정규화 – OX 3문항
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_normalization, 'PRACTICAL', 'OX', 'EASY',
       '제1정규형(1NF)은 모든 속성이 원자값을 가지도록 테이블을 설계하는 것을 의미한다.',
       'O',
       '반복되는 그룹/다중값을 제거해 한 칸에 한 값만 저장하는 것이 1NF 입니다.',
       'seed:prac:normalization:ox1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:normalization:ox1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_normalization, 'PRACTICAL', 'OX', 'NORMAL',
       '제3정규형(3NF)은 “기본키가 아닌 속성이 다른 기본키가 아닌 속성에 종속되지 않도록” 설계하는 것이다.',
       'O',
       '3NF는 이행 함수 종속(비키 → 비키)을 제거하는 단계입니다.',
       'seed:prac:normalization:ox2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:normalization:ox2'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_normalization, 'PRACTICAL', 'OX', 'NORMAL',
       '반정규화는 언제나 성능을 향상시키므로, 정규화가 된 모든 테이블에 대해 우선 적용하는 것이 좋다.',
       'X',
       '반정규화는 성능과 무결성 사이의 트레이드오프로, 필요할 때만 제한적으로 적용해야 합니다.',
       'seed:prac:normalization:ox3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:normalization:ox3'
);

/* =========================================================
 * P.3.1 SQL 작성 – OX 3문항
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_sql_query, 'PRACTICAL', 'OX', 'EASY',
       '집계 함수(SUM, COUNT 등)를 사용할 때, GROUP BY에 없는 컬럼을 SELECT 절에 그대로 쓰면 오류가 발생할 수 있다.',
       'O',
       '대부분의 DBMS에서 GROUP BY에 포함되지 않은 비집계 컬럼을 SELECT에 쓰면 에러 또는 예측 불가능한 결과가 나옵니다.',
       'seed:prac:sql_query:ox1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:sql_query:ox1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_sql_query, 'PRACTICAL', 'OX', 'EASY',
       'INNER JOIN은 양쪽 테이블에 모두 존재하는 행만 결과로 반환하고, LEFT OUTER JOIN은 왼쪽 테이블을 기준으로 존재하지 않는 쪽을 NULL로 채운다.',
       'O',
       'INNER/LEFT JOIN의 차이를 정확히 이해하는 것이 실기 SQL 기본입니다.',
       'seed:prac:sql_query:ox2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:sql_query:ox2'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_sql_query, 'PRACTICAL', 'OX', 'NORMAL',
       '서브쿼리는 항상 성능이 나쁘므로, 가능하면 JOIN으로 모두 바꾸는 것이 바람직하다.',
       'X',
       '서브쿼리가 항상 나쁜 것은 아니며, 상황에 따라 서브쿼리가 더 가독성·성능이 좋은 경우도 있습니다.',
       'seed:prac:sql_query:ox3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:sql_query:ox3'
);

/* =========================================================
 * P.3.2 인덱스/튜닝 – OX 3문항
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_sql_tuning, 'PRACTICAL', 'OX', 'EASY',
       '카디널리티(값의 다양성)가 높은 컬럼일수록 인덱스 효율이 좋아지는 경향이 있다.',
       'O',
       '예를 들어 성별처럼 값이 거의 두 개뿐인 컬럼보다는, 회원ID처럼 값이 많은 컬럼이 인덱스에 더 적합합니다.',
       'seed:prac:sql_tuning:ox1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:sql_tuning:ox1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_sql_tuning, 'PRACTICAL', 'OX', 'NORMAL',
       '모든 컬럼에 인덱스를 생성하면 조회 성능은 향상되지만, DML(INSERT/UPDATE/DELETE) 성능과 저장공간이 크게 나빠질 수 있다.',
       'O',
       '인덱스는 조회에는 이롭지만, 쓰기 작업에는 비용이 들어가므로 균형 있게 설계해야 합니다.',
       'seed:prac:sql_tuning:ox2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:sql_tuning:ox2'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_sql_tuning, 'PRACTICAL', 'OX', 'NORMAL',
       'WHERE 절에 사용되지 않는 컬럼이라도, ORDER BY에 자주 사용된다면 인덱스 후보가 될 수 있다.',
       'O',
       '정렬/그룹 기준으로 자주 사용되는 컬럼 역시 인덱스 후보입니다. (커버링 인덱스, 정렬 생략 등)',
       'seed:prac:sql_tuning:ox3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:sql_tuning:ox3'
);

/* =========================================================
 * P.4.1 트랜잭션/격리수준 – OX 3문항
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_tx_isolation, 'PRACTICAL', 'OX', 'EASY',
       '트랜잭션의 ACID 특성에서 “일관성(Consistency)”은 트랜잭션 실행 전후에 데이터 무결성이 유지되는 것을 의미한다.',
       'O',
       '무결성 제약조건을 깨지 않도록 유지하는 것이 Consistency입니다.',
       'seed:prac:tx_isolation:ox1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:tx_isolation:ox1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_tx_isolation, 'PRACTICAL', 'OX', 'NORMAL',
       'READ UNCOMMITTED 격리수준에서는 Dirty Read가 발생할 수 있으며, 다른 격리수준에서는 발생하지 않는다.',
       'O',
       '커밋되지 않은 데이터를 읽는 Dirty Read는 READ UNCOMMITTED에서만 허용됩니다.',
       'seed:prac:tx_isolation:ox2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:tx_isolation:ox2'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_tx_isolation, 'PRACTICAL', 'OX', 'NORMAL',
       'SERIALIZABLE 격리수준은 동시성을 극대화하기 위해 대부분의 읽기 작업에서 락을 사용하지 않는다.',
       'X',
       'SERIALIZABLE는 가장 엄격한 격리수준으로, 동시성보다 일관성을 우선하며 락/검사를 많이 수행합니다.',
       'seed:prac:tx_isolation:ox3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:tx_isolation:ox3'
);

/* =========================================================
 * P.4.2 동시성/락 – OX 3문항
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_concurrency, 'PRACTICAL', 'OX', 'EASY',
       'Lost Update는 두 트랜잭션이 같은 데이터를 수정할 때, 나중에 커밋된 트랜잭션이 먼저 커밋된 값을 덮어써버리는 현상이다.',
       'O',
       '실기에서 자주 나오는 전형적인 동시성 문제 유형입니다.',
       'seed:prac:concurrency:ox1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:concurrency:ox1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_concurrency, 'PRACTICAL', 'OX', 'NORMAL',
       '낙관적 락(Optimistic Lock)은 보통 버전 칼럼 등을 활용해 갱신 시점에 충돌을 감지하는 방식이다.',
       'O',
       '버전 필드나 타임스탬프를 이용해 “갱신 시점에만” 충돌을 확인하는 것이 낙관적 락의 대표적인 구현입니다.',
       'seed:prac:concurrency:ox2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:concurrency:ox2'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_concurrency, 'PRACTICAL', 'OX', 'NORMAL',
       '데드락을 줄이기 위한 일반적인 방법 중 하나는 여러 자원을 잠글 때 항상 같은 순서로 잠그도록 규칙을 정하는 것이다.',
       'O',
       '락 획득 순서를 일관되게 만드는 것은 데드락 방지의 대표적인 기법입니다.',
       'seed:prac:concurrency:ox3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:concurrency:ox3'
);

/* =========================================================
 * P.5.1 백업/복구 – OX 3문항
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_backup, 'PRACTICAL', 'OX', 'EASY',
       '전체 백업(Full Backup)은 장애 발생 시 복구 절차가 단순하다는 장점이 있다.',
       'O',
       '전체 백업만 있으면 해당 시점까지 한 번에 복구할 수 있기 때문에 절차가 단순합니다.',
       'seed:prac:backup:ox1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:backup:ox1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_backup, 'PRACTICAL', 'OX', 'NORMAL',
       '증분 백업은 항상 전체 백업보다 저장 공간을 더 많이 사용한다.',
       'X',
       '증분 백업은 “변경분만” 저장하므로 일반적으로 전체 백업보다 적은 공간을 사용합니다.',
       'seed:prac:backup:ox2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:backup:ox2'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_backup, 'PRACTICAL', 'OX', 'NORMAL',
       'RPO를 줄이려면, 전체 백업 빈도를 줄이고 로그/증분 백업 빈도를 높이는 전략이 유효할 수 있다.',
       'O',
       '로그·증분 백업 주기를 촘촘히 가져가면 장애 시점과 백업 사이의 간격(RPO)을 줄일 수 있습니다.',
       'seed:prac:backup:ox3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:backup:ox3'
);

/* =========================================================
 * P.5.2 장애 분석/개선 – OX 3문항
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_incident, 'PRACTICAL', 'OX', 'EASY',
       '장애 분석 보고서에서는 장애 발생 시간, 영향 범위, 원인, 재발 방지 대책 등을 함께 정리하는 것이 일반적이다.',
       'O',
       '이 항목들은 실제 현업 Postmortem 템플릿에서도 거의 필수로 포함됩니다.',
       'seed:prac:incident:ox1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:incident:ox1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_incident, 'PRACTICAL', 'OX', 'NORMAL',
       '장애가 재발하지 않았더라도, 유사한 장애를 예방하기 위한 개선 과제를 정의하는 것은 의미가 없다.',
       'X',
       '재발 여부와 관계없이 “유사 상황에서 어떻게 더 빨리 감지/대응할 것인가”를 정의하는 것이 중요합니다.',
       'seed:prac:incident:ox2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:incident:ox2'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_incident, 'PRACTICAL', 'OX', 'NORMAL',
       '장애 분석 보고서는 개발팀만 보는 문서이므로, 비즈니스 영향이나 사용자 관점 설명은 굳이 포함하지 않아도 된다.',
       'X',
       '장애의 비즈니스 영향과 사용자 관점을 함께 공유해야 우선순위와 개선 방향을 제대로 잡을 수 있습니다.',
       'seed:prac:incident:ox3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE source='seed:prac:incident:ox3'
);

SET FOREIGN_KEY_CHECKS = 1;
