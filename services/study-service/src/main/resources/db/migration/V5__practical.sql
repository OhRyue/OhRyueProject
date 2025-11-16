SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

-- 토픽 매핑 (cert-service의 topic.id와 동일하게 맞춤)
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

-- =========================================================
-- P.1.1 업무 시나리오 해석
-- =========================================================

-- SHORT
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_req_scenario, 'PRACTICAL', 'SHORT', 'NORMAL',
       '“고객이 온라인에서 상품을 주문하면 재고를 차감하고, 결제가 완료되면 배송 요청을 생성한다”라는 시나리오에서 '
       '업무 프로세스를 분석할 때 반드시 확인해야 할 항목 두 가지를 쓰세요.',
       '주문/결제/배송의 단계적 흐름, 각 단계에서 변경되는 상태(주문상태·재고·배송요청 등)',
       '실기에서는 “어떤 단계들이 있는지(주문→결제→배송요청)”와 “각 단계에서 어떤 상태/데이터가 바뀌는지”를 명확히 적어야 합니다. '
       '예를 들어 [주문 접수 → 결제 승인 → 재고 차감 → 배송 요청 생성]과 같이 흐름과 상태 변화를 함께 기술하면 좋습니다.',
       'seed:prac:req_scenario:short1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_req_scenario AND mode='PRACTICAL' AND type='SHORT'
     AND source='seed:prac:req_scenario:short1'
);

-- LONG
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_req_scenario, 'PRACTICAL', 'LONG', 'NORMAL',
       '다음은 학습 관리 시스템(LMS)의 일부 요구입니다. '
       '“수강생은 강의를 신청하고, 결제가 완료되면 수강상태가 활성화된다. '
       '강의 기간 동안에만 강의를 시청할 수 있으며, 기간이 지나면 자동으로 수강이 종료된다.” '
       '위 시나리오를 기반으로, 실기 문제에서 요구될 수 있는 업무 프로세스 다이어그램(또는 유스케이스/상태도) 설계 방향을 서술하세요.',
       '주요 Actor/이벤트/상태를 구분하고, [수강신청→결제완료→수강활성→기간만료→수강종료] 흐름과 상태 전이를 표시하는 것',
       '정답 예시는 다음과 같습니다. '
       '1) Actor: 수강생, 결제시스템(LG U+ 등), LMS. '
       '2) 주요 이벤트: 수강신청, 결제완료 콜백, 수강기간 시작/종료 배치. '
       '3) 상태: 신청중, 결제대기, 수강중, 수강종료. '
       '유스케이스/상태도에서는 수강신청 시 “신청중→결제대기”, 결제 성공 시 “수강중”, '
       '수강 종료일 배치 시 “수강종료”로 전이되는 것을 표현하는 것이 핵심입니다.',
       'seed:prac:req_scenario:long1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_req_scenario AND mode='PRACTICAL' AND type='LONG'
     AND source='seed:prac:req_scenario:long1'
);

-- 태그
INSERT INTO question_tag (question_id, tag)
SELECT q.id, '업무시나리오'
FROM question q
WHERE q.source IN ('seed:prac:req_scenario:short1','seed:prac:req_scenario:long1')
  AND NOT EXISTS (
    SELECT 1 FROM question_tag t WHERE t.question_id=q.id AND t.tag='업무시나리오'
  );

-- =========================================================
-- P.1.2 업무/데이터 요구 도출
-- =========================================================

-- SHORT
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_req_data, 'PRACTICAL', 'SHORT', 'NORMAL',
       '“배송 지연 시 고객에게 알림을 발송한다”라는 업무 요구로부터 '
       '데이터 요구사항을 최소 두 가지 이상 도출하여 서술하세요.',
       '주문ID/고객연락처/현재배송상태/지연사유/알림발송일시 등',
       '업무 요구는 행동(알림 발송)에 초점을 맞추고, 데이터 요구는 이를 위해 저장·조회해야 하는 항목을 명시하는 것입니다. '
       '예: 주문ID, 고객ID(또는 연락처), 현재배송상태, 지연사유, 알림발송일시 등을 도출할 수 있습니다.',
       'seed:prac:req_data:short1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_req_data AND mode='PRACTICAL' AND type='SHORT'
     AND source='seed:prac:req_data:short1'
);

-- LONG
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_req_data, 'PRACTICAL', 'LONG', 'NORMAL',
       '온라인 서점 시스템에서 “장바구니 기능”을 추가하려고 한다. '
       '업무 요구(사용자가 하고 싶은 일)와 데이터 요구(저장/조회해야 하는 데이터)를 구분하여, '
       '각각 3개 이상씩 예시를 들어 설명하세요.',
       '업무 요구: 상품담기/수량변경/삭제/주문으로 이동 등, 데이터 요구: 장바구니ID/상품ID/수량/담은시각/사용자ID 등',
       '예시 답안: '
       '업무 요구: (1) 사용자는 상품 상세 페이지에서 장바구니에 담을 수 있다. '
       '(2) 장바구니에서 수량을 변경할 수 있다. (3) 장바구니에서 선택한 상품만 주문으로 진행할 수 있다. '
       '데이터 요구: (1) 어느 사용자의 장바구니인지 구분하는 user_id, (2) 담긴 상품을 식별하는 product_id, '
       '(3) 수량 qty, (4) 담은 시각 created_at, (5) 옵션 정보(option_id) 등으로 정리할 수 있습니다.',
       'seed:prac:req_data:long1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_req_data AND mode='PRACTICAL' AND type='LONG'
     AND source='seed:prac:req_data:long1'
);

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '데이터요구'
FROM question q
WHERE q.source IN ('seed:prac:req_data:short1','seed:prac:req_data:long1')
  AND NOT EXISTS (
    SELECT 1 FROM question_tag t WHERE t.question_id=q.id AND t.tag='데이터요구'
  );

-- =========================================================
-- P.2.1 개념/논리/물리 모델링
-- =========================================================

-- SHORT
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_model_levels, 'PRACTICAL', 'SHORT', 'NORMAL',
       '개념 ERD와 논리 ERD의 차이점을 한 문장으로 요약해서 설명하세요.',
       '개념 ERD는 비즈니스 개념과 관계에 집중하고, 논리 ERD는 정규화된 속성과 키/관계를 포함해 DB 구조에 더 가깝게 표현한다.',
       '시험에서는 “개념은 비즈니스 관점, 논리는 DB 논리 구조 관점”이라는 키워드를 넣어 설명하면 좋습니다.',
       'seed:prac:model_levels:short1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_model_levels AND mode='PRACTICAL' AND type='SHORT'
     AND source='seed:prac:model_levels:short1'
);

-- LONG
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_model_levels, 'PRACTICAL', 'LONG', 'HARD',
       '주어진 업무 설명을 바탕으로 개념 모델 → 논리 모델 → 물리 모델로 발전시키는 과정을 서술하라는 실기 문제가 출제되었다. '
       '각 단계에서 수행해야 할 작업과 산출물을 중심으로, 세 단계의 차이를 정리해서 설명하세요.',
       '개념: 엔터티/관계 도출, 논리: 속성/키/정규화, 물리: 테이블/컬럼/인덱스/타입/제약 정의',
       '정답 예시: '
       '개념 모델 단계에서는 업무 용어를 정리해 “회원, 주문, 상품, 결제”와 같은 엔터티와 그 사이의 관계(1:N, N:M)를 도출한다. '
       '논리 모델 단계에서는 각 엔터티에 필요한 속성(주문일자, 수량 등)과 기본키/외래키를 정의하고, 정규화를 통해 중복과 이상현상을 줄인다. '
       '물리 모델 단계에서는 실제 DBMS에 맞게 테이블 이름/컬럼 타입/인덱스/제약조건을 정의하고, '
       '성능/용량을 고려해 파티셔닝이나 반정규화 여부를 결정한다.',
       'seed:prac:model_levels:long1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_model_levels AND mode='PRACTICAL' AND type='LONG'
     AND source='seed:prac:model_levels:long1'
);

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '모델링'
FROM question q
WHERE q.source IN ('seed:prac:model_levels:short1','seed:prac:model_levels:long1')
  AND NOT EXISTS (
    SELECT 1 FROM question_tag t WHERE t.question_id=q.id AND t.tag='모델링'
  );

-- =========================================================
-- P.2.2 정규화/반정규화
-- =========================================================

-- SHORT
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_normalization, 'PRACTICAL', 'SHORT', 'NORMAL',
       '정규화를 수행할 때 “삭제 이상(Deletion Anomaly)”가 무엇인지 예를 들어 한 문장으로 설명하세요.',
       '하나의 데이터를 삭제할 때 의도하지 않은 다른 정보까지 함께 사라지는 현상으로, 예를 들어 “수강기록 삭제 시 과목 정보까지 함께 사라지는 경우”를 들 수 있다.',
       '삭제 이상은 한 행이 여러 의미를 동시에 담고 있을 때 발생합니다. '
       '예: [학생+수강과목]이 한 테이블에 있을 때 마지막 수강 기록을 삭제하면 과목 자체 정보도 사라지는 상황입니다.',
       'seed:prac:normalization:short1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_normalization AND mode='PRACTICAL' AND type='SHORT'
     AND source='seed:prac:normalization:short1'
);

-- LONG
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_normalization, 'PRACTICAL', 'LONG', 'HARD',
       '다음과 같은 속성을 가진 테이블이 있다. '
       '(주문ID, 고객ID, 고객이름, 상품ID, 상품명, 주문일자, 상품단가, 카테고리명) '
       '이 테이블에서 발생할 수 있는 이상현상을 분석하고, 3정규형(3NF)까지 정규화하는 설계 방향을 서술하세요.',
       '고객/상품 반복, 갱신/삭제 이상 → 고객, 상품, 주문 엔터티로 분리 후 키/외래키 설계',
       '정답 예시: '
       '이 테이블은 고객과 상품 정보가 주문 레코드마다 반복되므로, 고객이름이나 상품명이 바뀌면 모든 행을 수정해야 하는 갱신 이상이 발생한다. '
       '또한 특정 상품이 더 이상 판매되지 않아 행을 삭제하면 해당 상품 정보도 함께 사라지는 삭제 이상이 생긴다. '
       '3NF 설계 방향: (1) 고객(고객ID, 고객이름, …), (2) 상품(상품ID, 상품명, 카테고리명, 상품단가, …), '
       '(3) 주문(주문ID, 고객ID, 주문일자), (4) 주문상세(주문ID, 상품ID, 수량, 주문단가) 등으로 분리해 '
       '각 테이블의 기본키에만 속성이 종속되도록 설계한다.',
       'seed:prac:normalization:long1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_normalization AND mode='PRACTICAL' AND type='LONG'
     AND source='seed:prac:normalization:long1'
);

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '정규화'
FROM question q
WHERE q.source IN ('seed:prac:normalization:short1','seed:prac:normalization:long1')
  AND NOT EXISTS (
    SELECT 1 FROM question_tag t WHERE t.question_id=q.id AND t.tag='정규화'
  );

-- =========================================================
-- P.3.1 SQL 쿼리 작성
-- =========================================================

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_sql_query, 'PRACTICAL', 'SHORT', 'NORMAL',
       '“회원(member)과 주문(order) 테이블에서 주문 금액 합계를 구하고, '
       '회원별 총 주문 금액이 10만원 이상인 회원ID와 합계를 조회하는 SQL의 핵심 절(SELECT~FROM~GROUP BY~HAVING)을 작성하세요.”',
       'SELECT m.member_id, SUM(o.amount) AS total_amount FROM member m JOIN orders o ON m.member_id = o.member_id GROUP BY m.member_id HAVING SUM(o.amount) >= 100000',
       'GROUP BY와 HAVING 절을 사용하는 문제입니다. 핵심은 [회원 별 집계]와 [조건: 합계가 10만원 이상]을 HAVING 절에 쓰는 것입니다.',
       'seed:prac:sql_query:short1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_sql_query AND mode='PRACTICAL' AND type='SHORT'
     AND source='seed:prac:sql_query:short1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_sql_query, 'PRACTICAL', 'LONG', 'NORMAL',
       '상품(product)과 주문상세(order_item) 테이블이 있다. '
       '각 상품별로 “지난 30일 동안의 판매 수량과 매출액”을 구하고, 매출액 기준 상위 5개 상품을 조회하는 SQL을 설계하세요. '
       '필요한 조건과 절(WHERE, GROUP BY, ORDER BY, LIMIT)을 중심으로 서술하세요.',
       'WHERE 주문일자 조건 + GROUP BY product_id + SUM(수량, 금액) + ORDER BY 매출액 DESC + LIMIT 5',
       '예시: '
       'SELECT p.product_id, p.product_name, SUM(oi.qty) AS total_qty, SUM(oi.qty * oi.price) AS total_amount '
       'FROM product p JOIN order_item oi ON p.product_id = oi.product_id '
       'WHERE oi.order_date >= CURRENT_DATE - INTERVAL 30 DAY '
       'GROUP BY p.product_id, p.product_name '
       'ORDER BY total_amount DESC '
       'LIMIT 5; 와 같은 형태로 작성할 수 있습니다.',
       'seed:prac:sql_query:long1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_sql_query AND mode='PRACTICAL' AND type='LONG'
     AND source='seed:prac:sql_query:long1'
);

INSERT INTO question_tag (question_id, tag)
SELECT q.id, 'SQL'
FROM question q
WHERE q.source IN ('seed:prac:sql_query:short1','seed:prac:sql_query:long1')
  AND NOT EXISTS (
    SELECT 1 FROM question_tag t WHERE t.question_id=q.id AND t.tag='SQL'
  );

-- =========================================================
-- P.3.2 인덱스/튜닝
-- =========================================================

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_sql_tuning, 'PRACTICAL', 'SHORT', 'NORMAL',
       '조회 조건에 항상 “회원ID + 주문일자 범위”가 함께 사용된다면, '
       '효율적인 인덱스 설계를 위해 어떤 컬럼 순서의 복합 인덱스를 고려해야 하는지 쓰고 이유를 설명하세요.',
       '(member_id, order_date) 순서의 복합 인덱스; 선행 컬럼인 member_id로 먼저 필터링하고 그 안에서 날짜 범위를 스캔해 효율이 좋다.',
       'WHERE 절이 (member_id = ? AND order_date BETWEEN ? AND ?) 형태라면, '
       '복합 인덱스의 선행 컬럼을 member_id로 두고, 두 번째에 order_date를 두는 것이 일반적으로 효율적입니다.',
       'seed:prac:sql_tuning:short1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_sql_tuning AND mode='PRACTICAL' AND type='SHORT'
     AND source='seed:prac:sql_tuning:short1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_sql_tuning, 'PRACTICAL', 'LONG', 'HARD',
       '주문 조회 화면에서 “최근 주문 100건”을 보여주는 쿼리가 매우 느리게 동작하고 있다. '
       '테이블 구조(orders: 주문ID, 회원ID, 주문일자, 상태, 금액, 등록일시, 수정일시)가 주어졌을 때, '
       '실기 관점에서 튜닝 방안을 3가지 이상 제시하세요. (인덱스, 쿼리 구조, 통계/요약 테이블 등)',
       '주문일자/등록일시 인덱스, 필요한 컬럼만 SELECT, LIMIT 사용, 요약 테이블/파티셔닝 등',
       '예시: '
       '(1) 최근 주문을 조회할 때 주문일자 또는 등록일시 기준으로 DESC 인덱스를 생성한다. '
       '(2) 화면에 필요한 컬럼만 SELECT하고, 불필요한 JOIN/서브쿼리를 제거한다. '
       '(3) 오래된 데이터와 분리하기 위해 기간별 파티셔닝을 적용하거나, “최근 주문”만 따로 관리하는 요약 테이블을 둔다.',
       'seed:prac:sql_tuning:long1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_sql_tuning AND mode='PRACTICAL' AND type='LONG'
     AND source='seed:prac:sql_tuning:long1'
);

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '튜닝'
FROM question q
WHERE q.source IN ('seed:prac:sql_tuning:short1','seed:prac:sql_tuning:long1')
  AND NOT EXISTS (
    SELECT 1 FROM question_tag t WHERE t.question_id=q.id AND t.tag='튜닝'
  );

-- =========================================================
-- P.4.1 트랜잭션/격리수준
-- =========================================================

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_tx_isolation, 'PRACTICAL', 'SHORT', 'NORMAL',
       '트랜잭션 격리수준 READ COMMITTED에서 발생할 수 있는 이상현상을 한 가지 쓰고, 간단히 설명하세요.',
       'Non-Repeatable Read; 같은 트랜잭션 안에서 같은 조건으로 두 번 조회했을 때, 중간에 다른 트랜잭션의 커밋으로 값이 달라지는 현상',
       'READ COMMITTED는 커밋된 데이터만 읽지만, 다른 트랜잭션이 중간에 값을 바꾸고 커밋하면 같은 조건으로 다시 조회했을 때 결과가 달라질 수 있습니다.',
       'seed:prac:tx_isolation:short1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_tx_isolation AND mode='PRACTICAL' AND type='SHORT'
     AND source='seed:prac:tx_isolation:short1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_tx_isolation, 'PRACTICAL', 'LONG', 'HARD',
       '은행 계좌 이체 기능을 설계할 때 트랜잭션과 격리수준을 어떻게 설정해야 하는지, '
       'ACID 관점에서 고려해야 할 사항을 포함하여 서술하세요.',
       '하나의 트랜잭션으로 묶고, 잔액 차감/증가가 모두 성공해야 커밋; 일관성과 격리 확보',
       '예시: '
       '송금 계좌의 잔액 차감과 수취 계좌의 잔액 증가를 하나의 트랜잭션으로 묶어 원자성을 보장한다. '
       '중간에 오류가 발생하면 전체 롤백한다. 격리수준은 동시성/성능을 고려해 REPEATABLE READ 또는 그 이상을 검토하며, '
       '동일 계좌에 대한 중복 이체를 방지하기 위해 행 수준 락을 적절히 사용한다.',
       'seed:prac:tx_isolation:long1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_tx_isolation AND mode='PRACTICAL' AND type='LONG'
     AND source='seed:prac:tx_isolation:long1'
);

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '트랜잭션'
FROM question q
WHERE q.source IN ('seed:prac:tx_isolation:short1','seed:prac:tx_isolation:long1')
  AND NOT EXISTS (
    SELECT 1 FROM question_tag t WHERE t.question_id=q.id AND t.tag='트랜잭션'
  );

-- =========================================================
-- P.4.2 동시성/락
-- =========================================================

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_concurrency, 'PRACTICAL', 'SHORT', 'NORMAL',
       '동시에 여러 사용자가 같은 재고 레코드를 수정하는 환경에서 발생할 수 있는 문제를 한 가지 쓰고, 이를 방지하기 위한 일반적인 방법을 설명하세요.',
       'Lost Update; 행 수준 락 또는 낙관적 락(버전 필드)을 사용해 방지',
       '같은 행을 두 트랜잭션이 읽고 각각 수정·커밋하면 먼저 커밋된 변경이 뒤에 커밋으로 덮어써지는 Lost Update가 발생할 수 있습니다. '
       '이를 막기 위해 SELECT ... FOR UPDATE 같은 비관적 잠금이나, 버전 칼럼을 이용한 낙관적 락을 사용할 수 있습니다.',
       'seed:prac:concurrency:short1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_concurrency AND mode='PRACTICAL' AND type='SHORT'
     AND source='seed:prac:concurrency:short1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_concurrency, 'PRACTICAL', 'LONG', 'HARD',
       '재고 관리 시스템에서 “입고/출고”가 매우 자주 발생하는 상품에 대해 '
       '락 경합과 데드락을 최소화하는 트랜잭션/락 설계 방안을 3가지 이상 제시하세요.',
       '짧은 트랜잭션, 일관된 락 순서, 재고 합계/로그 테이블 분리 등',
       '예시: '
       '(1) 트랜잭션 안에서 불필요한 조회/로직을 줄여 락 보유 시간을 최소화한다. '
       '(2) 여러 자원을 잠글 때는 항상 동일한 순서로 잠그도록 설계해 데드락 가능성을 줄인다. '
       '(3) 재고 합계 테이블과 이벤트 로그 테이블을 분리하고, 가능하면 합계는 배치로 계산하게 해 즉시 갱신 경합을 줄인다.',
       'seed:prac:concurrency:long1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_concurrency AND mode='PRACTICAL' AND type='LONG'
     AND source='seed:prac:concurrency:long1'
);

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '동시성'
FROM question q
WHERE q.source IN ('seed:prac:concurrency:short1','seed:prac:concurrency:long1')
  AND NOT EXISTS (
    SELECT 1 FROM question_tag t WHERE t.question_id=q.id AND t.tag='동시성'
  );

-- =========================================================
-- P.5.1 백업/복구
-- =========================================================

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_backup, 'PRACTICAL', 'SHORT', 'NORMAL',
       'RPO(Recovery Point Objective)와 RTO(Recovery Time Objective)의 차이를 한 문장씩 설명하세요.',
       'RPO는 “얼마 전 시점까지의 데이터만 복구되어도 되는가”를 의미하고, RTO는 “얼마 안에 서비스를 복구해야 하는가”를 의미한다.',
       '시험에서는 “데이터 손실 허용 범위(RPO)”와 “복구 완료까지 허용 시간(RTO)”라는 키워드를 중심으로 설명하면 됩니다.',
       'seed:prac:backup:short1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_backup AND mode='PRACTICAL' AND type='SHORT'
     AND source='seed:prac:backup:short1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_backup, 'PRACTICAL', 'LONG', 'NORMAL',
       '하루 한 번 전체 백업만 수행하던 시스템에서, '
       '업무 중요도가 높아져 RPO를 “15분 이내”로 줄여야 한다. '
       '이 요구를 만족시키기 위한 백업/로그 설계 방안을 2가지 이상 제시하세요.',
       '증분/로그 백업, 로그 전송, 이중화 등',
       '예시: '
       '(1) 1일 1회 전체 백업 + 15분 간격의 증분 백업 또는 트랜잭션 로그 백업을 수행한다. '
       '(2) 실시간 로그 전송(로그 배송) 또는 DB 이중화를 도입해 다른 서버에 거의 실시간으로 데이터를 복제한다. '
       '이렇게 하면 장애 시 최근 로그까지 적용하여 RPO를 단축할 수 있다.',
       'seed:prac:backup:long1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_backup AND mode='PRACTICAL' AND type='LONG'
     AND source='seed:prac:backup:long1'
);

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '백업복구'
FROM question q
WHERE q.source IN ('seed:prac:backup:short1','seed:prac:backup:long1')
  AND NOT EXISTS (
    SELECT 1 FROM question_tag t WHERE t.question_id=q.id AND t.tag='백업복구'
  );

-- =========================================================
-- P.5.2 장애 분석/개선
-- =========================================================

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_incident, 'PRACTICAL', 'SHORT', 'NORMAL',
       '장애 분석 보고서(Postmortem)에서 반드시 포함되어야 할 항목 두 가지를 쓰세요.',
       '장애 원인(직접/근본 원인)과 재발 방지 대책',
       '실제 현업에서도 원인과 재발 방지 대책이 가장 핵심입니다. 영향 범위, 타임라인 등을 함께 적으면 더 좋습니다.',
       'seed:prac:incident:short1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_incident AND mode='PRACTICAL' AND type='SHORT'
     AND source='seed:prac:incident:short1'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_incident, 'PRACTICAL', 'LONG', 'HARD',
       '야간 배치 작업 중 인덱스 재구성 작업으로 인해 전체 서비스가 30분 동안 응답 불가 상태가 되었다. '
       '실기 관점에서 장애 분석/개선 보고서에 포함될 수 있는 내용(원인, 영향, 개선방안)을 정리해서 서술하세요.',
       '원인: 피크 시간대 인덱스 재구성, 락/IO 경합; 영향: 전체 서비스 지연/중단; 개선: 작업 시간대 변경, 롤링/온라인 재구성, 사전 부하 테스트 등',
       '예시: '
       '원인: 피크 타임 직전에 대용량 인덱스 재구성을 수행하여, 장시간 테이블 락과 IO 경합이 발생했다. '
       '영향: 모든 조회/갱신 쿼리 응답이 지연되거나 타임아웃되었고, 평균 응답시간이 수 초 이상으로 증가했다. '
       '개선 방안: (1) 인덱스 재구성은 야간 저부하 시간대로 조정, (2) 온라인 인덱스 재구성 옵션 사용 검토, '
       '(3) 사전 부하 테스트를 통해 예상 작업 시간을 측정하고, 모니터링/알림을 강화한다.',
       'seed:prac:incident:long1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_incident AND mode='PRACTICAL' AND type='LONG'
     AND source='seed:prac:incident:long1'
);

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '장애분석'
FROM question q
WHERE q.source IN ('seed:prac:incident:short1','seed:prac:incident:long1')
  AND NOT EXISTS (
    SELECT 1 FROM question_tag t WHERE t.question_id=q.id AND t.tag='장애분석'
  );

SET FOREIGN_KEY_CHECKS = 1;
