-- =========================================
-- Topic ID 상수 (3과목 데이터베이스 구축 – 3.x.x)
-- =========================================
SET @tp_31101 := 13101; -- 3.1.1 데이터 모델링 개요
SET @tp_31102 := 13102; -- 3.1.2 엔터티·관계·식별자
SET @tp_32101 := 13201; -- 3.2.1 정규화
SET @tp_32102 := 13202; -- 3.2.2 반정규화/테이블 통합·분할
SET @tp_33101 := 13301; -- 3.3.1 물리 데이터베이스 설계
SET @tp_33102 := 13302; -- 3.3.2 인덱스 설계
SET @tp_34101 := 13401; -- 3.4.1 DDL/DML 기본
SET @tp_34102 := 13402; -- 3.4.2 조인·집계
SET @tp_34103 := 13403; -- 3.4.3 서브쿼리·집합연산
SET @tp_34104 := 13404; -- 3.4.4 고급 SQL/튜닝

/* ========================================================
 * 3.1.1 데이터 모델링 개요  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_31101, 'WRITTEN', 'OX', 'EASY',
       '데이터 모델링은 업무에서 필요한 데이터를 구조적으로 표현하여 정보시스템 설계의 기반을 마련하는 활동이다.',
       'O',
       '데이터 모델링은 업무 데이터를 이해하고 구조화해 이후 논리·물리 DB 설계의 기초를 제공하는 핵심 활동입니다.',
       'seed:3.1.1:modeling-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_31101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '데이터 모델링은 업무에서 필요한 데이터를 구조적으로 표현하여 정보시스템 설계의 기반을 마련하는 활동이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_31101, 'WRITTEN', 'OX', 'NORMAL',
       '데이터 모델링은 사용자 요구를 수집하는 요구 분석 단계와는 무관하며, 개발 막바지에 단독으로 수행된다.',
       'X',
       '데이터 모델링은 요구 분석과 밀접하게 연관되어 초기 단계부터 반복적으로 수행됩니다.',
       'seed:3.1.1:modeling-phase:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_31101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '데이터 모델링은 사용자 요구를 수집하는 요구 분석 단계와는 무관하며, 개발 막바지에 단독으로 수행된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_31101, 'WRITTEN', 'OX', 'EASY',
       '개념 데이터 모델은 업무를 데이터 관점에서 추상화한 모델로, 기술 독립적인 표현을 사용하는 것이 일반적이다.',
       'O',
       '개념 모델은 특정 DBMS에 종속되지 않는 추상적인 수준에서 엔터티와 관계를 정의합니다.',
       'seed:3.1.1:conceptual:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_31101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '개념 데이터 모델은 업무를 데이터 관점에서 추상화한 모델로, 기술 독립적인 표현을 사용하는 것이 일반적이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_31101, 'WRITTEN', 'OX', 'NORMAL',
       '논리 데이터 모델은 특정 DBMS의 저장 구조와 인덱스 구조까지 포함하여 표현하는 물리 지향 모델이다.',
       'X',
       '논리 모델은 엔터티, 속성, 관계, 식별자 등을 정의하지만 물리적 저장 구조나 인덱스 구조까지는 포함하지 않습니다.',
       'seed:3.1.1:logical-vs-physical:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_31101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '논리 데이터 모델은 특정 DBMS의 저장 구조와 인덱스 구조까지 포함하여 표현하는 물리 지향 모델이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_31101, 'WRITTEN', 'OX', 'NORMAL',
       '물리 데이터 모델에서는 데이터 타입, 인덱스, 파티션, 파일 그룹 등 구체적인 저장 구조를 설계한다.',
       'O',
       '물리 설계 단계에서는 논리 모델을 기반으로 DBMS별 데이터 타입과 저장·접근 구조를 구체화합니다.',
       'seed:3.1.1:physical:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_31101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '물리 데이터 모델에서는 데이터 타입, 인덱스, 파티션, 파일 그룹 등 구체적인 저장 구조를 설계한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_31101, 'WRITTEN', 'OX', 'EASY',
       '좋은 데이터 모델은 중복을 최소화하고, 일관성을 유지하며, 변경 요구에 쉽게 대응할 수 있어야 한다.',
       'O',
       '데이터 중복 최소화, 무결성 확보, 확장성과 유연성은 좋은 데이터 모델의 대표적인 특성입니다.',
       'seed:3.1.1:good-model:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_31101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '좋은 데이터 모델은 중복을 최소화하고, 일관성을 유지하며, 변경 요구에 쉽게 대응할 수 있어야 한다.%'
);

/* ========================================================
 * 3.1.2 엔터티·관계·식별자  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_31102, 'WRITTEN', 'OX', 'EASY',
       '엔터티(Entity)는 업무에서 관리해야 할 객체나 개념을 의미하며, 속성(Attribute)으로 특징을 표현한다.',
       'O',
       '고객, 주문, 상품과 같은 관리 대상 객체를 엔터티로 정의하고, 그 특성을 속성으로 표현합니다.',
       'seed:3.1.2:entity-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_31102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '엔터티(Entity)는 업무에서 관리해야 할 객체나 개념을 의미하며, 속성(Attribute)으로 특징을 표현한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_31102, 'WRITTEN', 'OX', 'NORMAL',
       '관계(Relationship)는 두 엔터티 간의 연관성을 의미하며, 1:1, 1:N, N:M과 같은 참여 형태로 표현될 수 있다.',
       'O',
       '고객-주문(1:N), 학생-강좌(N:M)처럼 엔터티 간 연관성을 관계로 표현하고, 참여 형태를 함께 정의합니다.',
       'seed:3.1.2:relationship:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_31102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '관계(Relationship)는 두 엔터티 간의 연관성을 의미하며, 1:1, 1:N, N:M과 같은 참여 형태로 표현될 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_31102, 'WRITTEN', 'OX', 'NORMAL',
       '식별자(Identifier)는 엔터티 내에 있는 각 인스턴스를 유일하게 구분할 수 있는 속성 또는 속성의 집합을 말한다.',
       'O',
       '기본키가 바로 식별자의 대표적인 예로, 엔터티 인스턴스를 유일하게 식별하는 역할을 합니다.',
       'seed:3.1.2:identifier:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_31102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '식별자(Identifier)는 엔터티 내에 있는 각 인스턴스를 유일하게 구분할 수 있는 속성 또는 속성의 집합을 말한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_31102, 'WRITTEN', 'OX', 'NORMAL',
       '복합 식별자는 두 개 이상의 속성을 조합하여 하나의 인스턴스를 유일하게 구분하는 식별자를 말한다.',
       'O',
       '예를 들어, (학생ID, 과목코드)와 같이 여러 속성을 묶어 유일성을 보장하는 경우가 복합 식별자입니다.',
       'seed:3.1.2:composite-id:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_31102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '복합 식별자는 두 개 이상의 속성을 조합하여 하나의 인스턴스를 유일하게 구분하는 식별자를 말한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_31102, 'WRITTEN', 'OX', 'NORMAL',
       '외래키(Foreign Key)는 다른 엔터티의 기본키를 참조하여 관계를 표현하지만, 항상 NULL을 허용해서는 안 된다.',
       'X',
       '외래키는 관계의 선택성(필수/선택)에 따라 NULL 허용 여부가 달라질 수 있습니다.',
       'seed:3.1.2:foreign-key:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_31102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '외래키(Foreign Key)는 다른 엔터티의 기본키를 참조하여 관계를 표현하지만, 항상 NULL을 허용해서는 안 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_31102, 'WRITTEN', 'OX', 'EASY',
       '관계 차수(cardinality)는 한 엔터티 인스턴스가 다른 엔터티 인스턴스와 몇 개까지 연결될 수 있는지를 나타내는 개념이다.',
       'O',
       '1:1, 1:N, N:M과 같은 차수는 관계의 연결 개수 범위를 보여주는 중요한 정보입니다.',
       'seed:3.1.2:cardinality:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_31102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '관계 차수(cardinality)는 한 엔터티 인스턴스가 다른 엔터티 인스턴스와 몇 개까지 연결될 수 있는지를 나타내는 개념이다.%'
);

/* ========================================================
 * 3.2.1 정규화  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_32101, 'WRITTEN', 'OX', 'EASY',
       '정규화는 이상(Anomaly)을 줄이고 데이터 중복을 감소시키기 위한 데이터 구조화 기법이다.',
       'O',
       '정규화를 통해 삽입·삭제·갱신 이상을 줄이고, 논리적으로 일관된 데이터 구조를 만들 수 있습니다.',
       'seed:3.2.1:normalization-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_32101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정규화는 이상(Anomaly)을 줄이고 데이터 중복을 감소시키기 위한 데이터 구조화 기법이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_32101, 'WRITTEN', 'OX', 'NORMAL',
       '제1정규형(1NF)은 모든 속성이 원자값(Atomic Value)을 가지도록 테이블을 분해한 상태를 말한다.',
       'O',
       '1NF에서는 반복되는 속성이나 다중 값 속성을 제거하고, 하나의 속성에는 더 이상 분해되지 않는 값만 저장합니다.',
       'seed:3.2.1:1nf:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_32101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '제1정규형(1NF)은 모든 속성이 원자값(Atomic Value)을 가지도록 테이블을 분해한 상태를 말한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_32101, 'WRITTEN', 'OX', 'NORMAL',
       '제2정규형(2NF)은 후보키가 복합키일 때 부분 함수 종속을 제거하여, 모든 비키 속성이 기본키 전체에 완전 함수 종속이 되도록 한 상태이다.',
       'O',
       '2NF에서는 키의 일부에만 종속된 속성을 분리해 기본키 전체에 종속되도록 테이블을 재구성합니다.',
       'seed:3.2.1:2nf:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_32101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '제2정규형(2NF)은 후보키가 복합키일 때 부분 함수 종속을 제거하여, 모든 비키 속성이 기본키 전체에 완전 함수 종속이 되도록 한 상태이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_32101, 'WRITTEN', 'OX', 'NORMAL',
       '제3정규형(3NF)은 기본키가 아닌 속성이 다른 비키 속성에 종속되는 이행 함수 종속을 허용하는 정규형이다.',
       'X',
       '3NF에서는 비키 속성이 다른 비키 속성에 이행적으로 종속되는 경우 이를 제거해야 합니다.',
       'seed:3.2.1:3nf:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_32101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '제3정규형(3NF)은 기본키가 아닌 속성이 다른 비키 속성에 종속되는 이행 함수 종속을 허용하는 정규형이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_32101, 'WRITTEN', 'OX', 'NORMAL',
       '정규화를 지나치게 수행하면 조인이 많아져 조회 성능이 저하될 수 있으므로, 상황에 따라 반정규화를 고려할 수 있다.',
       'O',
       '정규화와 성능 사이에서 균형을 맞추기 위해 일부 중복을 허용하는 반정규화를 적용하기도 합니다.',
       'seed:3.2.1:over-normalization:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_32101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정규화를 지나치게 수행하면 조인이 많아져 조회 성능이 저하될 수 있으므로, 상황에 따라 반정규화를 고려할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_32101, 'WRITTEN', 'OX', 'EASY',
       '정규화의 주요 목표 중 하나는 데이터 무결성을 유지하고 이상 현상을 줄이는 것이다.',
       'O',
       '정규화를 통해 무결성을 강화하고 삽입·삭제·갱신 이상을 방지할 수 있습니다.',
       'seed:3.2.1:goal:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_32101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정규화의 주요 목표 중 하나는 데이터 무결성을 유지하고 이상 현상을 줄이는 것이다.%'
);

/* ========================================================
 * 3.2.2 반정규화/테이블 통합·분할  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_32102, 'WRITTEN', 'OX', 'EASY',
       '반정규화는 성능 향상이나 구현 편의를 위해 의도적으로 데이터 중복을 허용하는 설계 기법이다.',
       'O',
       '조회 성능 개선 등을 위해 정규화된 구조를 일부 완화해 중복을 허용하는 것이 반정규화입니다.',
       'seed:3.2.2:denormalization-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_32102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '반정규화는 성능 향상이나 구현 편의를 위해 의도적으로 데이터 중복을 허용하는 설계 기법이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_32102, 'WRITTEN', 'OX', 'NORMAL',
       '반정규화를 적용하면 항상 시스템 품질이 향상되므로, 가능하면 모든 테이블에 적용하는 것이 좋다.',
       'X',
       '반정규화는 쓰기 성능 저하와 데이터 불일치 위험을 높일 수 있어, 명확한 근거가 있을 때 제한적으로 사용해야 합니다.',
       'seed:3.2.2:denormalization-risk:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_32102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '반정규화를 적용하면 항상 시스템 품질이 향상되므로, 가능하면 모든 테이블에 적용하는 것이 좋다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_32102, 'WRITTEN', 'OX', 'NORMAL',
       '테이블 통합은 조인이 과도하게 발생하는 여러 테이블을 하나로 합쳐 조회 성능을 향상시키기 위한 반정규화 기법 중 하나이다.',
       'O',
       '자주 함께 조회되는 테이블을 통합해 조인 횟수를 줄이는 것도 대표적인 반정규화 기법입니다.',
       'seed:3.2.2:merge:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_32102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '테이블 통합은 조인이 과도하게 발생하는 여러 테이블을 하나로 합쳐 조회 성능을 향상시키기 위한 반정규화 기법 중 하나이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_32102, 'WRITTEN', 'OX', 'NORMAL',
       '테이블 분할은 하나의 테이블을 수평 또는 수직으로 나누어 관리하기 쉬운 구조를 만드는 기법이다.',
       'O',
       '수평 분할은 행 기준, 수직 분할은 열 기준으로 나누어 성능 또는 관리성을 개선합니다.',
       'seed:3.2.2:split:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_32102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '테이블 분할은 하나의 테이블을 수평 또는 수직으로 나누어 관리하기 쉬운 구조를 만드는 기법이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_32102, 'WRITTEN', 'OX', 'NORMAL',
       '반정규화는 주로 읽기(조회) 성능 개선 목적이므로, 쓰기(입력/갱신) 성능이나 유지보수성에는 전혀 영향을 주지 않는다.',
       'X',
       '반정규화로 중복 데이터가 생기면 입력/갱신 시 여러 곳을 수정해야 하므로 쓰기 성능과 유지보수성에 영향을 줍니다.',
       'seed:3.2.2:write-impact:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_32102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '반정규화는 주로 읽기(조회) 성능 개선 목적이므로, 쓰기(입력/갱신) 성능이나 유지보수성에는 전혀 영향을 주지 않는다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_32102, 'WRITTEN', 'OX', 'EASY',
       '반정규화 적용 여부는 성능 요구, 데이터 일관성, 유지보수 난이도 등을 종합적으로 고려하여 결정해야 한다.',
       'O',
       '단순 성능뿐 아니라 전체 시스템 품질 관점에서 반정규화의 효과와 위험을 함께 판단해야 합니다.',
       'seed:3.2.2:decision:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_32102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '반정규화 적용 여부는 성능 요구, 데이터 일관성, 유지보수 난이도 등을 종합적으로 고려하여 결정해야 한다.%'
);

/* ========================================================
 * 3.3.1 물리 데이터베이스 설계  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_33101, 'WRITTEN', 'OX', 'EASY',
       '물리 데이터베이스 설계에서는 논리 모델을 기반으로 데이터 타입, 인덱스, 파티션, 저장 구조 등을 구체화한다.',
       'O',
       '논리 모델이 무엇을 저장할지 정의한다면, 물리 설계는 어떻게 저장·접근할지를 정의합니다.',
       'seed:3.3.1:physical-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_33101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '물리 데이터베이스 설계에서는 논리 모델을 기반으로 데이터 타입, 인덱스, 파티션, 저장 구조 등을 구체화한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_33101, 'WRITTEN', 'OX', 'NORMAL',
       '물리 설계 단계에서는 테이블과 인덱스의 저장 공간(테이블스페이스 등)을 고려하지 않아도 된다.',
       'X',
       '대용량 테이블의 저장 위치, 테이블스페이스 분리 등은 성능과 관리 측면에서 중요한 물리 설계 요소입니다.',
       'seed:3.3.1:tablespace:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_33101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '물리 설계 단계에서는 테이블과 인덱스의 저장 공간(테이블스페이스 등)을 고려하지 않아도 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_33101, 'WRITTEN', 'OX', 'NORMAL',
       '파티셔닝은 대용량 테이블을 여러 파티션으로 나누어 성능과 관리성을 향상시키는 물리 설계 기법이다.',
       'O',
       '파티션 단위로 백업·복구, 인덱스 재구성, 데이터 삭제 등을 수행할 수 있어 관리와 성능에 유리합니다.',
       'seed:3.3.1:partition:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_33101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '파티셔닝은 대용량 테이블을 여러 파티션으로 나누어 성능과 관리성을 향상시키는 물리 설계 기법이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_33101, 'WRITTEN', 'OX', 'NORMAL',
       '물리 설계에서 선택한 데이터 타입은 저장 공간과 성능에 영향을 줄 수 있으므로, 가능한 가장 큰 타입을 사용하는 것이 바람직하다.',
       'X',
       '불필요하게 큰 타입을 사용하면 저장 공간 낭비와 캐시 효율 저하를 초래할 수 있어, 적정 크기의 타입 선택이 중요합니다.',
       'seed:3.3.1:datatype:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_33101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '물리 설계에서 선택한 데이터 타입은 저장 공간과 성능에 영향을 줄 수 있으므로, 가능한 가장 큰 타입을 사용하는 것이 바람직하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_33101, 'WRITTEN', 'OX', 'NORMAL',
       '물리 데이터베이스 설계는 백업·복구 전략과 장애 복구 시간(RTO), 데이터 손실 허용 범위(RPO)와는 전혀 관련이 없다.',
       'X',
       '백업/복구 전략과 RTO/RPO 요구는 물리 구조(파티션, 로그 보관, 스토리지 구성)에 큰 영향을 미칩니다.',
       'seed:3.3.1:rpo-rto:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_33101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '물리 데이터베이스 설계는 백업·복구 전략과 장애 복구 시간(RTO), 데이터 손실 허용 범위(RPO)와는 전혀 관련이 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_33101, 'WRITTEN', 'OX', 'EASY',
       '물리 설계 결과는 실제 DB 생성 스크립트와 운영 환경에서의 튜닝 기준이 된다.',
       'O',
       '물리 설계 산출물은 CREATE TABLE/INDEX 스크립트와 초기 튜닝 가이드의 근거가 됩니다.',
       'seed:3.3.1:script:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_33101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '물리 설계 결과는 실제 DB 생성 스크립트와 운영 환경에서의 튜닝 기준이 된다.%'
);

/* ========================================================
 * 3.3.2 인덱스 설계  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_33102, 'WRITTEN', 'OX', 'EASY',
       '인덱스는 특정 컬럼에 대한 검색 속도를 향상시키기 위해 별도로 생성하는 데이터 구조이다.',
       'O',
       '인덱스는 책의 색인처럼 원하는 데이터를 더 빨리 찾기 위한 보조 구조입니다.',
       'seed:3.3.2:index-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_33102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인덱스는 특정 컬럼에 대한 검색 속도를 향상시키기 위해 별도로 생성하는 데이터 구조이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_33102, 'WRITTEN', 'OX', 'NORMAL',
       '인덱스는 조회 성능에만 영향을 주며, INSERT/UPDATE/DELETE와 같은 쓰기 작업에는 전혀 영향을 주지 않는다.',
       'X',
       '인덱스가 많을수록 쓰기 작업 시 인덱스까지 함께 갱신해야 하므로 오버헤드가 증가합니다.',
       'seed:3.3.2:write-cost:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_33102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인덱스는 조회 성능에만 영향을 주며, INSERT/UPDATE/DELETE와 같은 쓰기 작업에는 전혀 영향을 주지 않는다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_33102, 'WRITTEN', 'OX', 'NORMAL',
       '카디널리티(서로 다른 값의 개수)가 낮은 컬럼은 인덱스를 생성해도 효율이 떨어질 수 있다.',
       'O',
       '성별처럼 값 종류가 적은 컬럼은 인덱스를 사용해도 대부분의 레코드를 읽게 되어 효율이 좋지 않을 수 있습니다.',
       'seed:3.3.2:cardinality:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_33102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '카디널리티(서로 다른 값의 개수)가 낮은 컬럼은 인덱스를 생성해도 효율이 떨어질 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_33102, 'WRITTEN', 'OX', 'NORMAL',
       '복합 인덱스에서는 인덱스에 정의된 컬럼 순서가 중요하지 않으며, 어느 컬럼으로 조회해도 동일한 효율을 얻을 수 있다.',
       'X',
       '복합 인덱스는 선두 컬럼부터 사용되므로, WHERE 절에서 자주 사용되는 조건 순서를 고려해 인덱스 컬럼 순서를 설계해야 합니다.',
       'seed:3.3.2:composite:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_33102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '복합 인덱스에서는 인덱스에 정의된 컬럼 순서가 중요하지 않으며, 어느 컬럼으로 조회해도 동일한 효율을 얻을 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_33102, 'WRITTEN', 'OX', 'NORMAL',
       '인덱스는 항상 많이 만들수록 좋으므로, 모든 컬럼에 인덱스를 생성하는 것이 이상적이다.',
       'X',
       '인덱스는 조회 패턴과 카디널리티를 고려해 선택적으로 생성해야 하며, 과도한 인덱스는 쓰기 성능과 관리 비용을 증가시킵니다.',
       'seed:3.3.2:too-many:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_33102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인덱스는 항상 많이 만들수록 좋으므로, 모든 컬럼에 인덱스를 생성하는 것이 이상적이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_33102, 'WRITTEN', 'OX', 'EASY',
       '인덱스 설계 시에는 자주 조회되는 컬럼과 조인에 사용되는 컬럼을 중심으로 생성 여부를 검토하는 것이 일반적이다.',
       'O',
       'WHERE, JOIN, ORDER BY, GROUP BY에 자주 사용되는 컬럼이 인덱스 후보가 됩니다.',
       'seed:3.3.2:design:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_33102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인덱스 설계 시에는 자주 조회되는 컬럼과 조인에 사용되는 컬럼을 중심으로 생성 여부를 검토하는 것이 일반적이다.%'
);

/* ========================================================
 * 3.4.1 DDL/DML 기본  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34101, 'WRITTEN', 'OX', 'EASY',
       'DDL(Data Definition Language)는 CREATE, ALTER, DROP과 같이 객체 구조를 정의·변경·삭제하는 명령어를 말한다.',
       'O',
       '테이블, 인덱스, 뷰 등 데이터베이스 객체 구조를 정의하는 명령어가 DDL입니다.',
       'seed:3.4.1:ddl:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'DDL(Data Definition Language)는 CREATE, ALTER, DROP과 같이 객체 구조를 정의·변경·삭제하는 명령어를 말한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34101, 'WRITTEN', 'OX', 'EASY',
       'DML(Data Manipulation Language)는 SELECT, INSERT, UPDATE, DELETE와 같이 데이터를 조회·조작하는 명령어를 말한다.',
       'O',
       'DML은 테이블에 저장된 실제 데이터를 조회하거나 변경할 때 사용하는 명령어입니다.',
       'seed:3.4.1:dml:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'DML(Data Manipulation Language)는 SELECT, INSERT, UPDATE, DELETE와 같이 데이터를 조회·조작하는 명령어를 말한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34101, 'WRITTEN', 'OX', 'NORMAL',
       'CREATE TABLE 문으로 테이블을 생성한 후에는, ALTER TABLE 문을 사용하더라도 컬럼 추가나 삭제는 할 수 없다.',
       'X',
       'ALTER TABLE 문을 사용하면 컬럼 추가, 삭제, 데이터 타입 변경 등 구조 변경을 수행할 수 있습니다.',
       'seed:3.4.1:alter:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'CREATE TABLE 문으로 테이블을 생성한 후에는, ALTER TABLE 문을 사용하더라도 컬럼 추가나 삭제는 할 수 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34101, 'WRITTEN', 'OX', 'NORMAL',
       'DELETE 문은 테이블 구조 자체를 삭제하며, TRUNCATE 문은 데이터만 삭제하고 구조는 유지한다.',
       'X',
       'DELETE와 TRUNCATE 모두 데이터만 삭제하며, 테이블 구조는 유지됩니다. DROP이 구조 자체를 삭제하는 명령입니다.',
       'seed:3.4.1:delete-truncate-drop:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'DELETE 문은 테이블 구조 자체를 삭제하며, TRUNCATE 문은 데이터만 삭제하고 구조는 유지한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34101, 'WRITTEN', 'OX', 'NORMAL',
       'INSERT INTO 테이블 VALUES(...) 구문에서는 컬럼 목록을 생략해도 되므로, 항상 컬럼 개수와 순서를 신경 쓸 필요가 없다.',
       'X',
       '컬럼 목록을 생략하면 테이블 정의 순서와 컬럼 개수에 맞춰 값을 입력해야 하므로, 안전하게 사용하려면 컬럼 목록을 명시하는 것이 좋습니다.',
       'seed:3.4.1:insert:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'INSERT INTO 테이블 VALUES(...) 구문에서는 컬럼 목록을 생략해도 되므로, 항상 컬럼 개수와 순서를 신경 쓸 필요가 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34101, 'WRITTEN', 'OX', 'EASY',
       'UPDATE 문에서는 WHERE 절을 생략하면 해당 테이블의 모든 행이 갱신 대상이 된다.',
       'O',
       'WHERE 없이 UPDATE를 수행하면 전체 행이 변경되므로, 실무에서는 WHERE 절 작성에 특히 주의해야 합니다.',
       'seed:3.4.1:update-where:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'UPDATE 문에서는 WHERE 절을 생략하면 해당 테이블의 모든 행이 갱신 대상이 된다.%'
);

/* ========================================================
 * 3.4.2 조인·집계  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34102, 'WRITTEN', 'OX', 'EASY',
       'INNER JOIN은 두 테이블에서 조인 조건을 만족하는 행만 결과로 반환하는 조인 방식이다.',
       'O',
       'INNER JOIN은 교집합처럼 두 테이블 모두에 존재하는 매칭 행만 반환합니다.',
       'seed:3.4.2:inner-join:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'INNER JOIN은 두 테이블에서 조인 조건을 만족하는 행만 결과로 반환하는 조인 방식이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34102, 'WRITTEN', 'OX', 'NORMAL',
       'LEFT OUTER JOIN은 왼쪽 테이블의 모든 행과, 조건이 일치하는 오른쪽 테이블의 행을 함께 반환하며, 일치하지 않는 오른쪽 값은 NULL로 채운다.',
       'O',
       'LEFT OUTER JOIN은 왼쪽 테이블을 기준으로 결과를 구성하는 조인 방식입니다.',
       'seed:3.4.2:left-join:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'LEFT OUTER JOIN은 왼쪽 테이블의 모든 행과, 조건이 일치하는 오른쪽 테이블의 행을 함께 반환하며, 일치하지 않는 오른쪽 값은 NULL로 채운다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34102, 'WRITTEN', 'OX', 'NORMAL',
       'GROUP BY 절을 사용할 때, SELECT 절에는 집계 함수가 적용된 컬럼 또는 GROUP BY 대상 컬럼만 포함하는 것이 원칙이다.',
       'O',
       '표준 SQL에서는 GROUP BY에 포함되지 않은 컬럼이 SELECT에 나타나면 정의되지 않은 결과가 될 수 있습니다.',
       'seed:3.4.2:group-by:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'GROUP BY 절을 사용할 때, SELECT 절에는 집계 함수가 적용된 컬럼 또는 GROUP BY 대상 컬럼만 포함하는 것이 원칙이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34102, 'WRITTEN', 'OX', 'NORMAL',
       'HAVING 절은 그룹화되기 전의 개별 행을 필터링하는 데 사용되며, WHERE 절은 그룹화 후 집계 결과를 필터링하는 데 사용된다.',
       'X',
       'WHERE는 그룹화 전에 개별 행을 필터링하고, HAVING은 GROUP BY 이후 집계 결과를 대상으로 조건을 적용합니다.',
       'seed:3.4.2:where-having:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'HAVING 절은 그룹화되기 전의 개별 행을 필터링하는 데 사용되며, WHERE 절은 그룹화 후 집계 결과를 필터링하는 데 사용된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34102, 'WRITTEN', 'OX', 'EASY',
       '집계 함수 COUNT(*)는 NULL을 포함한 모든 행의 개수를 반환한다.',
       'O',
       'COUNT(*)는 NULL 여부에 상관없이 전체 행 수를 세고, COUNT(컬럼명)은 NULL이 아닌 값만 집계합니다.',
       'seed:3.4.2:count:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '집계 함수 COUNT(*)는 NULL을 포함한 모든 행의 개수를 반환한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34102, 'WRITTEN', 'OX', 'NORMAL',
       '조인 조건이 누락된 다중 테이블 SELECT는 의도치 않은 카티전 곱(Cartesian Product)을 발생시킬 수 있다.',
       'O',
       '조인 조건 없이 여러 테이블을 SELECT하면 모든 조합이 생성되는 카티전 곱이 발생합니다.',
       'seed:3.4.2:cartesian:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '조인 조건이 누락된 다중 테이블 SELECT는 의도치 않은 카티전 곱(Cartesian Product)을 발생시킬 수 있다.%'
);

/* ========================================================
 * 3.4.3 서브쿼리·집합연산  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34103, 'WRITTEN', 'OX', 'EASY',
       '서브쿼리는 다른 SQL 문장 안에 포함된 SELECT 문으로, WHERE, FROM, SELECT 절 등에서 사용할 수 있다.',
       'O',
       '서브쿼리는 메인 쿼리의 조건이나 가상 테이블 등 다양한 위치에서 활용됩니다.',
       'seed:3.4.3:subquery-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '서브쿼리는 다른 SQL 문장 안에 포함된 SELECT 문으로, WHERE, FROM, SELECT 절 등에서 사용할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34103, 'WRITTEN', 'OX', 'NORMAL',
       '단일 행 서브쿼리는 서브쿼리 결과가 여러 행을 반환해도 =, >, < 연산자와 함께 사용할 수 있다.',
       'X',
       '단일 행 서브쿼리는 한 행만 반환해야 하며, 여러 행을 반환하면 RUN-TIME 오류가 발생합니다.',
       'seed:3.4.3:single-row:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '단일 행 서브쿼리는 서브쿼리 결과가 여러 행을 반환해도 =, >, < 연산자와 함께 사용할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34103, 'WRITTEN', 'OX', 'NORMAL',
       'IN 연산자와 서브쿼리를 함께 사용하면, 서브쿼리 결과 집합에 포함되는 값 중 하나라도 일치하면 참으로 평가된다.',
       'O',
       'IN은 "여러 값 중 하나"를 만족하면 참이 되는 연산자입니다.',
       'seed:3.4.3:in:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'IN 연산자와 서브쿼리를 함께 사용하면, 서브쿼리 결과 집합에 포함되는 값 중 하나라도 일치하면 참으로 평가된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34103, 'WRITTEN', 'OX', 'NORMAL',
       'EXISTS 서브쿼리는 결과 집합의 행 수를 반환하며, 행 수가 많을수록 큰 값을 반환한다.',
       'X',
       'EXISTS는 결과 집합이 한 행 이상 존재하는지만 확인하며, 참/거짓만 반환합니다.',
       'seed:3.4.3:exists:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'EXISTS 서브쿼리는 결과 집합의 행 수를 반환하며, 행 수가 많을수록 큰 값을 반환한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34103, 'WRITTEN', 'OX', 'EASY',
       'UNION 연산자는 두 SELECT 결과를 합집합으로 결합하며, 기본적으로 중복 행을 제거한다.',
       'O',
       '중복을 포함해 모두 보여주고 싶을 때는 UNION ALL을 사용합니다.',
       'seed:3.4.3:union:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'UNION 연산자는 두 SELECT 결과를 합집합으로 결합하며, 기본적으로 중복 행을 제거한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34103, 'WRITTEN', 'OX', 'NORMAL',
       'INTERSECT 연산자는 두 SELECT 결과에서 공통으로 존재하지 않는 행만 반환하는 차집합 연산이다.',
       'X',
       'INTERSECT는 교집합, MINUS/EXCEPT가 차집합 연산입니다.',
       'seed:3.4.3:intersect-minus:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'INTERSECT 연산자는 두 SELECT 결과에서 공통으로 존재하지 않는 행만 반환하는 차집합 연산이다.%'
);

/* ========================================================
 * 3.4.4 고급 SQL/튜닝  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34104, 'WRITTEN', 'OX', 'EASY',
       '실행 계획(Execution Plan)은 SQL을 수행하기 위해 옵티마이저가 선택한 액세스 경로와 조인 순서 등을 보여주는 정보이다.',
       'O',
       '실행 계획을 통해 테이블 액세스 방식, 인덱스 사용 여부, 조인 순서 등을 확인할 수 있습니다.',
       'seed:3.4.4:plan:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34104 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '실행 계획(Execution Plan)은 SQL을 수행하기 위해 옵티마이저가 선택한 액세스 경로와 조인 순서 등을 보여주는 정보이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34104, 'WRITTEN', 'OX', 'NORMAL',
       '풀 테이블 스캔(Full Table Scan)은 항상 비효율적인 액세스 방식이므로 어떤 경우에도 사용해서는 안 된다.',
       'X',
       '데이터 양이 적거나 대부분의 행을 조회하는 경우 풀 스캔이 오히려 효율적일 수 있습니다.',
       'seed:3.4.4:full-scan:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34104 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '풀 테이블 스캔(Full Table Scan)은 항상 비효율적인 액세스 방식이므로 어떤 경우에도 사용해서는 안 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34104, 'WRITTEN', 'OX', 'NORMAL',
       'SQL 튜닝에서는 불필요한 SELECT * 사용을 줄이고, 실제로 필요한 컬럼만 조회하는 것이 바람직하다.',
       'O',
       'SELECT *는 네트워크 전송량과 I/O를 증가시킬 수 있으므로 필요한 컬럼만 명시하는 것이 좋습니다.',
       'seed:3.4.4:select-star:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34104 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'SQL 튜닝에서는 불필요한 SELECT * 사용을 줄이고, 실제로 필요한 컬럼만 조회하는 것이 바람직하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34104, 'WRITTEN', 'OX', 'NORMAL',
       'WHERE 절에서 인덱스 컬럼에 함수나 연산을 적용하면, 인덱스를 제대로 활용하지 못할 수 있다.',
       'O',
       '함수나 연산을 적용하면 인덱스를 사용하지 못하고 풀 스캔으로 전환될 수 있어, 가능한 한 원본 컬럼에 조건을 적용하는 것이 좋습니다.',
       'seed:3.4.4:function:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34104 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'WHERE 절에서 인덱스 컬럼에 함수나 연산을 적용하면, 인덱스를 제대로 활용하지 못할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34104, 'WRITTEN', 'OX', 'NORMAL',
       '많은 양의 데이터를 일괄로 삭제해야 할 때, 하나의 DELETE 문으로 모두 삭제하는 것만이 항상 최선의 방법이다.',
       'X',
       '대량 삭제는 트랜잭션 로그 폭증과 락 문제를 유발할 수 있어, 배치로 나누거나 파티션 드랍 등 다른 방법을 고려할 수 있습니다.',
       'seed:3.4.4:bulk-delete:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34104 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '많은 양의 데이터를 일괄로 삭제해야 할 때, 하나의 DELETE 문으로 모두 삭제하는 것만이 항상 최선의 방법이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT 1, @tp_34104, 'WRITTEN', 'OX', 'EASY',
       'SQL 튜닝 시에는 통계 정보가 최신 상태인지 확인하는 것도 중요하다.',
       'O',
       '옵티마이저는 통계 정보를 기반으로 실행 계획을 선택하므로, 오래된 통계는 비효율적인 계획으로 이어질 수 있습니다.',
       'seed:3.4.4:statistics:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_34104 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'SQL 튜닝 시에는 통계 정보가 최신 상태인지 확인하는 것도 중요하다.%'
);
