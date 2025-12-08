SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

-- =========================================================
-- 토픽 ID 상수 정의 (cert-service.topic.id 와 동일하게 사용)
-- =========================================================
SET @topic_analysis   := 11101;  -- 1.1.1 현행 시스템 분석
SET @topic_req        := 11102;  -- 1.1.2 요구사항 확인
SET @topic_model      := 11103;  -- 1.1.3 분석 모델 확인
SET @topic_ui         := 11201;  -- 1.2.1 UI 요구사항 확인
SET @topic_common     := 11301;  -- 1.3.1 공통 모듈 설계
SET @topic_ood        := 11302;  -- 1.3.2 객체 지향 설계
SET @topic_if_req     := 11401;  -- 1.4.1 인터페이스 요구사항 확인
SET @topic_if_target  := 11402;  -- 1.4.2 인터페이스 대상 식별
SET @topic_if_detail  := 11403;  -- 1.4.3 인터페이스 상세 설계

-- =========================================================
-- 1) WRITTEN OX – 1.1.3 분석 모델 확인 (모델링 관점)
-- =========================================================

-- 모델링 관점 3축
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_model, 'WRITTEN', 'OX', 'EASY',
       '분석 모델링에서는 기능, 정적 구조, 동작과 같은 여러 관점에서 시스템을 표현한다.',
       'O',
       '기능(무엇을 하는가), 정적 구조(무엇으로 구성되는가), 동작(어떻게 동작하는가) 관점으로 나누어 모델링합니다.',
       'seed:v3:model:viewpoints'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '분석 모델링에서는 기능, 정적 구조, 동작과 같은 여러 관점에서 시스템을 표현한다.%'
);

SET @q_ox_model_viewpoints := (
  SELECT id FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '분석 모델링에서는 기능, 정적 구조, 동작과 같은 여러 관점에서 시스템을 표현한다.%'
   LIMIT 1
);

-- 태그 변경: 모델링_관점 -> 모델링
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_model_viewpoints, '모델링'
WHERE @q_ox_model_viewpoints IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_model_viewpoints AND tag='모델링');

-- DFD는 동작이 아니라 정적 구조? (오답)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_model, 'WRITTEN', 'OX', 'NORMAL',
       '데이터 흐름도(DFD)는 정적 구조를 표현하는 다이어그램으로, 데이터 간 관계만 나타낸다.',
       'X',
       'DFD는 데이터의 흐름과 처리 과정을 나타내는 기능/동작 중심 다이어그램입니다. 정적 구조는 ERD/클래스 등으로 표현합니다.',
       'seed:v3:model:dfd'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '데이터 흐름도(DFD)는 정적 구조를 표현하는 다이어그램으로, 데이터 간 관계만 나타낸다.%'
);

SET @q_ox_model_dfd := (
  SELECT id FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '데이터 흐름도(DFD)는 정적 구조를 표현하는 다이어그램으로, 데이터 간 관계만 나타낸다.%'
   LIMIT 1
);

-- 태그 변경: DFD -> 모델링
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_model_dfd, '모델링'
WHERE @q_ox_model_dfd IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_model_dfd AND tag='모델링');

-- ERD의 역할
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_model, 'WRITTEN', 'OX', 'EASY',
       'ERD(Entity-Relationship Diagram)는 엔터티와 관계를 통해 데이터 구조를 표현하는 모델이다.',
       'O',
       'ERD는 데이터베이스 설계 전 단계에서 엔터티/속성/관계를 표현하는 대표적인 정적 모델입니다.',
       'seed:v3:model:erd'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'ERD(Entity-Relationship Diagram)는 엔터티와 관계를 통해 데이터 구조를 표현하는 모델이다.%'
);

SET @q_ox_model_erd := (
  SELECT id FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'ERD(Entity-Relationship Diagram)는 엔터티와 관계를 통해 데이터 구조를 표현하는 모델이다.%'
   LIMIT 1
);

-- 태그 변경: ERD -> 데이터요구 (엔터티/속성 도출 관점)
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_model_erd, '데이터요구'
WHERE @q_ox_model_erd IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_model_erd AND tag='데이터요구');

-- =========================================================
-- 2) WRITTEN OX – 1.2.1 UI 요구사항 확인
-- =========================================================

-- 사용자 중심 설계
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_ui, 'WRITTEN', 'OX', 'EASY',
       'UI 요구사항 확인 단계에서는 최종 사용자의 작업 맥락과 사용 환경을 파악하는 것이 중요하다.',
       'O',
       '사용자의 목적, 빈도, 디바이스, 제약사항 등을 파악해야 적절한 화면 흐름과 컴포넌트를 설계할 수 있습니다.',
       'seed:v3:ui:context'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_ui AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'UI 요구사항 확인 단계에서는 최종 사용자의 작업 맥락과 사용 환경을 파악하는 것이 중요하다.%'
);

SET @q_ox_ui_context := (
  SELECT id FROM question
   WHERE topic_id=@topic_ui AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'UI 요구사항 확인 단계에서는 최종 사용자의 작업 맥락과 사용 환경을 파악하는 것이 중요하다.%'
   LIMIT 1
);

-- 태그 변경: UI_사용자맥락 -> UI
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_ui_context, 'UI'
WHERE @q_ox_ui_context IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_ui_context AND tag='UI');

-- 일관성의 중요성 축소? (오답)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_ui, 'WRITTEN', 'OX', 'NORMAL',
       '화면 설계에서 일관성은 사용자 경험에 큰 영향을 주지 않으므로, 화면마다 자유롭게 구성해도 무방하다.',
       'X',
       '레이블, 색상, 컴포넌트 배치의 일관성은 학습 비용을 줄이고 오류를 줄이는 핵심 원칙입니다.',
       'seed:v3:ui:consistency'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_ui AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '화면 설계에서 일관성은 사용자 경험에 큰 영향을 주지 않으므로, 화면마다 자유롭게 구성해도 무방하다.%'
);

SET @q_ox_ui_consistency := (
  SELECT id FROM question
   WHERE topic_id=@topic_ui AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '화면 설계에서 일관성은 사용자 경험에 큰 영향을 주지 않으므로, 화면마다 자유롭게 구성해도 무방하다.%'
   LIMIT 1
);

-- 태그 변경: UI_일관성 -> UI
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_ui_consistency, 'UI'
WHERE @q_ox_ui_consistency IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_ui_consistency AND tag='UI');

-- =========================================================
-- 3) WRITTEN OX – 1.3.1 공통 모듈 설계 / 1.3.2 객체지향 설계
-- =========================================================

-- 공통 모듈 재사용성
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_common, 'WRITTEN', 'OX', 'EASY',
       '공통 모듈은 여러 서브시스템에서 반복적으로 사용되는 기능을 캡슐화하여 재사용성을 높이는 것을 목표로 한다.',
       'O',
       '공통 모듈화는 중복 구현을 줄이고 일관된 규칙/정책을 한 곳에서 관리할 수 있게 해 줍니다.',
       'seed:v3:common:reuse'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_common AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '공통 모듈은 여러 서브시스템에서 반복적으로 사용되는 기능을 캡슐화하여 재사용성을 높이는 것을 목표로 한다.%'
);

SET @q_ox_common_reuse := (
  SELECT id FROM question
   WHERE topic_id=@topic_common AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '공통 모듈은 여러 서브시스템에서 반복적으로 사용되는 기능을 캡슐화하여 재사용성을 높이는 것을 목표로 한다.%'
   LIMIT 1
);

-- 태그 변경: 공통모듈_재사용성 -> 공통모듈
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_common_reuse, '공통모듈'
WHERE @q_ox_common_reuse IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_common_reuse AND tag='공통모듈');

-- 결합도/응집도 방향
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_ood, 'WRITTEN', 'OX', 'NORMAL',
       '객체지향 설계에서 모듈 간 결합도는 높게, 모듈 내부 응집도는 낮게 만드는 것이 바람직하다.',
       'X',
       '좋은 설계는 낮은 결합도와 높은 응집도를 목표로 합니다.',
       'seed:v3:ood:cohesion-coupling'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_ood AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '객체지향 설계에서 모듈 간 결합도는 높게, 모듈 내부 응집도는 낮게 만드는 것이 바람직하다.%'
);

SET @q_ox_ood_coupling := (
  SELECT id FROM question
   WHERE topic_id=@topic_ood AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '객체지향 설계에서 모듈 간 결합도는 높게, 모듈 내부 응집도는 낮게 만드는 것이 바람직하다.%'
   LIMIT 1
);

-- 태그 변경: 결합도응집도 -> 설계원칙
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_ood_coupling, '설계원칙'
WHERE @q_ox_ood_coupling IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_ood_coupling AND tag='설계원칙');

-- 캡슐화/정보은닉
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_ood, 'WRITTEN', 'OX', 'EASY',
       '캡슐화는 객체의 내부 구현을 감추고 공용 인터페이스만 노출하여 변경 영향을 최소화하는 것을 의미한다.',
       'O',
       '캡슐화를 통해 모듈 내부 구현 변경이 외부 코드에 미치는 영향을 줄일 수 있습니다.',
       'seed:v3:ood:encapsulation'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_ood AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '캡슐화는 객체의 내부 구현을 감추고 공용 인터페이스만 노출하여 변경 영향을 최소화하는 것을 의미한다.%'
);

SET @q_ox_ood_encapsulation := (
  SELECT id FROM question
   WHERE topic_id=@topic_ood AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '캡슐화는 객체의 내부 구현을 감추고 공용 인터페이스만 노출하여 변경 영향을 최소화하는 것을 의미한다.%'
   LIMIT 1
);

-- 태그 변경: 캡슐화 -> OOP
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_ood_encapsulation, 'OOP'
WHERE @q_ox_ood_encapsulation IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_ood_encapsulation AND tag='OOP');

-- =========================================================
-- 4) WRITTEN OX – 1.4.x 인터페이스 설계(요구/대상/상세)
-- =========================================================

-- 인터페이스 요구사항: 가용성/성능 포함
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_if_req, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 요구사항에는 단순히 데이터 포맷만 정의하면 되며, 가용성이나 성능 목표는 포함하지 않는다.',
       'X',
       '인터페이스 요구사항에는 포맷뿐 아니라 응답 시간, 처리량, 가용성, 오류 처리 정책 등 비기능 요구도 함께 포함해야 합니다.',
       'seed:v3:if:req-nfr'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_if_req AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 요구사항에는 단순히 데이터 포맷만 정의하면 되며, 가용성이나 성능 목표는 포함하지 않는다.%'
);

SET @q_ox_if_req_nfr := (
  SELECT id FROM question
   WHERE topic_id=@topic_if_req AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 요구사항에는 단순히 데이터 포맷만 정의하면 되며, 가용성이나 성능 목표는 포함하지 않는다.%'
   LIMIT 1
);

-- 태그 변경: 인터페이스_NFR -> 인터페이스요구
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_if_req_nfr, '인터페이스요구'
WHERE @q_ox_if_req_nfr IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_if_req_nfr AND tag='인터페이스요구');

-- 인터페이스 대상 식별: 내부/외부 모두
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_if_target, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 대상 식별 단계에서는 내부 서브시스템 간 연동뿐 아니라 외부 시스템과의 연동 대상도 함께 파악해야 한다.',
       'O',
       '내부/외부 연동 대상과 데이터 흐름을 모두 파악해야 전체 인터페이스 구조를 설계할 수 있습니다.',
       'seed:v3:if:targets'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_if_target AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 대상 식별 단계에서는 내부 서브시스템 간 연동뿐 아니라 외부 시스템과의 연동 대상도 함께 파악해야 한다.%'
);

SET @q_ox_if_targets := (
  SELECT id FROM question
   WHERE topic_id=@topic_if_target AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 대상 식별 단계에서는 내부 서브시스템 간 연동뿐 아니라 외부 시스템과의 연동 대상도 함께 파악해야 한다.%'
   LIMIT 1
);

-- 태그 변경: 인터페이스대상 -> 연계대상
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_if_targets, '연계대상'
WHERE @q_ox_if_targets IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_if_targets AND tag='연계대상');

-- 인터페이스 상세 설계: 타임아웃/재시도
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_if_detail, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 상세 설계에서는 요청/응답 포맷만 정의하면 되며, 타임아웃이나 재시도 정책은 구현 단계에서 개발자가 임의로 결정한다.',
       'X',
       '타임아웃, 재시도, 서킷브레이커 등 안정성 관련 정책은 상세 설계 단계에서 미리 정의해야 일관된 구현이 가능합니다.',
       'seed:v3:if:timeout-retry'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_if_detail AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 상세 설계에서는 요청/응답 포맷만 정의하면 되며, 타임아웃이나 재시도 정책은 구현 단계에서 개발자가 임의로 결정한다.%'
);

SET @q_ox_if_timeout_retry := (
  SELECT id FROM question
   WHERE topic_id=@topic_if_detail AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 상세 설계에서는 요청/응답 포맷만 정의하면 되며, 타임아웃이나 재시도 정책은 구현 단계에서 개발자가 임의로 결정한다.%'
   LIMIT 1
);

-- 태그 변경: 인터페이스_타임아웃재시도 -> 인터페이스설계
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_if_timeout_retry, '인터페이스설계'
WHERE @q_ox_if_timeout_retry IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_if_timeout_retry AND tag='인터페이스설계');

SET FOREIGN_KEY_CHECKS = 1;
