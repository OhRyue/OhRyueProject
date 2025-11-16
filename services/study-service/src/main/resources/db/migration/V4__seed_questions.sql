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
       '분석 모델링에서는 기능, 정적 구조, 동작과 같은 여러 관점에서 시스템을 표현한다. (O/X)',
       'O',
       '기능(무엇을 하는가), 정적 구조(무엇으로 구성되는가), 동작(어떻게 동작하는가) 관점으로 나누어 모델링합니다.',
       'seed:v3:model:viewpoints'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '분석 모델링에서는 기능, 정적 구조, 동작과 같은 여러 관점에서 시스템을 표현한다.%'
);

-- DFD는 동작이 아니라 정적 구조? (오답)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_model, 'WRITTEN', 'OX', 'NORMAL',
       '데이터 흐름도(DFD)는 정적 구조를 표현하는 다이어그램으로, 데이터 간 관계만 나타낸다. (O/X)',
       'X',
       'DFD는 데이터의 흐름과 처리 과정을 나타내는 기능/동작 중심 다이어그램입니다. 정적 구조는 ERD/클래스 등으로 표현합니다.',
       'seed:v3:model:dfd'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '데이터 흐름도(DFD)는 정적 구조를 표현하는 다이어그램으로, 데이터 간 관계만 나타낸다.%'
);

-- ERD의 역할
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_model, 'WRITTEN', 'OX', 'EASY',
       'ERD(Entity-Relationship Diagram)는 엔터티와 관계를 통해 데이터 구조를 표현하는 모델이다. (O/X)',
       'O',
       'ERD는 데이터베이스 설계 전 단계에서 엔터티/속성/관계를 표현하는 대표적인 정적 모델입니다.',
       'seed:v3:model:erd'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'ERD(Entity-Relationship Diagram)는 엔터티와 관계를 통해 데이터 구조를 표현하는 모델이다.%'
);

-- =========================================================
-- 2) WRITTEN OX – 1.2.1 UI 요구사항 확인
-- =========================================================

-- 사용자 중심 설계
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_ui, 'WRITTEN', 'OX', 'EASY',
       'UI 요구사항 확인 단계에서는 최종 사용자의 작업 맥락과 사용 환경을 파악하는 것이 중요하다. (O/X)',
       'O',
       '사용자의 목적, 빈도, 디바이스, 제약사항 등을 파악해야 적절한 화면 흐름과 컴포넌트를 설계할 수 있습니다.',
       'seed:v3:ui:context'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_ui AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'UI 요구사항 확인 단계에서는 최종 사용자의 작업 맥락과 사용 환경을 파악하는 것이 중요하다.%'
);

-- 일관성의 중요성 축소? (오답)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_ui, 'WRITTEN', 'OX', 'NORMAL',
       '화면 설계에서 일관성은 사용자 경험에 큰 영향을 주지 않으므로, 화면마다 자유롭게 구성해도 무방하다. (O/X)',
       'X',
       '레이블, 색상, 컴포넌트 배치의 일관성은 학습 비용을 줄이고 오류를 줄이는 핵심 원칙입니다.',
       'seed:v3:ui:consistency'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_ui AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '화면 설계에서 일관성은 사용자 경험에 큰 영향을 주지 않으므로, 화면마다 자유롭게 구성해도 무방하다.%'
);

-- =========================================================
-- 3) WRITTEN OX – 1.3.1 공통 모듈 설계 / 1.3.2 객체지향 설계
-- =========================================================

-- 공통 모듈 재사용성
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_common, 'WRITTEN', 'OX', 'EASY',
       '공통 모듈은 여러 서브시스템에서 반복적으로 사용되는 기능을 캡슐화하여 재사용성을 높이는 것을 목표로 한다. (O/X)',
       'O',
       '공통 모듈화는 중복 구현을 줄이고 일관된 규칙/정책을 한 곳에서 관리할 수 있게 해 줍니다.',
       'seed:v3:common:reuse'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_common AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '공통 모듈은 여러 서브시스템에서 반복적으로 사용되는 기능을 캡슐화하여 재사용성을 높이는 것을 목표로 한다.%'
);

-- 결합도/응집도 방향
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_ood, 'WRITTEN', 'OX', 'NORMAL',
       '객체지향 설계에서 모듈 간 결합도는 높게, 모듈 내부 응집도는 낮게 만드는 것이 바람직하다. (O/X)',
       'X',
       '좋은 설계는 낮은 결합도와 높은 응집도를 목표로 합니다.',
       'seed:v3:ood:cohesion-coupling'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_ood AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '객체지향 설계에서 모듈 간 결합도는 높게, 모듈 내부 응집도는 낮게 만드는 것이 바람직하다.%'
);

-- 캡슐화/정보은닉
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_ood, 'WRITTEN', 'OX', 'EASY',
       '캡슐화는 객체의 내부 구현을 감추고 공용 인터페이스만 노출하여 변경 영향을 최소화하는 것을 의미한다. (O/X)',
       'O',
       '캡슐화를 통해 모듈 내부 구현 변경이 외부 코드에 미치는 영향을 줄일 수 있습니다.',
       'seed:v3:ood:encapsulation'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_ood AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '캡슐화는 객체의 내부 구현을 감추고 공용 인터페이스만 노출하여 변경 영향을 최소화하는 것을 의미한다.%'
);

-- =========================================================
-- 4) WRITTEN OX – 1.4.x 인터페이스 설계(요구/대상/상세)
-- =========================================================

-- 인터페이스 요구사항: 가용성/성능 포함
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_if_req, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 요구사항에는 단순히 데이터 포맷만 정의하면 되며, 가용성이나 성능 목표는 포함하지 않는다. (O/X)',
       'X',
       '인터페이스 요구사항에는 포맷뿐 아니라 응답 시간, 처리량, 가용성, 오류 처리 정책 등 비기능 요구도 함께 포함해야 합니다.',
       'seed:v3:if:req-nfr'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_if_req AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 요구사항에는 단순히 데이터 포맷만 정의하면 되며, 가용성이나 성능 목표는 포함하지 않는다.%'
);

-- 인터페이스 대상 식별: 내부/외부 모두
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_if_target, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 대상 식별 단계에서는 내부 서브시스템 간 연동뿐 아니라 외부 시스템과의 연동 대상도 함께 파악해야 한다. (O/X)',
       'O',
       '내부/외부 연동 대상과 데이터 흐름을 모두 파악해야 전체 인터페이스 구조를 설계할 수 있습니다.',
       'seed:v3:if:targets'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_if_target AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 대상 식별 단계에서는 내부 서브시스템 간 연동뿐 아니라 외부 시스템과의 연동 대상도 함께 파악해야 한다.%'
);

-- 인터페이스 상세 설계: 타임아웃/재시도
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_if_detail, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 상세 설계에서는 요청/응답 포맷만 정의하면 되며, 타임아웃이나 재시도 정책은 구현 단계에서 개발자가 임의로 결정한다. (O/X)',
       'X',
       '타임아웃, 재시도, 서킷브레이커 등 안정성 관련 정책은 상세 설계 단계에서 미리 정의해야 일관된 구현이 가능합니다.',
       'seed:v3:if:timeout-retry'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_if_detail AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 상세 설계에서는 요청/응답 포맷만 정의하면 되며, 타임아웃이나 재시도 정책은 구현 단계에서 개발자가 임의로 결정한다.%'
);

-- =========================================================
-- 5) WRITTEN MCQ – 모델링/화면설계/공통 모듈/객체지향/인터페이스
-- =========================================================

-- 모델링 관점 MCQ
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_model, 'WRITTEN', 'MCQ', 'NORMAL',
       '다음 중 시스템의 동작(상태 변화나 메시지 흐름)을 표현하는 데 가장 적합한 다이어그램은?',
       'C',
       '상태/시퀀스/활동 다이어그램은 동작/흐름을, ERD/클래스는 정적 구조를 표현합니다.',
       'seed:v3:model:behavior-diagram'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 시스템의 동작(상태 변화나 메시지 흐름)을 표현하는 데 가장 적합한 다이어그램은?%'
);

SET @q_mcq_model_behavior := (
  SELECT id FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 시스템의 동작(상태 변화나 메시지 흐름)을 표현하는 데 가장 적합한 다이어그램은?%'
   LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_model_behavior,'A','클래스 다이어그램',0
WHERE @q_mcq_model_behavior IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_model_behavior AND label='A');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_model_behavior,'B','ERD',0
WHERE @q_mcq_model_behavior IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_model_behavior AND label='B');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_model_behavior,'C','시퀀스 다이어그램',1
WHERE @q_mcq_model_behavior IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_model_behavior AND label='C');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_model_behavior,'D','패키지 다이어그램',0
WHERE @q_mcq_model_behavior IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_model_behavior AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_model_behavior, '모델링'
WHERE @q_mcq_model_behavior IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_model_behavior AND tag='모델링');

-- UI 원칙 MCQ
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_ui, 'WRITTEN', 'MCQ', 'EASY',
       '다음 중 좋은 UI 설계 원칙으로 가장 거리가 먼 것은?',
       'D',
       '좋은 UI는 일관성, 가시성, 피드백이 중요하며, 복잡한 용어 남발은 피해야 합니다.',
       'seed:v3:ui:principles'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_ui AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 좋은 UI 설계 원칙으로 가장 거리가 먼 것은?%'
);

SET @q_mcq_ui_principles := (
  SELECT id FROM question
   WHERE topic_id=@topic_ui AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 좋은 UI 설계 원칙으로 가장 거리가 먼 것은?%'
   LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_ui_principles,'A','일관된 컴포넌트와 색상 사용',0
WHERE @q_mcq_ui_principles IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_ui_principles AND label='A');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_ui_principles,'B','사용자 행동에 대한 명확한 피드백 제공',0
WHERE @q_mcq_ui_principles IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_ui_principles AND label='B');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_ui_principles,'C','중요 정보의 시각적 강조',0
WHERE @q_mcq_ui_principles IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_ui_principles AND label='C');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_ui_principles,'D','전문용어를 최대한 많이 사용하여 화면을 채운다',1
WHERE @q_mcq_ui_principles IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_ui_principles AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_ui_principles, 'UI'
WHERE @q_mcq_ui_principles IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_ui_principles AND tag='UI');

-- 공통 모듈/재사용 MCQ
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_common, 'WRITTEN', 'MCQ', 'NORMAL',
       '다음 중 공통 모듈 설계 시 고려해야 할 사항으로 가장 거리가 먼 것은?',
       'D',
       '공통 모듈은 재사용성, 변경 용이성, 성능을 고려해야 하며, 특정 화면 로직에 종속되도록 설계하면 재사용성이 떨어집니다.',
       'seed:v3:common:considerations'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_common AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 공통 모듈 설계 시 고려해야 할 사항으로 가장 거리가 먼 것은?%'
);

SET @q_mcq_common := (
  SELECT id FROM question
   WHERE topic_id=@topic_common AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 공통 모듈 설계 시 고려해야 할 사항으로 가장 거리가 먼 것은?%'
   LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_common,'A','여러 도메인에서 공통으로 사용하는 규칙 캡슐화',0
WHERE @q_mcq_common IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_common AND label='A');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_common,'B','명확한 인터페이스와 입력/출력 정의',0
WHERE @q_mcq_common IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_common AND label='B');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_common,'C','변경 가능성을 고려한 느슨한 결합 구조',0
WHERE @q_mcq_common IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_common AND label='C');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_common,'D','특정 화면에만 쓰이는 UI 이벤트 처리 로직 포함',1
WHERE @q_mcq_common IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_common AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_common, '공통모듈'
WHERE @q_mcq_common IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_common AND tag='공통모듈');

-- 인터페이스 상세 설계 MCQ (성능/타임아웃)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_if_detail, 'WRITTEN', 'MCQ', 'NORMAL',
       '외부 시스템과의 인터페이스 상세 설계 시 가장 먼저 정의해야 할 내용으로 가장 적절한 것은?',
       'B',
       '요청/응답 포맷뿐 아니라 타임아웃, 재시도, 오류 코드 정책 등 안정성 관련 요구를 우선적으로 정의해야 합니다.',
       'seed:v3:if:design-priority'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_if_detail AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '외부 시스템과의 인터페이스 상세 설계 시 가장 먼저 정의해야 할 내용으로 가장 적절한 것은?%'
);

SET @q_mcq_if := (
  SELECT id FROM question
   WHERE topic_id=@topic_if_detail AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '외부 시스템과의 인터페이스 상세 설계 시 가장 먼저 정의해야 할 내용으로 가장 적절한 것은?%'
   LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_if,'A','연동 담당 개발자의 이름과 연락처',0
WHERE @q_mcq_if IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_if AND label='A');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_if,'B','요청/응답 포맷과 타임아웃·재시도 정책',1
WHERE @q_mcq_if IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_if AND label='B');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_if,'C','개발팀의 주간 회의 일정',0
WHERE @q_mcq_if IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_if AND label='C');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_if,'D','사무실 네트워크 프린터 설정',0
WHERE @q_mcq_if IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_if AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_if, '인터페이스설계'
WHERE @q_mcq_if IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_if AND tag='인터페이스설계');

SET FOREIGN_KEY_CHECKS = 1;
