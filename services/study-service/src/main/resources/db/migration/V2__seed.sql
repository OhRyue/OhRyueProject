-- V2__seed.sql (study-service)
SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

-- =========================================================
-- 토픽 ID 상수 정의 (cert-service.topic.id 와 논리적으로 맞춘다고 가정)
-- =========================================================
-- 예시: 11101 = 현행 시스템 분석, 11102 = 요구사항 확인, 11403 = 인터페이스 상세 설계, 20001 = 실기 설계 토픽
SET @topic_analysis   := 11101;
SET @topic_oop        := 11102;
SET @topic_interface  := 11403;
SET @topic_practical  := 20001;

-- =========================================================
-- 1) 필기 OX 문제 (현행 시스템 분석)
-- =========================================================
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_analysis, 'WRITTEN', 'OX', 'NORMAL',
       '현행 시스템 분석 단계에서는 응답 시간과 처리량과 같은 성능 지표를 수집해야 한다. (O/X)',
       'O',
       '성능 지표는 향후 요구사항 정의와 설계 제약을 결정하므로 필수로 수집합니다.',
       'seed:analysis:perf'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석 단계에서는 응답 시간과 처리량과 같은 성능 지표를 수집해야 한다.%'
);

SET @q_ox_analysis := (
  SELECT id FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석 단계에서는 응답 시간과 처리량과 같은 성능 지표를 수집해야 한다.%'
   LIMIT 1
);

INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_analysis, '현행시스템'
WHERE @q_ox_analysis IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_analysis AND tag='현행시스템');

-- 추가 OX (비기능 요구)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_analysis, 'WRITTEN', 'OX', 'EASY',
       '현행 시스템 분석에서는 기능 요구사항뿐 아니라 보안·성능·가용성과 같은 비기능 요구도 함께 수집해야 한다. (O/X)',
       'O',
       '비기능 요구는 아키텍처 제약과 용량 산정에 직접 영향을 주므로, 현행 분석 단계에서부터 함께 수집해야 합니다.',
       'seed:analysis:nfr'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석에서는 기능 요구사항뿐 아니라 보안·성능·가용성과 같은 비기능 요구도 함께 수집해야 한다.%'
);

-- =========================================================
-- 2) 필기 MCQ 문제 (OOP)
-- =========================================================
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_oop, 'WRITTEN', 'MCQ', 'NORMAL',
       '다음 중 개방-폐쇄 원칙(OCP)에 대한 설명으로 옳은 것은 무엇인가?',
       'B',
       'OCP는 기능을 확장할 수 있도록 열어두되 기존 코드를 수정하지 않도록 닫아두라는 원칙입니다.',
       'seed:oop:ocp'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_oop AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 개방-폐쇄 원칙(OCP)에 대한 설명으로 옳은 것은 무엇인가?%'
);

SET @q_mcq_ocp := (
  SELECT id FROM question
   WHERE topic_id=@topic_oop AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 개방-폐쇄 원칙(OCP)에 대한 설명으로 옳은 것은 무엇인가?%'
   LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_ocp,'A','요구사항이 바뀔 때마다 클래스를 직접 수정하는 것이 좋다',0
WHERE @q_mcq_ocp IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_ocp AND label='A');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_ocp,'B','확장은 허용하되 기존 코드는 변경하지 않는다',1
WHERE @q_mcq_ocp IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_ocp AND label='B');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_ocp,'C','추상화보다 상속을 최우선으로 사용한다',0
WHERE @q_mcq_ocp IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_ocp AND label='C');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_ocp,'D','단일 책임 원칙을 무시해도 된다',0
WHERE @q_mcq_ocp IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_ocp AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_ocp, 'OOP'
WHERE @q_mcq_ocp IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_ocp AND tag='OOP');

-- 추가 MCQ: DIP
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_oop, 'WRITTEN', 'MCQ', 'NORMAL',
       '의존성 역전 원칙(DIP)에 대한 설명으로 가장 적절한 것은?',
       'C',
       'DIP는 고수준/저수준 모듈 모두 추상화에 의존하도록 설계해 구현 변경에 덜 민감하게 만드는 원칙입니다.',
       'seed:oop:dip'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_oop AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '의존성 역전 원칙(DIP)에 대한 설명으로 가장 적절한 것은?%'
);

SET @q_mcq_dip := (
  SELECT id FROM question
   WHERE topic_id=@topic_oop AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '의존성 역전 원칙(DIP)에 대한 설명으로 가장 적절한 것은?%'
   LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_dip,'A','상속 계층을 가능한 얕게 유지하는 원칙이다.',0
WHERE @q_mcq_dip IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_dip AND label='A');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_dip,'B','모든 모듈이 구체 클래스를 직접 참조하게 하는 원칙이다.',0
WHERE @q_mcq_dip IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_dip AND label='B');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_dip,'C','고수준/저수준 모듈 모두 추상화에 의존하게 하는 원칙이다.',1
WHERE @q_mcq_dip IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_dip AND label='C');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_dip,'D','데이터베이스 스키마를 캡슐화하는 원칙이다.',0
WHERE @q_mcq_dip IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_dip AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_dip, 'DIP'
WHERE @q_mcq_dip IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_dip AND tag='DIP');

-- =========================================================
-- 3) 필기 MCQ 문제 (인터페이스 설계)
-- =========================================================
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_interface, 'WRITTEN', 'MCQ', 'NORMAL',
       '외부 시스템과의 인터페이스 상세 설계 시 가장 우선 고려할 요소는 무엇인가?',
       'A',
       '트래픽 규모 대비 스루풋/지연시간 목표와 타임아웃·재시도·서킷브레이커 등 안정성 패턴을 우선 정의해야 합니다.',
       'seed:interface:design'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_interface AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '외부 시스템과의 인터페이스 상세 설계 시 가장 우선 고려할 요소는 무엇인가?%'
);

SET @q_mcq_interface := (
  SELECT id FROM question
   WHERE topic_id=@topic_interface AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '외부 시스템과의 인터페이스 상세 설계 시 가장 우선 고려할 요소는 무엇인가?%'
   LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_interface,'A','요청/응답 포맷과 타임아웃·재시도 정책을 정의한다',1
WHERE @q_mcq_interface IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_interface AND label='A');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_interface,'B','UI 테마 색상을 맞춘다',0
WHERE @q_mcq_interface IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_interface AND label='B');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_interface,'C','사무실 조명을 조정한다',0
WHERE @q_mcq_interface IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_interface AND label='C');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_interface,'D','개발자 별명을 정한다',0
WHERE @q_mcq_interface IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_interface AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_interface, '인터페이스설계'
WHERE @q_mcq_interface IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_interface AND tag='인터페이스설계');

-- =========================================================
-- 4) 실기 SHORT/LONG 문제 (PRACTICAL)
-- =========================================================
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_practical, 'PRACTICAL', 'SHORT', 'NORMAL',
       '서킷 브레이커 패턴의 목적을 한 문장으로 설명하세요.',
       '연쇄 실패 방지, 임계치 도달 시 호출 단락 후 복구 확인',
       '서킷 브레이커는 실패율이 임계치를 넘으면 호출을 차단하고 회복 신호가 올 때까지 대체 경로를 사용해 연쇄 실패를 방지합니다.',
       'seed:practical:cb'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='SHORT'
     AND stem LIKE '서킷 브레이커 패턴의 목적을 한 문장으로 설명하세요.%'
);

SET @q_short_cb := (
  SELECT id FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='SHORT'
     AND stem LIKE '서킷 브레이커 패턴의 목적을 한 문장으로 설명하세요.%'
   LIMIT 1
);

INSERT INTO question_tag (question_id, tag)
SELECT @q_short_cb, '서킷브레이커'
WHERE @q_short_cb IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_short_cb AND tag='서킷브레이커');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_practical, 'PRACTICAL', 'LONG', 'HARD',
       '초당 2,000 RPS를 처리해야 하는 외부 API 연동 모듈을 설계하세요. 요구사항: 평균 지연 150ms 이하, 실패 시 재시도 정책, 백프레셔, 모니터링/알림 포함.',
       '비동기 IO, 타임아웃, 재시도, 서킷브레이커, 큐 기반 백프레셔, 메트릭/알림',
       '비동기 I/O와 연결 풀을 사용하고, 타임아웃·재시도(지수 백오프)·서킷브레이커·큐 기반 백프레셔·메트릭/알림 구성을 포함하도록 설계합니다.',
       'seed:practical:api2000rps'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='LONG'
     AND stem LIKE '초당 2,000 RPS를 처리해야 하는 외부 API 연동 모듈을 설계하세요.%'
);

SET @q_long_api := (
  SELECT id FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='LONG'
     AND stem LIKE '초당 2,000 RPS를 처리해야 하는 외부 API 연동 모듈을 설계하세요.%'
   LIMIT 1
);

INSERT INTO question_tag (question_id, tag)
SELECT @q_long_api, '고가용성'
WHERE @q_long_api IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_long_api AND tag='고가용성');

SET FOREIGN_KEY_CHECKS = 1;
