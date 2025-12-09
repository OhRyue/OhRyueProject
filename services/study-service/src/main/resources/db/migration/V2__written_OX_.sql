SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

-- =========================================================
-- 토픽 ID 상수 정의 (cert-service.topic.id 와 논리 동일하게 사용)
-- =========================================================
SET @topic_analysis   := 11101;  -- 1.1.1 현행 시스템 분석
SET @topic_req        := 11102;  -- 1.1.2 요구사항 확인 (요구공학/OOP 개념 섞어서 활용)
SET @topic_model      := 11103;  -- 1.1.3 분석 모델 확인
SET @topic_ui         := 11201;  -- 1.2.1 UI 요구사항 확인
SET @topic_common     := 11301;  -- 1.3.1 공통 모듈 설계
SET @topic_ood        := 11302;  -- 1.3.2 객체 지향 설계
SET @topic_if_req     := 11401;  -- 1.4.1 인터페이스 요구사항 확인
SET @topic_if_target  := 11402;  -- 1.4.2 인터페이스 대상 식별
SET @topic_if_detail  := 11403;  -- 1.4.3 인터페이스 상세 설계

-- =========================================================
-- 1) WRITTEN OX – 1.1.1 현행 시스템 분석
--    OX 기반 미니체크용 문제 다수
-- =========================================================

-- 성능 지표 수집
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_analysis, 'WRITTEN', 'OX', 'NORMAL',
       '현행 시스템 분석 단계에서는 응답 시간과 처리량과 같은 성능 지표를 수집해야 한다.',
       'O',
       '성능 지표는 향후 요구사항 정의와 설계 제약을 결정하므로 현행 분석 단계에서 반드시 수집해야 합니다.',
       'seed:v2:analysis:perf'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석 단계에서는 응답 시간과 처리량과 같은 성능 지표를 수집해야 한다.%'
);

-- 비기능 요구 함께 수집
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_analysis, 'WRITTEN', 'OX', 'EASY',
       '현행 시스템 분석에서는 기능 요구사항만 수집하고 보안·성능·가용성과 같은 비기능 요구는 이후 단계에서 별도로 분석한다.',
       'X',
       '비기능 요구도 아키텍처/용량 산정에 직접 영향을 주므로 현행 분석 단계에서 함께 수집해야 합니다.',
       'seed:v2:analysis:nfr'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석에서는 기능 요구사항만 수집하고 보안·성능·가용성과 같은 비기능 요구는 이후 단계에서 별도로 분석한다.%'
);

-- 업무 프로세스/조직 구조 파악
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_analysis, 'WRITTEN', 'OX', 'EASY',
       '현행 시스템 분석 시에는 업무 프로세스 흐름과 관련 조직/사용자 역할을 함께 파악하는 것이 좋다.',
       'O',
       '업무 프로세스와 조직 구조를 함께 파악해야 요구 흐름과 시스템 경계를 올바르게 정의할 수 있습니다.',
       'seed:v2:analysis:process-org'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석 시에는 업무 프로세스 흐름과 관련 조직/사용자 역할을 함께 파악하는 것이 좋다.%'
);

-- 인터페이스 정의 없이도 충분? (오답)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_analysis, 'WRITTEN', 'OX', 'NORMAL',
       '현행 시스템 분석에서는 내부 기능만 상세히 분석하고 외부 시스템과의 인터페이스는 설계 단계에서 처음 분석한다.',
       'X',
       '외부 시스템과의 인터페이스 현황(프로토콜, 데이터 포맷, 장애 이력 등)은 현행 분석 단계에서 파악해 두어야 합니다.',
       'seed:v2:analysis:interface'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석에서는 내부 기능만 상세히 분석하고 외부 시스템과의 인터페이스는 설계 단계에서 처음 분석한다.%'
);

-- 장애/운영 지표 반영
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_analysis, 'WRITTEN', 'OX', 'NORMAL',
       '현행 시스템의 장애 이력과 모니터링 지표는 신규 시스템 요구사항과는 무관하므로 수집하지 않아도 된다.',
       'X',
       '장애 이력과 모니터링 지표는 신시스템에서 반드시 개선해야 할 요구사항(신뢰성, 가용성)을 도출하는 핵심 근거입니다.',
       'seed:v2:analysis:incidents'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템의 장애 이력과 모니터링 지표는 신규 시스템 요구사항과는 무관하므로 수집하지 않아도 된다.%'
);

-- =========================================================
-- 2) WRITTEN OX – 1.1.2 요구사항 확인 (요구공학/Agile 성격)
-- =========================================================

-- 이해관계자 식별 중요성
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_req, 'WRITTEN', 'OX', 'EASY',
       '요구사항 확인 단계에서 이해관계자를 식별하지 못하면 요구 누락 위험이 높아질 수 있다.',
       'O',
       '요구 출처가 되는 이해관계자를 식별하지 못하면 중요한 요구가 빠지거나 왜곡될 가능성이 커집니다.',
       'seed:v2:req:stakeholder'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_req AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '요구사항 확인 단계에서 이해관계자를 식별하지 못하면 요구 누락 위험이 높아질 수 있다.%'
);

-- 자연어 명세의 모호성
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_req, 'WRITTEN', 'OX', 'NORMAL',
       '자연어 기반 요구사항 명세는 이해가 쉽고 모호성이 거의 없기 때문에 추가 검토 과정이 필요 없다.',
       'X',
       '자연어 명세는 친숙하지만 표현이 모호하기 쉬워, 리뷰/프로토타입/모델링 등으로 보완해야 합니다.',
       'seed:v2:req:natural'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_req AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '자연어 기반 요구사항 명세는 이해가 쉽고 모호성이 거의 없기 때문에 추가 검토 과정이 필요 없다.%'
);

-- 애자일 변화 수용
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_req, 'WRITTEN', 'OX', 'EASY',
       '애자일(Agile) 방법론에서는 변경되는 요구사항을 계획 대비 예외로 보고 최대한 줄이는 것이 목표이다.',
       'X',
       '애자일은 요구 변경을 자연스러운 것으로 보고, 짧은 피드백 주기로 유연하게 수용하는 것을 강조합니다.',
       'seed:v2:req:agile-change'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_req AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '애자일(Agile) 방법론에서는 변경되는 요구사항을 계획 대비 예외로 보고 최대한 줄이는 것이 목표이다.%'
);

-- 유스케이스/시나리오
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_req, 'WRITTEN', 'OX', 'NORMAL',
       '유스케이스는 액터와 시스템 간 상호작용을 시나리오 형태로 표현하는 요구 분석 기법이다.',
       'O',
       '유스케이스는 사용자의 관점에서 시스템의 기능을 시나리오 형태로 표현하여 요구를 도출/검증하는 데 사용됩니다.',
       'seed:v2:req:usecase'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_req AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '유스케이스는 액터와 시스템 간 상호작용을 시나리오 형태로 표현하는 요구 분석 기법이다.%'
);

SET FOREIGN_KEY_CHECKS = 1;
