SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

-- =========================================================
-- 토픽 ID 상수 정의
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
-- 1) WRITTEN REVIEW용 보충 MCQ – 여러 토픽에 골고루 분포
--    (리뷰 세트가 루트 토픽 기준으로 문제를 많이 뽑을 수 있도록)
-- =========================================================

-- 분석 모델 확인: 추적성
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_model, 'WRITTEN', 'MCQ', 'NORMAL',
       '요구사항, 설계, 테스트 간의 관계를 관리하여 변경 영향을 추적하는 활동을 무엇이라고 하는가?',
       'C',
       '요구 추적성 관리는 요구~설계~테스트 아티팩트 간 링크를 관리하여 변경 영향 범위를 파악하는 활동입니다.',
       'seed:v4:review:traceability'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '요구사항, 설계, 테스트 간의 관계를 관리하여 변경 영향을 추적하는 활동을 무엇이라고 하는가?%'
);

SET @q_mcq_trace := (
  SELECT id FROM question
   WHERE topic_id=@topic_model AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '요구사항, 설계, 테스트 간의 관계를 관리하여 변경 영향을 추적하는 활동을 무엇이라고 하는가?%'
   LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_trace,'A','정형 명세',0
WHERE @q_mcq_trace IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_trace AND label='A');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_trace,'B','리팩터링',0
WHERE @q_mcq_trace IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_trace AND label='B');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_trace,'C','추적성 관리',1
WHERE @q_mcq_trace IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_trace AND label='C');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_trace,'D','성능 튜닝',0
WHERE @q_mcq_trace IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_trace AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_trace, '요구관리'
WHERE @q_mcq_trace IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_trace AND tag='요구관리');

-- UI 리뷰용: 접근성/가독성
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_ui, 'WRITTEN', 'MCQ', 'NORMAL',
       '다음 중 UI 접근성을 높이기 위한 방안으로 가장 적절한 것은?',
       'B',
       '충분한 대비, 폰트 크기, 대체 텍스트, 키보드 접근성 등이 접근성을 높이는 방안입니다.',
       'seed:v4:review:accessibility'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_ui AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 UI 접근성을 높이기 위한 방안으로 가장 적절한 것은?%'
);

SET @q_mcq_access := (
  SELECT id FROM question
   WHERE topic_id=@topic_ui AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 UI 접근성을 높이기 위한 방안으로 가장 적절한 것은?%'
   LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_access,'A','모든 텍스트를 작은 글씨로 표시한다',0
WHERE @q_mcq_access IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_access AND label='A');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_access,'B','충분한 색 대비와 대체 텍스트를 제공한다',1
WHERE @q_mcq_access IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_access AND label='B');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_access,'C','중요 정보는 색상만으로 구분한다',0
WHERE @q_mcq_access IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_access AND label='C');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_access,'D','키보드 탐색은 고려하지 않는다',0
WHERE @q_mcq_access IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_access AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_access, '접근성'
WHERE @q_mcq_access IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_access AND tag='접근성');

-- 객체지향 리뷰용: 다형성
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @topic_ood, 'WRITTEN', 'MCQ', 'EASY',
       '다음 중 다형성(Polymorphism)에 대한 설명으로 가장 적절한 것은?',
       'A',
       '다형성은 동일한 인터페이스를 통해 서로 다른 구현이 동작하도록 하는 객체지향 특성입니다.',
       'seed:v4:review:polymorphism'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_ood AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 다형성(Polymorphism)에 대한 설명으로 가장 적절한 것은?%'
);

SET @q_mcq_poly := (
  SELECT id FROM question
   WHERE topic_id=@topic_ood AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 다형성(Polymorphism)에 대한 설명으로 가장 적절한 것은?%'
   LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_poly,'A','동일한 메시지에 대해 서로 다른 구현이 실행될 수 있도록 하는 특성',1
WHERE @q_mcq_poly IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_poly AND label='A');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_poly,'B','데이터를 한 곳에만 저장하는 특성',0
WHERE @q_mcq_poly IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_poly AND label='B');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_poly,'C','사용자 인터페이스를 숨기는 특성',0
WHERE @q_mcq_poly IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_poly AND label='C');

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_poly,'D','데이터베이스 정규화를 수행하는 특성',0
WHERE @q_mcq_poly IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_poly AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_poly, '다형성'
WHERE @q_mcq_poly IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_poly AND tag='다형성');

SET FOREIGN_KEY_CHECKS = 1;
