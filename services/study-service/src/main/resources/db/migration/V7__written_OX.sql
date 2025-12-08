SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

SET @tp_11101 := 11101; -- 1.1.1 현행 시스템 분석
SET @tp_11102 := 11102; -- 1.1.2 요구사항 확인 기법/UML/애자일
SET @tp_11103 := 11103; -- 1.1.3 분석 모델/요구 관리

/* ================================================
 * 11101 – 현행 시스템 분석
 *  - OX만 유지
 * ================================================ */

-- [11101] OX 추가 (1개)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11101, 'WRITTEN', 'OX', 'NORMAL',
       '현행 시스템 분석에서 로그와 모니터링 지표는 “문제 발생 시 원인 추적”뿐 아니라, 신규 시스템의 용량 산정과 임계치 정의에도 활용된다.',
       NULL,
       'O',
       '로그·모니터링 지표는 단순 장애 분석을 넘어, 신시스템에서의 성능/용량 기준과 알림 임계치 정의에 중요한 근거가 됩니다.',
       'seed:v5:11101:ox:monitoring-capacity'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11101:ox:monitoring-capacity'
);

-- [11101] question_tag 매핑 (토픽 공통 태그)
INSERT INTO question_tag (question_id, tag)
SELECT q.id, '현행분석'
FROM question q
WHERE q.topic_id = @tp_11101
  AND q.mode = 'WRITTEN'
  AND NOT EXISTS (
    SELECT 1
    FROM question_tag qt
    WHERE qt.question_id = q.id
      AND qt.tag = '현행분석'
  );


/* ================================================
 * 11102 – 요구사항 확인 / UML / 애자일
 *  - OX만 유지
 * ================================================ */

-- [11102] OX 2개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11102, 'WRITTEN', 'OX', 'NORMAL',
       '요구사항 우선순위는 단순히 “요청한 사람의 직급”만을 기준으로 정하면 안 되며, 사업 가치와 구현 난이도 등을 함께 고려해야 한다.',
       NULL,
       'O',
       '요구 우선순위는 비즈니스 가치, 위험, 난이도, 일정 등을 종합적으로 고려해 결정해야 합니다.',
       'seed:v5:11102:ox:priority'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11102:ox:priority'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11102, 'WRITTEN', 'OX', 'NORMAL',
       '요구사항 검토 회의에서는 이해관계자 간의 요구 충돌을 발견하더라도, 문서에 그대로 두고 설계 단계에서 자동으로 조정되도록 두는 것이 일반적이다.',
       NULL,
       'X',
       '요구 충돌은 가능한 빨리 식별하고, 책임자/이해관계자 합의를 통해 해소한 뒤 명세서에 반영해야 합니다.',
       'seed:v5:11102:ox:conflict'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11102:ox:conflict'
);

-- [11102] question_tag 매핑 (토픽 공통 태그)
INSERT INTO question_tag (question_id, tag)
SELECT q.id, '요구도출'
FROM question q
WHERE q.topic_id = @tp_11102
  AND q.mode = 'WRITTEN'
  AND NOT EXISTS (
    SELECT 1
    FROM question_tag qt
    WHERE qt.question_id = q.id
      AND qt.tag = '요구도출'
  );


/* ================================================
 * 11103 – 분석 모델/요구 관리
 *  - OX만 유지
 * ================================================ */

-- [11103] OX 3개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11103, 'WRITTEN', 'OX', 'EASY',
       'ERD에서 관계 차수(1:1, 1:N, N:M)를 올바르게 표현하는 것은 데이터 중복과 이상 현상을 줄이는 데 도움을 준다.',
       NULL,
       'O',
       '올바른 관계 차수 표현은 정규화와 무결성 확보에 직접적으로 기여합니다.',
       'seed:v5:11103:ox:cardinality'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11103:ox:cardinality'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11103, 'WRITTEN', 'OX', 'NORMAL',
       '데이터 흐름도(DFD)는 프로세스, 데이터 저장소, 외부 엔티티, 데이터 흐름을 이용해 “어디에 무엇이 저장되는지”를 표현하는 정적 모델이다.',
       NULL,
       'X',
       'DFD는 데이터가 어떻게 처리·흘러가는지(동적 흐름)를 표현하며, 정적 구조는 ERD/클래스 다이어그램이 담당합니다.',
       'seed:v5:11103:ox:dfd-static'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11103:ox:dfd-static'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11103, 'WRITTEN', 'OX', 'NORMAL',
       '분석 모델은 설계와 구현 단계에서 참조 기준이 되므로, 요구사항과 일관성이 유지되도록 지속적으로 갱신해야 한다.',
       NULL,
       'O',
       '요구 변경 시 분석 모델도 함께 갱신해야 이후 설계/구현/테스트 불일치를 줄일 수 있습니다.',
       'seed:v5:11103:ox:model-sync'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11103:ox:model-sync'
);

-- [11103] question_tag 매핑 (토픽 공통 태그)
INSERT INTO question_tag (question_id, tag)
SELECT q.id, '모델링'
FROM question q
WHERE q.topic_id = @tp_11103
  AND q.mode = 'WRITTEN'
  AND NOT EXISTS (
    SELECT 1
    FROM question_tag qt
    WHERE qt.question_id = q.id
      AND qt.tag = '모델링'
  );

SET FOREIGN_KEY_CHECKS = 1;
