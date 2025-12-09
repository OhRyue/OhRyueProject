SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id   := 1;
SET @tp_31101  := 31101; -- 3.1.1 업무 요구사항 분석(시나리오)
SET @tp_31102  := 31102; -- 3.1.2 데이터 요구사항 도출
SET @tp_31201  := 31201; -- 3.1.3 모델링 수준(개념/논리/물리)


/* =======================================================
 * 31101 – 요구사항 확인: 업무 시나리오 분석
 *  - PRACTICAL OX
 * ======================================================= */

-- [31101] OX 3개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'OX', 'NORMAL',
       '업무 시나리오를 분석할 때는 “정상 흐름”뿐 아니라 예외 상황(실패, 취소 등)도 함께 식별해야 한다.',
       NULL,
       'O',
       '실전에서는 예외 흐름을 놓치면 장애나 누락 기능으로 이어지므로, 정상/예외를 함께 정리해야 합니다.',
       'seed:prac:req_scenario:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'OX', 'NORMAL',
       '업무 시나리오에서 “주문 취소”가 여러 번 등장한다면, 이를 별도의 프로세스로 묶어 표현하는 것이 도움이 될 수 있다.',
       NULL,
       'O',
       '여러 곳에서 반복되는 행위는 별도 프로세스로 분리해 공통 로직과 책임을 명확히 하는 것이 좋습니다.',
       'seed:prac:req_scenario:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'OX', 'NORMAL',
       '업무 시나리오에 등장하는 “암묵적인 비즈니스 규칙(예: 취소는 배송 전까지만 가능)”은 설계 단계에서 굳이 명시할 필요가 없다.',
       NULL,
       'X',
       '암묵적인 규칙을 명시하지 않으면 구현자마다 다르게 해석할 수 있으므로, 시나리오에서 규칙을 드러내는 것이 좋습니다.',
       'seed:prac:req_scenario:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:ox6');


/* =======================================================
 * 31102 – 요구사항 확인: 데이터 요구사항 도출
 *  - PRACTICAL OX
 * ======================================================= */

-- [31102] OX 3개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'OX', 'NORMAL',
       '데이터 요구사항을 도출할 때 “누가 데이터를 생성/수정/조회하는지”를 함께 정리하는 것이 도움이 된다.',
       NULL,
       'O',
       'CRUD 관점(생성/조회/변경/삭제 주체)을 함께 보면 테이블과 권한 설계를 같이 정리할 수 있습니다.',
       'seed:prac:req_data:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'OX', 'NORMAL',
       '“로그/이력 데이터”는 직접 화면에 보이지 않더라도 데이터 요구사항으로 간주될 수 있다.',
       NULL,
       'O',
       '감사, 분석, 장애 추적을 위해 로그/이력은 중요한 데이터 요구입니다.',
       'seed:prac:req_data:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'OX', 'NORMAL',
       '데이터 요구사항에는 허용 값 범위(예: 점수는 0~100) 같은 제약조건은 포함되지 않는다.',
       NULL,
       'X',
       '허용 범위, 필수 여부, 유일성 등은 모두 데이터 요구사항의 일부입니다.',
       'seed:prac:req_data:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:ox6');


/* =======================================================
 * 31201 – 데이터 모델링: 개념/논리/물리 모델
 *  - PRACTICAL OX
 * ======================================================= */

-- [31201] OX 3개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'OX', 'NORMAL',
       '개념 모델 단계에서는 정규화보다는 엔터티와 관계 자체를 잘 찾는 것이 더 중요한 목표이다.',
       NULL,
       'O',
       '개념 모델의 핵심은 “무엇과 무엇이 어떤 관계인지”를 비즈니스 관점에서 정리하는 것입니다.',
       'seed:prac:model_levels:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'OX', 'NORMAL',
       '논리 모델 단계에서는 정규화 결과를 바탕으로, 아직 특정 DBMS의 물리적 자료형은 결정하지 않아도 된다.',
       NULL,
       'O',
       '논리 모델은 DBMS 독립적 구조를 정의하고, 물리 모델에서 자료형과 인덱스 등을 결정합니다.',
       'seed:prac:model_levels:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'OX', 'NORMAL',
       '물리 모델 단계에서는 파티션, 인덱스, 스토리지 특성 등을 함께 고려해 테이블 구조를 설계한다.',
       NULL,
       'O',
       '물리 모델은 성능·운영까지 고려한 구체적인 DB 구조를 정의하는 단계입니다.',
       'seed:prac:model_levels:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:ox6');

SET FOREIGN_KEY_CHECKS = 1;
