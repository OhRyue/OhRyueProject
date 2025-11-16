SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id   := 1;
SET @tp_31101  := 31101; -- 3.1.1 업무 요구사항 분석(시나리오)
SET @tp_31102  := 31102; -- 3.1.2 데이터 요구사항 도출
SET @tp_31201  := 31201; -- 3.1.3 모델링 수준(개념/논리/물리)


/* =======================================================
 * 31101 – 요구사항 확인: 업무 시나리오 분석
 *  - 목표: PRACTICAL OX 총 6개, MCQ 총 10개
 * ======================================================= */

-- [31101] OX 추가 3개 (기존 3개 + 3개 = 6개)

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'OX', 'NORMAL',
       '업무 시나리오를 분석할 때는 “정상 흐름”뿐 아니라 예외 상황(실패, 취소 등)도 함께 식별해야 한다. (O/X)',
       NULL,
       'O',
       '실전에서는 예외 흐름을 놓치면 장애나 누락 기능으로 이어지므로, 정상/예외를 함께 정리해야 합니다.',
       'seed:prac:req_scenario:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'OX', 'NORMAL',
       '업무 시나리오에서 “주문 취소”가 여러 번 등장한다면, 이를 별도의 프로세스로 묶어 표현하는 것이 도움이 될 수 있다. (O/X)',
       NULL,
       'O',
       '여러 곳에서 반복되는 행위는 별도 프로세스로 분리해 공통 로직과 책임을 명확히 하는 것이 좋습니다.',
       'seed:prac:req_scenario:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'OX', 'NORMAL',
       '업무 시나리오에 등장하는 “암묵적인 비즈니스 규칙(예: 취소는 배송 전까지만 가능)”은 설계 단계에서 굳이 명시할 필요가 없다. (O/X)',
       NULL,
       'X',
       '암묵적인 규칙을 명시하지 않으면 구현자마다 다르게 해석할 수 있으므로, 시나리오에서 규칙을 드러내는 것이 좋습니다.',
       'seed:prac:req_scenario:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:ox6');


-- [31101] MCQ 10개 신규

-- Q1: 시나리오에서 Actor 찾기
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'MCQ', 'EASY',
       '“고객이 모바일 앱에서 상품을 주문하면, 결제 시스템이 결제를 승인하고, 물류 시스템이 배송 요청을 생성한다” 시나리오에서 **주요 Actor**로 보기 어려운 것은?',
       NULL,
       'D',
       '고객, 결제 시스템, 물류 시스템은 모두 행위 주체이지만, DB 서버는 내부 자원에 가까워 보통 Actor로 두지 않습니다.',
       'seed:prac:req_scenario:mcq1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:mcq1');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '고객', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq1'
UNION ALL
SELECT q.id, 'B', '모바일 앱', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq1'
UNION ALL
SELECT q.id, 'C', '결제 시스템', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq1'
UNION ALL
SELECT q.id, 'D', 'DB 서버(내부 스토리지)', 1
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq1';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:req_scenario:mcq1';


-- Q2: 상태 변화 읽기
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'MCQ', 'EASY',
       '“주문 접수 → 결제 완료 → 배송 준비 → 배송 완료” 흐름에서 **업무 분석 시 특히 주목해야 할 것**으로 가장 적절한 것은?',
       NULL,
       'B',
       '각 단계마다 주문 상태가 어떻게 바뀌는지가 시나리오 분석의 핵심입니다.',
       'seed:prac:req_scenario:mcq2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:mcq2');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '주문 화면의 색상', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq2'
UNION ALL
SELECT q.id, 'B', '각 단계에서 주문 상태가 어떻게 변경되는지', 1
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq2'
UNION ALL
SELECT q.id, 'C', '개발자의 이름', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq2'
UNION ALL
SELECT q.id, 'D', '서버 방의 온도', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq2';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:req_scenario:mcq2';


-- Q3: 선행 조건 / 사후 조건
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'MCQ', 'NORMAL',
       '업무 시나리오에서 “선행 조건(Pre-condition)”에 해당하는 설명은?',
       NULL,
       'C',
       '선행 조건은 기능이 실행되기 전에 만족되어야 하는 상태를 의미합니다.',
       'seed:prac:req_scenario:mcq3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:mcq3');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '기능 실행 후 DB에 남는 로그 항목', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq3'
UNION ALL
SELECT q.id, 'B', '예외 상황에서의 복구 절차', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq3'
UNION ALL
SELECT q.id, 'C', '해당 기능을 수행하기 전에 이미 만족하고 있어야 하는 상태나 조건', 1
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq3'
UNION ALL
SELECT q.id, 'D', '화면에 표시되는 안내 문구', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq3';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:req_scenario:mcq3';


-- Q4: 예외 흐름 인식
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 **예외 흐름**으로 분류하기에 가장 적절한 것은?',
       NULL,
       'B',
       '결제 실패, 재고 부족 등은 정상 경로가 아닌 예외 흐름입니다.',
       'seed:prac:req_scenario:mcq4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:mcq4');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '고객이 상품을 장바구니에 담는다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq4'
UNION ALL
SELECT q.id, 'B', '결제 승인 과정에서 카드 한도 초과로 결제가 거절된다.', 1
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq4'
UNION ALL
SELECT q.id, 'C', '배송 완료 후 고객이 상품을 수령한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq4'
UNION ALL
SELECT q.id, 'D', '고객이 상품 상세를 조회한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq4';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:req_scenario:mcq4';


-- Q5: 시나리오 → 유스케이스
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'MCQ', 'NORMAL',
       '업무 시나리오를 유스케이스로 정리할 때 가장 먼저 해야 할 일은?',
       NULL,
       'C',
       '유스케이스는 “Actor가 시스템을 통해 하고 싶은 일”을 기능 단위로 묶는 작업입니다.',
       'seed:prac:req_scenario:mcq5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:mcq5');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'DB 테이블명을 먼저 정한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq5'
UNION ALL
SELECT q.id, 'B', 'UI 색상 팔레트를 설계한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq5'
UNION ALL
SELECT q.id, 'C', 'Actor와 그 Actor가 시스템을 통해 수행하려는 목표를 식별한다.', 1
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq5'
UNION ALL
SELECT q.id, 'D', '서버 IP와 포트를 먼저 결정한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq5';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:req_scenario:mcq5';


-- Q6: 숨은 요구 파악
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 시나리오 분석에서 “숨은 요구사항”을 찾기 위한 질문으로 가장 적절한 것은?',
       NULL,
       'D',
       '예외 상황, 실패 조건, 경계 케이스를 질문하면 숨은 요구를 잘 끌어낼 수 있습니다.',
       'seed:prac:req_scenario:mcq6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:mcq6');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '이 기능을 누가 개발하나요?', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq6'
UNION ALL
SELECT q.id, 'B', 'UI는 어떤 색이 좋을까요?', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq6'
UNION ALL
SELECT q.id, 'C', '서버는 윈도우가 좋을까요 리눅스가 좋을까요?', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq6'
UNION ALL
SELECT q.id, 'D', '결제 실패, 재고 부족 같은 예외 상황에서는 어떻게 처리해야 하나요?', 1
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq6';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:req_scenario:mcq6';


-- Q7: 시나리오 → 테스트 케이스
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'MCQ', 'NORMAL',
       '업무 시나리오를 기반으로 테스트 케이스를 도출할 때 가장 우선적으로 해야 할 일은?',
       NULL,
       'B',
       '시나리오의 각 경로(정상/예외)를 케이스로 나누는 것이 기본입니다.',
       'seed:prac:req_scenario:mcq7'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:mcq7');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '테스트 도구를 먼저 선택한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq7'
UNION ALL
SELECT q.id, 'B', '정상/예외 흐름별로 입력·기대 결과를 정리한다.', 1
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq7'
UNION ALL
SELECT q.id, 'C', '서버 로그 포맷을 설계한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq7'
UNION ALL
SELECT q.id, 'D', '개발자의 업무 시간을 기록한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq7';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:req_scenario:mcq7';


-- Q8: 시나리오 경계값
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'MCQ', 'NORMAL',
       '“1회 주문 최대 수량은 100개까지”라는 요구가 있는 경우, 시나리오 기반 테스트 설계에서 가장 적절한 조합은?',
       NULL,
       'C',
       '경계값 분석: 99, 100, 101 등 경계 주변 값을 포함해야 합니다.',
       'seed:prac:req_scenario:mcq8'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:mcq8');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '1개, 2개만 테스트한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq8'
UNION ALL
SELECT q.id, 'B', '100개만 테스트한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq8'
UNION ALL
SELECT q.id, 'C', '99개, 100개, 101개 이상 상황을 포함해 테스트한다.', 1
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq8'
UNION ALL
SELECT q.id, 'D', '수량은 테스트 대상이 아니다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq8';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:req_scenario:mcq8';


-- Q9: 요구 명세의 모호성
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 **모호한 요구사항**으로 분류하기 가장 적절한 것은?',
       NULL,
       'D',
       '“적당히 빠르게” 같은 표현은 수치 기준이 없어 모호합니다.',
       'seed:prac:req_scenario:mcq9'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:mcq9');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '응답 시간은 1초 이내여야 한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq9'
UNION ALL
SELECT q.id, 'B', '하루 최대 주문 수는 1만 건까지 처리한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq9'
UNION ALL
SELECT q.id, 'C', '장바구니에는 최대 100개까지 담을 수 있다.', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq9'
UNION ALL
SELECT q.id, 'D', '화면 응답 속도는 적당히 빠르게 한다.', 1
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq9';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:req_scenario:mcq9';


-- Q10: 시나리오 단위 분리
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31101, 'PRACTICAL', 'MCQ', 'NORMAL',
       '업무 시나리오를 여러 개로 나눌 때 기준으로 가장 적절한 것은?',
       NULL,
       'B',
       '일반적으로 “사용자 또는 시스템이 느끼는 하나의 완결된 목표” 단위로 시나리오를 나눕니다.',
       'seed:prac:req_scenario:mcq10'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_scenario:mcq10');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '한 화면에 있는 버튼 개수', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq10'
UNION ALL
SELECT q.id, 'B', '사용자 또는 시스템이 느끼는 하나의 완결된 목표/업무 흐름', 1
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq10'
UNION ALL
SELECT q.id, 'C', '개발자가 구현하기 편한 크기', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq10'
UNION ALL
SELECT q.id, 'D', 'DB 테이블 개수', 0
FROM question q WHERE q.source = 'seed:prac:req_scenario:mcq10';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:req_scenario:mcq10';



/* =======================================================
 * 31102 – 요구사항 확인: 데이터 요구사항 도출
 *  - 목표: PRACTICAL OX 총 6개, MCQ 총 10개
 * ======================================================= */

-- [31102] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'OX', 'NORMAL',
       '데이터 요구사항을 도출할 때 “누가 데이터를 생성/수정/조회하는지”를 함께 정리하는 것이 도움이 된다. (O/X)',
       NULL,
       'O',
       'CRUD 관점(생성/조회/변경/삭제 주체)을 함께 보면 테이블과 권한 설계를 같이 정리할 수 있습니다.',
       'seed:prac:req_data:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'OX', 'NORMAL',
       '“로그/이력 데이터”는 직접 화면에 보이지 않더라도 데이터 요구사항으로 간주될 수 있다. (O/X)',
       NULL,
       'O',
       '감사, 분석, 장애 추적을 위해 로그/이력은 중요한 데이터 요구입니다.',
       'seed:prac:req_data:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'OX', 'NORMAL',
       '데이터 요구사항에는 허용 값 범위(예: 점수는 0~100) 같은 제약조건은 포함되지 않는다. (O/X)',
       NULL,
       'X',
       '허용 범위, 필수 여부, 유일성 등은 모두 데이터 요구사항의 일부입니다.',
       'seed:prac:req_data:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:ox6');


-- [31102] MCQ 10개

-- Q1: 업무 요구 vs 데이터 요구 구분
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'MCQ', 'EASY',
       '다음 중 **데이터 요구사항**으로 분류하기 가장 적절한 것은?',
       NULL,
       'C',
       '“어떤 값을, 어떤 형식으로, 어디에 저장/조회할지”는 데이터 요구입니다.',
       'seed:prac:req_data:mcq1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:mcq1');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '사용자는 상품을 장바구니에 담을 수 있어야 한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq1'
UNION ALL
SELECT q.id, 'B', '배송 지연 시 고객에게 알림을 보내야 한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq1'
UNION ALL
SELECT q.id, 'C', '장바구니 항목에는 상품ID, 수량, 담은 시각, 사용자ID를 저장해야 한다.', 1
FROM question q WHERE q.source = 'seed:prac:req_data:mcq1'
UNION ALL
SELECT q.id, 'D', '장바구니 화면은 모바일에서도 보기 좋아야 한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq1';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:req_data:mcq1';


-- Q2: 필수/선택 데이터
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'MCQ', 'EASY',
       '데이터 요구사항 정의 시 **필수로 고려해야 하는 내용**으로 가장 적절한 것은?',
       NULL,
       'B',
       '필수/선택 여부는 입력 검증, NULL 허용 여부와 직접 연결됩니다.',
       'seed:prac:req_data:mcq2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:mcq2');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '화면 색상 팔레트', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq2'
UNION ALL
SELECT q.id, 'B', '각 데이터 항목이 필수인지 선택인지', 1
FROM question q WHERE q.source = 'seed:prac:req_data:mcq2'
UNION ALL
SELECT q.id, 'C', '개발자별 업무량', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq2'
UNION ALL
SELECT q.id, 'D', '회의 참석 인원', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq2';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:req_data:mcq2';


-- Q3: 식별자 설계
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'MCQ', 'NORMAL',
       '주문 데이터를 설계할 때 **식별자(ID) 요구사항**으로 가장 적절한 것은?',
       NULL,
       'C',
       '주문을 유일하게 구분할 수 있는 키를 정의하는 것이 중요합니다.',
       'seed:prac:req_data:mcq3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:mcq3');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '주문ID는 없어도 된다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq3'
UNION ALL
SELECT q.id, 'B', '고객이름만으로 주문을 구분한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq3'
UNION ALL
SELECT q.id, 'C', '각 주문을 유일하게 구분할 수 있는 주문ID를 정의한다.', 1
FROM question q WHERE q.source = 'seed:prac:req_data:mcq3'
UNION ALL
SELECT q.id, 'D', '식별자는 화면에 보일 필요가 없으므로 설계하지 않는다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq3';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:req_data:mcq3';


-- Q4: 로그/이력 데이터 요구
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 **로그/이력**에 대한 데이터 요구사항으로 가장 적절한 것은?',
       NULL,
       'B',
       '누가, 언제, 무엇을 변경했는지 남기는 것이 일반적인 이력 요구입니다.',
       'seed:prac:req_data:mcq4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:mcq4');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '화면 배경색 변경 내역을 모두 저장한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq4'
UNION ALL
SELECT q.id, 'B', '중요 데이터 변경 시 변경자, 변경 일시, 변경 전/후 값을 이력으로 남긴다.', 1
FROM question q WHERE q.source = 'seed:prac:req_data:mcq4'
UNION ALL
SELECT q.id, 'C', '로그는 운영팀이 필요할 때만 수동으로 기록한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq4'
UNION ALL
SELECT q.id, 'D', '로그 데이터는 요구사항 대상이 아니다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq4';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:req_data:mcq4';


-- Q5: 값의 범위/제약조건
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 데이터 요구사항에서 “제약조건”에 해당하는 예로 가장 적절한 것은?',
       NULL,
       'D',
       '허용 범위, 형식 제약, 유일성 등은 전형적인 데이터 제약조건입니다.',
       'seed:prac:req_data:mcq5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:mcq5');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '주문 화면은 보기 좋아야 한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq5'
UNION ALL
SELECT q.id, 'B', '개발자는 주 5일 근무한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq5'
UNION ALL
SELECT q.id, 'C', '결제 버튼은 오른쪽 아래에 둔다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq5'
UNION ALL
SELECT q.id, 'D', '할인율은 0 이상 100 이하의 정수로 저장한다.', 1
FROM question q WHERE q.source = 'seed:prac:req_data:mcq5';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:req_data:mcq5';


-- Q6: 통계/분석 요구
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'MCQ', 'NORMAL',
       '“월별 매출 통계 보고서를 제공해야 한다”라는 요구에서, 데이터 요구사항으로 가장 적절한 것은?',
       NULL,
       'B',
       '통계를 위해 어떤 데이터를 어느 단위로 저장/집계할지 정의하는 것이 데이터 요구입니다.',
       'seed:prac:req_data:mcq6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:mcq6');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '보고서 화면 디자인을 정한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq6'
UNION ALL
SELECT q.id, 'B', '주문일자, 금액, 상품, 고객 정보 등을 저장하고 월 단위로 집계할 수 있도록 설계한다.', 1
FROM question q WHERE q.source = 'seed:prac:req_data:mcq6'
UNION ALL
SELECT q.id, 'C', '보고서를 출력하는 프린터 모델을 정한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq6'
UNION ALL
SELECT q.id, 'D', '개발 언어를 선택한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq6';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:req_data:mcq6';


-- Q7: 마스킹/익명화 요구
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'MCQ', 'NORMAL',
       '개인정보 보호 관점에서 **데이터 요구사항**으로 가장 적절한 것은?',
       NULL,
       'C',
       '저장 방식, 마스킹 규칙, 접근 권한 등을 요구사항 단계에서 정의해야 합니다.',
       'seed:prac:req_data:mcq7'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:mcq7');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '고객 이름은 항상 전체를 화면에 표시한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq7'
UNION ALL
SELECT q.id, 'B', '주민등록번호를 평문으로 저장한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq7'
UNION ALL
SELECT q.id, 'C', '고객 전화번호는 저장 시 일부를 마스킹하고, 조회 권한을 제한한다.', 1
FROM question q WHERE q.source = 'seed:prac:req_data:mcq7'
UNION ALL
SELECT q.id, 'D', '민감 정보는 로그에 그대로 남긴다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq7';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:req_data:mcq7';


-- Q8: 중복 데이터 요구 판단
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 **중복 저장을 피하기 위해 재검토해야 할 데이터 요구사항**으로 가장 적절한 것은?',
       NULL,
       'D',
       '고객 주소를 주문마다 복사 저장하면 갱신 이상 문제가 생길 수 있으므로 요구를 다시 설계해야 합니다.',
       'seed:prac:req_data:mcq8'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:mcq8');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '주문ID를 저장한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq8'
UNION ALL
SELECT q.id, 'B', '상품ID를 저장한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq8'
UNION ALL
SELECT q.id, 'C', '고객ID를 저장한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq8'
UNION ALL
SELECT q.id, 'D', '고객 주소 전체를 모든 주문 레코드에 복사해서 저장한다.', 1
FROM question q WHERE q.source = 'seed:prac:req_data:mcq8';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:req_data:mcq8';


-- Q9: 데이터 생명주기 요구
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'MCQ', 'NORMAL',
       '데이터 생명주기(Lifecycle) 관점의 요구사항으로 가장 적절한 것은?',
       NULL,
       'B',
       '저장 기간, 보존 정책, 삭제 시점 등은 전형적인 생명주기 요구입니다.',
       'seed:prac:req_data:mcq9'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:mcq9');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '화면은 반응형으로 구현한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq9'
UNION ALL
SELECT q.id, 'B', '주문 데이터는 5년간 보관 후 폐기해야 한다.', 1
FROM question q WHERE q.source = 'seed:prac:req_data:mcq9'
UNION ALL
SELECT q.id, 'C', '에러 메시지는 친절해야 한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq9'
UNION ALL
SELECT q.id, 'D', '로그인은 SNS 계정으로도 가능해야 한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq9';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:req_data:mcq9';


-- Q10: 참조 무결성 요구
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31102, 'PRACTICAL', 'MCQ', 'NORMAL',
       '데이터 요구사항 중 **참조 무결성**과 가장 관련이 깊은 설명은?',
       NULL,
       'C',
       '부모 없는 자식 레코드를 허용할지, 삭제 시 어떻게 처리할지 등이 참조 무결성 요구입니다.',
       'seed:prac:req_data:mcq10'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:req_data:mcq10');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'UI 테마 색상을 저장한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq10'
UNION ALL
SELECT q.id, 'B', '로그인 시 자동완성 여부를 저장한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq10'
UNION ALL
SELECT q.id, 'C', '고객이 삭제될 때 해당 고객의 주문 데이터를 어떻게 처리할지 정한다.', 1
FROM question q WHERE q.source = 'seed:prac:req_data:mcq10'
UNION ALL
SELECT q.id, 'D', '메뉴 배치 순서를 정한다.', 0
FROM question q WHERE q.source = 'seed:prac:req_data:mcq10';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:req_data:mcq10';



/* =======================================================
 * 31201 – 데이터 모델링: 개념/논리/물리 모델
 *  - 목표: PRACTICAL OX 총 6개, MCQ 총 10개
 * ======================================================= */

-- [31201] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'OX', 'NORMAL',
       '개념 모델 단계에서는 정규화보다는 엔터티와 관계 자체를 잘 찾는 것이 더 중요한 목표이다. (O/X)',
       NULL,
       'O',
       '개념 모델의 핵심은 “무엇과 무엇이 어떤 관계인지”를 비즈니스 관점에서 정리하는 것입니다.',
       'seed:prac:model_levels:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'OX', 'NORMAL',
       '논리 모델 단계에서는 정규화 결과를 바탕으로, 아직 특정 DBMS의 물리적 자료형은 결정하지 않아도 된다. (O/X)',
       NULL,
       'O',
       '논리 모델은 DBMS 독립적 구조를 정의하고, 물리 모델에서 자료형과 인덱스 등을 결정합니다.',
       'seed:prac:model_levels:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'OX', 'NORMAL',
       '물리 모델 단계에서는 파티션, 인덱스, 스토리지 특성 등을 함께 고려해 테이블 구조를 설계한다. (O/X)',
       NULL,
       'O',
       '물리 모델은 성능·운영까지 고려한 구체적인 DB 구조를 정의하는 단계입니다.',
       'seed:prac:model_levels:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:ox6');


-- [31201] MCQ 10개

-- Q1: 각 단계의 초점
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'MCQ', 'EASY',
       '다음 중 “개념 모델” 단계의 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       '개념 모델은 업무 용어 기반으로 엔터티와 관계를 거칠게 정의합니다.',
       'seed:prac:model_levels:mcq1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:mcq1');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '특정 DBMS의 인덱스 타입을 결정하는 단계이다.', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq1'
UNION ALL
SELECT q.id, 'B', '업무 용어로 엔터티와 관계를 식별하는 상위 수준 모델 단계이다.', 1
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq1'
UNION ALL
SELECT q.id, 'C', '테이블 파티션 키를 설정하는 단계이다.', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq1'
UNION ALL
SELECT q.id, 'D', '쿼리 튜닝을 수행하는 단계이다.', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq1';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:model_levels:mcq1';


-- Q2: 논리 모델의 특징
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'MCQ', 'EASY',
       '“논리 모델”에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       '정규화, 키 정의, 관계 정제 등이 논리 모델의 주요 작업입니다.',
       'seed:prac:model_levels:mcq2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:mcq2');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '서버 스펙(CPU, 메모리)을 결정하는 단계이다.', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq2'
UNION ALL
SELECT q.id, 'B', 'UI 컴포넌트 배치를 정의하는 단계이다.', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq2'
UNION ALL
SELECT q.id, 'C', '속성과 키, 정규화를 통해 DB 논리 구조를 정의하는 단계이다.', 1
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq2'
UNION ALL
SELECT q.id, 'D', '쿼리 인덱스 힌트를 추가하는 단계이다.', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq2';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:model_levels:mcq2';


-- Q3: 물리 모델의 산출물
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 “물리 모델” 단계의 산출물로 보기 가장 적절한 것은?',
       NULL,
       'D',
       '실제 테이블 정의서(자료형, 인덱스, 파티션 등)가 물리 모델 산출물입니다.',
       'seed:prac:model_levels:mcq3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:mcq3');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '업무 시나리오 문서', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq3'
UNION ALL
SELECT q.id, 'B', '유스케이스 다이어그램', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq3'
UNION ALL
SELECT q.id, 'C', '개념 ERD(엔터티 박스와 관계선만 있는 그림)', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq3'
UNION ALL
SELECT q.id, 'D', '테이블/컬럼/자료형/인덱스를 포함한 DDL 또는 테이블 정의서', 1
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq3';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:model_levels:mcq3';


-- Q4: 단계 간 추적성
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'MCQ', 'NORMAL',
       '개념 → 논리 → 물리 모델을 일관되게 관리하는 주된 이유로 가장 적절한 것은?',
       NULL,
       'B',
       '요구와 DB 구조가 끊기지 않고 연결되도록 추적성을 확보하기 위함입니다.',
       'seed:prac:model_levels:mcq4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:mcq4');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '문서 분량을 늘리기 위해', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq4'
UNION ALL
SELECT q.id, 'B', '업무 요구사항이 DB 테이블까지 어떻게 반영되었는지 추적하기 위해', 1
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq4'
UNION ALL
SELECT q.id, 'C', '개발자 수를 늘리기 위해', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq4'
UNION ALL
SELECT q.id, 'D', '서버 수를 줄이기 위해', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq4';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:model_levels:mcq4';


-- Q5: 정규화 적용 시점
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'MCQ', 'NORMAL',
       '정규화를 주로 적용하는 단계로 가장 적절한 것은?',
       NULL,
       'C',
       '정규화는 논리 모델에서 이상 현상을 제거하기 위해 적용합니다.',
       'seed:prac:model_levels:mcq5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:mcq5');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '개념 모델 단계', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq5'
UNION ALL
SELECT q.id, 'B', '업무 요구 파악 단계', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq5'
UNION ALL
SELECT q.id, 'C', '논리 모델 단계', 1
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq5'
UNION ALL
SELECT q.id, 'D', '운영 모니터링 단계', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq5';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:model_levels:mcq5';


-- Q6: 반정규화 적용 단계
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'MCQ', 'NORMAL',
       '반정규화를 적용하기에 가장 적절한 단계는?',
       NULL,
       'D',
       '보통 논리 모델 정규화 후, 물리 모델/성능 설계 단계에서 반정규화를 검토합니다.',
       'seed:prac:model_levels:mcq6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:mcq6');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '업무 요구 분석 단계', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq6'
UNION ALL
SELECT q.id, 'B', '개념 모델 작성 직후', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq6'
UNION ALL
SELECT q.id, 'C', '논리 모델을 그리기 전', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq6'
UNION ALL
SELECT q.id, 'D', '정규화된 논리 모델을 바탕으로 물리 모델/성능 설계 단계에서', 1
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq6';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:model_levels:mcq6';


-- Q7: 개념/논리 구분 예시
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 “개념 모델 → 논리 모델”로 발전시키는 예로 가장 적절한 것은?',
       NULL,
       'B',
       '엔터티에 속성/키를 상세히 붙이고, 관계의 카디널리티를 정제하는 것이 논리 모델 작업입니다.',
       'seed:prac:model_levels:mcq7'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:mcq7');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '테이블에 인덱스를 추가한다.', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq7'
UNION ALL
SELECT q.id, 'B', '개념 엔터티에 상세 속성과 기본키/외래키를 정의하고, 관계 참여도를 명확히 한다.', 1
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq7'
UNION ALL
SELECT q.id, 'C', 'DB 서버의 CPU 코어 수를 늘린다.', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq7'
UNION ALL
SELECT q.id, 'D', '쿼리 실행 계획을 분석한다.', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq7';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:prac:model_levels:mcq7';


-- Q8: 논리/물리 차이 예시
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'MCQ', 'NORMAL',
       '다음 중 “논리 모델”과 “물리 모델”의 차이를 설명한 것 중 가장 적절한 것은?',
       NULL,
       'C',
       '논리는 DBMS 독립, 물리는 특정 DBMS에 맞춘 구체 구조입니다.',
       'seed:prac:model_levels:mcq8'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:mcq8');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '논리 모델은 ERD를 사용하지 않는다.', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq8'
UNION ALL
SELECT q.id, 'B', '물리 모델은 비즈니스 용어를 사용하고, 논리 모델은 DB 용어를 사용한다.', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq8'
UNION ALL
SELECT q.id, 'C', '논리 모델은 DBMS에 독립적인 구조, 물리 모델은 특정 DBMS의 자료형/인덱스를 포함한 구조를 정의한다.', 1
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq8'
UNION ALL
SELECT q.id, 'D', '두 모델은 완전히 동일하며, 이름만 다르다.', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq8';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:prac:model_levels:mcq8';


-- Q9: 모델 변경 영향
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'MCQ', 'NORMAL',
       '물리 모델에서 특정 테이블을 파티셔닝하도록 변경할 때, 가장 직접적인 영향이 있는 것은?',
       NULL,
       'D',
       '파티셔닝은 주로 성능/운영(백업, 유지보수)에 영향을 줍니다.',
       'seed:prac:model_levels:mcq9'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:mcq9');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '비즈니스 용어 정의서', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq9'
UNION ALL
SELECT q.id, 'B', '사용자 매뉴얼의 글꼴', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq9'
UNION ALL
SELECT q.id, 'C', '요구사항 명세서의 범위', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq9'
UNION ALL
SELECT q.id, 'D', '대용량 데이터 조회/백업/보관과 관련된 성능 및 운영 방법', 1
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq9';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:prac:model_levels:mcq9';


-- Q10: 모델 품질 평가
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31201, 'PRACTICAL', 'MCQ', 'NORMAL',
       '데이터 모델의 품질을 평가할 때 보는 항목으로 가장 거리가 먼 것은?',
       NULL,
       'A',
       '엔터티/관계의 완전성, 중복 여부, 정규화 정도 등이 품질 기준입니다.',
       'seed:prac:model_levels:mcq10'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:model_levels:mcq10');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '개발자의 취미 생활', 1
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq10'
UNION ALL
SELECT q.id, 'B', '엔터티/관계가 업무 요구를 빠짐없이 반영하는지', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq10'
UNION ALL
SELECT q.id, 'C', '중복 데이터와 이상 현상이 최소화되었는지', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq10'
UNION ALL
SELECT q.id, 'D', '향후 확장성과 성능 요구를 어느 정도 수용할 수 있는지', 0
FROM question q WHERE q.source = 'seed:prac:model_levels:mcq10';

UPDATE question
SET answer_key = 'A'
WHERE source = 'seed:prac:model_levels:mcq10';


SET FOREIGN_KEY_CHECKS = 1;
