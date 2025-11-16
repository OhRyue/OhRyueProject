SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id   := 1;
SET @tp_31101  := 31101; -- 3.1.1 업무 요구사항 분석(시나리오)
SET @tp_31102  := 31102; -- 3.1.2 데이터 요구사항 도출
SET @tp_31201  := 31201; -- 3.1.3 모델링 수준(개념/논리/물리)


/* =======================================================
 * 31101 – 요구사항 확인: 업무 시나리오 분석
 *  - PRACTICAL OX 총 6개 (여기서는 3개 추가)
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


/* =======================================================
 * 31102 – 요구사항 확인: 데이터 요구사항 도출
 *  - 목표: PRACTICAL OX 총 6개 (여기서는 3개 추가)
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


/* =======================================================
 * 31201 – 데이터 모델링: 개념/논리/물리 모델
 *  - PRACTICAL OX 총 6개 (여기서는 3개 추가)
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


/* ===========================================
 * PRACTICAL – 31101 업무 요구사항 분석
 * SHORT 6개 + LONG 4개
 * =========================================== */

-- SHORT 1
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31101, 'PRACTICAL', 'SHORT', 'NORMAL',
  '다음 시나리오에서 “주요 Actor”를 두 가지 이상 적으세요. (고객이 주문을 생성하면 결제 시스템이 승인하고, 재고 시스템이 차감 처리한다.)',
  NULL,
  '고객, 결제시스템, 재고시스템',
  'Actor는 “행위를 유발하는 주체”입니다. 고객(주문 생성), 결제시스템(승인), 재고시스템(차감)이 핵심 Actor입니다.',
  'seed:prac:31101:short1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31101:short1');

-- SHORT 2
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31101, 'PRACTICAL', 'SHORT', 'NORMAL',
  '“이벤트(Event)”를 최소 두 가지 적으세요. (고객 주문 생성 → 결제 승인 → 재고 차감)',
  NULL,
  '주문 생성, 결제 승인, 재고 차감',
  'Actor의 행동으로 발생하는 상태 변화 또는 작업을 Event라고 합니다.',
  'seed:prac:31101:short2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31101:short2');

-- SHORT 3
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31101, 'PRACTICAL', 'SHORT', 'NORMAL',
  '다음 시나리오에서 “업무 흐름의 단계”를 순서에 맞게 적으세요. (회원가입 → 상품 선택 → 주문 → 결제)',
  NULL,
  '회원가입 → 상품 선택 → 주문 → 결제',
  '업무 흐름은 사용자 행동의 순차적 과정입니다.',
  'seed:prac:31101:short3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31101:short3');

-- SHORT 4
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31101, 'PRACTICAL', 'SHORT', 'NORMAL',
  '“업무 시나리오를 모델링할 때 반드시 확인해야 하는 정보” 두 가지를 쓰세요.',
  NULL,
  'Actor와 상태 변화(또는 업무 단계)',
  '실기에서는 Actor, 행동, 데이터 변화 중 최소 두 항목을 적어야 합니다.',
  'seed:prac:31101:short4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31101:short4');

-- SHORT 5
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31101, 'PRACTICAL', 'SHORT', 'NORMAL',
  '주문 처리 시나리오에서 “상태(state)”의 예를 두 가지 적으세요. (예: 주문상태, 결제상태 등)',
  NULL,
  '주문상태, 결제상태',
  '상태는 업무 프로세스의 특정 시점의 데이터 조건을 의미합니다.',
  'seed:prac:31101:short5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31101:short5');

-- SHORT 6
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31101, 'PRACTICAL', 'SHORT', 'NORMAL',
  '시나리오 분석 시 “시스템 경계(System Boundary)”를 확인하는 이유를 한 문장으로 적으세요.',
  NULL,
  '어떤 기능이 시스템 내부/외부 책임인지 구분하기 위해서',
  '외부 Actor vs 내부 시스템 기능을 나누어야 설계가 명확해집니다.',
  'seed:prac:31101:short6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31101:short6');


/* ===== LONG 문제 4개 ===== */

-- LONG 1
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31101, 'PRACTICAL', 'LONG', 'NORMAL',
  '다음 시나리오를 기반으로 주요 Actor, 이벤트 흐름, 상태 변화를 분석하여 5줄 내로 정리하세요.
(“고객은 장바구니에 상품을 담고, 주문을 생성하며, 결제가 완료되면 배송이 시작된다.”)',
  NULL,
  'Actor: 고객, 결제시스템
Event: 장바구니 담기 → 주문 생성 → 결제 완료 → 배송 시작
State: 주문상태(생성→결제완료→배송중)
핵심 데이터: 상품, 주문, 결제정보
업무 흐름: 고객 중심의 순차적 처리',
  '핵심은 Actor/이벤트/상태로 구분해 시나리오를 구조화하는 것입니다.',
  'seed:prac:31101:long1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31101:long1');

-- LONG 2
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31101, 'PRACTICAL', 'LONG', 'NORMAL',
  '“반품 요청이 들어오면 재고 복구 후 환불 처리한다”라는 시나리오를 기반으로
업무 프로세스를 (1) Actor, (2) 이벤트, (3) 상태 흐름 관점에서 설명하세요.',
  NULL,
  'Actor: 고객, 재고시스템, 결제시스템
Event: 반품 요청 → 재고 복구 → 환불 처리
State: 주문상태(반품요청→복구완료→환불완료)',
  '업무 시나리오를 3개 축(Actor/Event/State)으로 분석하는 것이 실기 기본입니다.',
  'seed:prac:31101:long2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31101:long2');

-- LONG 3
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31101, 'PRACTICAL', 'LONG', 'HARD',
  '다음 시나리오에서 “숨겨진 요구사항(Implicit Requirement)”을 2개 이상 찾아 설명하세요.
(“회원은 주문 내역을 조회할 수 있고, 배송 중인 상품의 위치를 확인할 수 있다.”)',
  NULL,
  '숨겨진 요구:
1) 주문 상세 화면이 있어야 한다
2) 배송추적 API 연동 필요
3) 배송 이력 저장 필요',
  '명시되지 않은 보조 기능(조회 화면, 데이터 저장, 외부 연동)을 찾아내는 것이 핵심입니다.',
  'seed:prac:31101:long3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31101:long3');

-- LONG 4
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31101, 'PRACTICAL', 'LONG', 'HARD',
  '업무 시나리오 “예약 취소 시 즉시 환불하고 좌석을 재오픈한다”를 분석하여
설계 시 필요한 데이터, 이벤트, 상태 변화 관점을 6줄 내로 정리하세요.',
  NULL,
  '데이터: 좌석, 예약, 고객, 환불이력
Event: 예약 취소 → 환불 처리 → 좌석 재오픈
State: 예약상태(확정→취소), 좌석상태(사용중→가능)',
  '업무/데이터/상태를 나눠서 보는 것이 실기형 모델링의 핵심입니다.',
  'seed:prac:31101:long4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31101:long4');



/* ===========================================
 * PRACTICAL – 31102 데이터 요구사항 도출
 * SHORT 6개 + LONG 4개
 * =========================================== */

-- SHORT 1
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31102, 'PRACTICAL', 'SHORT', 'NORMAL',
  '다음 요구에서 필요한 “기본 데이터 항목”을 두 가지 이상 적으세요.
(“배송 지연 시 고객에게 알림 발송”)',
  NULL,
  '주문ID, 고객연락처, 지연사유, 배송상태',
  '업무 수행을 위해 저장하거나 조회해야 하는 항목을 데이터 요구라 합니다.',
  'seed:prac:31102:short1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31102:short1');

-- SHORT 2
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31102, 'PRACTICAL', 'SHORT', 'NORMAL',
  '데이터 요구를 정리할 때 “식별자(Identifier)”가 중요한 이유를 한 문장으로 쓰세요.',
  NULL,
  '엔터티를 유일하게 구분해야 데이터 무결성과 조회 정확성을 보장하기 때문',
  '식별자 정의는 실기에서 가장 핵심적인 데이터 요구 분석 요소입니다.',
  'seed:prac:31102:short2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31102:short2');

-- SHORT 3
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31102, 'PRACTICAL', 'SHORT', 'NORMAL',
  '주문 시스템에서 필수 데이터 속성 3개를 적으세요.',
  NULL,
  '주문ID, 고객ID, 주문일자',
  '업무 최소 단위에 필요한 데이터 속성을 떠올리면 됩니다.',
  'seed:prac:31102:short3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31102:short3');

-- SHORT 4
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31102, 'PRACTICAL', 'SHORT', 'NORMAL',
  '장바구니 기능을 위해 반드시 필요한 데이터 두 가지를 적으세요.',
  NULL,
  '상품ID, 수량(또는 담은 날짜)',
  '업무 요구(담기·변경)와 데이터 요구(저장 항목)는 구분해야 합니다.',
  'seed:prac:31102:short4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31102:short4');

-- SHORT 5
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31102, 'PRACTICAL', 'SHORT', 'NORMAL',
  '데이터 요구 도출 시 “로그/이력 데이터”를 포함해야 하는 이유를 설명하세요.',
  NULL,
  '장애 분석, 고객 문의 대응, 변경 추적을 위해 필요하기 때문',
  '저장 목적이 다양하다는 점을 언급하면 정답 처리됩니다.',
  'seed:prac:31102:short5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31102:short5');

-- SHORT 6
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31102, 'PRACTICAL', 'SHORT', 'NORMAL',
  '데이터 요구를 도출할 때 비기능요구(성능/보안)가 왜 고려되어야 하는지 한 문장으로 쓰세요.',
  NULL,
  '성능·보안 요구에 따라 저장 방식, 조회 인덱스, 암호화 등이 달라지기 때문',
  '데이터 구조는 비기능 요구의 영향을 강하게 받습니다.',
  'seed:prac:31102:short6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31102:short6');


/* ===== LONG 문제 4개 ===== */

-- LONG 1
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31102, 'PRACTICAL', 'LONG', 'NORMAL',
  '“온라인 서점 장바구니 기능”에서 업무 요구와 데이터 요구를 각각 3개 이상 정리하세요.',
  NULL,
  '업무 요구: 담기, 수량 변경, 삭제
데이터 요구: 장바구니ID, 상품ID, 수량, 담은 시각',
  '업무(행위)와 데이터(저장 항목) 구분이 핵심입니다.',
  'seed:prac:31102:long1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31102:long1');

-- LONG 2
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31102, 'PRACTICAL', 'LONG', 'NORMAL',
  '“환불 처리 업무” 수행을 위해 필요한 데이터 요구사항을 5개 이상 서술하세요.',
  NULL,
  '주문ID, 환불금액, 환불사유, 환불일시, 고객ID',
  '환불은 법적/재무 요구사항이 포함되므로 데이터 범위가 넓습니다.',
  'seed:prac:31102:long2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31102:long2');

-- LONG 3
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31102, 'PRACTICAL', 'LONG', 'HARD',
  '배송 추적 시스템에서 필요한 “데이터 모델(엔터티+속성)”을 직접 구성해 5줄 내로 설명하세요.',
  NULL,
  '엔터티: 주문, 배송, 배송이력
속성 예: 배송ID, 배송상태, 위치, 이력시간
주문과 배송은 1:N 관계',
  '관계형 모델링의 기본을 적용하면 정답 처리됩니다.',
  'seed:prac:31102:long3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31102:long3');

-- LONG 4
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31102, 'PRACTICAL', 'LONG', 'HARD',
  '업무 요구 “재고부족 알림”에서 데이터 요구(저장 필요 항목)를 5개 이상 도출하여 설명하세요.',
  NULL,
  '상품ID, 현재재고, 임계재고량, 알림일시, 알림발송여부',
  '업무 요구 → 데이터 요구로 변환하는 능력을 평가하는 문제입니다.',
  'seed:prac:31102:long4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31102:long4');



/* ===========================================
 * PRACTICAL – 31201 모델링 수준(개념/논리/물리)
 * SHORT 6개 + LONG 4개
 * =========================================== */

-- SHORT 1
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31201, 'PRACTICAL', 'SHORT', 'NORMAL',
  '개념 모델(Conceptual Model)의 특징을 한 문장으로 설명하세요.',
  NULL,
  '업무 관점에서 엔터티와 관계만 정의하는 상위 수준 모델',
  '업무 용어 기반, 속성 최소, 관계 중심이라는 특징을 넣으면 정답 처리됩니다.',
  'seed:prac:31201:short1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31201:short1');

-- SHORT 2
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31201, 'PRACTICAL', 'SHORT', 'NORMAL',
  '논리 모델(Logical Model)에서 수행해야 하는 핵심 작업 두 가지를 적으세요.',
  NULL,
  '정규화, 키/속성 정의',
  '정규화와 속성 정의는 논리 모델의 필수 항목입니다.',
  'seed:prac:31201:short2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31201:short2');

-- SHORT 3
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31201, 'PRACTICAL', 'SHORT', 'NORMAL',
  '물리 모델(Physical Model)에서 정의해야 하는 요소 두 가지를 적으세요.',
  NULL,
  '데이터 타입, 인덱스',
  '실제 DBMS에 배포될 구조를 정의하는 단계입니다.',
  'seed:prac:31201:short3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31201:short3');

-- SHORT 4
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31201, 'PRACTICAL', 'SHORT', 'NORMAL',
  '개념/논리 모델의 차이를 한 문장으로 요약하세요.',
  NULL,
  '개념은 비즈니스 개념 중심, 논리는 정규화된 속성·키 중심',
  '두 모델의 초점 차이를 정확히 기술하면 정답입니다.',
  'seed:prac:31201:short4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31201:short4');

-- SHORT 5
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31201, 'PRACTICAL', 'SHORT', 'NORMAL',
  '논리 모델을 물리 모델로 변환할 때 필요한 작업 한 가지를 적으세요.',
  NULL,
  'DBMS 자료형 지정',
  'VARCHAR/NVARCHAR/INT 등 실제 타입을 부여하는 것이 핵심입니다.',
  'seed:prac:31201:short5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31201:short5');

-- SHORT 6
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31201, 'PRACTICAL', 'SHORT', 'NORMAL',
  '모델링 단계에서 “정규화”를 수행하는 이유를 한 문장으로 설명하세요.',
  NULL,
  '이상현상을 방지하고 데이터 무결성을 향상시키기 위해서',
  '정규화는 논리 모델의 중심 작업입니다.',
  'seed:prac:31201:short6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31201:short6');


/* ===== LONG 4개 ===== */

-- LONG 1
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31201, 'PRACTICAL', 'LONG', 'NORMAL',
  '개념→논리→물리 모델로 발전시키는 전체 과정을 6줄 내로 요약해 서술하세요.',
  NULL,
  '개념: 엔터티/관계 도출
논리: 속성/키 정의, 정규화
물리: 테이블/컬럼/타입/제약/인덱스 설계
전체 흐름: 추상 → 구조화 → 구현 단계',
  '업무 관점 → 구조화된 데이터 → DBMS 구조의 순서가 핵심입니다.',
  'seed:prac:31201:long1'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31201:long1');

-- LONG 2
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31201, 'PRACTICAL', 'LONG', 'NORMAL',
  '논리 모델 설계 시 “정규화가 필요한 이유”를 예시와 함께 5줄 이내로 설명하세요.',
  NULL,
  '중복 제거, 이상현상 방지, 무결성 확보.
예: 고객이름이 주문 테이블에 반복되면 변경 시 전체 수정 필요.',
  '정규화의 목적+예시를 함께 적으면 고득점.',
  'seed:prac:31201:long2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31201:long2');

-- LONG 3
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31201, 'PRACTICAL', 'LONG', 'HARD',
  '아래 개념 모델을 논리 모델로 변환하며 필요한 속성과 키를 정의하고, 정규화 관점에서 문제점을 설명하세요.
(엔터티: 학생–수강–과목)',
  NULL,
  '학생: 학생ID(PK), 이름
과목: 과목ID(PK), 이름
수강: 학생ID, 과목ID, 성적
3NF 만족(비키→비키 종속 제거)',
  '개념→논리 변환의 핵심은 속성/키/정규화입니다.',
  'seed:prac:31201:long3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31201:long3');

-- LONG 4
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31201, 'PRACTICAL', 'LONG', 'HARD',
  '물리 모델로 변환할 때 고려해야 할 성능 요소(인덱스/파티션/자료형 등)를 5줄 내로 서술하세요.',
  NULL,
  '인덱스 전략, 파티션 키, 자료형 최소화, 제약조건, I/O 패턴 고려',
  '논리→물리 변환에서 성능 요소 반영은 실기 주요 평가 포인트입니다.',
  'seed:prac:31201:long4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:31201:long4');



/* =========================================================
 * PRACTICAL question_tag 매핑 (31101~31201)
 *  - 31101: 업무시나리오
 *  - 31102: 데이터요구
 *  - 31201: 모델링
 *  규칙:
 *   1) 위에서 정의한 태그들만 사용
 *   2) 기존의 다른 태그는 제거
 *   3) 태그가 전혀 없는 문제는 대표 태그로 새로 매핑
 * ========================================================= */

-- 31101 – 업무 시나리오 해석 → 업무시나리오
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31101
  AND qt.tag <> '업무시나리오';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '업무시나리오'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '업무시나리오'
WHERE q.topic_id = @tp_31101
  AND qt.question_id IS NULL;


-- 31102 – 업무/데이터 요구 도출 → 데이터요구
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31102
  AND qt.tag <> '데이터요구';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '데이터요구'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '데이터요구'
WHERE q.topic_id = @tp_31102
  AND qt.question_id IS NULL;


-- 31201 – 개념/논리/물리 모델링 → 모델링
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31201
  AND qt.tag <> '모델링';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '모델링'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '모델링'
WHERE q.topic_id = @tp_31201
  AND qt.question_id IS NULL;

SET FOREIGN_KEY_CHECKS = 1;
