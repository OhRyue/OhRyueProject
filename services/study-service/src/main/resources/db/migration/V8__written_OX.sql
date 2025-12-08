SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

SET @tp_11201 := 11201; -- 1.2.1 UI 요구사항 확인 및 화면흐름
SET @tp_11301 := 11301; -- 1.3.1 공통 모듈 설계
SET @tp_11302 := 11302; -- 1.3.2 객체지향 설계 원칙

/* =======================================================
 * 11201 – UI 요구사항 확인 및 화면흐름
 *  - OX 6개
 * ======================================================= */

-- [11201] OX 6개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'OX', 'EASY',
       'UI 설계에서 사용자의 작업 순서를 고려한 화면 흐름은 불필요한 이동을 줄이고 효율을 높이는 데 도움이 된다.',
       NULL,
       'O',
       '사용자의 실제 작업 순서를 반영하면 불필요한 이동과 클릭이 줄어 전체 사용성이 좋아집니다.',
       'seed:v5:11201:ox:flow-sequence'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:ox:flow-sequence');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'OX', 'NORMAL',
       '와이어프레임은 실제 디자인 색상과 폰트까지 완전히 반영해야 하므로, 화면 구조만 단순히 표현해서는 안 된다.',
       NULL,
       'X',
       '와이어프레임은 레이아웃과 정보 구조를 중심으로 표현하며, 색상·폰트는 보통 간략하게 표현하거나 생략합니다.',
       'seed:v5:11201:ox:wireframe-purpose'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:ox:wireframe-purpose');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'OX', 'NORMAL',
       'UI 요구사항에는 단순히 버튼 위치와 색상만 정의하면 되며, 오류 메시지나 피드백 방식은 구현 단계에서 자연스럽게 결정된다.',
       NULL,
       'X',
       '오류 메시지·피드백 방식 등은 사용성에 큰 영향을 주므로 UI 요구사항 단계에서 함께 정의하는 것이 좋습니다.',
       'seed:v5:11201:ox:feedback'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:ox:feedback');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'OX', 'EASY',
       '일관된 컴포넌트 스타일과 인터랙션 패턴은 학습 비용을 줄여 사용자 경험을 향상시킨다.',
       NULL,
       'O',
       '일관된 패턴은 사용자가 매 화면마다 새로 학습하지 않아도 되도록 도와줍니다.',
       'seed:v5:11201:ox:consistency'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:ox:consistency');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'OX', 'NORMAL',
       '사용성 테스트는 실제 사용자를 대상으로 UI 시안의 문제를 발견하는 효과적인 방법이지만, 반드시 정식 출시 이후에만 수행할 수 있다.',
       NULL,
       'X',
       '프로토타입 단계에서도 사용성 테스트를 수행해 문제를 조기에 발견할 수 있습니다.',
       'seed:v5:11201:ox:usability-test'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:ox:usability-test');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'OX', 'NORMAL',
       '접근성 고려는 장애인 사용자에게만 필요한 요구사항이므로, 일반 업무 시스템에서는 무시해도 무방하다.',
       NULL,
       'X',
       '접근성은 다양한 사용자 환경(연령, 기기, 네트워크 등)에 공통적으로 영향을 주는 중요한 요구사항입니다.',
       'seed:v5:11201:ox:accessibility'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:ox:accessibility');


/* =======================================================
 * 11301 – 공통 모듈 설계
 *  - OX 6개
 * ======================================================= */

-- [11301] OX 6개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'OX', 'EASY',
       '공통 모듈은 여러 시스템이나 서브시스템에서 반복적으로 사용하는 기능을 캡슐화해 재사용성을 높이기 위한 것이다.',
       NULL,
       'O',
       '공통 모듈의 핵심 목적은 중복 구현을 줄이고 재사용 가능한 기능 단위를 제공하는 것입니다.',
       'seed:v5:11301:ox:common-purpose'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:ox:common-purpose');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈은 특정 화면이나 특정 업무에 강하게 종속되도록 설계하는 것이 재사용성을 높이는 데 유리하다.',
       NULL,
       'X',
       '공통 모듈은 특정 UI나 업무에 종속되지 않도록 설계해야 재사용성이 높아집니다.',
       'seed:v5:11301:ox:tight-coupling'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:ox:tight-coupling');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈의 인터페이스는 가능한 한 명확하고 단순하게 설계하는 것이 유지보수에 유리하다.',
       NULL,
       'O',
       '단순하고 명확한 인터페이스는 사용성을 높이고 변경 시 영향을 줄여줍니다.',
       'seed:v5:11301:ox:interface-simple'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:ox:interface-simple');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈은 예외 상황을 내부에서 모두 숨기고, 호출자에게는 정상 처리 결과만 반환하는 것이 바람직하다.',
       NULL,
       'X',
       '공통 모듈은 예외 발생 시 적절한 예외 타입이나 오류 코드를 호출자에게 전달해야 합니다.',
       'seed:v5:11301:ox:error-handling'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:ox:error-handling');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈의 변경 이력과 버전을 체계적으로 관리하지 않으면, 여러 시스템에서 사용하는 경우 문제를 추적하기 어려워질 수 있다.',
       NULL,
       'O',
       '여러 소비자가 있는 공통 모듈은 버전·변경 이력을 관리하지 않으면 호환성 문제가 자주 발생합니다.',
       'seed:v5:11301:ox:versioning'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:ox:versioning');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈은 성능 이슈와는 무관하므로, 부하 테스트 대상에서 제외해도 무방하다.',
       NULL,
       'X',
       '여러 시스템에서 동시에 호출되는 공통 모듈은 성능·스케일링 측면에서도 중요한 대상입니다.',
       'seed:v5:11301:ox:perf'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:ox:perf');


/* =======================================================
 * 11302 – 객체지향 설계 원칙
 *  - OX 6개
 * ======================================================= */

-- [11302] OX 6개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'OX', 'EASY',
       '단일 책임 원칙(SRP)은 클래스가 하나의 책임만 가지도록 설계하라는 객체지향 설계 원칙이다.',
       NULL,
       'O',
       'SRP는 변경 이유가 한 가지인 클래스를 만들자는 원칙입니다.',
       'seed:v5:11302:ox:srp'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:ox:srp');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'OX', 'NORMAL',
       '개방-폐쇄 원칙(OCP)은 기능 확장에 닫혀 있고, 코드 수정에는 열려 있어야 한다는 원칙이다.',
       NULL,
       'X',
       'OCP는 확장에는 열려 있고, 수정에는 닫혀 있도록 설계하라는 원칙입니다.',
       'seed:v5:11302:ox:ocp'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:ox:ocp');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'OX', 'NORMAL',
       '리스코프 치환 원칙(LSP)에 따르면, 하위 클래스는 상위 타입이 사용되는 곳에서 문제없이 대체 가능해야 한다.',
       NULL,
       'O',
       'LSP는 하위 타입이 상위 타입의 계약을 깨지 않고 대체될 수 있어야 한다는 원칙입니다.',
       'seed:v5:11302:ox:lsp'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:ox:lsp');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'OX', 'NORMAL',
       '의존성 역전 원칙(DIP)은 고수준 모듈이 저수준 모듈에 직접 의존하도록 설계하라는 원칙이다.',
       NULL,
       'X',
       'DIP는 고수준/저수준 모듈 모두 추상화에 의존하도록 설계하자는 원칙입니다.',
       'seed:v5:11302:ox:dip'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:ox:dip');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 분리 원칙(ISP)은 클라이언트가 사용하지 않는 기능까지 담긴 비대한 인터페이스에 의존하지 않도록 하라는 원칙이다.',
       NULL,
       'O',
       'ISP는 클라이언트별로 필요한 인터페이스만 제공하자는 원칙입니다.',
       'seed:v5:11302:ox:isp'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:ox:isp');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'OX', 'NORMAL',
       '높은 응집도와 낮은 결합도는 객체지향 설계에서 지향해야 할 중요한 품질 특성이다.',
       NULL,
       'O',
       '모듈 내부는 관련있는 기능끼리 모으고, 모듈 간 의존성은 최소화하는 것이 좋습니다.',
       'seed:v5:11302:ox:cohesion-coupling'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:ox:cohesion-coupling');


/* ===========================================
 * TAG 매핑 – topic별 1개 태그
 * 11201 → 'UI'
 * 11301 → '공통모듈'
 * 11302 → 'OOP'
 * =========================================== */

-- 11201 – UI 요구사항 / 화면흐름
INSERT INTO question_tag (question_id, tag)
SELECT q.id, 'UI'
FROM question q
LEFT JOIN question_tag qt ON qt.question_id = q.id
WHERE q.cert_id = @cert_id
  AND q.topic_id = @tp_11201
  AND qt.id IS NULL;

-- 11301 – 공통 모듈 설계
INSERT INTO question_tag (question_id, tag)
SELECT q.id, '공통모듈'
FROM question q
LEFT JOIN question_tag qt ON qt.question_id = q.id
WHERE q.cert_id = @cert_id
  AND q.topic_id = @tp_11301
  AND qt.id IS NULL;

-- 11302 – 객체지향 설계 원칙
INSERT INTO question_tag (question_id, tag)
SELECT q.id, 'OOP'
FROM question q
LEFT JOIN question_tag qt ON qt.question_id = q.id
WHERE q.cert_id = @cert_id
  AND q.topic_id = @tp_11302
  AND qt.id IS NULL;

SET FOREIGN_KEY_CHECKS = 1;
