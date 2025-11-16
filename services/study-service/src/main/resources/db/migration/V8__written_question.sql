SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

SET @tp_11201 := 11201; -- 1.2.1 UI 요구사항 확인 및 화면흐름
SET @tp_11301 := 11301; -- 1.3.1 공통 모듈 설계
SET @tp_11302 := 11302; -- 1.3.2 객체지향 설계 원칙

/* =======================================================
 * 11201 – UI 요구사항 확인 및 화면흐름
 *  - OX 6개, MCQ 10개 추가
 * ======================================================= */

-- [11201] OX 6개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'OX', 'EASY',
       'UI 설계에서 사용자의 작업 순서를 고려한 화면 흐름은 불필요한 이동을 줄이고 효율을 높이는 데 도움이 된다. (O/X)',
       NULL,
       'O',
       '사용자의 실제 작업 순서를 반영하면 불필요한 이동과 클릭이 줄어 전체 사용성이 좋아집니다.',
       'seed:v5:11201:ox:flow-sequence'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:ox:flow-sequence');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'OX', 'NORMAL',
       '와이어프레임은 실제 디자인 색상과 폰트까지 완전히 반영해야 하므로, 화면 구조만 단순히 표현해서는 안 된다. (O/X)',
       NULL,
       'X',
       '와이어프레임은 레이아웃과 정보 구조를 중심으로 표현하며, 색상·폰트는 보통 간략하게 표현하거나 생략합니다.',
       'seed:v5:11201:ox:wireframe-purpose'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:ox:wireframe-purpose');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'OX', 'NORMAL',
       'UI 요구사항에는 단순히 버튼 위치와 색상만 정의하면 되며, 오류 메시지나 피드백 방식은 구현 단계에서 자연스럽게 결정된다. (O/X)',
       NULL,
       'X',
       '오류 메시지·피드백 방식 등은 사용성에 큰 영향을 주므로 UI 요구사항 단계에서 함께 정의하는 것이 좋습니다.',
       'seed:v5:11201:ox:feedback'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:ox:feedback');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'OX', 'EASY',
       '일관된 컴포넌트 스타일과 인터랙션 패턴은 학습 비용을 줄여 사용자 경험을 향상시킨다. (O/X)',
       NULL,
       'O',
       '일관된 패턴은 사용자가 매 화면마다 새로 학습하지 않아도 되도록 도와줍니다.',
       'seed:v5:11201:ox:consistency'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:ox:consistency');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'OX', 'NORMAL',
       '사용성 테스트는 실제 사용자를 대상으로 UI 시안의 문제를 발견하는 효과적인 방법이지만, 반드시 정식 출시 이후에만 수행할 수 있다. (O/X)',
       NULL,
       'X',
       '프로토타입 단계에서도 사용성 테스트를 수행해 문제를 조기에 발견할 수 있습니다.',
       'seed:v5:11201:ox:usability-test'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:ox:usability-test');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'OX', 'NORMAL',
       '접근성 고려는 장애인 사용자에게만 필요한 요구사항이므로, 일반 업무 시스템에서는 무시해도 무방하다. (O/X)',
       NULL,
       'X',
       '접근성은 다양한 사용자 환경(연령, 기기, 네트워크 등)에 공통적으로 영향을 주는 중요한 요구사항입니다.',
       'seed:v5:11201:ox:accessibility'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:ox:accessibility');

-- [11201] MCQ 10개
/* Q1: 화면 흐름 설계 목적 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'MCQ', 'EASY',
       '화면 흐름도를 작성하는 주된 목적에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       '화면 흐름도는 사용자의 작업 절차와 화면 전환 관계를 시각적으로 표현해 요구·설계 간 오해를 줄입니다.',
       'seed:v5:11201:mcq:screen-flow-purpose'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:mcq:screen-flow-purpose');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '데이터베이스 인덱스를 설계하기 위해서', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:screen-flow-purpose'
UNION ALL
SELECT q.id, 'B', '사용자의 작업 절차와 화면 전환 관계를 명확히 공유하기 위해서', 1
FROM question q WHERE q.source = 'seed:v5:11201:mcq:screen-flow-purpose'
UNION ALL
SELECT q.id, 'C', '서버 물리 배치 구성을 나타내기 위해서', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:screen-flow-purpose'
UNION ALL
SELECT q.id, 'D', '암호화 알고리즘을 선정하기 위해서', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:screen-flow-purpose';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11201:mcq:screen-flow-purpose';

/* Q2: 프로토타입 활용 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'MCQ', 'NORMAL',
       'UI 설계에서 프로토타입을 활용하는 주된 이유로 가장 거리가 먼 것은?',
       NULL,
       'D',
       '프로토타입은 요구 검증, 사용성 평가, 이해관계자 간 의사소통에 유용합니다.',
       'seed:v5:11201:mcq:prototype'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:mcq:prototype');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '요구사항이 실제 화면에서 어떻게 보이는지 검증하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:prototype'
UNION ALL
SELECT q.id, 'B', '사용성 테스트를 통해 문제를 조기에 발견하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:prototype'
UNION ALL
SELECT q.id, 'C', '이해관계자 간 UI에 대한 공통 이미지를 형성하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:prototype'
UNION ALL
SELECT q.id, 'D', '데이터베이스 스키마를 자동 생성하기 위해', 1
FROM question q WHERE q.source = 'seed:v5:11201:mcq:prototype';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11201:mcq:prototype';

/* Q3: UI 원칙 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'MCQ', 'EASY',
       '다음 중 좋은 UI 설계 원칙에 해당하지 않는 것은?',
       NULL,
       'D',
       '좋은 UI는 일관성, 가시성, 피드백이 중요하며, 불필요한 전문 용어 남발은 피해야 합니다.',
       'seed:v5:11201:mcq:ui-principles-2'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:mcq:ui-principles-2');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '일관된 컴포넌트와 레이아웃 사용', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:ui-principles-2'
UNION ALL
SELECT q.id, 'B', '상태 변화에 대한 명확한 피드백 제공', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:ui-principles-2'
UNION ALL
SELECT q.id, 'C', '자주 사용하는 기능은 접근 경로를 짧게 제공', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:ui-principles-2'
UNION ALL
SELECT q.id, 'D', '가능한 한 많은 전문 용어와 약어 사용', 1
FROM question q WHERE q.source = 'seed:v5:11201:mcq:ui-principles-2';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11201:mcq:ui-principles-2';

/* Q4: 입력 검증 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'MCQ', 'NORMAL',
       '사용자 입력 검증과 관련된 UI 요구사항으로 가장 적절한 것은?',
       NULL,
       'C',
       '실시간 검증과 명확한 오류 메시지는 사용자가 빠르게 문제를 인지하고 수정하게 도와줍니다.',
       'seed:v5:11201:mcq:validation'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:mcq:validation');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '모든 검증은 서버 응답 후에만 수행한다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:validation'
UNION ALL
SELECT q.id, 'B', '오류 메시지는 가능하면 숨기고, 로그에만 남긴다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:validation'
UNION ALL
SELECT q.id, 'C', '실시간으로 형식 오류를 표시하고, 수정 방법을 명확히 안내한다.', 1
FROM question q WHERE q.source = 'seed:v5:11201:mcq:validation'
UNION ALL
SELECT q.id, 'D', '필수 입력 항목 표시를 생략해 화면을 단순하게 유지한다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:validation';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11201:mcq:validation';

/* Q5: 접근성 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'MCQ', 'NORMAL',
       '접근성을 고려한 UI 설계 방안으로 가장 적절한 것은?',
       NULL,
       'B',
       '충분한 대비, 글자 크기, 대체 텍스트, 키보드 접근성 등은 접근성 향상에 중요한 요소입니다.',
       'seed:v5:11201:mcq:accessibility'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:mcq:accessibility');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '모든 텍스트를 이미지로 만들어 디자인 자유도를 높인다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:accessibility'
UNION ALL
SELECT q.id, 'B', '충분한 색 대비와 폰트 크기, 대체 텍스트를 제공한다.', 1
FROM question q WHERE q.source = 'seed:v5:11201:mcq:accessibility'
UNION ALL
SELECT q.id, 'C', '키보드 사용자는 고려하지 않고 마우스 사용만 전제로 설계한다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:accessibility'
UNION ALL
SELECT q.id, 'D', '에러 메시지를 색상만으로 구분한다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:accessibility';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11201:mcq:accessibility';

/* Q6: 정보 구조(IA) */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'MCQ', 'NORMAL',
       '정보 구조(Information Architecture)를 잘 설계했을 때 기대할 수 있는 효과로 가장 적절한 것은?',
       NULL,
       'C',
       '명확한 정보 구조는 사용자가 원하는 정보를 빠르게 찾게 도와 탐색 시간을 줄입니다.',
       'seed:v5:11201:mcq:ia'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:mcq:ia');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '쿼리 실행 시간이 짧아진다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:ia'
UNION ALL
SELECT q.id, 'B', '네트워크 트래픽이 자동으로 감소한다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:ia'
UNION ALL
SELECT q.id, 'C', '사용자가 원하는 정보를 빠르게 찾을 수 있다.', 1
FROM question q WHERE q.source = 'seed:v5:11201:mcq:ia'
UNION ALL
SELECT q.id, 'D', '데이터베이스 백업 시간이 줄어든다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:ia';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11201:mcq:ia';

/* Q7: UI 패턴 재사용 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'MCQ', 'NORMAL',
       '로그인, 검색, 목록/상세와 같이 반복적으로 등장하는 UI 패턴을 재사용하는 주된 이유로 가장 알맞은 것은?',
       NULL,
       'B',
       '반복되는 패턴을 재사용하면 일관성과 개발 효율을 동시에 확보할 수 있습니다.',
       'seed:v5:11201:mcq:pattern-reuse'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:mcq:pattern-reuse');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '디자인 팀의 역할을 최소화하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:pattern-reuse'
UNION ALL
SELECT q.id, 'B', '일관된 경험을 제공하고, 구현·유지보수를 효율화하기 위해', 1
FROM question q WHERE q.source = 'seed:v5:11201:mcq:pattern-reuse'
UNION ALL
SELECT q.id, 'C', '코드를 복잡하게 보이게 하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:pattern-reuse'
UNION ALL
SELECT q.id, 'D', '각 화면을 완전히 다른 구조로 만들기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:pattern-reuse';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11201:mcq:pattern-reuse';

/* Q8: 에러 메시지 작성 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'MCQ', 'NORMAL',
       '좋은 에러 메시지 작성 원칙으로 가장 적절한 것은?',
       NULL,
       'C',
       '사용자 관점에서 무엇이 문제이고 어떻게 해결할 수 있는지를 안내하는 것이 중요합니다.',
       'seed:v5:11201:mcq:error-message'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:mcq:error-message');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '내부 예외 메시지와 스택 트레이스를 그대로 노출한다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:error-message'
UNION ALL
SELECT q.id, 'B', '사용자에게 책임을 전가하는 표현을 사용한다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:error-message'
UNION ALL
SELECT q.id, 'C', '무엇이 문제인지와 해결 방법을 간단한 언어로 안내한다.', 1
FROM question q WHERE q.source = 'seed:v5:11201:mcq:error-message'
UNION ALL
SELECT q.id, 'D', '에러 코드 숫자만 보여주고 별도의 안내는 제공하지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:error-message';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11201:mcq:error-message';

/* Q9: 반응형 UI */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'MCQ', 'NORMAL',
       '반응형 UI 요구사항으로 가장 적절한 것은?',
       NULL,
       'B',
       '주요 화면에서 모바일/태블릿/PC 해상도별 레이아웃 동작을 정의하는 것이 일반적입니다.',
       'seed:v5:11201:mcq:responsive'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:mcq:responsive');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'PC 해상도만 지원하고 모바일은 고려하지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:responsive'
UNION ALL
SELECT q.id, 'B', '핵심 화면에 대해 모바일/태블릿/PC별 레이아웃 동작을 정의한다.', 1
FROM question q WHERE q.source = 'seed:v5:11201:mcq:responsive'
UNION ALL
SELECT q.id, 'C', '모바일에서는 모든 기능을 비활성화한다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:responsive'
UNION ALL
SELECT q.id, 'D', '기기별로 전혀 다른 기능을 제공해 사용자를 혼란스럽게 한다.', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:responsive';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11201:mcq:responsive';

/* Q10: UI 요구사항 명세 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11201, 'WRITTEN', 'MCQ', 'NORMAL',
       'UI 요구사항 명세서에 포함되기 어려운 항목은?',
       NULL,
       'D',
       'UI 요구사항에는 화면 레이아웃, 흐름, 상호작용, 접근성, 에러 처리 등이 포함됩니다.',
       'seed:v5:11201:mcq:ui-spec'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11201:mcq:ui-spec');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '화면별 주요 컴포넌트와 레이아웃', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:ui-spec'
UNION ALL
SELECT q.id, 'B', '화면 전환 흐름과 조건', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:ui-spec'
UNION ALL
SELECT q.id, 'C', '에러 상황에서의 안내 메시지 방식', 0
FROM question q WHERE q.source = 'seed:v5:11201:mcq:ui-spec'
UNION ALL
SELECT q.id, 'D', '데이터베이스 백업 주기와 전략', 1
FROM question q WHERE q.source = 'seed:v5:11201:mcq:ui-spec';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11201:mcq:ui-spec';


/* =======================================================
 * 11301 – 공통 모듈 설계
 *  - OX 6개, MCQ 10개 추가
 * ======================================================= */

-- [11301] OX 6개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'OX', 'EASY',
       '공통 모듈은 여러 시스템이나 서브시스템에서 반복적으로 사용하는 기능을 캡슐화해 재사용성을 높이기 위한 것이다. (O/X)',
       NULL,
       'O',
       '공통 모듈의 핵심 목적은 중복 구현을 줄이고 재사용 가능한 기능 단위를 제공하는 것입니다.',
       'seed:v5:11301:ox:common-purpose'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:ox:common-purpose');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈은 특정 화면이나 특정 업무에 강하게 종속되도록 설계하는 것이 재사용성을 높이는 데 유리하다. (O/X)',
       NULL,
       'X',
       '공통 모듈은 특정 UI나 업무에 종속되지 않도록 설계해야 재사용성이 높아집니다.',
       'seed:v5:11301:ox:tight-coupling'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:ox:tight-coupling');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈의 인터페이스는 가능한 한 명확하고 단순하게 설계하는 것이 유지보수에 유리하다. (O/X)',
       NULL,
       'O',
       '단순하고 명확한 인터페이스는 사용성을 높이고 변경 시 영향을 줄여줍니다.',
       'seed:v5:11301:ox:interface-simple'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:ox:interface-simple');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈은 예외 상황을 내부에서 모두 숨기고, 호출자에게는 정상 처리 결과만 반환하는 것이 바람직하다. (O/X)',
       NULL,
       'X',
       '공통 모듈은 예외 발생 시 적절한 예외 타입이나 오류 코드를 호출자에게 전달해야 합니다.',
       'seed:v5:11301:ox:error-handling'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:ox:error-handling');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈의 변경 이력과 버전을 체계적으로 관리하지 않으면, 여러 시스템에서 사용하는 경우 문제를 추적하기 어려워질 수 있다. (O/X)',
       NULL,
       'O',
       '여러 소비자가 있는 공통 모듈은 버전·변경 이력을 관리하지 않으면 호환성 문제가 자주 발생합니다.',
       'seed:v5:11301:ox:versioning'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:ox:versioning');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈은 성능 이슈와는 무관하므로, 부하 테스트 대상에서 제외해도 무방하다. (O/X)',
       NULL,
       'X',
       '여러 시스템에서 동시에 호출되는 공통 모듈은 성능·스케일링 측면에서도 중요한 대상입니다.',
       'seed:v5:11301:ox:perf'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:ox:perf');

-- [11301] MCQ 10개
/* Q1: 공통 모듈 후보 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'MCQ', 'EASY',
       '다음 중 공통 모듈로 분리하기에 가장 적절한 기능은?',
       NULL,
       'C',
       '여러 시스템에서 공통으로 사용하는 인증/로그/메시지 포맷 변환 등은 전형적인 공통 모듈 후보입니다.',
       'seed:v5:11301:mcq:candidate'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:mcq:candidate');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '특정 화면의 배경 이미지 설정 기능', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:candidate'
UNION ALL
SELECT q.id, 'B', '개별 페이지에서만 사용하는 임시 계산 로직', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:candidate'
UNION ALL
SELECT q.id, 'C', '여러 서비스에서 공통으로 사용하는 인증 토큰 검증 기능', 1
FROM question q WHERE q.source = 'seed:v5:11301:mcq:candidate'
UNION ALL
SELECT q.id, 'D', '단 한 번만 사용되는 데이터 정제 스크립트', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:candidate';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11301:mcq:candidate';

/* Q2: 공통 모듈 인터페이스 설계 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'MCQ', 'NORMAL',
       '공통 모듈 인터페이스 설계 시 가장 바람직한 방향은?',
       NULL,
       'B',
       '불필요한 의존성을 줄이고, 의미 있는 입력·출력 모델을 사용해 인터페이스를 단순하게 유지하는 것이 좋습니다.',
       'seed:v5:11301:mcq:interface-design'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:mcq:interface-design');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '모든 내부 구현 클래스를 외부에 공개한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:interface-design'
UNION ALL
SELECT q.id, 'B', '입출력 모델을 명확히 정의하고, 최소한의 파라미터로 기능을 캡슐화한다.', 1
FROM question q WHERE q.source = 'seed:v5:11301:mcq:interface-design'
UNION ALL
SELECT q.id, 'C', '호출자가 내부 필드를 직접 조작할 수 있도록 한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:interface-design'
UNION ALL
SELECT q.id, 'D', '에러 발생 시 항상 null을 반환해서 단순화한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:interface-design';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11301:mcq:interface-design';

/* Q3: 공통 모듈 이점 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'MCQ', 'EASY',
       '공통 모듈 도입으로 기대할 수 있는 효과로 가장 거리가 먼 것은?',
       NULL,
       'D',
       '공통 모듈은 중복 코드 제거, 일관성 향상, 변경 비용 감소에 도움이 됩니다.',
       'seed:v5:11301:mcq:benefit'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:mcq:benefit');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '중복 코드 감소', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:benefit'
UNION ALL
SELECT q.id, 'B', '비즈니스 규칙의 일관성 향상', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:benefit'
UNION ALL
SELECT q.id, 'C', '변경 시 수정 범위 축소', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:benefit'
UNION ALL
SELECT q.id, 'D', '각 시스템마다 규칙을 제각각 구현하도록 유도', 1
FROM question q WHERE q.source = 'seed:v5:11301:mcq:benefit';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11301:mcq:benefit';

/* Q4: 공통 모듈 버전 관리 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'MCQ', 'NORMAL',
       '여러 시스템이 사용하는 공통 모듈 버전을 업그레이드할 때 가장 먼저 고려해야 할 사항은?',
       NULL,
       'C',
       '하위 호환성과 영향 범위를 분석하고, 점진적으로 배포하는 전략이 필요합니다.',
       'seed:v5:11301:mcq:version-upgrade'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:mcq:version-upgrade');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '호출하는 모든 시스템을 동시에 중단하고 일괄 교체한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:version-upgrade'
UNION ALL
SELECT q.id, 'B', '기존 인터페이스를 모두 제거하고 새로운 것만 지원한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:version-upgrade'
UNION ALL
SELECT q.id, 'C', '하위 호환성 여부와 영향받는 시스템을 파악하고, 단계적으로 배포 계획을 수립한다.', 1
FROM question q WHERE q.source = 'seed:v5:11301:mcq:version-upgrade'
UNION ALL
SELECT q.id, 'D', '테스트 없이 바로 운영에 적용한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:version-upgrade';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11301:mcq:version-upgrade';

/* Q5: 공통 모듈 성능 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'MCQ', 'NORMAL',
       '공통 모듈의 성능을 검증할 때 가장 적절한 접근 방법은?',
       NULL,
       'B',
       '공통 모듈은 여러 시스템에서 동시에 사용되므로, 예상 호출량과 부하를 고려한 성능 테스트가 필요합니다.',
       'seed:v5:11301:mcq:perf-test'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:mcq:perf-test');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '단일 사용자만을 대상으로 간단한 수동 테스트만 수행한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:perf-test'
UNION ALL
SELECT q.id, 'B', '예상 동시 호출 수를 고려한 부하 테스트를 수행한다.', 1
FROM question q WHERE q.source = 'seed:v5:11301:mcq:perf-test'
UNION ALL
SELECT q.id, 'C', '성능 검증은 운영 환경에서 장애가 발생한 후에 수행한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:perf-test'
UNION ALL
SELECT q.id, 'D', '성능 검증 대신 서버 사양만 높게 잡는다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:perf-test';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11301:mcq:perf-test';

/* Q6: 공통 모듈 예외 처리 정책 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'MCQ', 'NORMAL',
       '공통 모듈 예외 처리 정책에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       '공통 모듈은 호출자가 적절히 처리할 수 있도록 의미 있는 예외 타입/오류 정보를 제공해야 합니다.',
       'seed:v5:11301:mcq:error-policy'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:mcq:error-policy');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '모든 예외를 무시하고 정상 값만 반환한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:error-policy'
UNION ALL
SELECT q.id, 'B', '예외가 발생하면 시스템을 즉시 강제 종료한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:error-policy'
UNION ALL
SELECT q.id, 'C', '예외 유형에 따라 의미 있는 메시지와 코드를 제공하고, 호출자에서 처리할 수 있게 한다.', 1
FROM question q WHERE q.source = 'seed:v5:11301:mcq:error-policy'
UNION ALL
SELECT q.id, 'D', '예외 메시지에는 내부 시스템 정보를 최대한 상세히 포함한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:error-policy';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11301:mcq:error-policy';

/* Q7: 공통 모듈 의존성 관리 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'MCQ', 'NORMAL',
       '공통 모듈이 너무 많은 외부 라이브러리에 의존할 때 발생하기 쉬운 문제로 가장 적절한 것은?',
       NULL,
       'B',
       '의존성이 많으면 버전 충돌과 배포 복잡도가 증가합니다.',
       'seed:v5:11301:mcq:dependency'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:mcq:dependency');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '성능이 무조건 향상된다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:dependency'
UNION ALL
SELECT q.id, 'B', '라이브러리 버전 충돌과 배포 복잡도가 증가한다.', 1
FROM question q WHERE q.source = 'seed:v5:11301:mcq:dependency'
UNION ALL
SELECT q.id, 'C', '테스트 케이스가 자동으로 줄어든다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:dependency'
UNION ALL
SELECT q.id, 'D', '보안 취약점이 자동으로 제거된다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:dependency';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11301:mcq:dependency';

/* Q8: 공통 모듈 로깅 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'MCQ', 'NORMAL',
       '공통 모듈에서 로깅을 설계할 때 가장 적절한 방향은?',
       NULL,
       'C',
       '로깅 수준과 포맷을 통일하고, 개인정보는 마스킹하는 것이 중요합니다.',
       'seed:v5:11301:mcq:logging'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:mcq:logging');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '모든 로그를 DEBUG 수준으로만 출력한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:logging'
UNION ALL
SELECT q.id, 'B', '로그 포맷은 호출 시스템마다 제각각 사용한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:logging'
UNION ALL
SELECT q.id, 'C', '공통 포맷과 수준을 정의하고, 민감 정보는 마스킹 처리한다.', 1
FROM question q WHERE q.source = 'seed:v5:11301:mcq:logging'
UNION ALL
SELECT q.id, 'D', '성능을 위해 로그를 전혀 남기지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:logging';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11301:mcq:logging';

/* Q9: 공통 모듈 테스트 전략 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'MCQ', 'NORMAL',
       '공통 모듈 테스트 전략으로 가장 적절한 것은?',
       NULL,
       'B',
       '재사용되는 공통 모듈은 단위/통합 테스트를 충분히 작성해 회귀를 방지해야 합니다.',
       'seed:v5:11301:mcq:test-strategy'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:mcq:test-strategy');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '공통 모듈은 다른 시스템에서 테스트하므로 별도 테스트가 필요 없다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:test-strategy'
UNION ALL
SELECT q.id, 'B', '단위·통합 테스트를 작성해 재배포 시에도 회귀를 방지한다.', 1
FROM question q WHERE q.source = 'seed:v5:11301:mcq:test-strategy'
UNION ALL
SELECT q.id, 'C', '운영 환경에서 장애가 발생하면 그때 고친다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:test-strategy'
UNION ALL
SELECT q.id, 'D', '테스트 대신 운영자 매뉴얼만 상세히 작성한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:test-strategy';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11301:mcq:test-strategy';

/* Q10: 공통 모듈 도입 시기 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11301, 'WRITTEN', 'MCQ', 'NORMAL',
       '공통 모듈을 도입·정리하기에 가장 적절한 시점에 대한 설명으로 알맞은 것은?',
       NULL,
       'C',
       '초기부터 모든 것을 공통으로 만들기보다, 일정 수준 기능이 쌓인 뒤 패턴을 분석해 공통화하는 전략이 현실적입니다.',
       'seed:v5:11301:mcq:timing'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11301:mcq:timing');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '첫 기능 한 개를 만들기 전 모든 공통 모듈을 완성한다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:timing'
UNION ALL
SELECT q.id, 'B', '공통 모듈은 프로젝트 종료 후에만 도입할 수 있다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:timing'
UNION ALL
SELECT q.id, 'C', '여러 기능을 구현하면서 반복 패턴을 관찰하고, 적절한 시점에 공통화한다.', 1
FROM question q WHERE q.source = 'seed:v5:11301:mcq:timing'
UNION ALL
SELECT q.id, 'D', '공통 모듈은 필요성을 느끼더라도 절대 도입하지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11301:mcq:timing';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11301:mcq:timing';


/* =======================================================
 * 11302 – 객체지향 설계 원칙
 *  - OX 6개, MCQ 10개 추가
 * ======================================================= */

-- [11302] OX 6개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'OX', 'EASY',
       '단일 책임 원칙(SRP)은 클래스가 하나의 책임만 가지도록 설계하라는 객체지향 설계 원칙이다. (O/X)',
       NULL,
       'O',
       'SRP는 변경 이유가 한 가지인 클래스를 만들자는 원칙입니다.',
       'seed:v5:11302:ox:srp'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:ox:srp');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'OX', 'NORMAL',
       '개방-폐쇄 원칙(OCP)은 기능 확장에 닫혀 있고, 코드 수정에는 열려 있어야 한다는 원칙이다. (O/X)',
       NULL,
       'X',
       'OCP는 확장에는 열려 있고, 수정에는 닫혀 있도록 설계하라는 원칙입니다.',
       'seed:v5:11302:ox:ocp'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:ox:ocp');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'OX', 'NORMAL',
       '리스코프 치환 원칙(LSP)에 따르면, 하위 클래스는 상위 타입이 사용되는 곳에서 문제없이 대체 가능해야 한다. (O/X)',
       NULL,
       'O',
       'LSP는 하위 타입이 상위 타입의 계약을 깨지 않고 대체될 수 있어야 한다는 원칙입니다.',
       'seed:v5:11302:ox:lsp'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:ox:lsp');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'OX', 'NORMAL',
       '의존성 역전 원칙(DIP)은 고수준 모듈이 저수준 모듈에 직접 의존하도록 설계하라는 원칙이다. (O/X)',
       NULL,
       'X',
       'DIP는 고수준/저수준 모듈 모두 추상화에 의존하도록 설계하자는 원칙입니다.',
       'seed:v5:11302:ox:dip'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:ox:dip');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 분리 원칙(ISP)은 클라이언트가 사용하지 않는 기능까지 담긴 비대한 인터페이스에 의존하지 않도록 하라는 원칙이다. (O/X)',
       NULL,
       'O',
       'ISP는 클라이언트별로 필요한 인터페이스만 제공하자는 원칙입니다.',
       'seed:v5:11302:ox:isp'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:ox:isp');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'OX', 'NORMAL',
       '높은 응집도와 낮은 결합도는 객체지향 설계에서 지향해야 할 중요한 품질 특성이다. (O/X)',
       NULL,
       'O',
       '모듈 내부는 관련있는 기능끼리 모으고, 모듈 간 의존성은 최소화하는 것이 좋습니다.',
       'seed:v5:11302:ox:cohesion-coupling'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:ox:cohesion-coupling');

-- [11302] MCQ 10개
/* Q1: SRP 예시 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'MCQ', 'EASY',
       '단일 책임 원칙(SRP)을 가장 잘 지킨 클래스 예시는?',
       NULL,
       'C',
       'SRP는 하나의 클래스가 하나의 책임만 가지도록 하는 원칙입니다.',
       'seed:v5:11302:mcq:srp-example'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:mcq:srp-example');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '화면 렌더링, DB 저장, 이메일 전송을 모두 담당하는 클래스', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:srp-example'
UNION ALL
SELECT q.id, 'B', '로그 출력과 결제 승인 로직을 동시에 처리하는 클래스', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:srp-example'
UNION ALL
SELECT q.id, 'C', '사용자 비밀번호 검증만 담당하는 클래', 1
FROM question q WHERE q.source = 'seed:v5:11302:mcq:srp-example'
UNION ALL
SELECT q.id, 'D', '보고서 생성, 인쇄, 이메일 전송까지 모두 처리하는 클래스', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:srp-example';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11302:mcq:srp-example';

/* Q2: OCP 적용 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'MCQ', 'NORMAL',
       '개방-폐쇄 원칙(OCP)을 잘 적용한 설계에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       '새로운 요구는 상속·구현 클래스 추가 등 확장으로 처리하고, 기존 코드는 최소 수정하는 것이 이상적입니다.',
       'seed:v5:11302:mcq:ocp'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:mcq:ocp');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '새 요구가 생길 때마다 if-else 분기를 기존 코드에 계속 추가한다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:ocp'
UNION ALL
SELECT q.id, 'B', '새 정렬 방식이 필요하면 전략(Strategy) 구현 클래스를 추가해 확장한다.', 1
FROM question q WHERE q.source = 'seed:v5:11302:mcq:ocp'
UNION ALL
SELECT q.id, 'C', '모든 로직을 한 메서드에 작성해 한 번에 수정한다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:ocp'
UNION ALL
SELECT q.id, 'D', '기존 코드를 매번 복사해 새로운 기능을 추가한다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:ocp';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11302:mcq:ocp';

/* Q3: DIP 적용 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'MCQ', 'NORMAL',
       '의존성 역전 원칙(DIP)을 잘 따른 예시는?',
       NULL,
       'C',
       '상위 모듈이 구체 클래스가 아닌 인터페이스에 의존하도록 설계하는 것이 DIP의 핵심입니다.',
       'seed:v5:11302:mcq:dip'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:mcq:dip');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '서비스가 특정 구현 클래스(EmailSenderImpl)에 직접 new로 의존한다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:dip'
UNION ALL
SELECT q.id, 'B', 'UI 레이어가 DB 드라이버 구현 클래스에 직접 의존한다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:dip'
UNION ALL
SELECT q.id, 'C', '서비스가 메일 전송 인터페이스(MailSender)에 의존하고, 구현은 외부에서 주입받는다.', 1
FROM question q WHERE q.source = 'seed:v5:11302:mcq:dip'
UNION ALL
SELECT q.id, 'D', '저수준 모듈이 상위 정책을 변경하도록 직접 호출한다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:dip';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11302:mcq:dip';

/* Q4: LSP 위반 사례 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'MCQ', 'NORMAL',
       '리스코프 치환 원칙(LSP)을 위반한 사례로 가장 적절한 것은?',
       NULL,
       'D',
       '상위 타입의 계약을 지키지 못해 하위 타입을 대체했을 때 예외나 잘못된 동작이 발생하면 LSP 위반입니다.',
       'seed:v5:11302:mcq:lsp-violation'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:mcq:lsp-violation');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '상위 타입 메서드를 그대로 상속받아 동작을 확장했다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:lsp-violation'
UNION ALL
SELECT q.id, 'B', '하위 타입에서 상위 타입의 전제조건과 결과를 모두 만족한다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:lsp-violation'
UNION ALL
SELECT q.id, 'C', '상위 타입의 메서드를 override하되, 계약 범위 내에서 동작을 강화했다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:lsp-violation'
UNION ALL
SELECT q.id, 'D', '상위 타입이 허용한 입력에 대해 하위 타입이 예외를 던지거나, 결과 제약을 만족하지 못한다.', 1
FROM question q WHERE q.source = 'seed:v5:11302:mcq:lsp-violation';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11302:mcq:lsp-violation';

/* Q5: 응집도/결합도 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'MCQ', 'EASY',
       '좋은 객체지향 설계에서 지향해야 할 상태로 가장 적절한 것은?',
       NULL,
       'B',
       '모듈 내부는 관련 기능끼리 모으고(높은 응집도), 모듈 간 의존성은 최소화(낮은 결합도)하는 것이 좋습니다.',
       'seed:v5:11302:mcq:cohesion-coupling'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:mcq:cohesion-coupling');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '낮은 응집도, 높은 결합도', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:cohesion-coupling'
UNION ALL
SELECT q.id, 'B', '높은 응집도, 낮은 결합도', 1
FROM question q WHERE q.source = 'seed:v5:11302:mcq:cohesion-coupling'
UNION ALL
SELECT q.id, 'C', '낮은 응집도, 낮은 결합도', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:cohesion-coupling'
UNION ALL
SELECT q.id, 'D', '높은 응집도, 높은 결합도', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:cohesion-coupling';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11302:mcq:cohesion-coupling';

/* Q6: 인터페이스 분리 원칙(ISP) */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 분리 원칙(ISP)에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       'ISP는 클라이언트가 필요로 하는 기능만 가지는 인터페이스를 제공하자는 원칙입니다.',
       'seed:v5:11302:mcq:isp'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:mcq:isp');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '모든 기능을 하나의 거대한 인터페이스에 모은다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:isp'
UNION ALL
SELECT q.id, 'B', '클라이언트는 사용하지 않는 기능에도 의존해야 한다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:isp'
UNION ALL
SELECT q.id, 'C', '각 클라이언트가 사용하는 기능만 가진 작은 인터페이스로 분리한다.', 1
FROM question q WHERE q.source = 'seed:v5:11302:mcq:isp'
UNION ALL
SELECT q.id, 'D', '인터페이스 대신 모든 구현 클래스에 직접 의존한다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:isp';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11302:mcq:isp';

/* Q7: 캡슐화 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'MCQ', 'EASY',
       '캡슐화(encapsulation)의 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       '캡슐화는 내부 구현을 숨기고, 공용 인터페이스를 통해서만 접근하게 하는 개념입니다.',
       'seed:v5:11302:mcq:encapsulation'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:mcq:encapsulation');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '클래스의 모든 필드를 public으로 공개한다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:encapsulation'
UNION ALL
SELECT q.id, 'B', '내부 구현을 숨기고, 인터페이스를 통해 필요한 기능만 노출한다.', 1
FROM question q WHERE q.source = 'seed:v5:11302:mcq:encapsulation'
UNION ALL
SELECT q.id, 'C', '하위 클래스에서만 접근 가능한 필드를 모두 제거한다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:encapsulation'
UNION ALL
SELECT q.id, 'D', '상속 구조를 없애고 모든 클래스를 하나로 합친다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:encapsulation';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11302:mcq:encapsulation';

/* Q8: 설계 패턴 활용 목적 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'MCQ', 'NORMAL',
       'GoF 디자인 패턴과 같은 설계 패턴을 사용하는 주된 이유로 가장 적절한 것은?',
       NULL,
       'C',
       '설계 패턴은 검증된 해결 방식과 공통 용어를 제공해 설계 품질과 의사소통을 돕습니다.',
       'seed:v5:11302:mcq:pattern-purpose'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:mcq:pattern-purpose');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '코드 줄 수를 늘려 개발 난이도를 높이기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:pattern-purpose'
UNION ALL
SELECT q.id, 'B', '특정 언어에서만 동작하는 기능을 만들기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:pattern-purpose'
UNION ALL
SELECT q.id, 'C', '반복적으로 등장하는 설계 문제에 대한 검증된 해결 방식과 공통 용어를 제공하기 위해', 1
FROM question q WHERE q.source = 'seed:v5:11302:mcq:pattern-purpose'
UNION ALL
SELECT q.id, 'D', '컴파일러 최적화 옵션을 설정하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:pattern-purpose';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11302:mcq:pattern-purpose';

/* Q9: 리팩터링 필요 징후 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'MCQ', 'NORMAL',
       '리팩터링이 필요한 코드 냄새(Code Smell)로 보기 가장 적절한 것은?',
       NULL,
       'D',
       '거대한 클래스, 중복 코드, 긴 메서드 등은 대표적인 코드 냄새입니다.',
       'seed:v5:11302:mcq:code-smell'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:mcq:code-smell');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '짧고 의미 있는 메서드 이름', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:code-smell'
UNION ALL
SELECT q.id, 'B', '중복이 없는 모듈화된 코드', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:code-smell'
UNION ALL
SELECT q.id, 'C', '테스트 코드가 풍부한 구조', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:code-smell'
UNION ALL
SELECT q.id, 'D', '여러 기능이 뒤섞인 거대한 클래스와 중복된 코드', 1
FROM question q WHERE q.source = 'seed:v5:11302:mcq:code-smell';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11302:mcq:code-smell';

/* Q10: SOLID 종합 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11302, 'WRITTEN', 'MCQ', 'NORMAL',
       '다음 중 SOLID 원칙을 적용할 때 기대할 수 있는 효과로 가장 적절한 것은?',
       NULL,
       'B',
       'SOLID 원칙은 유지보수성과 확장성이 높은 설계를 만드는 데 도움을 줍니다.',
       'seed:v5:11302:mcq:solid-benefit'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11302:mcq:solid-benefit');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '코드가 복잡해져 변경이 어려워진다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:solid-benefit'
UNION ALL
SELECT q.id, 'B', '유지보수성과 확장성이 높은 설계를 만들 수 있다.', 1
FROM question q WHERE q.source = 'seed:v5:11302:mcq:solid-benefit'
UNION ALL
SELECT q.id, 'C', '테스트 코드 작성이 불가능해진다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:solid-benefit'
UNION ALL
SELECT q.id, 'D', '객체지향보다는 절차지향 구조가 강화된다.', 0
FROM question q WHERE q.source = 'seed:v5:11302:mcq:solid-benefit';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11302:mcq:solid-benefit';

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
