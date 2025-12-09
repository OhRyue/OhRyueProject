SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id   := 1;
SET @tp_11401  := 11401; -- 1.4.1 인터페이스 요구사항 확인
SET @tp_11402  := 11402; -- 1.4.2 인터페이스 대상 식별
SET @tp_11403  := 11403; -- 1.4.3 인터페이스 상세 설계


/* =======================================================
 * 11401 – 인터페이스 요구사항 확인
 *  - OX 6개
 * ======================================================= */

-- [11401] OX 6개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 요구사항에는 단순히 데이터 포맷뿐 아니라 응답 시간, 처리량과 같은 성능 요구도 포함될 수 있다.',
       NULL,
       'O',
       '인터페이스는 시스템 간 계약이므로 데이터 포맷뿐 아니라 성능·가용성 등 비기능 요구도 함께 정의해야 합니다.',
       'seed:v5:11401:ox:perf-nfr'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:ox:perf-nfr');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 오류 처리 정책은 구현 단계에서 개발자가 자유롭게 정하면 되므로, 요구사항 단계에서는 굳이 정의하지 않아도 된다.',
       NULL,
       'X',
       '오류 코드 체계, 재시도 가능 여부 등은 여러 시스템이 공유하는 규칙이므로 요구사항 단계에서 합의해야 합니다.',
       'seed:v5:11401:ox:error-policy'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:ox:error-policy');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'OX', 'NORMAL',
       '동기/비동기 호출 방식은 인터페이스 요구사항에 영향을 주지 않으므로, 상세 설계에서만 고려하면 된다.',
       NULL,
       'X',
       '동기/비동기 방식은 응답 시간, 타임아웃, 호출 패턴 등에 영향을 주므로 요구사항 검토 시부터 논의해야 합니다.',
       'seed:v5:11401:ox:sync-async'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:ox:sync-async');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 요구사항을 확인할 때는 요청·응답에 포함되는 필수/선택 항목을 구분하는 것이 도움이 된다.',
       NULL,
       'O',
       '필수/선택 항목 구분은 호출 측과 제공 측의 검증 로직과 오류 처리 정책을 정하는 데 중요합니다.',
       'seed:v5:11401:ox:mandatory-optional'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:ox:mandatory-optional');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 보안 요구사항에는 인증/인가 방식뿐 아니라 전송 구간 암호화 여부도 포함될 수 있다.',
       NULL,
       'O',
       '토큰 방식, 역할 권한, TLS 적용 여부 등은 인터페이스 요구사항에서 함께 정의해야 하는 보안 요소입니다.',
       'seed:v5:11401:ox:security'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:ox:security');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'OX', 'NORMAL',
       '계약 기반 개발(Contract-based Development)에서 인터페이스 사양은 클라이언트와 서버 간의 합의 문서 역할을 한다.',
       NULL,
       'O',
       '인터페이스 사양은 요청·응답 구조와 제약조건을 명시한 계약 문서로, 양측의 공통 기준이 됩니다.',
       'seed:v5:11401:ox:contract'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:ox:contract');



/* =======================================================
 * 11402 – 인터페이스 대상 식별
 *  - OX 6개
 * ======================================================= */

-- [11402] OX 6개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 대상 식별 단계에서는 내부 서브시스템뿐 아니라 외부 연계 시스템도 함께 파악해야 한다.',
       NULL,
       'O',
       '내부/외부 모든 연계 대상을 파악해야 전체 인터페이스 구조가 보입니다.',
       'seed:v5:11402:ox:internal-external'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:ox:internal-external');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'OX', 'NORMAL',
       '배치(일괄) 인터페이스는 온라인(실시간) 인터페이스와는 별도로 식별·정의할 필요가 없다.',
       NULL,
       'X',
       '배치는 처리 주기·데이터 양·오류 복구 방식이 달라서 별도로 식별·정의해야 합니다.',
       'seed:v5:11402:ox:batch'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:ox:batch');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 목록에는 송신·수신 방향, 호출 주기, 데이터 양 등을 함께 기록하는 것이 좋다.',
       NULL,
       'O',
       '방향/주기/데이터 양은 설계·용량 산정·운영에 모두 중요한 메타정보입니다.',
       'seed:v5:11402:ox:meta'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:ox:meta');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'OX', 'NORMAL',
       '메시지 큐나 이벤트 스트림을 사용하는 간접 연계 방식은 인터페이스 대상 식별 범위에 포함되지 않는다.',
       NULL,
       'X',
       'MQ, 이벤트 기반 연계도 시스템 간 인터페이스이므로 식별 대상입니다.',
       'seed:v5:11402:ox:async-channel'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:ox:async-channel');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'OX', 'EASY',
       '기존 레거시 시스템과의 연계가 많을수록 인터페이스 대상 식별 작업의 중요성은 커진다.',
       NULL,
       'O',
       '레거시 연계는 제약과 리스크가 크기 때문에 초기에 명확히 파악해야 합니다.',
       'seed:v5:11402:ox:legacy'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:ox:legacy');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 대상 식별 시 데이터 출처(출발지)와 최종 목적지 정보를 함께 파악하는 것이 바람직하다.',
       NULL,
       'O',
       '출발지·목적지·중간 경유 시스템을 파악해야 데이터 흐름과 책임이 명확해집니다.',
       'seed:v5:11402:ox:source-target'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:ox:source-target');



/* =======================================================
 * 11403 – 인터페이스 상세 설계
 *  - OX 6개
 * ======================================================= */

-- [11403] OX 6개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 상세 설계에서는 요청/응답 메시지 구조를 필드 단위까지 구체적으로 정의해야 한다.',
       NULL,
       'O',
       '필드명, 타입, 길이, 제약조건 등을 구체적으로 정의해야 구현자 간 오해를 줄일 수 있습니다.',
       'seed:v5:11403:ox:field-detail'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:ox:field-detail');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'OX', 'NORMAL',
       '타임아웃, 재시도, 서킷브레이커와 같은 안정성 정책은 인터페이스 상세 설계에서 다루지 않아도 된다.',
       NULL,
       'X',
       '안정성 정책은 호출 방법의 일부이므로 인터페이스 상세 설계에서 정의하는 것이 바람직합니다.',
       'seed:v5:11403:ox:resilience'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:ox:resilience');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 상세 설계 시, 날짜·시간 필드는 타임존과 포맷(예: ISO8601)을 명시해야 한다.',
       NULL,
       'O',
       '날짜/시간은 포맷과 타임존을 명시하지 않으면 시스템 간 해석이 달라질 수 있습니다.',
       'seed:v5:11403:ox:datetime'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:ox:datetime');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 버전 관리는 API URL이나 헤더 등을 활용해 표현할 수 있으며, 하위 호환성 전략과 함께 설계되어야 한다.',
       NULL,
       'O',
       'v1/v2 또는 헤더 기반 버전 관리 등과 함께 하위 호환 전략을 설계해야 합니다.',
       'seed:v5:11403:ox:versioning'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:ox:versioning');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'OX', 'EASY',
       'Idempotent(멱등)한 인터페이스 설계는 중복 요청이 발생해도 시스템 상태가 한 번 수행한 것과 동일하게 유지되도록 한다.',
       NULL,
       'O',
       '멱등성은 네트워크 재시도나 중복 전송 상황에서 중요한 설계 개념입니다.',
       'seed:v5:11403:ox:idempotent'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:ox:idempotent');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 상세 설계에서 예시 요청/응답(JSON, XML 등)을 제공하면 개발·테스트에 도움이 된다.',
       NULL,
       'O',
       '예시 메시지는 구현자가 사양을 오해하지 않도록 돕는 좋은 수단입니다.',
       'seed:v5:11403:ox:example'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:ox:example');

SET FOREIGN_KEY_CHECKS = 1;
