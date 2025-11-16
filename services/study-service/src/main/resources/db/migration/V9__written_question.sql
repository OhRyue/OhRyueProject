SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id   := 1;
SET @tp_11401  := 11401; -- 1.4.1 인터페이스 요구사항 확인
SET @tp_11402  := 11402; -- 1.4.2 인터페이스 대상 식별
SET @tp_11403  := 11403; -- 1.4.3 인터페이스 상세 설계;


/* =======================================================
 * 11401 – 인터페이스 요구사항 확인
 *  - OX 6개, MCQ 10개
 * ======================================================= */

-- [11401] OX 6개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 요구사항에는 단순히 데이터 포맷뿐 아니라 응답 시간, 처리량과 같은 성능 요구도 포함될 수 있다. (O/X)',
       NULL,
       'O',
       '인터페이스는 시스템 간 계약이므로 데이터 포맷뿐 아니라 성능·가용성 등 비기능 요구도 함께 정의해야 합니다.',
       'seed:v5:11401:ox:perf-nfr'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:ox:perf-nfr');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 오류 처리 정책은 구현 단계에서 개발자가 자유롭게 정하면 되므로, 요구사항 단계에서는 굳이 정의하지 않아도 된다. (O/X)',
       NULL,
       'X',
       '오류 코드 체계, 재시도 가능 여부 등은 여러 시스템이 공유하는 규칙이므로 요구사항 단계에서 합의해야 합니다.',
       'seed:v5:11401:ox:error-policy'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:ox:error-policy');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'OX', 'NORMAL',
       '동기/비동기 호출 방식은 인터페이스 요구사항에 영향을 주지 않으므로, 상세 설계에서만 고려하면 된다. (O/X)',
       NULL,
       'X',
       '동기/비동기 방식은 응답 시간, 타임아웃, 호출 패턴 등에 영향을 주므로 요구사항 검토 시부터 논의해야 합니다.',
       'seed:v5:11401:ox:sync-async'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:ox:sync-async');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 요구사항을 확인할 때는 요청·응답에 포함되는 필수/선택 항목을 구분하는 것이 도움이 된다. (O/X)',
       NULL,
       'O',
       '필수/선택 항목 구분은 호출 측과 제공 측의 검증 로직과 오류 처리 정책을 정하는 데 중요합니다.',
       'seed:v5:11401:ox:mandatory-optional'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:ox:mandatory-optional');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 보안 요구사항에는 인증/인가 방식뿐 아니라 전송 구간 암호화 여부도 포함될 수 있다. (O/X)',
       NULL,
       'O',
       '토큰 방식, 역할 권한, TLS 적용 여부 등은 인터페이스 요구사항에서 함께 정의해야 하는 보안 요소입니다.',
       'seed:v5:11401:ox:security'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:ox:security');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'OX', 'NORMAL',
       '계약 기반 개발(Contract-based Development)에서 인터페이스 사양은 클라이언트와 서버 간의 합의 문서 역할을 한다. (O/X)',
       NULL,
       'O',
       '인터페이스 사양은 요청·응답 구조와 제약조건을 명시한 계약 문서로, 양측의 공통 기준이 됩니다.',
       'seed:v5:11401:ox:contract'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:ox:contract');


-- [11401] MCQ 10개

-- Q1: 인터페이스 요구사항 범위
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'MCQ', 'EASY',
       '인터페이스 요구사항에 포함되기 어려운 항목은?',
       NULL,
       'D',
       '백업 주기는 인프라/운영 요구에 가깝고, 인터페이스 계약의 직접 대상은 아닙니다.',
       'seed:v5:11401:mcq:scope'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:mcq:scope');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '요청·응답 데이터 항목과 포맷', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:scope'
UNION ALL
SELECT q.id, 'B', '응답 시간과 최대 처리량', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:scope'
UNION ALL
SELECT q.id, 'C', '오류 코드 체계와 메시지 규칙', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:scope'
UNION ALL
SELECT q.id, 'D', 'DB 백업 주기와 백업 파일 보관 경로', 1
FROM question q WHERE q.source = 'seed:v5:11401:mcq:scope';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11401:mcq:scope';


-- Q2: SLA 항목
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'MCQ', 'NORMAL',
       '시스템 간 인터페이스 SLA(Service Level Agreement)에 해당하는 항목으로 가장 적절한 것은?',
       NULL,
       'B',
       '응답 시간, 가용성, 허용 장애 시간 등은 인터페이스 수준에서 합의하는 SLA의 대표 항목입니다.',
       'seed:v5:11401:mcq:sla'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:mcq:sla');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '개발자 개인의 근무 시간', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:sla'
UNION ALL
SELECT q.id, 'B', '평균 응답 시간, 목표 가용률, 허용 장애 시간', 1
FROM question q WHERE q.source = 'seed:v5:11401:mcq:sla'
UNION ALL
SELECT q.id, 'C', '코드 작성 스타일 가이드', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:sla'
UNION ALL
SELECT q.id, 'D', '테스트 케이스 명명 규칙', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:sla';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11401:mcq:sla';


-- Q3: 오류 코드 설계
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 오류 코드 설계에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       '일관된 체계와 범주를 정의해 두면 여러 시스템에서 공통으로 이해하고 처리하기 쉽습니다.',
       'seed:v5:11401:mcq:error-code'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:mcq:error-code');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '각 개발자가 임의의 문자열을 오류 코드로 사용한다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:error-code'
UNION ALL
SELECT q.id, 'B', '오류 코드는 모두 동일한 값으로 통일해 구분하지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:error-code'
UNION ALL
SELECT q.id, 'C', '오류 유형별 코드 체계와 메시지 작성 규칙을 인터페이스 수준에서 합의한다.', 1
FROM question q WHERE q.source = 'seed:v5:11401:mcq:error-code'
UNION ALL
SELECT q.id, 'D', '내부 예외 메시지만 보내고 오류 코드는 생략한다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:error-code';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11401:mcq:error-code';


-- Q4: 타임아웃 요구사항
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 타임아웃 요구사항을 정의할 때 가장 바람직한 설명은?',
       NULL,
       'B',
       '업무 특성, 평균 응답 시간, 재시도 정책 등을 고려해 적절한 타임아웃을 합의해야 합니다.',
       'seed:v5:11401:mcq:timeout'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:mcq:timeout');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '모든 호출의 타임아웃을 무제한으로 설정한다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:timeout'
UNION ALL
SELECT q.id, 'B', '업무 특성과 평균 응답 시간을 고려해 적절한 타임아웃과 재시도 정책을 합의한다.', 1
FROM question q WHERE q.source = 'seed:v5:11401:mcq:timeout'
UNION ALL
SELECT q.id, 'C', '타임아웃은 구현자가 각자 경험에 따라 임의로 정한다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:timeout'
UNION ALL
SELECT q.id, 'D', '타임아웃이 발생해도 무조건 무한 재시도 한다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:timeout';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11401:mcq:timeout';


-- Q5: 인터페이스 변경 관리
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 요구사항 변경을 관리하는 방법으로 가장 적절한 것은?',
       NULL,
       'C',
       '버전 관리와 변경 이력 관리를 통해 양측 시스템이 호환 가능 여부를 판단할 수 있어야 합니다.',
       'seed:v5:11401:mcq:change-mgmt'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:mcq:change-mgmt');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '변경 시 구두로만 전달하고 문서는 수정하지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:change-mgmt'
UNION ALL
SELECT q.id, 'B', '호출 시스템이 알아서 변경을 추측하도록 한다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:change-mgmt'
UNION ALL
SELECT q.id, 'C', '사양 문서에 버전과 변경 이력을 기록하고, 영향 범위를 함께 공유한다.', 1
FROM question q WHERE q.source = 'seed:v5:11401:mcq:change-mgmt'
UNION ALL
SELECT q.id, 'D', '인터페이스는 한 번 정의하면 영원히 변경할 수 없다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:change-mgmt';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11401:mcq:change-mgmt';


-- Q6: 입력 검증 책임 분담
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 입력 검증 책임 분담에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       '클라이언트/서버 양측에서 역할을 나누되, 서버는 항상 최종 검증 책임을 가져야 합니다.',
       'seed:v5:11401:mcq:validation-resp'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:mcq:validation-resp');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '클라이언트에서만 검증하고 서버는 아무 검증도 하지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:validation-resp'
UNION ALL
SELECT q.id, 'B', '클라이언트는 사용자 친화적 검증을, 서버는 최종 무결성 검증을 수행하도록 역할을 분담한다.', 1
FROM question q WHERE q.source = 'seed:v5:11401:mcq:validation-resp'
UNION ALL
SELECT q.id, 'C', '검증은 모두 데이터베이스에 위임한다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:validation-resp'
UNION ALL
SELECT q.id, 'D', '성능을 위해 어느 쪽에서도 검증을 수행하지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:validation-resp';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11401:mcq:validation-resp';


-- Q7: 보안 요구사항
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 보안 요구사항으로 보기 가장 적절한 것은?',
       NULL,
       'C',
       '어떤 인증/인가/암호화 방식을 사용할지 인터페이스 차원에서 합의해야 합니다.',
       'seed:v5:11401:mcq:security'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:mcq:security');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '개발자의 PC 계정 비밀번호 길이', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:security'
UNION ALL
SELECT q.id, 'B', '로컬 로그 파일 저장 경로', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:security'
UNION ALL
SELECT q.id, 'C', 'API 호출 시 토큰 인증 방식과 TLS 적용 여부', 1
FROM question q WHERE q.source = 'seed:v5:11401:mcq:security'
UNION ALL
SELECT q.id, 'D', 'IDE 테마 색상 설정', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:security';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11401:mcq:security';


-- Q8: 인터페이스 요구사항 도출 기법
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 요구사항을 도출·정제하는 방법으로 가장 거리가 먼 것은?',
       NULL,
       'D',
       '워크숍, 프로토타입, 시퀀스 다이어그램 등은 요구를 구체화하는 데 유용합니다.',
       'seed:v5:11401:mcq:elicitation'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:mcq:elicitation');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '관련 시스템 담당자와 워크숍 진행', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:elicitation'
UNION ALL
SELECT q.id, 'B', '프로토타입과 샘플 메시지를 활용한 검토', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:elicitation'
UNION ALL
SELECT q.id, 'C', '시퀀스 다이어그램으로 호출 흐름을 함께 검토', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:elicitation'
UNION ALL
SELECT q.id, 'D', '개발자가 혼자 상상으로 메시지 포맷을 작성 후 바로 개발', 1
FROM question q WHERE q.source = 'seed:v5:11401:mcq:elicitation';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11401:mcq:elicitation';


-- Q9: 인터페이스 모니터링 요구사항
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 모니터링 요구사항으로 가장 적절한 것은?',
       NULL,
       'B',
       '호출 건수, 오류율, 응답 시간 등 모니터링 지표 정의는 운영 단계 품질 관리에 중요합니다.',
       'seed:v5:11401:mcq:monitoring'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:mcq:monitoring');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '운영자는 로그를 전혀 볼 수 없도록 한다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:monitoring'
UNION ALL
SELECT q.id, 'B', '호출 건수, 오류율, 평균 응답 시간 등의 지표를 수집·대시보드화한다.', 1
FROM question q WHERE q.source = 'seed:v5:11401:mcq:monitoring'
UNION ALL
SELECT q.id, 'C', '장애가 발생해도 알림을 보내지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:monitoring'
UNION ALL
SELECT q.id, 'D', '성능 데이터는 수집하지 않고, 사용자 불만 접수만으로 판단한다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:monitoring';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11401:mcq:monitoring';


-- Q10: 인터페이스 요구사항 문서화 수준
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11401, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 요구사항 문서화 수준에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       '최소한 요청/응답 구조, 제약조건, 오류, 비기능 요구까지 포함한 문서가 필요합니다.',
       'seed:v5:11401:mcq:doc-level'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11401:mcq:doc-level');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '엔드포인트 URL만 적어두면 충분하다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:doc-level'
UNION ALL
SELECT q.id, 'B', '코드에만 주석으로 작성하고 문서는 만들지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:doc-level'
UNION ALL
SELECT q.id, 'C', '요청/응답 구조, 제약조건, 주요 오류, 비기능 요구까지 포함해 문서화한다.', 1
FROM question q WHERE q.source = 'seed:v5:11401:mcq:doc-level'
UNION ALL
SELECT q.id, 'D', '인터페이스는 문서화 대상이 아니다.', 0
FROM question q WHERE q.source = 'seed:v5:11401:mcq:doc-level';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11401:mcq:doc-level';



/* =======================================================
 * 11402 – 인터페이스 대상 식별
 *  - OX 6개, MCQ 10개
 * ======================================================= */

-- [11402] OX 6개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 대상 식별 단계에서는 내부 서브시스템뿐 아니라 외부 연계 시스템도 함께 파악해야 한다. (O/X)',
       NULL,
       'O',
       '내부/외부 모든 연계 대상을 파악해야 전체 인터페이스 구조가 보입니다.',
       'seed:v5:11402:ox:internal-external'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:ox:internal-external');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'OX', 'NORMAL',
       '배치(일괄) 인터페이스는 온라인(실시간) 인터페이스와는 별도로 식별·정의할 필요가 없다. (O/X)',
       NULL,
       'X',
       '배치는 처리 주기·데이터 양·오류 복구 방식이 달라서 별도로 식별·정의해야 합니다.',
       'seed:v5:11402:ox:batch'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:ox:batch');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 목록에는 송신·수신 방향, 호출 주기, 데이터 양 등을 함께 기록하는 것이 좋다. (O/X)',
       NULL,
       'O',
       '방향/주기/데이터 양은 설계·용량 산정·운영에 모두 중요한 메타정보입니다.',
       'seed:v5:11402:ox:meta'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:ox:meta');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'OX', 'NORMAL',
       '메시지 큐나 이벤트 스트림을 사용하는 간접 연계 방식은 인터페이스 대상 식별 범위에 포함되지 않는다. (O/X)',
       NULL,
       'X',
       'MQ, 이벤트 기반 연계도 시스템 간 인터페이스이므로 식별 대상입니다.',
       'seed:v5:11402:ox:async-channel'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:ox:async-channel');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'OX', 'EASY',
       '기존 레거시 시스템과의 연계가 많을수록 인터페이스 대상 식별 작업의 중요성은 커진다. (O/X)',
       NULL,
       'O',
       '레거시 연계는 제약과 리스크가 크기 때문에 초기에 명확히 파악해야 합니다.',
       'seed:v5:11402:ox:legacy'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:ox:legacy');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 대상 식별 시 데이터 출처(출발지)와 최종 목적지 정보를 함께 파악하는 것이 바람직하다. (O/X)',
       NULL,
       'O',
       '출발지·목적지·중간 경유 시스템을 파악해야 데이터 흐름과 책임이 명확해집니다.',
       'seed:v5:11402:ox:source-target'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:ox:source-target');


-- [11402] MCQ 10개

-- Q1: 인터페이스 목록 항목
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'MCQ', 'EASY',
       '인터페이스 목록(Interface List)에 포함되기 어려운 항목은?',
       NULL,
       'D',
       '인터페이스 목록에는 보통 대상 시스템, 방향, 주기, 프로토콜 등을 포함합니다.',
       'seed:v5:11402:mcq:list'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:mcq:list');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '송신 시스템과 수신 시스템 명', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:list'
UNION ALL
SELECT q.id, 'B', '연계 유형(온라인/배치)과 호출 주기', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:list'
UNION ALL
SELECT q.id, 'C', '프로토콜(HTTP, MQ 등)과 포맷(JSON, XML 등)', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:list'
UNION ALL
SELECT q.id, 'D', '각 개발자의 근무 부서 연락처 목록', 1
FROM question q WHERE q.source = 'seed:v5:11402:mcq:list';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11402:mcq:list';


-- Q2: 인터페이스 유형 분류
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'MCQ', 'NORMAL',
       '다음 중 인터페이스 대상 식별 시 고려하는 연계 유형 분류로 가장 적절한 것은?',
       NULL,
       'B',
       '온라인/배치, 동기/비동기, 단방향/양방향 등으로 분류하는 것이 일반적입니다.',
       'seed:v5:11402:mcq:type'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:mcq:type');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '개발자/운영자/기획자', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:type'
UNION ALL
SELECT q.id, 'B', '온라인/배치, 동기/비동기, 단방향/양방향', 1
FROM question q WHERE q.source = 'seed:v5:11402:mcq:type'
UNION ALL
SELECT q.id, 'C', '윈도우/리눅스/맥OS', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:type'
UNION ALL
SELECT q.id, 'D', '내부/외부 개발자', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:type';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11402:mcq:type';


-- Q3: 데이터 흐름 파악
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 대상 식별과 함께 데이터 흐름을 파악하는 이유로 가장 적절한 것은?',
       NULL,
       'C',
       '데이터 출처·경로·도착지를 알아야 보안/품질/책임을 명확히 할 수 있습니다.',
       'seed:v5:11402:mcq:data-flow'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:mcq:data-flow');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '사용자 인터페이스 색상을 결정하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:data-flow'
UNION ALL
SELECT q.id, 'B', '개발자 수를 산정하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:data-flow'
UNION ALL
SELECT q.id, 'C', '데이터 출처와 이동 경로, 최종 이용 시스템을 명확히 해 품질과 책임을 관리하기 위해', 1
FROM question q WHERE q.source = 'seed:v5:11402:mcq:data-flow'
UNION ALL
SELECT q.id, 'D', '운영 체제를 선택하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:data-flow';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11402:mcq:data-flow';


-- Q4: 외부 기관 연계
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'MCQ', 'NORMAL',
       '외부 기관과의 인터페이스를 식별할 때 우선적으로 확인해야 할 사항으로 가장 적절한 것은?',
       NULL,
       'B',
       '외부 기관이 제공하는 사양, 보안·망 구성 제약, 테스트 환경 지원 여부 등을 먼저 확인해야 합니다.',
       'seed:v5:11402:mcq:external'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:mcq:external');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '외부 기관 담당자의 취미 생활', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:external'
UNION ALL
SELECT q.id, 'B', '제공되는 API/파일 사양, 보안·망 구성 제약, 테스트 환경 지원 여부', 1
FROM question q WHERE q.source = 'seed:v5:11402:mcq:external'
UNION ALL
SELECT q.id, 'C', '해당 기관 직원 수', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:external'
UNION ALL
SELECT q.id, 'D', '사무실 인테리어 색상', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:external';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11402:mcq:external';


-- Q5: 중복 인터페이스 통합
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 대상 식별 과정에서 동일 데이터가 여러 인터페이스로 중복 전송되는 것을 발견했다. 가장 바람직한 대처는?',
       NULL,
       'C',
       '중복 전송은 장애·불일치 원인이므로 통합·단순화 전략을 검토해야 합니다.',
       'seed:v5:11402:mcq:dup'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:mcq:dup');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '중복이 많을수록 안전하므로 그대로 둔다.', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:dup'
UNION ALL
SELECT q.id, 'B', '로그에서 숨겨서 보이지 않게 한다.', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:dup'
UNION ALL
SELECT q.id, 'C', '중복 인터페이스를 통합하거나 단일 출처를 정하는 방안을 검토한다.', 1
FROM question q WHERE q.source = 'seed:v5:11402:mcq:dup'
UNION ALL
SELECT q.id, 'D', '중복 전송 건수만 늘려서 테스트한다.', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:dup';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11402:mcq:dup';


-- Q6: 인터페이스 영향 범위 분석
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 대상 식별 결과는 향후 어떤 분석에 가장 직접적으로 활용되는가?',
       NULL,
       'B',
       '어떤 시스템이 어떤 시스템에 의존하는지 알면 변경/장애 시 영향 범위를 분석할 수 있습니다.',
       'seed:v5:11402:mcq:impact'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:mcq:impact');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '개발자 성과 평가', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:impact'
UNION ALL
SELECT q.id, 'B', '시스템 간 의존 관계와 변경/장애 영향 범위 분석', 1
FROM question q WHERE q.source = 'seed:v5:11402:mcq:impact'
UNION ALL
SELECT q.id, 'C', '사무실 좌석 배치도 설계', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:impact'
UNION ALL
SELECT q.id, 'D', '개발 언어 선택', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:impact';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11402:mcq:impact';


-- Q7: MSA 환경 인터페이스 식별
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'MCQ', 'NORMAL',
       '마이크로서비스 아키텍처(MSA)에서 인터페이스 대상 식별 시 추가로 주의해야 할 점으로 가장 적절한 것은?',
       NULL,
       'C',
       '서비스 간 호출이 세분화되므로 호출 패턴과 서킷브레이커/재시도 정책 영향도 함께 고려해야 합니다.',
       'seed:v5:11402:mcq:msa'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:mcq:msa');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '서비스 수를 최대한 적게 산정한다.', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:msa'
UNION ALL
SELECT q.id, 'B', '모든 마이크로서비스를 한 서버에만 배포한다.', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:msa'
UNION ALL
SELECT q.id, 'C', '서비스 간 호출 빈도와 체인 길이, 서킷브레이커/재시도 정책의 영향을 함께 분석한다.', 1
FROM question q WHERE q.source = 'seed:v5:11402:mcq:msa'
UNION ALL
SELECT q.id, 'D', '인터페이스 대상 식별은 단일 시스템일 때만 수행한다.', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:msa';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11402:mcq:msa';


-- Q8: 인터페이스 식별 산출물
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 대상 식별 단계의 대표적인 산출물로 가장 적절한 것은?',
       NULL,
       'B',
       '인터페이스 목록과 시스템 간 연계도(컨텍스트 다이어그램 등)가 대표적인 산출물입니다.',
       'seed:v5:11402:mcq:deliverable'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:mcq:deliverable');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '소스 코드 레포지토리 구조도', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:deliverable'
UNION ALL
SELECT q.id, 'B', '인터페이스 목록과 시스템 간 연계 다이어그램', 1
FROM question q WHERE q.source = 'seed:v5:11402:mcq:deliverable'
UNION ALL
SELECT q.id, 'C', '개발자 개인별 작업일지', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:deliverable'
UNION ALL
SELECT q.id, 'D', '사내 게시판 공지 글 모음', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:deliverable';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11402:mcq:deliverable';


-- Q9: 파일 인터페이스 식별
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'MCQ', 'NORMAL',
       '파일 기반 인터페이스를 식별할 때 우선적으로 확인해야 할 사항으로 가장 거리가 먼 것은?',
       NULL,
       'D',
       '파일 경로, 암호화 여부, 전송 방식, 포맷 등은 필수 확인 사항입니다.',
       'seed:v5:11402:mcq:file'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:mcq:file');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '파일 전송 방식(FTP/SFTP 등)', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:file'
UNION ALL
SELECT q.id, 'B', '파일 포맷(CSV, XML 등)과 인코딩 방식', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:file'
UNION ALL
SELECT q.id, 'C', '파일 생성·전송 주기와 파일 크기', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:file'
UNION ALL
SELECT q.id, 'D', '개발자 개인의 메일 계정 주소', 1
FROM question q WHERE q.source = 'seed:v5:11402:mcq:file';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11402:mcq:file';


-- Q10: 인터페이스 대상 우선순위
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11402, 'WRITTEN', 'MCQ', 'NORMAL',
       '식별된 여러 인터페이스 대상 중 우선적으로 설계해야 할 대상을 선정하는 기준으로 가장 적절한 것은?',
       NULL,
       'C',
       '업무 중요도, 리스크, 외부 제약이 큰 대상부터 우선 설계하는 것이 일반적입니다.',
       'seed:v5:11402:mcq:priority'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11402:mcq:priority');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '담당 개발자의 선호도', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:priority'
UNION ALL
SELECT q.id, 'B', '가장 구현이 쉬운 인터페이스부터', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:priority'
UNION ALL
SELECT q.id, 'C', '업무 영향도와 리스크, 외부 기관 연계 여부 등을 고려해 우선순위를 정한다.', 1
FROM question q WHERE q.source = 'seed:v5:11402:mcq:priority'
UNION ALL
SELECT q.id, 'D', '이름이 알파벳 순으로 먼저 오는 대상부터', 0
FROM question q WHERE q.source = 'seed:v5:11402:mcq:priority';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11402:mcq:priority';



/* =======================================================
 * 11403 – 인터페이스 상세 설계
 *  - OX 6개, MCQ 10개
 * ======================================================= */

-- [11403] OX 6개
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 상세 설계에서는 요청/응답 메시지 구조를 필드 단위까지 구체적으로 정의해야 한다. (O/X)',
       NULL,
       'O',
       '필드명, 타입, 길이, 제약조건 등을 구체적으로 정의해야 구현자 간 오해를 줄일 수 있습니다.',
       'seed:v5:11403:ox:field-detail'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:ox:field-detail');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'OX', 'NORMAL',
       '타임아웃, 재시도, 서킷브레이커와 같은 안정성 정책은 인터페이스 상세 설계에서 다루지 않아도 된다. (O/X)',
       NULL,
       'X',
       '안정성 정책은 호출 방법의 일부이므로 인터페이스 상세 설계에서 정의하는 것이 바람직합니다.',
       'seed:v5:11403:ox:resilience'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:ox:resilience');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 상세 설계 시, 날짜·시간 필드는 타임존과 포맷(예: ISO8601)을 명시해야 한다. (O/X)',
       NULL,
       'O',
       '날짜/시간은 포맷과 타임존을 명시하지 않으면 시스템 간 해석이 달라질 수 있습니다.',
       'seed:v5:11403:ox:datetime'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:ox:datetime');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 버전 관리는 API URL이나 헤더 등을 활용해 표현할 수 있으며, 하위 호환성 전략과 함께 설계되어야 한다. (O/X)',
       NULL,
       'O',
       'v1/v2 또는 헤더 기반 버전 관리 등과 함께 하위 호환 전략을 설계해야 합니다.',
       'seed:v5:11403:ox:versioning'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:ox:versioning');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'OX', 'EASY',
       'Idempotent(멱등)한 인터페이스 설계는 중복 요청이 발생해도 시스템 상태가 한 번 수행한 것과 동일하게 유지되도록 한다. (O/X)',
       NULL,
       'O',
       '멱등성은 네트워크 재시도나 중복 전송 상황에서 중요한 설계 개념입니다.',
       'seed:v5:11403:ox:idempotent'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:ox:idempotent');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 상세 설계에서 예시 요청/응답(JSON, XML 등)을 제공하면 개발·테스트에 도움이 된다. (O/X)',
       NULL,
       'O',
       '예시 메시지는 구현자가 사양을 오해하지 않도록 돕는 좋은 수단입니다.',
       'seed:v5:11403:ox:example'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:ox:example');


-- [11403] MCQ 10개

-- Q1: 필드 설계 시 유의사항
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'MCQ', 'EASY',
       '인터페이스 필드 설계 시 가장 적절한 설명은?',
       NULL,
       'B',
       '의미 있는 이름, 타입/길이/제약 정의, 널 허용 여부 등을 명확히 해야 합니다.',
       'seed:v5:11403:mcq:field'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:mcq:field');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '모든 필드 타입을 문자열로 통일한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:field'
UNION ALL
SELECT q.id, 'B', '필드명, 타입, 길이, 허용 값 범위 등을 명확히 정의한다.', 1
FROM question q WHERE q.source = 'seed:v5:11403:mcq:field'
UNION ALL
SELECT q.id, 'C', '필드명은 a1, a2처럼 짧게 아무렇게나 정한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:field'
UNION ALL
SELECT q.id, 'D', '필드 설명은 문서에 포함하지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:field';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11403:mcq:field';


-- Q2: 에러 응답 포맷 설계
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'MCQ', 'NORMAL',
       '에러 응답 포맷을 설계할 때 가장 적절한 항목 조합은?',
       NULL,
       'C',
       '에러 코드, 메시지, 상세 정보(필요 시)를 구조화해 제공하는 것이 일반적입니다.',
       'seed:v5:11403:mcq:error-format'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:mcq:error-format');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '에러 여부만 true/false로 전달한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:error-format'
UNION ALL
SELECT q.id, 'B', 'HTTP 상태 코드를 항상 200으로 고정하고, 본문에만 에러 여부를 넣는다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:error-format'
UNION ALL
SELECT q.id, 'C', '에러 코드, 메시지, 필요 시 상세 원인 정보를 구조화하여 제공한다.', 1
FROM question q WHERE q.source = 'seed:v5:11403:mcq:error-format'
UNION ALL
SELECT q.id, 'D', '예외 스택 트레이스를 그대로 클라이언트에 전달한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:error-format';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11403:mcq:error-format';


-- Q3: HTTP 메서드와 멱등성
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'MCQ', 'NORMAL',
       'HTTP 기반 인터페이스 설계에서 멱등성(Idempotency)을 고려한 메서드 사용에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       'GET, PUT, DELETE는 멱등, POST는 일반적으로 비멱등으로 설계합니다(예외 가능).',
       'seed:v5:11403:mcq:http-idempotent'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:mcq:http-idempotent');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'POST 요청은 항상 멱등이어야 한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:http-idempotent'
UNION ALL
SELECT q.id, 'B', 'GET/PUT/DELETE는 멱등하게, POST는 보통 비멱등하게 설계하는 것이 일반적이다.', 1
FROM question q WHERE q.source = 'seed:v5:11403:mcq:http-idempotent'
UNION ALL
SELECT q.id, 'C', 'PUT은 반드시 비멱등이어야 한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:http-idempotent'
UNION ALL
SELECT q.id, 'D', '멱등성은 HTTP 인터페이스와는 무관한 개념이다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:http-idempotent';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11403:mcq:http-idempotent';


-- Q4: 예시 메시지 제공 목적
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'MCQ', 'EASY',
       '인터페이스 상세 설계 시 예시 요청/응답 메시지를 제공하는 주된 이유로 가장 적절한 것은?',
       NULL,
       'C',
       '예시는 필드 의미와 구조를 직관적으로 이해하게 해 주고, 구현 시 오해를 줄입니다.',
       'seed:v5:11403:mcq:example'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:mcq:example');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '문서 분량을 늘려 가독성을 떨어뜨리기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:example'
UNION ALL
SELECT q.id, 'B', '개발자가 사양을 읽지 못하게 하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:example'
UNION ALL
SELECT q.id, 'C', '실제 데이터 구조를 예로 보여줘 구현자 간 해석 차이를 줄이기 위해', 1
FROM question q WHERE q.source = 'seed:v5:11403:mcq:example'
UNION ALL
SELECT q.id, 'D', '테스트 데이터를 만들지 않기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:example';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11403:mcq:example';


-- Q5: 버전 관리 전략
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 버전 관리 전략에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       '주로 URL(예: /v1, /v2) 또는 헤더를 활용해 버전을 구분하고, 하위 호환/중단 정책을 함께 정의합니다.',
       'seed:v5:11403:mcq:version'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:mcq:version');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '항상 하나의 버전만 운영하고 변경 시 기존 클라이언트는 고려하지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:version'
UNION ALL
SELECT q.id, 'B', '버전 번호는 문서에만 적고 실제 API에는 표시하지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:version'
UNION ALL
SELECT q.id, 'C', 'URL 또는 헤더에 버전을 명시하고, 하위 호환성과 중단 시점 정책을 함께 정의한다.', 1
FROM question q WHERE q.source = 'seed:v5:11403:mcq:version'
UNION ALL
SELECT q.id, 'D', '버전은 개발자마다 자유롭게 부여한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:version';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11403:mcq:version';


-- Q6: 시퀀스 다이어그램 활용
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 상세 설계 시 시퀀스 다이어그램을 활용하는 주된 이유로 가장 적절한 것은?',
       NULL,
       'B',
       '요청/응답 순서, 조건 분기, 재시도 흐름 등을 시각적으로 표현해 이해를 돕습니다.',
       'seed:v5:11403:mcq:sequence'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:mcq:sequence');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '데이터 구조를 정적 관점에서 표현하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:sequence'
UNION ALL
SELECT q.id, 'B', '시스템 간 메시지 교환 순서와 조건을 시각적으로 표현하기 위해', 1
FROM question q WHERE q.source = 'seed:v5:11403:mcq:sequence'
UNION ALL
SELECT q.id, 'C', 'DB 테이블 간 관계를 표현하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:sequence'
UNION ALL
SELECT q.id, 'D', 'UI 레이아웃을 설계하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:sequence';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11403:mcq:sequence';


-- Q7: 보안 설계 (민감정보)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 상세 설계에서 민감 정보(개인정보 등)를 다룰 때 가장 적절한 설계는?',
       NULL,
       'C',
       '필요 최소한만 전송하고, 암호화/마스킹/접근 통제를 설계에 반영해야 합니다.',
       'seed:v5:11403:mcq:pii'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:mcq:pii');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '모든 개인정보를 평문으로 전송한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:pii'
UNION ALL
SELECT q.id, 'B', '민감 정보를 가급적 많이 전송해 두고, 사용 여부는 클라이언트에 맡긴다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:pii'
UNION ALL
SELECT q.id, 'C', '필요 최소한의 필드만 포함하고, 암호화/마스킹 및 접근 통제를 설계에 반영한다.', 1
FROM question q WHERE q.source = 'seed:v5:11403:mcq:pii'
UNION ALL
SELECT q.id, 'D', '보안을 위해 민감 정보를 로그에 모두 남긴다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:pii';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11403:mcq:pii';


-- Q8: Idempotency Key 설계
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'MCQ', 'NORMAL',
       '멱등성 보장을 위해 Idempotency Key를 사용하는 설계에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'B',
       '클라이언트가 고유 키를 보내고, 서버가 처리 결과를 키 기준으로 기억·재사용하는 방식이 일반적입니다.',
       'seed:v5:11403:mcq:idempotency-key'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:mcq:idempotency-key');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '서버가 매 요청마다 임의의 키를 생성해 클라이언트에 전송한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:idempotency-key'
UNION ALL
SELECT q.id, 'B', '클라이언트가 멱등 키를 전송하면, 서버는 해당 키에 대한 처리를 한 번만 수행하고 결과를 재사용한다.', 1
FROM question q WHERE q.source = 'seed:v5:11403:mcq:idempotency-key'
UNION ALL
SELECT q.id, 'C', '멱등 키는 응답에만 포함하고 요청에는 포함하지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:idempotency-key'
UNION ALL
SELECT q.id, 'D', '멱등 키는 로그 출력에만 사용되며, 처리에는 영향을 주지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:idempotency-key';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11403:mcq:idempotency-key';


-- Q9: 타임아웃/재시도/서킷브레이커 조합
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'MCQ', 'NORMAL',
       '다음 중 인터페이스 안정성 설계(타임아웃/재시도/서킷브레이커)에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       '적절한 타임아웃과 제한된 재시도, 서킷브레이커를 조합해 연쇄 장애를 방지해야 합니다.',
       'seed:v5:11403:mcq:resilience'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:mcq:resilience');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '타임아웃은 무제한, 재시도는 무한대로 설정한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:resilience'
UNION ALL
SELECT q.id, 'B', '서킷브레이커는 장애 분석을 어렵게 하므로 사용하지 않는다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:resilience'
UNION ALL
SELECT q.id, 'C', '적절한 타임아웃과 제한된 재시도, 서킷브레이커를 조합해 연쇄 장애를 방지한다.', 1
FROM question q WHERE q.source = 'seed:v5:11403:mcq:resilience'
UNION ALL
SELECT q.id, 'D', '안정성 설계는 운영 단계에서 장애가 난 후에만 고려한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:resilience';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11403:mcq:resilience';


-- Q10: 문서-스웨거 정합성
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11403, 'WRITTEN', 'MCQ', 'NORMAL',
       '인터페이스 상세 설계 문서와 Swagger(OpenAPI) 스펙을 병행 관리할 때 가장 바람직한 방법은?',
       NULL,
       'B',
       '단일 소스를 기준으로 삼고, 변경 시 양쪽을 함께 갱신하는 프로세스를 마련해야 합니다.',
       'seed:v5:11403:mcq:swagger'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:v5:11403:mcq:swagger');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '문서와 Swagger는 서로 다른 내용을 가져도 상관없다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:swagger'
UNION ALL
SELECT q.id, 'B', '하나를 기준 소스로 정하고, 변경 시 두 스펙이 항상 일치되도록 관리 프로세스를 둔다.', 1
FROM question q WHERE q.source = 'seed:v5:11403:mcq:swagger'
UNION ALL
SELECT q.id, 'C', 'Swagger는 운영 환경에서만 관리하고, 문서는 개발용으로만 사용한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:swagger'
UNION ALL
SELECT q.id, 'D', '스펙 변경 시 커뮤니케이션 없이 바로 배포한다.', 0
FROM question q WHERE q.source = 'seed:v5:11403:mcq:swagger';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11403:mcq:swagger';

/* =======================================================
 * question_tag – 11401/11402/11403 태그 매핑
 *  - 11401: 인터페이스요구
 *  - 11402: 연계대상
 *  - 11403: 인터페이스설계 / 데이터명세
 * ======================================================= */

-- 11401 – 인터페이스 요구사항 확인
--  -> 모든 WRITTEN 문제에 '인터페이스요구' 태그 부여
INSERT INTO question_tag (question_id, tag, created_at)
SELECT q.id, '인터페이스요구', NOW()
FROM question q
WHERE q.topic_id = @tp_11401
  AND q.mode = 'WRITTEN'
  AND NOT EXISTS (
    SELECT 1
    FROM question_tag qt
    WHERE qt.question_id = q.id
      AND qt.tag = '인터페이스요구'
  );


-- 11402 – 인터페이스 대상 식별
--  -> 모든 WRITTEN 문제에 '연계대상' 태그 부여
INSERT INTO question_tag (question_id, tag, created_at)
SELECT q.id, '연계대상', NOW()
FROM question q
WHERE q.topic_id = @tp_11402
  AND q.mode = 'WRITTEN'
  AND NOT EXISTS (
    SELECT 1
    FROM question_tag qt
    WHERE qt.question_id = q.id
      AND qt.tag = '연계대상'
  );


-- 11403 – 인터페이스 상세 설계
-- 1) 전체 문제 공통 태그: '인터페이스설계'
INSERT INTO question_tag (question_id, tag, created_at)
SELECT q.id, '인터페이스설계', NOW()
FROM question q
WHERE q.topic_id = @tp_11403
  AND q.mode = 'WRITTEN'
  AND NOT EXISTS (
    SELECT 1
    FROM question_tag qt
    WHERE qt.question_id = q.id
      AND qt.tag = '인터페이스설계'
  );

-- 2) 데이터 스펙에 더 가까운 문제들에 '데이터명세' 추가 태그
--    (필드 정의, 날짜/시간 포맷, 예시 메시지, 에러 응답 구조 등)
INSERT INTO question_tag (question_id, tag, created_at)
SELECT q.id, '데이터명세', NOW()
FROM question q
WHERE q.source IN (
  'seed:v5:11403:ox:field-detail',
  'seed:v5:11403:ox:datetime',
  'seed:v5:11403:ox:example',
  'seed:v5:11403:mcq:field',
  'seed:v5:11403:mcq:example',
  'seed:v5:11403:mcq:error-format'
)
AND NOT EXISTS (
  SELECT 1
  FROM question_tag qt
  WHERE qt.question_id = q.id
    AND qt.tag = '데이터명세'
);


SET FOREIGN_KEY_CHECKS = 1;
