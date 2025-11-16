SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

SET @tp_11101 := 11101; -- 1.1.1 현행 시스템 분석
SET @tp_11102 := 11102; -- 1.1.2 요구사항 확인 기법/UML/애자일
SET @tp_11103 := 11103; -- 1.1.3 분석 모델/요구 관리

/* ================================================
 * 11101 – 현행 시스템 분석
 *  - OX: 기존 5개 + 1개 추가 = 6개
 *  - MCQ: 기존 0개 + 10개 추가 = 10개
 * ================================================ */

-- [11101] OX 추가 (1개 → 총 6개)
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11101, 'WRITTEN', 'OX', 'NORMAL',
       '현행 시스템 분석에서 로그와 모니터링 지표는 “문제 발생 시 원인 추적”뿐 아니라, 신규 시스템의 용량 산정과 임계치 정의에도 활용된다. (O/X)',
       NULL,
       'O',
       '로그·모니터링 지표는 단순 장애 분석을 넘어, 신시스템에서의 성능/용량 기준과 알림 임계치 정의에 중요한 근거가 됩니다.',
       'seed:v5:11101:ox:monitoring-capacity'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11101:ox:monitoring-capacity'
);

-- [11101] MCQ 10개 추가
/* Q1: 현행 분석 범위 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11101, 'WRITTEN', 'MCQ', 'EASY',
       '현행 시스템 분석 단계에서 “가장 먼저” 확인해야 할 내용으로 가장 적절한 것은?',
       NULL,
       'B',
       '현행 분석의 출발점은 “무슨 일을 하는 시스템인지(주요 기능/업무 프로세스)”를 파악하는 것입니다.',
       'seed:v5:11101:mcq:scope-first'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11101:mcq:scope-first'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '서버 OS와 DBMS 버전부터 상세히 파악한다.', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:scope-first'
UNION ALL
SELECT q.id, 'B', '시스템이 지원하는 주요 업무 프로세스와 기능을 파악한다.', 1
FROM question q WHERE q.source = 'seed:v5:11101:mcq:scope-first'
UNION ALL
SELECT q.id, 'C', '추후 도입할 신기술 목록을 정리한다.', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:scope-first'
UNION ALL
SELECT q.id, 'D', '테스트 케이스를 먼저 설계한다.', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:scope-first';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11101:mcq:scope-first';

/* Q2: 인터페이스 현황 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11101, 'WRITTEN', 'MCQ', 'NORMAL',
       '다음 중 현행 시스템 분석에서 “인터페이스 현황”을 정리할 때 필수적으로 포함해야 할 항목이 아닌 것은?',
       NULL,
       'D',
       '인터페이스 분석에는 연계 대상, 데이터, 프로토콜, 주기, 오류 처리 방식 등이 포함되며, 소스코드 양 자체는 필수 항목은 아닙니다.',
       'seed:v5:11101:mcq:interface-items'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11101:mcq:interface-items'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '연계 대상 시스템 정보와 인터페이스 ID', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:interface-items'
UNION ALL
SELECT q.id, 'B', '전송 데이터 항목과 포맷(JSON/CSV 등)', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:interface-items'
UNION ALL
SELECT q.id, 'C', '전송 주기/트리거와 재전송 정책', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:interface-items'
UNION ALL
SELECT q.id, 'D', '인터페이스 관련 소스코드 라인 수(LOC)', 1
FROM question q WHERE q.source = 'seed:v5:11101:mcq:interface-items';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11101:mcq:interface-items';

/* Q3: 성능 지표 선택 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11101, 'WRITTEN', 'MCQ', 'NORMAL',
       '현행 시스템의 “사용자 체감 성능”을 파악하기 위해 가장 먼저 확인해야 할 지표 조합으로 가장 적절한 것은?',
       NULL,
       'C',
       '사용자 체감 성능은 보통 응답시간, 처리량, 동시 접속자 수와 밀접한 관련이 있습니다.',
       'seed:v5:11101:mcq:perf-metrics'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11101:mcq:perf-metrics'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'CPU 온도, 전원 사용량', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:perf-metrics'
UNION ALL
SELECT q.id, 'B', '디스크 용량 사용률, OS 버전', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:perf-metrics'
UNION ALL
SELECT q.id, 'C', '응답시간, 처리량, 동시 접속자 수', 1
FROM question q WHERE q.source = 'seed:v5:11101:mcq:perf-metrics'
UNION ALL
SELECT q.id, 'D', '개발 인원 수, 코드 라인 수', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:perf-metrics';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11101:mcq:perf-metrics';

/* Q4: 장애 이력 활용 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11101, 'WRITTEN', 'MCQ', 'NORMAL',
       '현행 시스템 분석 시 장애 이력을 수집하는 “가장 큰 목적”으로 알맞은 것은?',
       NULL,
       'B',
       '장애 이력은 신시스템에서 반드시 개선해야 할 품질 요구(가용성, 신뢰성, 회복 시간 등)를 도출하는 근거가 됩니다.',
       'seed:v5:11101:mcq:incident-purpose'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11101:mcq:incident-purpose'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '장애 담당자를 평가하기 위한 근거를 확보하기 위해서', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:incident-purpose'
UNION ALL
SELECT q.id, 'B', '신규 시스템의 신뢰성/가용성 요구사항을 도출하기 위해서', 1
FROM question q WHERE q.source = 'seed:v5:11101:mcq:incident-purpose'
UNION ALL
SELECT q.id, 'C', '백업 정책을 완전히 제거할 근거를 찾기 위해서', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:incident-purpose'
UNION ALL
SELECT q.id, 'D', '테스트 케이스 수를 줄이기 위한 명분을 만들기 위해서', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:incident-purpose';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11101:mcq:incident-purpose';

/* Q5: AS-IS/TO-BE 비교 관점 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11101, 'WRITTEN', 'MCQ', 'NORMAL',
       'AS-IS/TO-BE 분석에서 AS-IS 모델을 만드는 주된 이유로 가장 알맞은 것은?',
       NULL,
       'C',
       'AS-IS를 통해 현재 문제점과 제약을 구조화해서 보여주어, TO-BE 방향성을 이해관계자와 공유하기 위함입니다.',
       'seed:v5:11101:mcq:as-is-reason'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11101:mcq:as-is-reason'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '개발 언어를 선택하기 위해서', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:as-is-reason'
UNION ALL
SELECT q.id, 'B', '테스트 자동화 도구를 고르기 위해서', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:as-is-reason'
UNION ALL
SELECT q.id, 'C', '현재 구조와 문제점을 시각화해 개선 방향을 합의하기 위해서', 1
FROM question q WHERE q.source = 'seed:v5:11101:mcq:as-is-reason'
UNION ALL
SELECT q.id, 'D', '조직도 작성을 대신하기 위해서', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:as-is-reason';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11101:mcq:as-is-reason';

/* Q6: 분석 산출물 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11101, 'WRITTEN', 'MCQ', 'EASY',
       '현행 시스템 분석 산출물로 보기 어려운 것은?',
       NULL,
       'D',
       '현행 분석 산출물에는 보통 현행 구조도, 업무 흐름도, 인터페이스 목록, 장애/성능 현황 등이 포함됩니다.',
       'seed:v5:11101:mcq:deliverables'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11101:mcq:deliverables'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '현행 업무 프로세스 다이어그램', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:deliverables'
UNION ALL
SELECT q.id, 'B', '현행 시스템 구성도(아키텍처 다이어그램)', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:deliverables'
UNION ALL
SELECT q.id, 'C', '현행 인터페이스 목록과 데이터 흐름', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:deliverables'
UNION ALL
SELECT q.id, 'D', '미래 조직 개편 시나리오 상세 설계서', 1
FROM question q WHERE q.source = 'seed:v5:11101:mcq:deliverables';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11101:mcq:deliverables';

/* Q7: 보안 관점 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11101, 'WRITTEN', 'MCQ', 'NORMAL',
       '다음 중 현행 시스템 분석에서 “보안 요구 도출”과 가장 직접적으로 연관된 활동은?',
       NULL,
       'B',
       '보안 요구는 민감정보 처리 위치, 접근 권한, 암복호화/로그 정책 등을 파악하면서 도출됩니다.',
       'seed:v5:11101:mcq:security'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11101:mcq:security'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '향후 사용할 암호화 알고리즘을 미리 선정', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:security'
UNION ALL
SELECT q.id, 'B', '민감정보 저장 위치, 접근 권한, 로그 정책을 조사', 1
FROM question q WHERE q.source = 'seed:v5:11101:mcq:security'
UNION ALL
SELECT q.id, 'C', '테스트 데이터 생성을 위한 목업 도구 선정', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:security'
UNION ALL
SELECT q.id, 'D', 'UI 색상 팔레트 정의', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:security';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11101:mcq:security';

/* Q8: 용량 산정 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11101, 'WRITTEN', 'MCQ', 'NORMAL',
       '현행 시스템의 데이터 용량을 조사하는 주된 이유로 가장 알맞은 것은?',
       NULL,
       'C',
       '데이터 증가 추이를 기반으로 신시스템의 저장소/아카이빙/파티션 정책을 설계할 수 있습니다.',
       'seed:v5:11101:mcq:capacity'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11101:mcq:capacity'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '개발자의 노트북 최소 사양을 결정하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:capacity'
UNION ALL
SELECT q.id, 'B', '회사 건물 전력 설비 증설을 위해', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:capacity'
UNION ALL
SELECT q.id, 'C', '신규 시스템의 저장소/파티션/아카이빙 전략 수립을 위해', 1
FROM question q WHERE q.source = 'seed:v5:11101:mcq:capacity'
UNION ALL
SELECT q.id, 'D', '모니터 해상도를 결정하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:capacity';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11101:mcq:capacity';

/* Q9: 사용자 불만 분석 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11101, 'WRITTEN', 'MCQ', 'EASY',
       '현행 시스템 분석 시 콜센터/헬프데스크의 VOC(불만/문의)를 조사하는 이유로 가장 적절한 것은?',
       NULL,
       'B',
       'VOC는 사용자가 실제로 겪는 불편과 결함을 보여주므로, 개선 요구를 정리하는 데 매우 중요합니다.',
       'seed:v5:11101:mcq:voc'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11101:mcq:voc'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '콜센터 인력을 줄이기 위한 근거를 확보하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:voc'
UNION ALL
SELECT q.id, 'B', '사용자 관점에서의 불편 사항과 개선 요구를 파악하기 위해', 1
FROM question q WHERE q.source = 'seed:v5:11101:mcq:voc'
UNION ALL
SELECT q.id, 'C', '콜센터 운영비 회계처리를 위해', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:voc'
UNION ALL
SELECT q.id, 'D', '데이터베이스 인덱스 구조를 설계하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:voc';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11101:mcq:voc';

/* Q10: 이해관계자 인터뷰 우선순위 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11101, 'WRITTEN', 'MCQ', 'NORMAL',
       '현행 시스템 분석을 위해 이해관계자 인터뷰 대상을 선정할 때, 우선순위를 판단하는 기준으로 가장 적절한 것은?',
       NULL,
       'C',
       '업무 중요도와 시스템 영향도가 높은 역할(예: 핵심 사용자, 운영 담당자, 장애 대응 담당자)을 우선 인터뷰하는 것이 일반적입니다.',
       'seed:v5:11101:mcq:stakeholder-priority'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11101:mcq:stakeholder-priority'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '개발팀과 친한 사람부터 인터뷰한다.', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:stakeholder-priority'
UNION ALL
SELECT q.id, 'B', '근무년수가 가장 짧은 사람부터 인터뷰한다.', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:stakeholder-priority'
UNION ALL
SELECT q.id, 'C', '업무·시스템 영향도가 높은 핵심 사용자/운영 담당자를 우선 인터뷰한다.', 1
FROM question q WHERE q.source = 'seed:v5:11101:mcq:stakeholder-priority'
UNION ALL
SELECT q.id, 'D', '시간이 되는 사람부터 무작위로 인터뷰한다.', 0
FROM question q WHERE q.source = 'seed:v5:11101:mcq:stakeholder-priority';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11101:mcq:stakeholder-priority';


/* ================================================
 * 11102 – 요구사항 확인 / UML / 애자일
 *  - OX: 기존 4개 + 2개 추가 = 6개
 *  - MCQ: 기존 3개(10,11,12) + 7개 추가 = 10개
 * ================================================ */

-- [11102] OX 2개 추가
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11102, 'WRITTEN', 'OX', 'NORMAL',
       '요구사항 우선순위는 단순히 “요청한 사람의 직급”만을 기준으로 정하면 안 되며, 사업 가치와 구현 난이도 등을 함께 고려해야 한다. (O/X)',
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
       '요구사항 검토 회의에서는 이해관계자 간의 요구 충돌을 발견하더라도, 문서에 그대로 두고 설계 단계에서 자동으로 조정되도록 두는 것이 일반적이다. (O/X)',
       NULL,
       'X',
       '요구 충돌은 가능한 빨리 식별하고, 책임자/이해관계자 합의를 통해 해소한 뒤 명세서에 반영해야 합니다.',
       'seed:v5:11102:ox:conflict'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11102:ox:conflict'
);

-- [11102] MCQ 7개 추가
/* Q1: 이해관계자 식별 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11102, 'WRITTEN', 'MCQ', 'EASY',
       '다음 중 요구사항 도출을 위한 이해관계자로 보기 가장 어려운 대상은?',
       NULL,
       'D',
       '이해관계자는 시스템에 영향을 주거나 영향을 받는 사람/조직으로, 실제로 관련이 없는 타 회사 고객은 포함되지 않습니다.',
       'seed:v5:11102:mcq:stakeholder'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11102:mcq:stakeholder'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '시스템을 사용하는 최종 사용자', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:stakeholder'
UNION ALL
SELECT q.id, 'B', '시스템을 운영·모니터링하는 운영 담당자', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:stakeholder'
UNION ALL
SELECT q.id, 'C', '관련 법규를 관리·감독하는 규제 기관', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:stakeholder'
UNION ALL
SELECT q.id, 'D', '시스템과 전혀 관련이 없는 타 회사 고객', 1
FROM question q WHERE q.source = 'seed:v5:11102:mcq:stakeholder';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11102:mcq:stakeholder';

/* Q2: 요구 수집 기법 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11102, 'WRITTEN', 'MCQ', 'EASY',
       '다수의 사용자의 의견을 정량적으로 수집하기에 가장 적절한 요구사항 수집 기법은?',
       NULL,
       'B',
       '설문조사는 많은 사람의 의견을 짧은 시간에 정량적으로 수집할 수 있는 전형적인 요구 수집 기법입니다.',
       'seed:v5:11102:mcq:elicitation'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11102:mcq:elicitation'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '심층 인터뷰', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:elicitation'
UNION ALL
SELECT q.id, 'B', '설문조사', 1
FROM question q WHERE q.source = 'seed:v5:11102:mcq:elicitation'
UNION ALL
SELECT q.id, 'C', '프로토타입 테스트', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:elicitation'
UNION ALL
SELECT q.id, 'D', '워크숍에서 브레인스토밍', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:elicitation';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11102:mcq:elicitation';

/* Q3: 요구 명세 품질 기준 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11102, 'WRITTEN', 'MCQ', 'NORMAL',
       '다음 중 좋은 요구사항 명세가 가져야 할 품질 특성으로 가장 적절하지 않은 것은?',
       NULL,
       'D',
       '좋은 요구는 명확성, 일관성, 검증 가능성 등을 가져야 하며, “개발자에게만 이해 가능한 표현”은 좋은 특성이 아닙니다.',
       'seed:v5:11102:mcq:quality'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11102:mcq:quality'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '모호하지 않고 명확해야 한다.', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:quality'
UNION ALL
SELECT q.id, 'B', '상호 모순되거나 충돌되는 요구가 없어야 한다.', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:quality'
UNION ALL
SELECT q.id, 'C', '테스트를 통해 검증 가능해야 한다.', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:quality'
UNION ALL
SELECT q.id, 'D', '특정 개발자만 이해할 수 있도록 기술적으로 복잡해야 한다.', 1
FROM question q WHERE q.source = 'seed:v5:11102:mcq:quality';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11102:mcq:quality';

/* Q4: UML 다이어그램 선택 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11102, 'WRITTEN', 'MCQ', 'EASY',
       '사용자와 시스템 간 상호작용(시나리오)을 표현하는 데 가장 적절한 UML 다이어그램은?',
       NULL,
       'C',
       '유스케이스 다이어그램은 액터와 시스템 간의 상호작용을 기능 관점에서 표현합니다.',
       'seed:v5:11102:mcq:uml-usecase'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11102:mcq:uml-usecase'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '클래스 다이어그램', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:uml-usecase'
UNION ALL
SELECT q.id, 'B', '컴포넌트 다이어그램', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:uml-usecase'
UNION ALL
SELECT q.id, 'C', '유스케이스 다이어그램', 1
FROM question q WHERE q.source = 'seed:v5:11102:mcq:uml-usecase'
UNION ALL
SELECT q.id, 'D', '상태 다이어그램', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:uml-usecase';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11102:mcq:uml-usecase';

/* Q5: 애자일 특징 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11102, 'WRITTEN', 'MCQ', 'NORMAL',
       '다음 중 애자일(Agile) 개발 방법의 특징으로 가장 적절한 것은?',
       NULL,
       'B',
       '애자일은 짧은 반복 주기와 지속적인 피드백을 통해 요구 변경에 유연하게 대응합니다.',
       'seed:v5:11102:mcq:agile'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11102:mcq:agile'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '초기에 상세 문서를 모두 완성한 후 개발에 들어간다.', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:agile'
UNION ALL
SELECT q.id, 'B', '짧은 반복과 피드백으로 요구 변경에 유연하게 대응한다.', 1
FROM question q WHERE q.source = 'seed:v5:11102:mcq:agile'
UNION ALL
SELECT q.id, 'C', '요구 변경은 무조건 금지하는 것이 목표이다.', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:agile'
UNION ALL
SELECT q.id, 'D', '개발팀만 참여하고 고객은 개발 완료 후에만 참여한다.', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:agile';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11102:mcq:agile';

/* Q6: 요구 변경 관리 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11102, 'WRITTEN', 'MCQ', 'NORMAL',
       '요구사항 변경 요청이 접수되었을 때 가장 “먼저” 수행해야 할 활동은?',
       NULL,
       'C',
       '변경 요청의 내용을 정확히 이해하고 영향 범위를 분석한 뒤, 승인/반려를 결정해야 합니다.',
       'seed:v5:11102:mcq:change'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11102:mcq:change'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '일단 개발부터 시작하고 나중에 문서를 수정한다.', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:change'
UNION ALL
SELECT q.id, 'B', '요청자의 직급을 기준으로 자동 승인한다.', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:change'
UNION ALL
SELECT q.id, 'C', '변경 내용과 영향 범위를 분석한다.', 1
FROM question q WHERE q.source = 'seed:v5:11102:mcq:change'
UNION ALL
SELECT q.id, 'D', '테스트 케이스 수를 줄이는 방안을 먼저 찾는다.', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:change';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11102:mcq:change';

/* Q7: 요구 추적성 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11102, 'WRITTEN', 'MCQ', 'NORMAL',
       '요구 추적성 관리의 주된 목적에 대한 설명으로 가장 알맞은 것은?',
       NULL,
       'B',
       '요구~설계~테스트 간 링크를 관리해 변경 시 영향을 받는 아티팩트를 추적할 수 있게 하는 것이 목표입니다.',
       'seed:v5:11102:mcq:traceability'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11102:mcq:traceability'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '개발자 인사 평가를 자동화하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:traceability'
UNION ALL
SELECT q.id, 'B', '요구 변경 시 영향받는 설계/코드/테스트를 추적하기 위해', 1
FROM question q WHERE q.source = 'seed:v5:11102:mcq:traceability'
UNION ALL
SELECT q.id, 'C', '소스코드 라인 수를 줄이기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:traceability'
UNION ALL
SELECT q.id, 'D', '테스트 자동화 도구를 선택하기 위해', 0
FROM question q WHERE q.source = 'seed:v5:11102:mcq:traceability';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11102:mcq:traceability';


/* ================================================
 * 11103 – 분석 모델/요구 관리
 *  - OX: 기존 3개 + 3개 추가 = 6개
 *  - MCQ: 기존 2개(24,28) + 8개 추가 = 10개
 * ================================================ */

-- [11103] OX 3개 추가
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11103, 'WRITTEN', 'OX', 'EASY',
       'ERD에서 관계 차수(1:1, 1:N, N:M)를 올바르게 표현하는 것은 데이터 중복과 이상 현상을 줄이는 데 도움을 준다. (O/X)',
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
       '데이터 흐름도(DFD)는 프로세스, 데이터 저장소, 외부 엔티티, 데이터 흐름을 이용해 “어디에 무엇이 저장되는지”를 표현하는 정적 모델이다. (O/X)',
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
       '분석 모델은 설계와 구현 단계에서 참조 기준이 되므로, 요구사항과 일관성이 유지되도록 지속적으로 갱신해야 한다. (O/X)',
       NULL,
       'O',
       '요구 변경 시 분석 모델도 함께 갱신해야 이후 설계/구현/테스트 불일치를 줄일 수 있습니다.',
       'seed:v5:11103:ox:model-sync'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11103:ox:model-sync'
);

-- [11103] MCQ 8개 추가
/* Q1: 모델링 관점 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11103, 'WRITTEN', 'MCQ', 'EASY',
       '다음 중 “동적인 동작/흐름”을 표현하는 다이어그램으로 가장 적절한 것은?',
       NULL,
       'C',
       '시퀀스 다이어그램은 객체 간 메시지 흐름과 시간 순서를 표현하는 대표적인 동적 다이어그램입니다.',
       'seed:v5:11103:mcq:dynamic-diagram'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11103:mcq:dynamic-diagram'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'ERD', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:dynamic-diagram'
UNION ALL
SELECT q.id, 'B', '클래스 다이어그램', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:dynamic-diagram'
UNION ALL
SELECT q.id, 'C', '시퀀스 다이어그램', 1
FROM question q WHERE q.source = 'seed:v5:11103:mcq:dynamic-diagram'
UNION ALL
SELECT q.id, 'D', '패키지 다이어그램', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:dynamic-diagram';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11103:mcq:dynamic-diagram';

/* Q2: ERD 구성 요소 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11103, 'WRITTEN', 'MCQ', 'EASY',
       '다음 중 ERD(Entity-Relationship Diagram)의 기본 구성 요소가 아닌 것은?',
       NULL,
       'D',
       'ERD의 기본 요소는 엔터티, 속성, 관계이며, 알고리즘 흐름은 ERD 대상이 아닙니다.',
       'seed:v5:11103:mcq:erd-elements'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11103:mcq:erd-elements'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '엔터티(Entity)', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:erd-elements'
UNION ALL
SELECT q.id, 'B', '속성(Attribute)', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:erd-elements'
UNION ALL
SELECT q.id, 'C', '관계(Relationship)', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:erd-elements'
UNION ALL
SELECT q.id, 'D', '알고리즘 흐름(Algorithm Flow)', 1
FROM question q WHERE q.source = 'seed:v5:11103:mcq:erd-elements';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11103:mcq:erd-elements';

/* Q3: 상태 다이어그램 활용 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11103, 'WRITTEN', 'MCQ', 'NORMAL',
       '상태 다이어그램(State Diagram)을 사용하기에 가장 적절한 대상은?',
       NULL,
       'B',
       '상태 다이어그램은 주문, 티켓 등 “상태가 바뀌며 수명주기를 가지는 객체”를 표현하는 데 적합합니다.',
       'seed:v5:11103:mcq:state-diagram'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11103:mcq:state-diagram'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '단순 산술 계산 로직', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:state-diagram'
UNION ALL
SELECT q.id, 'B', '주문 상태(접수→결제→배송→완료)', 1
FROM question q WHERE q.source = 'seed:v5:11103:mcq:state-diagram'
UNION ALL
SELECT q.id, 'C', '데이터베이스 테이블 구조', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:state-diagram'
UNION ALL
SELECT q.id, 'D', '서버 물리 배치도', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:state-diagram';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11103:mcq:state-diagram';

/* Q4: 활동 다이어그램 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11103, 'WRITTEN', 'MCQ', 'NORMAL',
       '업무 절차의 분기/병행 흐름을 시각화하기에 가장 적합한 UML 다이어그램은?',
       NULL,
       'C',
       '활동 다이어그램(Activity Diagram)은 분기, 병행, 합류 등 절차 흐름을 표현하는 데 적합합니다.',
       'seed:v5:11103:mcq:activity-diagram'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11103:mcq:activity-diagram'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', 'ERD', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:activity-diagram'
UNION ALL
SELECT q.id, 'B', '클래스 다이어그램', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:activity-diagram'
UNION ALL
SELECT q.id, 'C', '활동 다이어그램', 1
FROM question q WHERE q.source = 'seed:v5:11103:mcq:activity-diagram'
UNION ALL
SELECT q.id, 'D', '컴포넌트 다이어그램', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:activity-diagram';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11103:mcq:activity-diagram';

/* Q5: 모델 일관성 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11103, 'WRITTEN', 'MCQ', 'NORMAL',
       '분석 모델 간 일관성이 깨졌을 때 가장 먼저 수행해야 할 활동은?',
       NULL,
       'B',
       '요구와 가장 가까운 모델(예: 유스케이스, 요구 명세)을 기준으로 다른 모델들을 재검토·정렬하는 것이 일반적입니다.',
       'seed:v5:11103:mcq:model-consistency'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11103:mcq:model-consistency'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '바로 코드 구현부터 시작해 모델을 무시한다.', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:model-consistency'
UNION ALL
SELECT q.id, 'B', '요구와 가장 가까운 모델을 기준으로 다른 모델을 재검토한다.', 1
FROM question q WHERE q.source = 'seed:v5:11103:mcq:model-consistency'
UNION ALL
SELECT q.id, 'C', '테스트 케이스를 모두 삭제한다.', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:model-consistency'
UNION ALL
SELECT q.id, 'D', '이슈를 무시하고 일정에 맞춰 배포한다.', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:model-consistency';

UPDATE question
SET answer_key = 'B'
WHERE source = 'seed:v5:11103:mcq:model-consistency';

/* Q6: 요구 관리 도구 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11103, 'WRITTEN', 'MCQ', 'NORMAL',
       '요구 관리 도구가 제공해야 할 기능으로 가장 거리가 먼 것은?',
       NULL,
       'D',
       '요구 관리 도구는 요구 버전 관리, 추적성, 우선순위, 상태 관리 등을 지원합니다.',
       'seed:v5:11103:mcq:req-tool'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11103:mcq:req-tool'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '요구 변경 이력과 버전 관리', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:req-tool'
UNION ALL
SELECT q.id, 'B', '요구 상태(제안/승인/폐기) 관리', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:req-tool'
UNION ALL
SELECT q.id, 'C', '요구~테스트 케이스 간 추적성 관리', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:req-tool'
UNION ALL
SELECT q.id, 'D', '운영 체제 커널 디버깅 기능 제공', 1
FROM question q WHERE q.source = 'seed:v5:11103:mcq:req-tool';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11103:mcq:req-tool';

/* Q7: DFD/ERD 비교 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11103, 'WRITTEN', 'MCQ', 'NORMAL',
       'DFD와 ERD의 차이에 대한 설명으로 가장 적절한 것은?',
       NULL,
       'C',
       'DFD는 데이터 흐름과 처리(동적), ERD는 엔터티와 관계(정적)를 표현합니다.',
       'seed:v5:11103:mcq:dfd-vs-erd'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11103:mcq:dfd-vs-erd'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '두 다이어그램 모두 정적 구조만 표현한다.', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:dfd-vs-erd'
UNION ALL
SELECT q.id, 'B', '두 다이어그램 모두 동작 흐름만 표현한다.', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:dfd-vs-erd'
UNION ALL
SELECT q.id, 'C', 'DFD는 데이터 흐름/처리, ERD는 엔터티/관계를 표현한다.', 1
FROM question q WHERE q.source = 'seed:v5:11103:mcq:dfd-vs-erd'
UNION ALL
SELECT q.id, 'D', 'DFD와 ERD는 완전히 동일한 다이어그램이다.', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:dfd-vs-erd';

UPDATE question
SET answer_key = 'C'
WHERE source = 'seed:v5:11103:mcq:dfd-vs-erd';

/* Q8: 모델 검증 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_11103, 'WRITTEN', 'MCQ', 'NORMAL',
       '분석 모델을 검증하는 활동으로 가장 거리가 먼 것은?',
       NULL,
       'D',
       '모델 검증은 시나리오 검토, 워크스루/리뷰, 프로토타입, 일관성 체크 등으로 수행합니다.',
       'seed:v5:11103:mcq:model-review'
WHERE NOT EXISTS (
  SELECT 1 FROM question WHERE source = 'seed:v5:11103:mcq:model-review'
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT q.id, 'A', '유스케이스 시나리오를 따라가며 모델과의 일치 여부를 확인', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:model-review'
UNION ALL
SELECT q.id, 'B', '업무 담당자와 함께 다이어그램 워크스루 수행', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:model-review'
UNION ALL
SELECT q.id, 'C', '모델 간 용어/관계의 불일치 여부를 점검', 0
FROM question q WHERE q.source = 'seed:v5:11103:mcq:model-review'
UNION ALL
SELECT q.id, 'D', '운영 체제 커널 구현 코드를 리팩터링', 1
FROM question q WHERE q.source = 'seed:v5:11103:mcq:model-review';

UPDATE question
SET answer_key = 'D'
WHERE source = 'seed:v5:11103:mcq:model-review';


SET FOREIGN_KEY_CHECKS = 1;
