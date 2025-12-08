SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id   := 1;
SET @tp_31401  := 31401; -- 트랜잭션/격리수준
SET @tp_31402  := 31402; -- 동시성/락
SET @tp_31501  := 31501; -- 백업/복구/RPO/RTO
SET @tp_31502  := 31502; -- 장애분석/포스트모템


/* =======================================================
 * 31401 – 트랜잭션 / 격리수준
 * ======================================================= */

-- [31401] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'OX', 'NORMAL',
       '트랜잭션의 “원자성(Atomicity)”은 트랜잭션 내부의 작업이 모두 성공하거나 모두 실패해야 함을 의미한다.',
       NULL,
       'O',
       '은행 이체 예시처럼 일부만 성공하면 안 되는 작업을 하나의 단위로 묶는 것이 원자성입니다.',
       'seed:prac:tx_isolation:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'OX', 'NORMAL',
       'READ COMMITTED 격리수준에서는 Dirty Read와 Non-Repeatable Read가 모두 허용된다.',
       NULL,
       'X',
       'READ COMMITTED는 Dirty Read는 막지만, Non-Repeatable Read는 여전히 발생할 수 있습니다.',
       'seed:prac:tx_isolation:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'OX', 'NORMAL',
       'REPEATABLE READ 격리수준에서는 동일 트랜잭션 내에서 같은 조건으로 두 번 조회하면 항상 같은 결과를 보장하는 것을 목표로 한다.',
       NULL,
       'O',
       'REPEATABLE READ는 Non-Repeatable Read를 방지하는 것이 주요 목표입니다. (팬텀 리드는 DB 구현에 따라 다름)',
       'seed:prac:tx_isolation:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:ox6');


/* =======================================================
 * 31402 – 동시성 제어 / 락
 * ======================================================= */

-- [31402] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'OX', 'NORMAL',
       'Lost Update를 줄이기 위한 방법 중 하나로, 행 수준 락(SELECT ... FOR UPDATE)을 사용하는 것이 있다.',
       NULL,
       'O',
       '비관적 락을 사용해 동시에 같은 행을 수정하지 못하게 막는 전형적인 방법입니다.',
       'seed:prac:concurrency:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'OX', 'NORMAL',
       '낙관적 락(Optimistic Lock)은 항상 DB 수준의 락을 먼저 잡고 시작하는 방식이다.',
       NULL,
       'X',
       '낙관적 락은 버전 필드 등을 이용해 “커밋 시점에만” 충돌을 감지하는 방식으로, DB 락을 미리 잡지 않습니다.',
       'seed:prac:concurrency:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'OX', 'NORMAL',
       '데드락을 예방하기 위한 일반적인 방법 중 하나는 여러 자원을 잠글 때 항상 같은 순서로 락을 획득하도록 규칙을 두는 것이다.',
       NULL,
       'O',
       '락 획득 순서를 통일하면 순환 대기가 줄어들어 데드락 가능성이 감소합니다.',
       'seed:prac:concurrency:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:ox6');


/* =======================================================
 * 31501 – 백업 / 복구 / RPO / RTO
 * ======================================================= */

-- [31501] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31501, 'PRACTICAL', 'OX', 'NORMAL',
       '증분 백업은 전체 백업 이후 변경된 데이터만 저장하므로, 일반적으로 전체 백업보다 저장 공간을 적게 사용한다.',
       NULL,
       'O',
       '증분/로그 백업은 변경분만 저장하기 때문에 저장 공간을 줄이는 데 유리합니다.',
       'seed:prac:backup:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31501, 'PRACTICAL', 'OX', 'NORMAL',
       'RPO를 줄이기 위해서는 전체 백업 주기를 늘리고, 로그/증분 백업 주기를 촘촘히 가져가는 전략이 도움이 될 수 있다.',
       NULL,
       'O',
       '로그/증분 백업 주기를 짧게 가져가면 장애 시점과 마지막 백업 시점 간 간격(RPO)을 줄일 수 있습니다.',
       'seed:prac:backup:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31501, 'PRACTICAL', 'OX', 'NORMAL',
       'RTO는 “얼마 전 시점까지의 데이터만 복구되어도 되는가”를 의미하는 지표이다.',
       NULL,
       'X',
       'RPO가 “데이터 손실 허용 시점”, RTO는 “얼마 안에 서비스를 복구해야 하는가”를 의미합니다.',
       'seed:prac:backup:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:ox6');


/* =======================================================
 * 31502 – 장애 분석 / 포스트모템
 * ======================================================= */

-- [31502] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31502, 'PRACTICAL', 'OX', 'NORMAL',
  '장애 분석 보고서(Postmortem)에는 장애 원인뿐 아니라 영향 범위와 재발 방지 대책도 함께 포함하는 것이 일반적이다.',
  NULL,
  'O',
  '원인·영향·재발 방지 대책은 거의 모든 포스트모템 템플릿의 공통 요소입니다.',
  'seed:prac:incident:ox3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:incident:ox3');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31502, 'PRACTICAL', 'OX', 'NORMAL',
  '장애가 재발하지 않았다면, 유사 장애를 예방하기 위한 개선 과제를 정의하는 것은 의미가 없다.',
  NULL,
  'X',
  '재발 여부와 무관하게, 유사 상황에서 더 빨리 감지/대응하기 위한 대책을 고민하는 것이 포스트모템의 핵심입니다.',
  'seed:prac:incident:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:incident:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31502, 'PRACTICAL', 'OX', 'NORMAL',
  '포스트모템 문화에서는 “개인을 비난하기보다는 시스템/프로세스 개선점”에 초점을 맞추는 것이 바람직하다.',
  NULL,
  'O',
  'Blameless Postmortem은 개인 탓이 아닌 시스템/프로세스 개선을 지향합니다.',
  'seed:prac:incident:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:incident:ox5');


/* =========================================================
 * QUESTION_TAG 매핑 (31401 ~ 31502)
 * - 허용되지 않은 태그 제거
 * - 토픽별 대표 태그로 재매핑
 *   31401 → 트랜잭션
 *   31402 → 동시성
 *   31501 → 백업복구
 *   31502 → 장애분석
 * ========================================================= */

-- 31401 트랜잭션/격리수준 → '트랜잭션'
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31401
  AND qt.tag <> '트랜잭션';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '트랜잭션'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '트랜잭션'
WHERE q.topic_id = @tp_31401
  AND qt.question_id IS NULL;

-- 31402 동시성/락 → '동시성'
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31402
  AND qt.tag <> '동시성';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '동시성'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '동시성'
WHERE q.topic_id = @tp_31402
  AND qt.question_id IS NULL;

-- 31501 백업/복구/RPO/RTO → '백업복구'
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31501
  AND qt.tag <> '백업복구';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '백업복구'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '백업복구'
WHERE q.topic_id = @tp_31501
  AND qt.question_id IS NULL;

-- 31502 장애 분석/포스트모템 → '장애분석'
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31502
  AND qt.tag <> '장애분석';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '장애분석'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '장애분석'
WHERE q.topic_id = @tp_31502
  AND qt.question_id IS NULL;


SET FOREIGN_KEY_CHECKS = 1;
