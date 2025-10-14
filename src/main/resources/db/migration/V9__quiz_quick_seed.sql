/* V10__quiz_quick_seed.sql
 * 태그 기반 퀵퀴즈용 최소 시드 (6문항)
 * - 주제: 트랜잭션, 격리수준, 인덱스, 정규화, 조인
 * - choices_json: 유효한 JSON 문자열
 * - answer_idx: 0-based
 */

-- Q1: 트랜잭션 ACID
INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
VALUES (
  'custom', 2025, 1, 1,
  '트랜잭션의 ACID 중 "원자성(Atomicity)"에 대한 올바른 설명은 무엇인가?',
  '["트랜잭션은 일부만 성공할 수 있다","트랜잭션은 모두 성공하거나 모두 실패한다","트랜잭션은 항상 즉시 디스크에 기록된다","트랜잭션은 다른 트랜잭션과 반드시 직렬로 실행된다"]',
  1,
  '원자성은 All-or-Nothing 특성으로, 부분 완료가 아닌 전부 성공/전부 실패를 보장한다.',
  '{"tags":["트랜잭션","ACID"],"level":"basic"}'
);
SET @q1 := LAST_INSERT_ID();
INSERT INTO question_tag(question_id, tag) VALUES
(@q1,'트랜잭션'),(@q1,'ACID');

-- Q2: 격리수준 - Dirty Read
INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
VALUES (
  'custom', 2025, 1, 2,
  'Dirty Read를 허용하는 격리수준은 무엇인가?',
  '["READ UNCOMMITTED","READ COMMITTED","REPEATABLE READ","SERIALIZABLE"]',
  0,
  'READ UNCOMMITTED에서는 커밋되지 않은 데이터를 읽을 수 있어 Dirty Read가 발생한다.',
  '{"tags":["격리수준","동시성"],"level":"intermediate"}'
);
SET @q2 := LAST_INSERT_ID();
INSERT INTO question_tag(question_id, tag) VALUES
(@q2,'격리수준'),(@q2,'동시성'),(@q2,'트랜잭션');

-- Q3: 격리수준 - Phantom Read
INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
VALUES (
  'custom', 2025, 1, 2,
  'Phantom Read 방지를 가장 강하게 보장하는 격리수준은?',
  '["READ COMMITTED","REPEATABLE READ","SERIALIZABLE","SNAPSHOT"]',
  2,
  'SERIALIZABLE은 가장 보수적인 수준으로, 범위 잠금 등을 통해 팬텀 리드를 방지한다.',
  '{"tags":["격리수준","팬텀리드"],"level":"intermediate"}'
);
SET @q3 := LAST_INSERT_ID();
INSERT INTO question_tag(question_id, tag) VALUES
(@q3,'격리수준'),(@q3,'팬텀리드'),(@q3,'트랜잭션');

-- Q4: 인덱스 선택
INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
VALUES (
  'custom', 2025, 1, 2,
  '다음 중 B-Tree 인덱스의 효율이 가장 떨어지는 조건은 무엇인가?',
  '["부등호(>) 범위 조건","= 동등 조건","앞 부분 LIKE 접두사 검색(abc%)","함수 적용된 컬럼(LOWER(col)) 검색"]',
  3,
  '일반적으로 함수가 적용된 컬럼은 인덱스를 타지 못한다(함수 기반 인덱스가 없다면).',
  '{"tags":["인덱스","쿼리최적화"],"level":"intermediate"}'
);
SET @q4 := LAST_INSERT_ID();
INSERT INTO question_tag(question_id, tag) VALUES
(@q4,'인덱스'),(@q4,'튜닝'),(@q4,'쿼리최적화');

-- Q5: 정규화
INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
VALUES (
  'custom', 2025, 1, 1,
  '제3정규형(3NF)의 핵심 목표는 무엇인가?',
  '["부분 함수적 종속 제거","이행적 종속 제거","다치 종속 제거","보이스-코드 정규형 충족"]',
  1,
  '3NF는 이행적 종속(키가 아닌 속성이 다른 키가 아닌 속성에 의존)을 제거하는 것이 핵심이다.',
  '{"tags":["정규화","데이터모델링"],"level":"basic"}'
);
SET @q5 := LAST_INSERT_ID();
INSERT INTO question_tag(question_id, tag) VALUES
(@q5,'정규화'),(@q5,'데이터모델링');

-- Q6: 조인/카디널리티
INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
VALUES (
  'custom', 2025, 1, 2,
  '대용량 테이블 간 조인에서 옵티마이저가 드라이빙 테이블로 선호하는 것은 일반적으로?',
  '["카디널리티가 낮은 테이블","카디널리티가 높은 테이블","임의의 임시 테이블","항상 오른쪽 테이블"]',
  1,
  '일반적으로 선택도가 높은(카디널리티가 큰) 테이블을 먼저 필터링하는 것이 효율적이다(옵티마이저/통계에 따라 상이).',
  '{"tags":["조인","카디널리티","튜닝"],"level":"intermediate"}'
);
SET @q6 := LAST_INSERT_ID();
INSERT INTO question_tag(question_id, tag) VALUES
(@q6,'조인'),(@q6,'카디널리티'),(@q6,'튜닝');

-- 태그 연동을 퀵퀴즈에서 활용하려면 최소 하나 이상은 '트랜잭션' 또는 '격리수준' 태그를 포함하도록 시드합니다.
