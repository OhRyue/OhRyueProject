USE certpilot_study;

/* =========================================
 * 2024년 1회 – 데이터베이스 구축 (3과목)
 * Q51 ~ Q60
 * ========================================= */


/* =======================================================
 * 2024-1 Q51. 카디널리티와 차수
 * topic_id = 13003
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13003,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 릴레이션의 카디널리티(튜플 수)와 차수(속성 수)가 옳게 짝지어진 것은 무엇인가요?

| 아이디   | 성명   | 나이 | 등급 | 적립금 | 가입년도 |
|----------|--------|------|------|--------|----------|
| yuyu01   | 원유철 | 36   | 3    | 2000   | 2008     |
| gykim10  | 김성일 | 29   | 2    | 3300   | 2014     |
| kshan4   | 한경선 | 45   | 3    | 2800   | 2009     |
| namsu52  | 이남수 | 33   | 5   | 1000   | 2016     |',
  'B',
  '릴레이션에서
- 카디널리티(Cardinality)는 “행(튜플)의 개수”,
- 차수(Degree)는 “열(속성)의 개수”를 의미합니다.

표를 기준으로 하면
- 튜플 수 = 4,
- 속성 수 = 6 입니다.

따라서 카디널리티는 4, 차수는 6이며 정답은 ②번입니다.',
  'past:2024-1:Q51'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '카디널리티: 4, 차수: 4', 0),
  (@q_id, 'B', '카디널리티: 4, 차수: 6', 1),
  (@q_id, 'C', '카디널리티: 6, 차수: 4', 0),
  (@q_id, 'D', '카디널리티: 6, 차수: 6', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');


/* =======================================================
 * 2024-1 Q52. 병행제어 기법 종류가 아닌 것
 * topic_id = 13001
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13001,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 데이터베이스 병행제어(Concurrency Control) 기법의 종류가 아닌 것은 무엇인가요?',
  'A',
  '대표적인 병행제어 기법에는 다음과 같은 것들이 있습니다.
- 로킹(Locking) 기법
- 타임스탬프(Timestamp) 기법
- 다중 버전(MVCC, Multi-Version Concurrency Control) 기법 등

반면 “시분할 기법”은 운영체제에서 CPU 시간을 여러 프로세스에 일정 시간 단위로 분할해서 주는 방식으로, 병행제어 기법이라기보다는 CPU 스케줄링/자원 관리 방식에 가깝습니다.
따라서 병행제어 기법이 아닌 것은 ①번 시분할 기법입니다.',
  'past:2024-1:Q52'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '시분할 기법(Time Sharing)',      1),
  (@q_id, 'B', '로킹 기법(Locking)',              0),
  (@q_id, 'C', '다중 버전 기법(Multi-Version)',   0),
  (@q_id, 'D', '타임 스탬프 기법(Timestamp)',     0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');


/* =======================================================
 * 2024-1 Q53. 이상(Anomaly)이 아닌 것
 * topic_id = 13003
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13003,
  'WRITTEN',
  'MCQ',
  'EASY',
  '데이터 속성 간의 종속성을 제대로 고려하지 않고 잘못 설계된 데이터베이스에서는 데이터 처리 연산 시 여러 가지 “이상(Anomaly)” 현상이 발생할 수 있습니다.
다음 중 이상 현상에 해당하지 않는 것은 무엇인가요?',
  'B',
  '정규화가 제대로 되지 않은 테이블에서는 다음과 같은 이상 현상이 발생합니다.
- 삽입 이상: 일부 속성 때문에 데이터를 자유롭게 삽입하지 못하는 문제
- 갱신 이상: 같은 정보가 여러 곳에 중복 저장되어 있어, 한 곳만 수정하면 불일치가 나는 문제
- 삭제 이상: 한 정보를 삭제하면서 원치 않는 다른 정보까지 함께 사라지는 문제

반면 “검색 이상”은 일반적으로 정규화 이론에서 정의하는 대표적인 이상 유형에 포함되지 않습니다. 따라서 이상 현상이 아닌 것은 ②번입니다.',
  'past:2024-1:Q53'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '삽입 이상(Insertion Anomaly)',  0),
  (@q_id, 'B', '검색 이상(Search Anomaly)',      1),
  (@q_id, 'C', '갱신 이상(Update Anomaly)',      0),
  (@q_id, 'D', '삭제 이상(Deletion Anomaly)',    0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');


/* =======================================================
 * 2024-1 Q54. 트랜잭션의 격리성(ACID)
 * topic_id = 13001
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13001,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '트랜잭션의 주요 특성(ACID) 중 하나로, 둘 이상의 트랜잭션이 동시에 병행 실행될 때 어느 하나의 트랜잭션 실행 중에 다른 트랜잭션의 연산이 끼어들 수 없음을 의미하는 것은 무엇인가요?',
  'D',
  '트랜잭션의 ACID 특성은 다음과 같습니다.
- Atomicity(원자성): 전부 수행되거나 전혀 수행되지 않아야 함
- Consistency(일관성): 트랜잭션 전후에 데이터 일관성이 유지되어야 함
- Isolation(고립성, 격리성): 동시에 실행되는 트랜잭션끼리 서로 간섭하지 않아야 함
- Durability(지속성): 커밋된 결과는 장애가 발생해도 보존되어야 함

문제에서 “다른 트랜잭션의 연산이 끼어들 수 없다”라고 한 부분이 바로 격리성(Isolation)을 설명하는 부분이므로 ④번이 정답입니다.',
  'past:2024-1:Q54'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Consistency(일관성)',       0),
  (@q_id, 'B', 'Log(로그)',                 0),
  (@q_id, 'C', 'Durability(지속성)',        0),
  (@q_id, 'D', 'Isolation(고립성, 격리성)', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');


/* =======================================================
 * 2024-1 Q55. 슈퍼키 정의
 * topic_id = 13003
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13003,
  'WRITTEN',
  'MCQ',
  'EASY',
  '관계형 데이터베이스에서 다음 설명에 해당하는 키는 무엇인가요?

> “한 릴레이션 내의 속성들의 집합으로 구성된 키로서,
>  릴레이션을 구성하는 모든 튜플에 대한 유일성을 만족시키지만,
>  최소성은 만족시키지 못한다.”',
  'D',
  '키의 개념을 정리하면 다음과 같습니다.
- 슈퍼키: 튜플을 유일하게 식별할 수 있는 속성들의 집합(유일성 O, 최소성 X)
- 후보키: 슈퍼키 중에서 더 이상 속성을 줄일 수 없는 최소 키(유일성 O, 최소성 O)
- 기본키: 여러 후보키 중 대표로 선택한 키
- 대체키: 기본키로 선택되지 않은 나머지 후보키

문제의 설명처럼 “유일성은 만족하지만 최소성은 만족하지 못한다”는 정의는 슈퍼키에 해당하므로 ④번이 정답입니다.',
  'past:2024-1:Q55'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '대체키(Alternate Key)', 0),
  (@q_id, 'B', '후보키(Candidate Key)', 0),
  (@q_id, 'C', '외래키(Foreign Key)',   0),
  (@q_id, 'D', '슈퍼키(Super Key)',     1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');


/* =======================================================
 * 2024-1 Q56. 물리적 설계 설명 (거리가 먼 것)
 * topic_id = 13004
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13004,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '물리적 데이터베이스 설계에 대한 설명으로 가장 거리가 먼 것은 무엇인가요?',
  'D',
  '물리적 설계는 “데이터를 실제 저장장치에 어떻게 배치할지”를 결정하는 단계로, 다음과 같은 것들을 다룹니다.
- 레코드 형식, 저장 구조, 인덱스, 파티션, 파일 조직 방식
- 트랜잭션 처리량, 응답 시간, 디스크 용량 등 성능 요소

반면 ④번에서 언급한 “트랜잭션의 인터페이스 설계, 데이터 타입 및 타입 간의 관계 표현”은 논리 설계나 애플리케이션 인터페이스 설계에 더 가까운 내용입니다. 따라서 물리적 설계와는 거리가 먼 설명이므로 ④번이 정답입니다.',
  'past:2024-1:Q56'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '물리적 설계의 목적은 효율적인 방법으로 데이터를 저장하는 것이다.',          0),
  (@q_id, 'B', '트랜잭션 처리량, 응답 시간, 디스크 용량 등을 고려해야 한다.',               0),
  (@q_id, 'C', '저장 레코드의 형식, 순서, 접근 경로와 같은 정보를 사용하여 설계한다.',       0),
  (@q_id, 'D', '트랜잭션의 인터페이스를 설계하며, 데이터 타입 및 타입 간의 관계로 표현한다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');


/* =======================================================
 * 2024-1 Q57. 관계해석 논리기호 (∀)
 * topic_id = 13001
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13001,
  'WRITTEN',
  'MCQ',
  'EASY',
  '관계해석(Relational Calculus)에서 “모든 것에 대하여”의 의미를 나타내는 논리 기호는 무엇인가요?',
  'C',
  '수리 논리에서
- ∃ : “존재한다(there exists)” → 존재 한정자
- ∀ : “모든 것에 대하여(for all)” → 전칭 한정자
- ∈ : “…에 속한다”를 의미하는 포함 관계 기호
- ⊂ : “진부분집합”을 의미합니다.

관계해석에서도 동일한 논리 기호를 사용하므로, “모든 것에 대하여”를 뜻하는 기호는 ∀이며, ③번이 정답입니다.',
  'past:2024-1:Q57'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '∃', 0),
  (@q_id, 'B', '∈', 0),
  (@q_id, 'C', '∀', 1),
  (@q_id, 'D', '⊂', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');


/* =======================================================
 * 2024-1 Q58. 서브쿼리를 이용한 팀원 조회
 * topic_id = 13002
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13002,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 조건에 부합하는 SQL 문을 작성하려고 합니다.
단, 팀코드와 이름은 모두 직원 테이블의 속성입니다.

> [조건]
> 이름이 ''정도일''인 팀원이 소속된 팀코드를 이용하여
> 해당 팀에 소속된 팀원들의 이름을 출력하시오.

> [SQL]
> SELECT 이름
> FROM 직원
> WHERE 팀코드 = (      );

위 SQL에서 괄호 ( ) 안에 들어갈 내용으로 옳은 것은 무엇인가요?',
  'D',
  '원하는 동작은 다음과 같습니다.
1) “이름이 정도일인 직원”의 팀코드를 구하고
2) 그 팀코드를 이용해 같은 팀 직원들의 이름을 조회

올바른 SQL:

SELECT 이름
FROM 직원
WHERE 팀코드 = (
  SELECT 팀코드
  FROM 직원
  WHERE 이름 = ''정도일''
);

따라서 괄호 안에는 팀코드를 반환하는 서브쿼리가 들어가야 하므로 ④번이 정답입니다.',
  'past:2024-1:Q58'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '이름 = ''정도일''', 0),
  (@q_id, 'B', '팀코드 = (SELECT 이름 FROM 직원 WHERE 이름 = ''정도일'')', 0),
  (@q_id, 'C', '직원 = ''정도일''', 0),
  (@q_id, 'D', '팀코드 = (SELECT 팀코드 FROM 직원 WHERE 이름 = ''정도일'')', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');


/* =======================================================
 * 2024-1 Q59. CRUD 매트릭스 분석
 * topic_id = 13003
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13003,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '데이터베이스에 영향을 주는 생성(Create), 읽기(Read), 갱신(Update), 삭제(Delete) 연산에 대해, 프로세스와 테이블 간의 매트릭스를 만들어 트랜잭션을 분석하는 기법은 무엇인가요?',
  'D',
  '각 프로세스(또는 기능)가 어떤 테이블에 대해 Create/Read/Update/Delete 연산을 수행하는지 매트릭스로 정리한 것을 CRUD 매트릭스라고 합니다.

이를 통해
- 특정 테이블을 많이 갱신하는 프로세스를 파악하거나,
- 누락된 처리나 중복 처리를 발견하고,
- 트랜잭션 단위 및 권한 설계에 도움을 받을 수 있습니다.

따라서 CRUD(Create, Read, Update, Delete) 연산을 기준으로 분석하는 이 기법은 “CRUD 분석”이며 ④번이 정답입니다.',
  'past:2024-1:Q59'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '일치 분석(Matching Analysis)',          0),
  (@q_id, 'B', 'CASE 분석(CASE Analysis)',               0),
  (@q_id, 'C', '연관성 분석(Relationship Analysis)',     0),
  (@q_id, 'D', 'CRUD 분석(CRUD Matrix Analysis)',        1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_META_CRUD');


/* =======================================================
 * 2024-1 Q60. DB 유형 구분 기준
 * topic_id = 13003
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13003,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '데이터베이스에는 관계형, 계층형, 네트워크형 등 다양한 종류가 있습니다. 이들 데이터베이스 유형을 서로 구분하는 주된 기준은 무엇인가요?',
  'B',
  '데이터베이스 모델은 “데이터를 어떻게 구조화해서 표현하느냐”에 따라 구분합니다.
- 계층형 DB: 트리 구조로 상·하위 개체를 표현
- 네트워크형 DB: 그래프 구조로 복잡한 개체 간 관계를 표현
- 관계형 DB: 테이블(릴레이션) 구조로 개체와 관계를 표현

즉, 개체와 그들 간의 관계를 어떤 데이터 구조(트리, 그래프, 테이블 등)로 표현하는지가 모델 구분의 핵심 기준입니다. 보기 중에서는 이를 가장 잘 나타내는 ② “개체를 표현하는 구조”가 정답입니다.',
  'past:2024-1:Q60'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '관계(Relationship)',             0),
  (@q_id, 'B', '개체(Object)를 표현하는 구조',   1),
  (@q_id, 'C', '제약 조건(Constraint)의 종류',   0),
  (@q_id, 'D', '속성(Attribute)의 데이터 타입',  0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');
