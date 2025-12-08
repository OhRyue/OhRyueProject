USE certpilot_study;

------------------------------------------------------------
-- Q41. 데이터 모델 구성요소  [정답: B]
-- topic_id = 13302 (데이터 모델)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13302,
  'WRITTEN',
  'MCQ',
  'EASY',
  '데이터 모델의 구성 요소가 아닌 것은?',
  'B',
  '데이터 모델은 보통 ① 데이터 구조(개체·속성·관계), ② 연산(Operation), ③ 제약조건(Constraint) 세 가지로 설명합니다.
개체(Entity), 관계(Relationship)는 구조의 일부이고, 연산(Operation)도 주요 구성요소입니다.
그러나 속성(Attribute)은 개체의 세부 구성요소일 뿐, “모델의 3대 구성요소”로 따로 분리해 부르지는 않으므로
보기 중에서 상대적으로 구성요소라고 보기 어려운 것은 B입니다.',
  'past:2024-2:Q41'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '연산(Operation)',       0),
  (@q_id, 'B', '속성(Attribute)',       1),
  (@q_id, 'C', '개체(Entity)',          0),
  (@q_id, 'D', '관계(Relationship)',    0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q42. 시스템 카탈로그  [정답: B]
-- topic_id = 13402 (시스템 카탈로그/메타데이터)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13402,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '시스템 카탈로그(System Catalog)에 대한 설명으로 옳지 않은 것은?',
  'B',
  '시스템 카탈로그는 DBMS가 관리하는 메타데이터(테이블 정의, 인덱스, 뷰, 권한 등)를 저장한 시스템 테이블입니다.
일반 사용자는 보통 “조회(SELECT)”는 가능하지만, 임의의 수정(INSERT/UPDATE/DELETE)은 할 수 없습니다.
내용 수정은 DBMS 내부 모듈만 수행합니다. 따라서 “검색하거나 수정할 수 있다”라고 한 B가 옳지 않은 설명입니다.',
  'past:2024-2:Q42'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '사용자 객체에 대한 정의나 명세에 관한 정보를 유지·관리하는 시스템 테이블이다.', 0),
  (@q_id, 'B', '일반 이용자도 SQL을 이용하여 내용을 검색하거나 수정할 수 있다.',               1),
  (@q_id, 'C', 'DBMS가 스스로 생성하고 유지한다.',                                                   0),
  (@q_id, 'D', '데이터 디렉터리, 번역기, 질의 최적화기 등과 함께 DBMS 내부에서 사용된다.',        0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_META_CRUD');

------------------------------------------------------------
-- Q43. 관계대수  [정답: A]
-- topic_id = 13301 (관계대수/관계해석)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13301,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '관계대수(Relational Algebra)에 대한 설명으로 옳지 않은 것은?',
  'A',
  '관계대수는 “어떤 순서로 어떤 연산을 적용할지”를 기술하는 절차적(Procedural) 언어입니다.
반대로 “무엇을” 구할지만 기술하는 비절차적 언어는 관계해석(Relational Calculus)입니다.
나머지 선택지 B·C·D는 관계대수의 특성을 올바르게 설명하고 있으므로,
“비절차적 언어”라고 한 A가 틀렸습니다.',
  'past:2024-2:Q43'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '원하는 릴레이션을 정의하는 방법을 제공하며 비절차적 언어이다.', 1),
  (@q_id, 'B', '릴레이션 조작을 위한 연산의 집합으로, 피연산자와 결과가 모두 릴레이션이다.', 0),
  (@q_id, 'C', '일반 집합 연산과 순수 관계 연산으로 구분된다.',                                 0),
  (@q_id, 'D', '질의에 대한 해를 구하기 위해 수행해야 할 연산의 순서를 명시한다.',             0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q44. SQL DML  [정답: B 로 수정]
-- topic_id = 13201 (SQL 기본)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13201,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 SQL DML(데이터 조작어)에 해당하는 것만 나열한 것은?

> ㉠ UPDATE   ㉡ SELECT   ㉢ GRANT   ㉣ INSERT',
  'B',
  'SQL에서 DML(Data Manipulation Language)은 테이블의 데이터를 조작하는 명령어로
SELECT, INSERT, UPDATE, DELETE 등이 포함됩니다.
여기서는 UPDATE(㉠), SELECT(㉡), INSERT(㉣)가 DML이고,
GRANT(㉢)는 권한 부여를 위한 DCL(Data Control Language)입니다.
따라서 DML만 모은 조합은 ㉠, ㉡, ㉣인 B입니다.',
  'past:2024-2:Q44'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '㉠, ㉡, ㉢',           0),
  (@q_id, 'B', '㉠, ㉡, ㉣',           1),
  (@q_id, 'C', '㉠, ㉢, ㉣',           0),
  (@q_id, 'D', '㉠, ㉡, ㉢, ㉣',       0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q45. 무결성 제약 – 개체 무결성  [정답: A]
-- topic_id = 13301 (무결성 제약)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13301,
  'WRITTEN',
  'MCQ',
  'EASY',
  '관계 데이터 모델의 무결성 제약 중, 기본키 속성이 NULL 값이 아닌 원자 값을 가져야 하는 성질은 무엇인가?',
  'A',
  '기본키는 각 튜플을 유일하게 식별해야 하므로 NULL을 가질 수 없고, 원자값이어야 합니다.
이는 “개체 무결성(Entity Integrity)” 제약의 핵심 내용입니다.
참조 무결성은 외래키가 참조 대상 기본키 값만 가지도록 강제하는 규칙이고,
도메인 무결성은 속성이 정의된 도메인 범위를 벗어나지 않도록 하는 제약입니다.',
  'past:2024-2:Q45'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '개체 무결성(Entity Integrity)',                 1),
  (@q_id, 'B', '참조 무결성(Referential Integrity)',           0),
  (@q_id, 'C', '튜플의 유일성(Tuple Uniqueness)',              0),
  (@q_id, 'D', '도메인 무결성(Domain Integrity)',              0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q46. 참조 무결성 / 키 제약  [정답: D]
-- topic_id = 13301 (무결성/키 제약)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13301,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '릴레이션 R1의 기본키를 참조하는 R2의 외래키 조합을 변경하려면,
R2 릴레이션의 외래키 값 제약을 함께 고려해야 한다. 이러한 제약을 무엇이라고 하는가?',
  'D',
  '두 릴레이션이 기본키–외래키 관계로 연결되어 있을 때,
부모 릴레이션의 키 변경/삭제가 자식 릴레이션의 외래키 제약에 영향을 줍니다.
이런 제약을 넓게는 “키 제약(Key Constraint)”이라고 부르며,
특히 부모–자식 간 관계를 보장하는 규칙은 “참조 무결성 제약(Referential Integrity Constraint)”으로 구분합니다.
보기 중 가장 알맞은 것은 D입니다.',
  'past:2024-2:Q46'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '고유 무결성(Unique Integrity)',                                 0),
  (@q_id, 'B', '정보 무결성(Information Integrity)',                            0),
  (@q_id, 'C', '도메인 제약성(Domain Constraint)',                              0),
  (@q_id, 'D', '키 제약성(Key Constraint, 참조 무결성 포함)',                    1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q47. 트리 차수  [정답: B]
-- topic_id = 13401 (인덱스/트리)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source, image_url
) VALUES (
  1,
  13401,
  'WRITTEN',
  'MCQ',
  'EASY',
  '아래와 같이 구성된 트리의 차수(degree)를 구하면 얼마인가?
(가장 많은 자식을 가진 노드의 자식 수를 기준으로 한다고 가정)',
  'B',
  '트리의 차수(degree)는 “한 노드가 가질 수 있는 최대 자식 노드 수”입니다.
문제의 그림(원문 기준)에서 가장 많은 자식을 가진 노드의 자식 수가 3개이므로, 트리의 차수는 3입니다.',
  'past:2024-2:Q47',
  'https://api.mycertpilot.com/static/images/questions/q_2024_02_47.png'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '2', 0),
  (@q_id, 'B', '3', 1),
  (@q_id, 'C', '4', 0),
  (@q_id, 'D', '5', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');

------------------------------------------------------------
-- Q48. 1NF → 2NF 조건  [정답: D]
-- topic_id = 13302 (정규화)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13302,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '정규화 과정 중 1NF에서 2NF가 되기 위한 조건으로 옳은 것은?',
  'D',
  '2정규형(2NF)의 핵심 조건은 “부분 함수 종속 제거”입니다.
즉, 기본키가 복합키인 경우, 키가 아닌 모든 속성이 기본키 전체에 완전 함수 종속(Partial Dependency가 없음)이어야 합니다.
이를 가장 잘 표현한 선택지는 D입니다. 이행적 종속 제거는 3NF 단계에서 다루는 조건입니다.',
  'past:2024-2:Q48'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '1NF를 만족하고 모든 도메인이 원자 값을 가져야 한다.',                    0),
  (@q_id, 'B', '1NF를 만족하고 키가 아닌 속성이 기본키에 이행적으로 함수 종속되지 않아야 한다.', 0),
  (@q_id, 'C', '1NF를 만족하고 다치 종속(Multi-valued Dependency)이 제거되어야 한다.',       0),
  (@q_id, 'D', '1NF를 만족하고 키가 아닌 모든 속성이 기본키에 완전 함수 종속이어야 한다.',     1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q49. 로킹 단위  [정답: D]
-- topic_id = 13404 (병행제어/트랜잭션)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13404,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '병행제어 기법 중 로킹(Locking)에 대한 설명으로 옳지 않은 것은?',
  'D',
  '로킹 단위가 커질수록 한 번에 잠그는 범위가 넓어져 여러 트랜잭션이 동시에 접근하기 어려워지므로
공유도는 오히려 감소합니다. 반대로 로킹 단위를 작게 하면 공유도는 증가하지만,
잠금/해제 관리가 더 복잡해져 오버헤드가 커집니다.
따라서 “로킹 단위가 커지면 공유도가 증가한다”라고 한 D가 틀린 설명입니다.',
  'past:2024-2:Q49'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '로킹의 대상이 되는 객체의 크기를 로킹 단위라고 한다.', 0),
  (@q_id, 'B', '데이터베이스, 파일, 레코드 등은 로킹 단위가 될 수 있다.', 0),
  (@q_id, 'C', '로킹 단위가 작아지면 로킹 오버헤드가 증가한다.',         0),
  (@q_id, 'D', '로킹 단위가 커지면 데이터베이스 공유도가 증가한다.',       1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q50. 파티션  [정답: A]
-- topic_id = 13402 (저장구조/파티션)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13402,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '데이터베이스 파티션(Partition)에 대한 설명으로 틀린 것은?',
  'A',
  '파티셔닝은 큰 테이블을 여러 파티션으로 나누어 I/O를 분산시키고,
특정 파티션만 대상으로 작업할 수 있게 하므로 쿼리 성능뿐 아니라
백업·복구 작업도 더 유연하고 빠르게 할 수 있는 경우가 많습니다.
“성능은 향상되지만 백업·복구 속도는 느려진다”라는 A는 일반적인 효과와 반대되는 설명이므로 틀린 보기입니다.',
  'past:2024-2:Q50'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '파티셔닝으로 인해 쿼리 성능은 향상되지만 백업 및 복구 속도는 느려진다.', 1),
  (@q_id, 'B', '파티셔닝된 테이블은 물리적으로 별도의 세그먼트에 저장될 수 있다.',       0),
  (@q_id, 'C', '파티션은 하나의 테이블을 더 작은 논리적 단위로 나눈 것이다.',             0),
  (@q_id, 'D', '파티셔닝을 수행하면 일부 파티션만 백업/복구할 수 있어 데이터 가용성을 높일 수 있다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');
