USE certpilot_study;

------------------------------------------------------------
-- Q50. 시그마(σ) – Select 연산 (관계대수)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13301, -- 3.3.1 관계 데이터베이스 모델
  'WRITTEN',
  'MCQ',
  'EASY',
  '조건을 만족하는 튜플들로 구성된 릴레이션의 수평적 부분집합을 반환하며, 연산자 기호로 그리스 문자 시그마(σ)를 사용하는 관계대수 연산은 무엇인가?',
  'B',
  '관계대수에서 시그마(σ)를 사용하는 연산은 선택(Select) 연산입니다.
- σ 조건 (R) 형태로 쓰며, 릴레이션 R에서 주어진 조건을 만족하는 튜플(행)만 뽑아 “수평 부분집합”을 만듭니다.
- 반대로 π(파이)는 Project 연산으로, 특정 속성(열)만 선택해 “수직 부분집합”을 만드는 연산입니다.
따라서 “조건을 만족하는 수평적 부분집합 + σ 기호”에 해당하는 연산은 Select입니다.',
  'past:2024-3:Q50'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Project(프로젝션) 연산', 0),
  (@q_id, 'B', 'Select(선택) 연산',      1),
  (@q_id, 'C', 'Division(나눗셈) 연산',  0),
  (@q_id, 'D', 'Join(조인) 연산',        0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q51. DB 설계 단계 – 물리적 설계
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13402, -- 3.4.2 DB 물리 속성 설계
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '데이터베이스 설계 단계 중 “저장 레코드 양식 설계, 레코드 집중의 분석 및 설계, 접근 경로 설계”와 직접적으로 관련된 설계 단계는 무엇인가?',
  'D',
  '데이터베이스 설계는 보통
1) 요구 조건 분석 → 2) 개념적 설계(개체·관계) → 3) 논리적 설계(릴레이션 구조) → 4) 물리적 설계(저장 구조, 인덱스, 접근 경로) 순서로 진행됩니다.
문제에서 언급한 “저장 레코드 양식, 레코드 집중, 접근 경로 설계”는 모두 실제 저장 구조·파일·인덱스를 다루는 물리적 설계 단계의 작업입니다.
따라서 정답은 물리적 설계입니다.',
  'past:2024-3:Q51'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '요구 조건 분석', 0),
  (@q_id, 'B', '개념적 설계',     0),
  (@q_id, 'C', '논리적 설계',     0),
  (@q_id, 'D', '물리적 설계',     1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q52. 릴레이션의 성질 – 속성 사이에는 "논리적" 순서 없음
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13301, -- 3.3.1 관계 데이터베이스 모델
  'WRITTEN',
  'MCQ',
  'EASY',
  '관계형 데이터 모델에서 릴레이션에 대한 설명으로 틀린 것은?',
  'D',
  '관계형 모델에서 릴레이션(테이블)은 다음과 같은 성질을 가집니다.
- 모든 속성 값은 더 이상 분해할 수 없는 원자값(Atomic Value)이다.
- 한 릴레이션에 포함된 튜플(행)은 서로 상이하며, 중복 튜플이 없다.
- 튜플 사이에는 논리적인 순서가 없다(물리적으로는 정렬될 수 있어도 의미상 순서 없음).
- 속성(열)들 사이에도 의미상의 순서는 없으며, 이름에 의해 구분된다.
따라서 “한 릴레이션을 구성하는 속성 사이에는 순서가 존재한다”는 설명은 관계형 모델의 기본 가정과 맞지 않아 틀린 설명입니다.',
  'past:2024-3:Q52'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '모든 속성 값은 원자 값을 갖는다.',            0),
  (@q_id, 'B', '한 릴레이션에 포함된 튜플은 모두 상이하다.',  0),
  (@q_id, 'C', '한 릴레이션에 포함된 튜플 사이에는 순서가 없다.', 0),
  (@q_id, 'D', '한 릴레이션을 구성하는 속성 사이에는 순서가 존재한다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q53. 트랜잭션 실패 시 결과 복구 – ROLLBACK
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13201, -- 3.2.1 기본 SQL 작성 (트랜잭션 포함)
  'WRITTEN',
  'MCQ',
  'EASY',
  '트랜잭션 실행이 실패하였음을 알리고, 트랜잭션이 수행한 결과를 모두 취소하여 데이터베이스를 원래 상태로 되돌리는 연산은 무엇인가?',
  'D',
  '트랜잭션 제어어(TCL) 중에서
- COMMIT: 지금까지의 변경 내용을 영구적으로 반영
- ROLLBACK: 트랜잭션 단위로 수행된 변경 내용을 모두 취소하고, 트랜잭션 시작 이전 또는 SAVEPOINT 지점으로 되돌림
따라서 “실패 시 원상 복귀”를 수행하는 연산은 ROLLBACK입니다. BACKUP, LOG는 물리/논리 백업 혹은 기록에 관련된 개념일 뿐, 트랜잭션 즉시 복구 연산 그 자체를 의미하지 않습니다.',
  'past:2024-3:Q53'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'COMMIT 연산',   0),
  (@q_id, 'B', 'BACKUP 연산',   0),
  (@q_id, 'C', 'LOG 연산',      0),
  (@q_id, 'D', 'ROLLBACK 연산', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q54. 카티션 프로덕트 – 차수/카디널리티 계산
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13301, -- 3.3.1 관계 데이터베이스 모델
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '릴레이션 R의 차수가 4, 카디널리티가 5이고, 릴레이션 S의 차수가 6, 카디널리티가 7일 때, 두 릴레이션의 카티션 프로덕트 R×S의 차수와 카디널리티는 각각 얼마인가?',
  'C',
  '카티션 프로덕트 R×S의 구조는 다음과 같습니다.
- 차수(속성 수): R의 차수 + S의 차수 = 4 + 6 = 10
- 카디널리티(튜플 수): R의 튜플 수 × S의 튜플 수 = 5 × 7 = 35
따라서 새로운 릴레이션의 차수는 10, 카디널리티는 35입니다.',
  'past:2024-3:Q54'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '차수 24, 카디널리티 12', 0),
  (@q_id, 'B', '차수 24, 카디널리티 35', 0),
  (@q_id, 'C', '차수 10, 카디널리티 35', 1),
  (@q_id, 'D', '차수 10, 카디널리티 12', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q55. 정규화의 시점 – 개념적 설계 "이후"
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13302, -- 3.3.2 데이터 모델링 및 설계
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '정규화(Normalization)에 대한 설명으로 가장 적절하지 않은 것은?',
  'A',
  '정규화는 릴레이션 설계 시 중복을 제거하고 이상(Anomaly)을 방지하기 위해 수행하는 과정입니다.
일반적인 특징은 다음과 같습니다.
- 데이터 구조의 안정성과 일관성을 높인다.
- 삽입/삭제/갱신 이상을 줄이기 위해 중복을 최소화한다.
- 데이터 삽입·갱신 시 릴레이션 재구성 필요성을 낮춘다.
정규화는 보통 개념적 설계에서 도출한 개체-관계 구조를 기반으로 논리적 설계 단계(릴레이션 설계)에서 수행합니다. 따라서 “개념적 설계 단계 이전에 수행한다”는 설명은 시점상 맞지 않아 부적절한 설명입니다.',
  'past:2024-3:Q55'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '데이터베이스의 개념적 설계 단계 이전에 수행한다.', 1),
  (@q_id, 'B', '데이터 구조의 안정성을 최대화한다.',               0),
  (@q_id, 'C', '중복을 배제하여 삽입·삭제·갱신 이상의 발생을 방지한다.', 0),
  (@q_id, 'D', '데이터 삽입 시 릴레이션 재구성 필요성을 줄인다.',      0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q56. 트랜잭션의 정의
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13201, -- 3.2.1 기본 SQL 작성 / 트랜잭션
  'WRITTEN',
  'MCQ',
  'EASY',
  '데이터베이스에서 하나의 논리적 기능을 수행하기 위한 작업의 단위 또는 한꺼번에 모두 수행되어야 할 일련의 연산들을 의미하는 것은 무엇인가?',
  'A',
  '트랜잭션(Transaction)은 데이터베이스의 논리적 일관성을 보장하기 위한 작업 단위입니다.
- 여러 SQL 연산(INSERT/UPDATE/DELETE 등)을 하나의 묶음으로 봅니다.
- 전부 성공하면 COMMIT, 중간에 오류가 나면 ROLLBACK으로 모두 취소하여 원자성(Atomicity)을 보장합니다.
뷰(View), 카디널리티, 튜플은 각각 논리적 테이블, 튜플 수, 행(레코드) 개념에 해당하며 “작업 단위”를 의미하지는 않습니다.',
  'past:2024-3:Q56'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '트랜잭션(Transaction)', 1),
  (@q_id, 'B', '뷰(View)',             0),
  (@q_id, 'C', '카디널리티(Cardinality)', 0),
  (@q_id, 'D', '튜플(Tuple)',          0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q57. DROP VIEW ... CASCADE – 의존 뷰까지 삭제
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13202, -- 3.2.2 고급 SQL 작성 (VIEW)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '테이블 두 개를 조인해 정의한 뷰 V_1을 이용하여 또 다른 뷰 V_2를 정의하였다.
이 상태에서 다음 명령을 수행하면 결과로 옳은 것은?

> DROP VIEW V_1 CASCADE;',
  'C',
  '뷰 V_2는 뷰 V_1을 기반으로 정의되었으므로, V_1이 삭제되면 V_2는 의존 객체(Dependent Object)가 됩니다.
CASCADE 옵션은 “해당 객체뿐 아니라 그 객체에 의존하는 뷰 등도 함께 삭제하라”는 의미입니다.
따라서 V_1과, V_1에 의존하는 V_2가 모두 삭제됩니다.',
  'past:2024-3:Q57'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '뷰 V_1만 삭제된다.',              0),
  (@q_id, 'B', '뷰 V_2만 삭제된다.',              0),
  (@q_id, 'C', '뷰 V_1과 V_2가 모두 삭제된다.',   1),
  (@q_id, 'D', '뷰 V_1과 V_2 모두 삭제되지 않는다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q58. SQL – REVOKE는 권한 회수, 열 이름 변경 아님
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13201, -- 3.2.1 기본 SQL 작성
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 SQL에 관한 설명으로 틀린 것은?',
  'A',
  'SQL의 주요 특징을 정리하면 다음과 같습니다.
- DDL: CREATE, ALTER, DROP 등으로 테이블/뷰/인덱스를 생성·변경·삭제
- DML: SELECT, INSERT, UPDATE, DELETE로 데이터 조회·조작
- DISTINCT: SELECT 결과에서 중복 행을 제거
- JOIN: 여러 테이블의 레코드를 조합해 하나의 결과로 표현
REVOKE는 GRANT로 부여한 권한을 “회수”할 때 사용하는 명령어이며, 열 이름을 다시 부여하는 기능은 AS(별칭)나 RENAME 등으로 수행합니다. 따라서 REVOKE를 열 이름 부여와 연결한 설명은 틀린 설명입니다.',
  'past:2024-3:Q58'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'REVOKE 키워드를 사용하여 열 이름을 다시 부여할 수 있다.', 1),
  (@q_id, 'B', '데이터 정의어는 기본 테이블·뷰·인덱스 등을 생성·변경·제거하는 명령어이다.', 0),
  (@q_id, 'C', 'DISTINCT를 사용하여 중복 값을 제거할 수 있다.', 0),
  (@q_id, 'D', 'JOIN을 통해 여러 테이블의 레코드를 조합하여 표현할 수 있다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q59. 외래키(Foreign Key)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13403, -- 3.4.3 데이터베이스 무결성과 키
  'WRITTEN',
  'MCQ',
  'EASY',
  '다른 릴레이션의 기본키를 참조하는 키를 무엇이라 하는가?',
  'C',
  '관계 데이터베이스에서 키의 종류는 보통 다음과 같습니다.
- 슈퍼키(Super Key): 튜플을 유일하게 식별할 수 있는 속성(집합)
- 후보키(Candidate Key): 최소성을 만족하는 슈퍼키
- 기본키(Primary Key): 후보키 중 선택된 대표 키
- 외래키(Foreign Key): 다른 릴레이션의 기본키를 참조하는 키
따라서 “다른 릴레이션의 기본키를 참조하는 키”는 외래키(Foreign Key)입니다.',
  'past:2024-3:Q59'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '슈퍼키(Super Key)', 0),
  (@q_id, 'B', '필드키(Field Key)', 0),
  (@q_id, 'C', '외래키(Foreign Key)', 1),
  (@q_id, 'D', '후보키(Candidate Key)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q60. 데이터 사전(Data Dictionary) – 일반 사용자가 직접 수정 X
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13302, -- 3.3.2 데이터 모델링 및 설계 (메타데이터)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '데이터 사전(Data Dictionary)에 대한 설명으로 틀린 것은?',
  'B',
  '데이터 사전은 “데이터베이스에 대한 데이터(메타데이터)”를 저장하는 특수한 데이터베이스입니다.
- 시스템 카탈로그(system catalog), 시스템 데이터베이스라고도 부르며, 테이블·뷰·인덱스·사용자·권한 등 구조 정보를 저장합니다.
- 실제 저장 위치나 접근 경로 등은 별도의 데이터 디렉터리(Data Directory)에서 관리합니다.
데이터 사전은 DBMS가 내부적으로 관리하는 영역이므로, 일반 사용자가 임의로 생성·수정·삭제하는 것은 허용되지 않습니다.
따라서 “일반 사용자가 데이터 사전을 생성·유지·수정할 수 있다”는 설명은 틀린 설명입니다.',
  'past:2024-3:Q60'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '시스템 카탈로그 또는 시스템 데이터베이스라고도 한다.', 0),
  (@q_id, 'B', '일반 사용자가 데이터 사전을 생성·유지·수정할 수 있다.',   1),
  (@q_id, 'C', '데이터베이스에 대한 데이터(메타데이터)를 저장한다.',        0),
  (@q_id, 'D', '데이터 사전의 실제 위치 정보는 데이터 디렉터리에서 관리한다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_META_CRUD');
