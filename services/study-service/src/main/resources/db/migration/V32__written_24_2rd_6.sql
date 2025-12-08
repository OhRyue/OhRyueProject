USE certpilot_study;

------------------------------------------------------------
-- Q51. UPDATE 문 SET 위치  [정답: B]
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
  '다음 SQL 문에서 괄호 안에 들어갈 키워드로 옳은 것은?

> ```sql
> UPDATE MEMBER ( ) GRADE = ''GOLD''
> WHERE POINT >= 1000;
> ```',
  'B',
  'UPDATE 문에서 컬럼 값 변경을 지정할 때는 반드시 SET 키워드를 사용합니다.
형식: UPDATE 테이블명 SET 컬럼 = 값 WHERE 조건;
따라서 괄호 안에는 SET이 들어가야 합니다.',
  'past:2024-2:Q51'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'FROM', 0),
  (@q_id, 'B', 'SET',  1),
  (@q_id, 'C', 'IN',   0),
  (@q_id, 'D', 'INTO', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q52. 분산 DB 투명성 - 장애 투명성  [정답: B]
-- topic_id = 13405 (분산 DB/투명성)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13405,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '분산 데이터베이스 목표 중 “특정 지역 시스템이나 네트워크에 장애가 발생해도 데이터 무결성이 보장되는 것”과 가장 관련 있는 것은?',
  'B',
  '특정 노드나 네트워크에 장애가 발생해도 전체 시스템이 논리적으로 정상 동작하고,
데이터의 일관성과 무결성이 유지되도록 하는 성질을 “장애 투명성(Failure Transparency)”라고 합니다.
병행 투명성은 동시 트랜잭션 간 충돌을 숨기는 성질, 위치·중복 투명성은 물리 배치와 복제를 감추는 성질입니다.',
  'past:2024-2:Q52'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '병행 투명성(Concurrency Transparency)',  0),
  (@q_id, 'B', '장애 투명성(Failure Transparency)',      1),
  (@q_id, 'C', '중복 투명성(Replication Transparency)',  0),
  (@q_id, 'D', '위치 투명성(Location Transparency)',     0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q53. 설계 단계와 작업 매핑  [정답: A]
-- topic_id = 13101 (DB 설계 과정)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13101,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '데이터베이스 설계 단계와 그 단계에서 수행되는 작업의 연결이 잘못된 것은?',
  'A',
  '요구 조건 분석 단계에서는 사용자의 정보 요구를 수집/정리하는 것이 핵심이며,
트랜잭션 모델링은 보통 개념/논리 설계 이후에 함께 수행됩니다.
나머지 보기: 물리적 설계에서 저장구조/인덱스/파티션 설계,
논리적 설계에서 논리 스키마 설계, 구현 단계에서 DDL로 스키마 생성은 올바른 연결입니다.
따라서 A가 잘못된 연결입니다.',
  'past:2024-2:Q53'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '요구 조건 분석 → 트랜잭션 모델링',                            1),
  (@q_id, 'B', '물리적 설계 단계 → DBMS 목표에 맞는 물리적 구조 설계',        0),
  (@q_id, 'C', '논리적 설계 단계 → DBMS 목표에 종속적인 논리 스키마 설계',    0),
  (@q_id, 'D', '구현 단계 → DBMS 목표로 스키마 작성 (DDL)',                  0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q54. UPDATE + WHERE (부서=인사, 코드<=3000)  [정답: D]
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
  'NORMAL',
  '다음 SQL 질의어를 문장으로 바르게 표현한 것은?  
(사원 테이블: 사원코드, 이름, 부서)

> "부서가 인사이고, 사원코드가 3000 이하인 사원의  
>  사원코드에 1000을 더하라."',
  'D',
  '조건은 “부서 = 인사” 이면서 동시에 “사원코드 ≤ 3000” 입니다.
따라서 WHERE 절은 AND 로 묶여야 하고, 사원코드 값 변경은 사원코드 = 사원코드 + 1000 형태여야 합니다.
이를 만족하는 문장은 D뿐입니다.',
  'past:2024-2:Q54'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'UPDATE 사원 SET 사원코드 +1000 WHERE 사원코드 = ''인사'' OR 사원코드 <= 3000;', 0),
  (@q_id, 'B', 'UPDATE 사원 SET 사원코드 = 사원코드 +1000 WHERE 부서 = ''인사'' OR 사원코드 <= 3000;', 0),
  (@q_id, 'C', 'UPDATE 사원 SET 사원코드 +1000 WHERE 사원코드 = ''인사'' AND 사원코드 <= 3000;', 0),
  (@q_id, 'D', 'UPDATE 사원 SET 사원코드 = 사원코드 +1000 WHERE 부서 = ''인사'' AND 사원코드 <= 3000;', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q55. 분산 DB 장점  [정답: B]
-- topic_id = 13405 (분산 DB/특징)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13405,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '분산 데이터베이스의 장점으로 거리가 가장 먼 것은?',
  'B',
  '분산 구조는 네트워크, 노드, 동기화 등 고려 요소가 늘어나기 때문에
오히려 잠재적 오류 요인이 증가할 수 있습니다.
반면 지역 자치성, 분산 제어, 유연한 확장(효용성과 융통성 향상)은 분산 DB의 대표적인 장점입니다.
따라서 “잠재적 오류가 감소한다”는 B가 부적절한 설명입니다.',
  'past:2024-2:Q55'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '지역 자치성이 높다.',   0),
  (@q_id, 'B', '잠재적 오류가 감소한다.', 1),
  (@q_id, 'C', '분산 제어가 가능하다.',   0),
  (@q_id, 'D', '효용성과 융통성이 높다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q56. 외래키 설명  [정답: D]
-- topic_id = 13301 (키/무결성)
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
  '다음 문장의 괄호 ( ) 안에 공통으로 들어갈 가장 적절한 용어는 무엇인가?

> "( )는 관계형 데이터 모델에서 한 릴레이션의 속성이  
>  참조되는 릴레이션의 기본키와 대응되어,  
>  릴레이션 간 참조 관계를 표현하는 중요한 도구이다.  
>  ( )를 포함하는 릴레이션이 참조하는 릴레이션이 되고,  
>  대응되는 기본키를 포함하는 릴레이션이 참조 릴레이션이 된다."',
  'D',
  '다른 릴레이션의 기본키를 참조하는 속성을 “외래키(Foreign Key)”라고 합니다.
외래키를 가진 릴레이션은 참조하는 릴레이션(Child/참조측),
기본키를 가진 쪽은 참조되는 릴레이션(Parent)이 됩니다.
설명 내용이 전형적인 외래키 정의입니다.',
  'past:2024-2:Q56'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '대체키(Alternate Key)', 0),
  (@q_id, 'B', '후보키(Candidate Key)',  0),
  (@q_id, 'C', '수퍼키(Super Key)',      0),
  (@q_id, 'D', '외래키(Foreign Key)',    1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q57. 트리거  [정답: B]
-- topic_id = 13202 (고급 SQL/트리거)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13202,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '트리거(Trigger)에 대한 설명으로 틀린 것은?',
  'B',
  '트리거는 특정 DML 이벤트(before/after INSERT, UPDATE, DELETE 등)에 반응하여 자동 실행되는 절차형 객체이며,
일반적으로 값을 반환하지 않습니다.
트리거 정의에 RETURN 값이 필수라는 규정은 없으므로, B가 틀린 설명입니다.',
  'past:2024-2:Q57'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '데이터 변경, 무결성 유지, 로그 메시지 출력 등의 목적으로 사용된다.',            0),
  (@q_id, 'B', '트리거 생성문에는 반드시 값을 반환하는 RETURN 명령이 포함되어야 한다.',        1),
  (@q_id, 'C', 'INSERT/UPDATE/DELETE 등의 이벤트 발생 시 관련 작업을 자동 수행하는 절차형 SQL이다.', 0),
  (@q_id, 'D', 'CREATE TRIGGER 명령어를 통해 생성된다.',                                      0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q58. DB 특성  [정답: D]
-- topic_id = 13101 (DB 개요/특성)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13101,
  'WRITTEN',
  'MCQ',
  'EASY',
  '데이터베이스의 특성에 해당하는 것들을 모두 고른 것은?

> ㉠ 계속적인 변화  
>  ㉡ 실시간 접근성  
>  ㉢ 내용에 의한 참조  
>  ㉣ 동시 공용',
  'D',
  '전통적으로 데이터베이스의 대표적 특성은
- 실시간 접근성(Real-time Accessibility)
- 계속적인 변화(Continuous Evolution)
- 내용에 의한 참조(Content Reference)
- 동시 공용(Concurrent Sharing)
이므로 ㉠~㉣ 모두 해당합니다. 따라서 정답은 D입니다.',
  'past:2024-2:Q58'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '㉠',                 0),
  (@q_id, 'B', '㉡, ㉢',             0),
  (@q_id, 'C', '㉠, ㉢, ㉣',         0),
  (@q_id, 'D', '㉠, ㉡, ㉢, ㉣',     1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q59. DBMS 기능 - 조작 기능  [정답: C]
-- topic_id = 13102 (DBMS 기능)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13102,
  'WRITTEN',
  'MCQ',
  'EASY',
  'DBMS의 필수 기능 중, 사용자와 데이터베이스 사이의 인터페이스 수단을 제공하는 기능은?',
  'C',
  'DBMS의 세 가지 기본 기능은
- 데이터 정의(DDL) 기능: 스키마/테이블 정의
- 데이터 조작(DML) 기능: SELECT/INSERT/UPDATE/DELETE
- 데이터 제어(DCL) 기능: 권한, 동시성, 무결성 제어
입니다. 사용자 질의/갱신 인터페이스 역할을 하는 것은 “조작 기능(Manipulation)”입니다.',
  'past:2024-2:Q59'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '정의 기능(Definition)', 0),
  (@q_id, 'B', '제어 기능(Control)',     0),
  (@q_id, 'C', '조작 기능(Manipulation)', 1),
  (@q_id, 'D', '전략 기능(Strategy)',     0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q60. View 특성  [정답: D]
-- topic_id = 13203 (뷰/서브쿼리)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13203,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '뷰(View)에 대한 설명으로 옳지 않은 것은?',
  'D',
  '뷰는 실제 데이터를 별도 공간에 복사해 저장하는 것이 아니라,
기본 테이블을 참조하는 논리적 가상 테이블입니다. 정의만 카탈로그에 저장될 뿐,
일반 테이블처럼 별도의 물리 저장공간을 갖지 않습니다.
따라서 “물리적으로 존재한다”라고 한 D가 틀린 설명입니다.',
  'past:2024-2:Q60'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '뷰는 CREATE문을 사용하여 정의한다.', 0),
  (@q_id, 'B', '뷰는 데이터의 논리적 독립성을 제공한다.', 0),
  (@q_id, 'C', '뷰를 제거할 때에는 DROP문을 사용한다.',  0),
  (@q_id, 'D', '뷰는 저장장치 내에 물리적으로 존재한다.',  1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');
