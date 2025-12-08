-- =========================================
-- 2024년 1회 – 데이터베이스 구축 (3과목)
-- Q41 ~ Q50
-- =========================================

------------------------------------------------------------
-- Q41. DROP 시 참조 무결성 유지 옵션
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13002,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '참조 무결성을 유지하기 위하여, 부모 테이블의 항목 값을 DELETE/DROP할 경우 자동으로 자식 테이블의 해당 레코드를 삭제하기 위한 옵션은 무엇인가요?',
  'A',
  '외래키 제약 조건에서 부모-자식 테이블이 연결되어 있을 때, 부모 쪽 레코드를 삭제하면 참조 무결성이 깨질 수 있습니다. 이때 “CASCADE” 옵션을 사용하면 부모 레코드 삭제 시 자동으로 자식 테이블의 관련 레코드도 함께 삭제되도록 설정할 수 있습니다.

  - CASCADE: 부모 삭제/수정 시, 자식 레코드도 연쇄적으로 삭제/수정
  - SET NULL: 부모 삭제 시, 자식의 외래키 값을 NULL로 변경
  - RESTRICT/NO ACTION: 자식에서 참조 중이면 부모 삭제를 허용하지 않음

  따라서 참조 무결성을 유지하면서 자식 레코드를 자동으로 삭제하는 옵션은 CASCADE입니다.',
  'past:2024-1:Q41'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'CASCADE',   1),
  (@q_id, 'B', 'CLUSTER',   0),
  (@q_id, 'C', 'RESTRICTED',0),
  (@q_id, 'D', 'SET NULL',  0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q42. View에 대한 설명 중 옳지 않은 것
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13002,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 뷰(View)에 대한 설명으로 옳지 않은 것은 무엇인가요?',
  'D',
  '뷰는 실제 데이터를 별도로 복사해 두는 “테이블”이 아니라, 기본 테이블에 대한 질의(SELECT)를 이름으로 저장해 둔 논리적 객체입니다. 즉, 보통은 정의(쿼리)만 저장되고, 실제 데이터는 기본 테이블에 존재합니다.

  - A: CREATE VIEW로 정의 → 맞는 설명입니다.
  - B: 물리 구조를 숨기고 논리적인 관점을 제공하므로 논리적 독립성에 기여합니다.
  - C: DROP VIEW로 뷰 객체를 제거합니다.
  - D: 뷰 자체가 별도의 물리적 데이터 파일을 항상 가지는 것은 아니므로 틀린 설명입니다.

  따라서 옳지 않은 설명은 ④입니다.',
  'past:2024-1:Q42'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '뷰는 CREATE문을 사용하여 정의한다.',          0),
  (@q_id, 'B', '뷰는 데이터의 논리적 독립성을 제공하는 데 도움을 준다.', 0),
  (@q_id, 'C', '뷰를 제거할 때에는 DROP문을 사용한다.',         0),
  (@q_id, 'D', '뷰는 저장장치 내에 항상 물리적으로 실제 데이터가 저장된다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q43. DML에 해당하는 SQL 명령어만 나열된 것은?
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13002,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 DML(Data Manipulation Language)에 해당하는 명령으로만 나열된 것은 무엇인가요?',
  'C',
  'DML은 “테이블에 저장된 데이터를 조작”하는 명령들을 의미합니다.
  대표적인 DML 명령은 다음과 같습니다.
  - SELECT: 데이터 조회
  - INSERT: 데이터 삽입
  - UPDATE: 데이터 수정
  - DELETE: 데이터 삭제

  반면 CREATE, ALTER, DROP은 테이블·뷰·인덱스 등의 구조를 정의·변경·삭제하는 DDL(Data Definition Language)에 속합니다. 따라서 DML로만 구성된 보기 C가 정답입니다.',
  'past:2024-1:Q43'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'DELETE, UPDATE, CREATE, ALTER', 0),
  (@q_id, 'B', 'INSERT, DELETE, UPDATE, DROP',  0),
  (@q_id, 'C', 'SELECT, INSERT, DELETE, UPDATE',1),
  (@q_id, 'D', 'SELECT, INSERT, DELETE, ALTER', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q44. 관계대수의 순수 관계 연산자가 아닌 것은?
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13001,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 관계대수(Relational Algebra)의 “순수 관계 연산자”에 해당하지 않는 것은 무엇인가요?',
  'A',
  '관계대수 연산은 크게 “일반 집합 연산자”와 “순수 관계 연산자”로 나눌 수 있습니다.
  - 일반 집합 연산: 합집합, 교집합, 차집합, 카티션 곱 등
  - 순수 관계 연산: 셀렉트(σ), 프로젝트(π), 조인(⋈), 디비전(÷) 등

  카티션 곱은 집합 이론에서 가져온 일반 연산이며, 셀렉트·프로젝트·조인·디비전 등이 관계대수에서 새롭게 도입된 순수 관계 연산자에 해당합니다. 따라서 순수 관계 연산자가 아닌 것은 카티션 곱입니다.',
  'past:2024-1:Q44'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '카티션 곱(Cartesian Product)', 1),
  (@q_id, 'B', '셀렉트(Select)',               0),
  (@q_id, 'C', '프로젝트(Project)',            0),
  (@q_id, 'D', '디비전(Division)',             0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q45. 기본키의 널 불허 성질 (개체 무결성)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13003,
  'WRITTEN',
  'MCQ',
  'EASY',
  '관계 데이터 모델의 무결성 제약 중, 기본키를 구성하는 속성 값이 NULL이 아닌 원자 값을 가져야 하는 성질은 무엇인가요?',
  'A',
  '개체 무결성(Entity Integrity)은 “각 릴레이션의 기본키는 NULL을 가질 수 없고, 한 튜플을 유일하게 식별할 수 있어야 한다”는 제약입니다. 즉, 기본키는 항상 값이 존재해야 하고, 그 값으로 행을 구분할 수 있어야 합니다.

  - 참조 무결성: 외래키가 가리키는 기본키가 실제로 존재해야 함
  - 도메인 무결성: 속성 값이 정의된 도메인 범위(자료형, 허용값)를 벗어나지 않도록 하는 제약

  문제에서 묻는 “기본키 속성이 NULL이 아닌 원자 값을 가져야 한다”는 내용은 개체 무결성에 해당합니다.',
  'past:2024-1:Q45'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '개체 무결성(Entity Integrity)',        1),
  (@q_id, 'B', '참조 무결성(Referential Integrity)',    0),
  (@q_id, 'C', '튜플의 유일성(Tuple Uniqueness)',      0),
  (@q_id, 'D', '도메인 무결성(Domain Integrity)',      0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q46. 3NF에서 BCNF로 정규화
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13003,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '제3정규형(3NF)에서 보이스-코드 정규형(BCNF)으로 더 정규화하기 위해 수행해야 하는 작업은 무엇인가요?',
  'D',
  '정규형 간의 관계를 정리하면 다음과 같습니다.
  - 제1정규형(1NF): 모든 속성이 원자 값이 되도록 도메인을 분해 → A에 해당
  - 제2정규형(2NF): 기본키의 “일부”에만 종속된 부분 함수 종속 제거 → B에 해당
  - 제3정규형(3NF): 기본키가 아닌 속성을 통해 간접적으로 종속되는 이행 함수 종속 제거 → C에 해당
  - BCNF: “모든 함수 종속에서 결정자(왼쪽)가 반드시 후보키가 되도록” 분해 → D에 해당

  따라서 3NF에서 BCNF까지 가려면, 아직 남아 있는 “결정자가 후보키가 아닌 함수 종속”을 제거하는 분해 작업이 필요합니다.',
  'past:2024-1:Q46'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '원자 값이 아닌 도메인을 분해한다.',             0),
  (@q_id, 'B', '부분 함수 종속을 제거한다.',                     0),
  (@q_id, 'C', '이행 함수 종속을 제거한다.',                     0),
  (@q_id, 'D', '결정자가 후보키가 아닌 함수 종속을 제거한다.',   1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q47. 로킹(Locking) 기법 중 틀린 설명
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13001,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '로킹(Locking) 기법에 대한 설명으로 틀린 것은 무엇인가요?',
  'B',
  '로킹 단위(granularity)가 작다는 것은 “더 작은 범위(예: 행 단위, 레코드 단위)”로 잠근다는 뜻입니다. 이렇게 하면 여러 트랜잭션이 서로 다른 행을 동시에 갱신할 수 있으므로 병행성 수준은 오히려 높아집니다. 대신 관리해야 할 로크 수가 많아져 오버헤드는 증가합니다.

  반대로 로킹 단위가 크면(테이블 단위, 데이터베이스 단위) 걸리는 로크 수는 줄어들어 관리 오버헤드는 감소하지만, 한 번 잠그면 다른 트랜잭션이 접근할 수 있는 범위도 크게 막혀 병행성이 떨어지게 됩니다.

  따라서 “로킹 단위가 작아지면 병행성 수준이 낮아진다”는 ②번 설명이 틀린 문장입니다.',
  'past:2024-1:Q47'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '로킹의 대상이 되는 객체의 크기를 로킹 단위라고 한다.',          0),
  (@q_id, 'B', '로킹 단위가 작아지면 병행성 수준이 낮아진다.',                    1),
  (@q_id, 'C', '데이터베이스 전체도 하나의 로킹 단위가 될 수 있다.',              0),
  (@q_id, 'D', '로킹 단위가 커지면 로크 수가 줄어들어 로킹 오버헤드가 감소한다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');

------------------------------------------------------------
-- Q48. UPDATE 문 빈칸
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13002,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 SQL 문에서 괄호 ( ) 안에 들어갈 키워드는 무엇인가요?

> UPDATE 회원 ( ) 전화번호 = ''010-14''
> WHERE 회원번호 = ''N4'';',
  'B',
  'UPDATE문의 기본 형식은 다음과 같습니다.

  UPDATE 테이블명
  SET 열1 = 값1, 열2 = 값2, ...
  WHERE 조건;

따라서 “회원” 테이블의 전화번호를 변경하려면
  UPDATE 회원 SET 전화번호 = ''010-14''
  WHERE 회원번호 = ''N4'';
와 같이 작성해야 합니다. 괄호 안에는 SET이 들어가야 하므로 정답은 ②번입니다.',
  'past:2024-1:Q48'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'FROM', 0),
  (@q_id, 'B', 'SET',  1),
  (@q_id, 'C', 'TO',   0),
  (@q_id, 'D', 'INTO', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');


------------------------------------------------------------
-- Q49. 데이터베이스 전체를 정의한 스키마
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13003,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음에서 설명하는 스키마는 무엇인가요?

> “데이터베이스 전체를 정의한 것으로, 데이터 개체, 관계, 제약조건,
>  접근 권한, 무결성 규칙 등을 명세한 것”',
  'B',
  '3단계 스키마 구조에서
- 외부 스키마: 사용자·응용 프로그램 관점의 개별 뷰(서브 스키마)
- 개념 스키마: 데이터베이스 전체를 통합적으로 정의한 논리적 스키마
- 내부 스키마: 저장 장치 관점에서 데이터가 실제로 어떻게 저장되는지 정의

문제에서 “데이터베이스 전체를 정의”하고 개체·관계·제약조건·접근 권한·무결성 규칙 등을 명세한다고 했으므로,
이는 개념 스키마에 대한 설명입니다.',
  'past:2024-1:Q49'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '내부 스키마(Internal Schema)',   0),
  (@q_id, 'B', '개념 스키마(Conceptual Schema)', 1),
  (@q_id, 'C', '내용 스키마(Content Schema)',    0),
  (@q_id, 'D', '외부 스키마(External Schema)',   0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q50. 시스템 카탈로그에 대한 설명 중 틀린 것
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13003,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '시스템 카탈로그(System Catalog)에 대한 설명으로 틀린 것은 무엇인가요?',
  'A',
  '시스템 카탈로그는 테이블, 뷰, 인덱스, 사용자, 권한 등 데이터베이스 객체에 대한 “정의 정보”를 저장하는 특수한 시스템 테이블들의 집합입니다. 이 정보들을 메타데이터라고 부르며, DBMS가 DDL 실행 결과를 기반으로 자동으로 생성·갱신합니다.

  무결성·일관성을 위해 시스템 카탈로그는 사용자가 직접 UPDATE/DELETE 등으로 수정하지 않고, DBMS가 DDL 처리 과정에서만 변경하도록 되어 있습니다. 따라서 “사용자가 SQL로 직접 갱신해야 한다”는 ①번 설명은 틀린 문장입니다.',
  'past:2024-1:Q50'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '시스템 카탈로그의 갱신은 무결성 유지를 위해 사용자가 SQL로 직접 갱신해야 한다.', 1),
  (@q_id, 'B', '데이터베이스에 포함되는 데이터 객체에 대한 정의나 명세 정보를 유지·관리한다.',    0),
  (@q_id, 'C', 'DBMS가 스스로 생성하고 유지하는 데이터베이스 내의 특별한 테이블 집합이다.',        0),
  (@q_id, 'D', '카탈로그에 저장된 정보를 메타데이터(Metadata)라고도 한다.',                         0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_META_CRUD');
