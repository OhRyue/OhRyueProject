USE certpilot_study;

/* =======================================================
 * Q41. ON DELETE CASCADE (참조 무결성)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13403, -- 3.4.3 데이터베이스 무결성과 키
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'DROP 문에서 부모 테이블의 항목 값을 삭제할 경우, 참조 무결성을 유지하기 위해 자식 테이블의 해당 레코드를 자동으로 삭제하도록 설정하는 옵션은 무엇인가?',
  'B',
  '관계 데이터베이스에서 외래키(FK)는 기본키(PK)를 참조합니다.
부모 테이블의 행(기본키 값)을 삭제할 때, 이를 참조하는 자식 레코드가 남아 있으면 참조 무결성이 깨집니다.
이때 ON DELETE CASCADE 옵션을 사용하면 부모 행이 삭제될 때 해당 키를 참조하던 자식 행도 자동으로 함께 삭제되어 참조 무결성이 유지됩니다.
CLUSTER, RESTRICTED, SET NULL 등은 각각 인덱스/저장 구조나 다른 제약 방식에 관련되지만, “부모 삭제 시 자식 자동 삭제”라는 의미를 갖는 것은 CASCADE입니다.',
  'past:2024-3:Q41'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'CASCADE, CLUSTER',   0),
  (@q_id, 'B', 'CASCADE',            1),
  (@q_id, 'C', 'RESTRICTED',         0),
  (@q_id, 'D', 'SET-NULL',           0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');


/* =======================================================
 * Q42. VIEW 특징 – 물리 저장 X
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13202, -- 3.2.2 고급 SQL 작성
  'WRITTEN',
  'MCQ',
  'EASY',
  '뷰(View)에 대한 설명으로 옳지 않은 것은?',
  'D',
  '뷰는 CREATE VIEW 문으로 정의하는 논리적 가상 테이블입니다.
- 하나 이상의 기본 테이블을 대상으로 하는 SELECT 문을 저장해 두고, 이를 테이블처럼 조회할 수 있게 해줍니다.
- 뷰를 사용하면 특정 컬럼만 노출하거나 조건을 고정해서 데이터의 논리적 독립성과 보안을 높일 수 있습니다.
- 필요 없어진 뷰는 DROP VIEW 문으로 제거합니다.
하지만 뷰 자체는 보통 SELECT 정의만 메타데이터에 저장해 두는 “논리적 객체”일 뿐, 일반 테이블처럼 모든 데이터를 물리적으로 중복 저장하지 않습니다. 따라서 “뷰는 저장장치 내에 물리적으로 존재한다”는 설명은 옳지 않습니다.',
  'past:2024-3:Q42'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '뷰는 CREATE 문을 사용하여 정의한다.', 0),
  (@q_id, 'B', '뷰는 데이터의 논리적 독립성을 제공한다.', 0),
  (@q_id, 'C', '뷰를 제거할 때에는 DROP 문을 사용한다.', 0),
  (@q_id, 'D', '뷰는 저장장치 내에 물리적으로 존재한다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');


/* =======================================================
 * Q43. DML 명령 집합
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13201, -- 3.2.1 기본 SQL 작성
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 DML(Data Manipulation Language)에 해당하는 명령으로만 나열된 것은?',
  'C',
  'DML은 테이블에 저장된 데이터를 “조작(조회/삽입/수정/삭제)”하는 명령들의 집합입니다.
- SELECT : 데이터 조회
- INSERT : 행 삽입
- UPDATE : 행 수정
- DELETE : 행 삭제
반면 CREATE, ALTER, DROP 등은 테이블/뷰/인덱스 등의 구조를 정의·변경·삭제하는 DDL(Data Definition Language)에 해당합니다.
따라서 SELECT, INSERT, DELETE, UPDATE로만 구성된 선택지가 DML 집합에 해당합니다.',
  'past:2024-3:Q43'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'DELETE, UPDATE, CREATE, ALTER', 0),
  (@q_id, 'B', 'INSERT, DELETE, UPDATE, DROP',  0),
  (@q_id, 'C', 'SELECT, INSERT, DELETE, UPDATE', 1),
  (@q_id, 'D', 'SELECT, INSERT, DELETE, ALTER', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');


/* =======================================================
 * Q44. 반복 속성 분해 → 제1정규형
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source, image_url
) VALUES (
  1,
  13302, -- 3.3.2 데이터 모델링 및 설계
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음과 같이 위쪽 릴레이션을 아래쪽 릴레이션으로 변환한 정규화 작업은 어느 단계에 해당하는가?',
  'A',
  '위쪽 릴레이션은 한 튜플(한 행) 안에 “도시”가 복수 값(서울, 부산 등)으로 저장된 상태입니다. 즉, 하나의 속성이 반복 그룹(Repeating Group)을 가지는 비정규형(UNF) 구조입니다.
반복 속성을 제거하고, 각 도시를 별도의 행으로 분리하여 모든 속성이 더 이상 분해 불가능한 원자값(Atomic Value)만 갖도록 만든 것이 제1정규형(1NF)으로의 변환입니다.
따라서 이 작업은 비정규형 → 제1정규형으로의 정규화에 해당합니다.',
  'past:2024-3:Q44',
  'https://api.mycertpilot.com/static/images/questions/q_2024_03_44.png'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '비정규형에서 제1정규형으로의 정규화', 1),
  (@q_id, 'B', '제1정규형에서 제2정규형으로의 정규화', 0),
  (@q_id, 'C', '제2정규형에서 제3정규형으로의 정규화', 0),
  (@q_id, 'D', '제3정규형에서 제4정규형으로의 정규화', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');


/* =======================================================
 * Q45. 기본키의 NOT NULL 성질 → 개체 무결성
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13403, -- 3.4.3 데이터베이스 무결성과 키
  'WRITTEN',
  'MCQ',
  'EASY',
  '관계 데이터 모델의 무결성 제약 중, 기본키(PK)를 구성하는 속성 값이 NULL이 아닌 원자 값을 가져야 하는 성질은 무엇이라 하는가?',
  'A',
  '관계 데이터 모델에서 기본키는 각 튜플(행)을 유일하게 식별해야 합니다.
- 기본키는 NULL을 허용하지 않아야 하며, 각 값은 원자적이고 중복되지 않아야 합니다.
이와 같이 “기본키 속성은 NULL이 될 수 없다”는 제약을 개체 무결성(Entity Integrity)이라고 합니다.
참조 무결성은 외래키가 존재할 경우, 그 값이 참조하는 기본키 값 또는 NULL이어야 한다는 제약을 의미합니다.',
  'past:2024-3:Q45'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '개체 무결성(Entity Integrity)', 1),
  (@q_id, 'B', '참조 무결성(Referential Integrity)', 0),
  (@q_id, 'C', '튜플의 유일성(Tuple Uniqueness)', 0),
  (@q_id, 'D', '도메인 무결성(Domain Integrity)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');


/* =======================================================
 * Q46. 데이터 모델 요소 – 출력 구조는 아님
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13301, -- 3.3.1 관계 데이터베이스 모델
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '데이터 모델에 의해 표현해야 할 요소로서 거리가 가장 먼 것은?',
  'B',
  '일반적으로 데이터 모델은 세 가지 요소를 표현합니다.
1) 구조(Structure): 개체, 속성, 관계 등 데이터가 어떻게 구성되는지
2) 제약조건(Constraint): 무결성, 비즈니스 규칙 등 허용/금지 조건
3) 연산(Operation): 데이터를 검색·삽입·수정·삭제하는 연산 집합
반면 “출력 구조”는 화면 설계나 보고서 레이아웃에 더 가까운 개념으로, 데이터 모델의 기본 요소로 보지는 않습니다. 그러므로 출력 구조를 중심으로 한 선택지가 데이터 모델의 필수 요소와는 거리가 있습니다.',
  'past:2024-3:Q46'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '논리적 데이터 구조', 0),
  (@q_id, 'B', '출력 구조',          1),
  (@q_id, 'C', '제약 조건',          0),
  (@q_id, 'D', '연산',               0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');


/* =======================================================
 * Q47. GRANT UPDATE ON STUDENT TO PARK;
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13102, -- 3.1.2 응용 SQL 작성
  'WRITTEN',
  'MCQ',
  'EASY',
  'DBA가 사용자 PARK에게 테이블 STUDENT의 데이터를 갱신할 수 있는 시스템 권한을 부여하려 한다. 다음 SQL 문의 빈칸 ㉠, ㉡에 알맞은 것은?

> GRANT ㉠ ㉡ STUDENT TO PARK;',
  'C',
  '특정 테이블에 대한 UPDATE 권한을 부여할 때는 다음과 같이 작성합니다.

GRANT UPDATE ON STUDENT TO PARK;

- GRANT : 권한 부여 명령
- UPDATE : 부여할 권한(갱신)
- ON : 대상 객체 지정
- TO : 권한을 부여받는 사용자 지정

따라서 ㉠에는 UPDATE, ㉡에는 ON이 들어갑니다.',
  'past:2024-3:Q47'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'INSERT, INTO', 0),
  (@q_id, 'B', 'ALTER, TO',   0),
  (@q_id, 'C', 'UPDATE, ON',  1),
  (@q_id, 'D', 'REPLACE, IN', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');


/* =======================================================
 * Q48. CRUD 매트릭스 분석
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13302, -- 3.3.2 데이터 모델링 및 설계 (트랜잭션/CRUD 분석)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '데이터베이스에 영향을 주는 생성(Create), 읽기(Read), 갱신(Update), 삭제(Delete) 연산에 대해, 프로세스와 테이블 간의 매트릭스를 작성하여 트랜잭션을 분석하는 기법은 무엇인가?',
  'C',
  'CRUD 분석은 각 프로세스(또는 기능)가 어떤 테이블에 대해 Create/Read/Update/Delete 중 어떤 연산을 수행하는지 매트릭스 형태로 나타내는 기법입니다.
- 행: 프로세스(업무 기능)
- 열: 테이블(데이터 엔티티)
- 셀: C/R/U/D 중 해당되는 연산 표시
이를 통해 트랜잭션 부하, 데이터 접근 패턴, 불필요한 연산 등을 파악할 수 있습니다.
따라서 “생성·읽기·갱신·삭제 연산을 기준으로 프로세스와 테이블 간 매트릭스를 만들어 분석하는 기법”은 CRUD 분석입니다.',
  'past:2024-3:Q48'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '일치 분석', 0),
  (@q_id, 'B', '연관성 분석', 0),
  (@q_id, 'C', 'CRUD 분석', 1),
  (@q_id, 'D', 'CASE 분석', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_META_CRUD');


/* =======================================================
 * Q49. 개념 스키마(Conceptual Schema)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13301, -- 3.3.1 관계 데이터베이스 모델
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음에서 설명하는 스키마는 무엇인가?

> “데이터베이스 전체를 정의한 것으로, 데이터 개체, 관계, 제약조건, 접근 권한, 무결성 규칙 등을 명세한 것”',
  'A',
  '3계층 스키마 구조에서
- 외부 스키마(External Schema): 사용자/응용 프로그램 관점의 개별 뷰
- 개념 스키마(Conceptual Schema): 데이터베이스 전체를 통합적으로 표현한 논리 구조
- 내부 스키마(Internal Schema): 실제 저장 구조와 접근 경로를 정의하는 물리 구조
문제에서 “데이터베이스 전체를 정의하고, 데이터 개체·관계·제약조건·접근 권한·무결성 규칙 등을 명세한 것”이라고 설명하고 있으므로, 이는 개념 스키마에 대한 설명입니다.',
  'past:2024-3:Q49'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '개념 스키마(Conceptual Schema)', 1),
  (@q_id, 'B', '내부 스키마(Internal Schema)',   0),
  (@q_id, 'C', '내용 스키마(Content Schema)',    0),
  (@q_id, 'D', '외부 스키마(External Schema)',   0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');
