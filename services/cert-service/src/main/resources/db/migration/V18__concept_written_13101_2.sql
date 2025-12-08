-- =========================================
-- 3.1.1 절차형 SQL 작성 (concept 보강)
-- topic_id = 13101
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 13101,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              3.1.1.1 트리거(Trigger) 개념과 구성 요소
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '3.1.1.1',
             'title', '트리거 개념과 구성 요소',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','트리거(Trigger)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '트리거는 특정 이벤트(INSERT, UPDATE, DELETE 등)가 발생했을 때 데이터베이스가 자동으로 실행하는 절차형 SQL 블록이다. '
                 '데이터 무결성 유지, 자동 로그 기록, 업무 규칙 강제 등에 활용된다.'
               ),

               JSON_OBJECT('type','heading','text','트리거 구성 요소 (다·이·비·컨·SE)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','트리거 구성 요소 정리',
                 'headers', JSON_ARRAY('구성요소','키워드/역할'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('선언부','DECLARE – 트리거에서 사용할 변수, 상수, 커서 선언'),
                   JSON_ARRAY('이벤트부','EVENT – 트리거 실행 시점(BEFORE/AFTER)과 이벤트(INSERT/UPDATE/DELETE) 정의'),
                   JSON_ARRAY('시작/종료부','BEGIN / END – 트리거 본문을 감싸는 블록 범위 정의'),
                   JSON_ARRAY('제어부','CONTROL – IF, LOOP 등을 사용해 순차·조건·반복 제어'),
                   JSON_ARRAY('SQL','SQL – 주로 DML(INSERT/UPDATE/DELETE)을 수행하는 부분'),
                   JSON_ARRAY('예외부','EXCEPTION – 실행 중 오류 발생 시 처리 로직 정의')
                 )
               ),

               JSON_OBJECT('type','heading','text','트리거의 특징 및 주의점'),
               JSON_OBJECT('type','list','items', JSON_ARRAY(
                 '트리거는 데이터 조작에 반응해 자동으로 실행되므로, 로직이 복잡해질수록 디버깅이 어려워진다.',
                 '트리거 내부에서는 COMMIT, ROLLBACK 같은 TCL을 사용하면 컴파일 오류가 발생할 수 있다.',
                 '트리거 실행 중 오류가 발생하면 트리거를 유발한 DML 작업도 함께 롤백될 수 있어, 검증된 로직만 작성해야 한다.'
               ))
             )
           ),

           /* -------------------------------------
              3.1.1.2 트리거 작성 예시와 동작
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '3.1.1.2',
             'title', '트리거 작성 예시와 동작 원리',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','트리거 작성 예시'),
               JSON_OBJECT('type','paragraph','text',
                 '예시) 학생 테이블에 미성년자를 등록하지 못하도록 막는 BEFORE INSERT 트리거\n'
                 'CREATE TRIGGER T_STUDENT\n'
                 '  BEFORE INSERT ON STUDENT\n'
                 'BEGIN\n'
                 '  IF (AGE < 19) THEN\n'
                 '    RAISE_APPLICATION_ERROR(-20502, ''미성년자 추가 불가 오류'' );\n'
                 '  END IF;\n'
                 'END;'
               ),

               JSON_OBJECT('type','heading','text','예시 트리거 동작 설명'),
               JSON_OBJECT('type','list','items', JSON_ARRAY(
                 'INSERT 문이 실행되기 전에(BEFORE INSERT) 트리거가 자동으로 호출된다.',
                 '입력하려는 튜플의 AGE 값이 19 미만이면 사용자 정의 오류 코드를 발생시켜 INSERT를 중단한다.',
                 '업무 규칙(미성년자 등록 금지)을 DB 레벨에서 강제할 수 있어, 애플리케이션이 달라져도 일관된 제약을 유지할 수 있다.'
               ))
             )
           ),

           /* -------------------------------------
              3.1.1.3 이벤트(Event) 개념과 사용 예
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '3.1.1.3',
             'title', '이벤트(Event)의 개념과 등록 구문',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','이벤트(Event)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '이벤트는 특정 시점이나 주기마다 지정된 쿼리, 프로시저, 함수를 자동 실행하도록 예약하는 기능이다. '
                 '정기 리포트 생성, 로그 정리, 자동 집계 작업 등 스케줄 기반 배치 처리에 활용된다.'
               ),

               JSON_OBJECT('type','heading','text','이벤트 등록 기본 구문'),
               JSON_OBJECT('type','paragraph','text',
                 'CREATE EVENT 이벤트_이름\n'
                 '  ON SCHEDULE 스케줄_정의\n'
                 '  [ON COMPLETION [NOT] PRESERVE]\n'
                 '  [ENABLE | DISABLE]\n'
                 '  [COMMENT ''설명'']\n'
                 'DO\n'
                 '  [BEGIN]\n'
                 '    실행할 SQL문;\n'
                 '  [END];'
               ),

               JSON_OBJECT('type','heading','text','이벤트 등록 예시'),
               JSON_OBJECT('type','paragraph','text',
                 '예시) 50초마다 TEST 테이블에 현재 시각을 기록하는 이벤트\n'
                 'CREATE EVENT T_TEST\n'
                 '  ON SCHEDULE EVERY 50 SECOND\n'
                 'DO\n'
                 '  INSERT INTO TEST (REGDATE)\n'
                 '  VALUES (NOW());'
               )
             )
           ),

           /* -------------------------------------
              3.1.1.4 사용자 정의 함수(UDF)와 오류 처리
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '3.1.1.4',
             'title', '사용자 정의 함수(UDF)와 오류 처리 개념',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','사용자 정의 함수(User-Defined Function)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '사용자 정의 함수는 절차형 SQL로 작성하는 함수로, 입력 파라미터를 받아 일련의 연산을 수행한 뒤 단일 값을 반환한다. '
                 '복잡한 계산 로직을 캡슐화해 재사용성을 높이고, SELECT 문 등에서 일반 함수처럼 호출할 수 있다.'
               ),

               JSON_OBJECT('type','heading','text','사용자 정의 함수 특징'),
               JSON_OBJECT('type','list','items', JSON_ARRAY(
                 '결과를 단일 값으로 반환하며, 일반적으로 SELECT 등 SQL 문장에서 호출한다.',
                 '데이터 조작을 위한 INSERT, DELETE, UPDATE는 직접 사용을 제한하는 경우가 많다.',
                 '정보 은닉과 캡슐화를 통해 로직을 숨기고, 인터페이스만 제공한다.'
               )),

               JSON_OBJECT('type','heading','text','사용자 정의 함수 구성 요소'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'DECLARE: 지역 변수, 상수 등 선언',
                   'BEGIN / END: 함수 본문 블록 범위',
                   'CONTROL: IF, LOOP 등 제어 구조',
                   'SQL: 조회, 계산 등을 수행하는 SQL 문장',
                   'EXCEPTION: 실행 도중 예외 처리 블록',
                   'RETURN: 함수 결과 값을 반환'
                 )
               ),

               JSON_OBJECT('type','heading','text','오류 처리(Error Handling)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '오류 처리는 프로시저나 함수 실행 중 발생할 수 있는 예외 상황을 탐지하고, 의미 있는 에러 메시지나 복구 로직을 제공하는 과정이다. '
                 'DECLARE HANDLER 구문으로 특정 오류 코드에 대한 핸들러를 정의해, 트랜잭션 롤백 또는 대체 처리 등을 수행한다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);


-- =========================================
-- 3.1.2 응용 SQL 작성 (concept 보강)
-- topic_id = 13102
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 13102,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              3.1.2.1 집계 함수와 기본 응용
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '3.1.2.1',
             'title', '집계 함수 개념과 활용',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','집계 함수(Aggregate Function)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '집계 함수는 여러 행을 한 번에 모아서 하나의 결과(합계, 평균 등)를 계산하는 함수이다. '
                 'GROUP BY 절과 함께 사용하면 그룹별 통계 값을 쉽게 구할 수 있다.'
               ),

               JSON_OBJECT(
                 'type','table',
                 'caption','주요 집계 함수 정리',
                 'headers', JSON_ARRAY('함수','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('COUNT','행(튜플)의 개수를 구한다.'),
                   JSON_ARRAY('SUM','수치 컬럼의 합계를 구한다.'),
                   JSON_ARRAY('AVG','수치 컬럼의 평균을 구한다.'),
                   JSON_ARRAY('MAX','가장 큰 값을 구한다.'),
                   JSON_ARRAY('MIN','가장 작은 값을 구한다.'),
                   JSON_ARRAY('STDDEV','표준편차를 구한다.'),
                   JSON_ARRAY('VARIANCE','분산을 구한다.')
                 )
               ),

               JSON_OBJECT('type','heading','text','집계 함수 사용 시 주의점'),
               JSON_OBJECT('type','list','items', JSON_ARRAY(
                 'GROUP BY에 없는 일반 컬럼은 SELECT 절에서 집계 함수와 함께 사용할 수 없다.',
                 'NULL 값은 대부분 집계 대상에서 제외되므로, 필요 시 NVL, COALESCE 등으로 대체해야 한다.',
                 'WHERE 절은 그룹핑 이전 조건, HAVING 절은 그룹핑 이후 조건에 사용한다.'
               ))
             )
           ),

           /* -------------------------------------
              3.1.2.2 순위 함수(RANK, DENSE_RANK, ROW_NUMBER)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '3.1.2.2',
             'title', '순위 함수와 차이점',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','순위 함수의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '순위 함수는 특정 기준 컬럼을 기준으로 각 행의 순위를 계산하는 윈도우 함수이다. '
                 '시험에서는 RANK, DENSE_RANK, ROW_NUMBER의 차이를 구분하는 문제가 자주 출제된다.'
               ),

               JSON_OBJECT(
                 'type','table',
                 'caption','순위 함수 비교',
                 'headers', JSON_ARRAY('함수','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('RANK','동일 값에는 같은 순위를 부여하고, 다음 순위는 건너뛴다. 예: 1, 2, 2, 4, 5'),
                   JSON_ARRAY('DENSE_RANK','동일 값에는 같은 순위를 부여하지만, 순위를 건너뛰지 않는다. 예: 1, 2, 2, 3, 4'),
                   JSON_ARRAY('ROW_NUMBER','행마다 고유한 연속 번호를 부여한다. 예: 1, 2, 3, 4, 5')
                 )
               ),

               JSON_OBJECT('type','heading','text','순위 함수 활용 포인트'),
               JSON_OBJECT('type','list','items', JSON_ARRAY(
                 '특정 그룹에서 상위 N명만 추출할 때 자주 사용된다.',
                 'OVER(ORDER BY ...) 절을 통해 정렬 기준을, PARTITION BY로 그룹 기준을 지정한다.',
                 '시험에서는 예시 순위를 보고 어떤 함수인지 맞추는 유형이 자주 나온다.'
               ))
             )
           ),

           /* -------------------------------------
              3.1.2.3 OLAP와 윈도우 함수
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '3.1.2.3',
             'title', 'OLAP 개념과 윈도우 함수 분류',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','OLAP(On-Line Analytical Processing)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 'OLAP은 동일한 데이터를 다양한 관점(시간, 지역, 상품 등)에서 다차원적으로 분석할 수 있도록 지원하는 의사결정 지원 시스템이다. '
                 'ROLAP, MOLAP 등 구현 방식은 다양하지만, 공통적으로 다차원 분석과 요약 기능을 제공한다.'
               ),

               JSON_OBJECT('type','heading','text','윈도우 함수와 OLAP 연산'),
               JSON_OBJECT('type','paragraph','text',
                 '윈도우 함수는 행 집합 위에 슬라이딩 윈도우를 정의하고, 그 범위 안에서 집계·순위·비율 등을 계산하는 함수이다. '
                 'OLAP 연산으로는 Roll-Up, Drill-Down, Slicing, Dicing, Pivoting 등이 있으며, 시험에서는 이름과 의미를 매칭하는 문제가 자주 출제된다.'
               ),

               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '집계 함수: SUM, AVG 등을 윈도우 범위에 적용',
                   '순위 함수: RANK, DENSE_RANK, ROW_NUMBER 등',
                   '행 순서 함수: LAG, LEAD, FIRST_VALUE, LAST_VALUE 등',
                   '비율 함수: RATIO_TO_REPORT, PERCENT_RANK 등'
                 )
               )
             )
           ),

           /* -------------------------------------
              3.1.2.4 그룹 함수와 고급 집계 응용
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '3.1.2.4',
             'title', '그룹 함수와 오류 처리 개념',
             'importance', 3,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','그룹 함수(Group Function)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '그룹 함수는 GROUP BY 절과 함께 사용되어, 테이블의 행들을 하나 이상의 컬럼을 기준으로 그룹핑하고 그룹 단위로 집계 결과를 반환하는 함수이다.'
               ),

               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'ROLLUP: 그룹 계층에 대해 소계와 총계를 함께 구한다.',
                   'CUBE: 가능한 모든 조합에 대한 다차원 집계를 제공한다.',
                   'GROUPING SETS: 필요한 그룹 조합만 선택적으로 집계할 수 있다.'
                 )
               ),

               JSON_OBJECT('type','heading','text','오류 처리와 핸들러 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '고급 SQL 응용에서는 집계·윈도우 함수뿐 아니라, 프로시저나 함수 실행 중 발생하는 예외에 대한 오류 처리도 중요하다. '
                 'DECLARE HANDLER 구문으로 특정 오류 코드에 대한 핸들러를 선언하고, 오류 발생 시 롤백 또는 대체 처리 로직을 수행하도록 설계한다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);


-- =========================================
-- 3.2.1 기본 SQL 작성 (concept 보강)
-- topic_id = 13201
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 13201,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              3.2.1.1 SQL 문법 분류 (DDL/DML/DCL)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '3.2.1.1',
             'title', 'SQL 문법 분류와 역할',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','SQL 문법의 기본 분류 (정·조·제)'),
               JSON_OBJECT('type','paragraph','text',
                 'SQL은 데이터 정의, 조작, 제어 관점에서 크게 DDL, DML, DCL로 분류한다. '
                 '각 분류의 대표 명령어와 역할을 정확히 기억해두는 것이 중요하다.'
               ),

               JSON_OBJECT(
                 'type','table',
                 'caption','SQL 문법 분류',
                 'headers', JSON_ARRAY('분류','대표 명령어 및 설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('DDL (데이터 정의어)','CREATE, ALTER, DROP, TRUNCATE – 테이블, 뷰, 인덱스 등 데이터 구조를 정의·변경·삭제'),
                   JSON_ARRAY('DML (데이터 조작어)','SELECT, INSERT, UPDATE, DELETE – 테이블에 저장된 데이터를 조회·추가·수정·삭제'),
                   JSON_ARRAY('DCL (데이터 제어어)','GRANT, REVOKE – 사용자 권한 부여·회수, 보안 및 접근 제어 담당')
                 )
               ),

               JSON_OBJECT('type','heading','text','DCL의 주요 기능 (보·무·병·회)'),
               JSON_OBJECT('type','list','items', JSON_ARRAY(
                 '데이터 보안: 특정 사용자만 필요한 데이터에 접근하도록 권한을 부여한다.',
                 '무결성 유지: 권한을 제한하여 의도치 않은 데이터 변경을 방지한다.',
                 '병행 수행 제어: 여러 사용자가 동시에 작업할 때 충돌을 줄인다.',
                 '회복 지원: 트랜잭션 제어와 함께 안정적인 복구 환경을 제공한다.'
               ))
             )
           ),

           /* -------------------------------------
              3.2.1.2 DDL 대상 (도·스·테·뷰·인)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '3.2.1.2',
             'title', 'DDL 대상과 개념 정리',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','DDL 대상 개념 (도·스·테·뷰·인)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','DDL 대상 객체',
                 'headers', JSON_ARRAY('객체','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('도메인(Domain)','하나의 속성이 가질 수 있는 원자값들의 집합'),
                   JSON_ARRAY('스키마(Schema)','데이터베이스 전체 구조와 제약조건을 정의하는 메타데이터 집합'),
                   JSON_ARRAY('테이블(Table)','실제 데이터가 저장되는 기본 구조'),
                   JSON_ARRAY('뷰(View)','하나 이상의 테이블로부터 유도되는 가상의 테이블'),
                   JSON_ARRAY('인덱스(Index)','검색 성능을 향상시키기 위한 별도의 데이터 구조')
                 )
               ),

               JSON_OBJECT('type','paragraph','text',
                 'DDL은 이러한 객체들을 생성, 변경, 삭제하는 명령어이며, 실행 즉시 자동 커밋이 되는 경우가 많아 주의해야 한다.'
               )
             )
           ),

           /* -------------------------------------
              3.2.1.3 WHERE 조건 연산자와 패턴 매칭
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '3.2.1.3',
             'title', 'WHERE 절 조건 연산자 정리',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','조건식에 사용되는 연산자'),
               JSON_OBJECT(
                 'type','table',
                 'caption','WHERE 절 주요 연산자',
                 'headers', JSON_ARRAY('구분','연산자 및 설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('비교 연산','=, <>, <, <=, >, >=' ),
                   JSON_ARRAY('범위 조건','A BETWEEN B AND C – A는 B 이상, C 이하'),
                   JSON_ARRAY('집합 조건','IN, NOT IN – 지정된 집합에 포함 여부 검사'),
                   JSON_ARRAY('NULL 검사','IS NULL, IS NOT NULL – 컬럼 값이 NULL인지 확인'),
                   JSON_ARRAY('복합 조건','AND, OR, NOT – 여러 조건을 결합하여 표현')
                 )
               )
             )
           ),

           /* -------------------------------------
              3.2.1.4 LIKE 와일드카드 패턴
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '3.2.1.4',
             'title', 'LIKE 와일드카드와 패턴 검색',
             'importance', 3,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','LIKE 연산과 와일드카드'),
               JSON_OBJECT('type','paragraph','text',
                 'LIKE 연산자는 문자열 패턴 일치를 위해 사용되며, %, _, 대괄호([]) 등의 와일드카드를 함께 사용한다.'
               ),

               JSON_OBJECT(
                 'type','table',
                 'caption','주요 와일드카드 정리',
                 'headers', JSON_ARRAY('기호','설명','예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('%','0개 이상의 임의 문자열','LIKE ''노란색%'' → 노란색으로 시작'),
                   JSON_ARRAY('_','임의의 한 글자','LIKE ''_동%'' → 두 번째 글자가 ''동''인 값'),
                   JSON_ARRAY('[0-8]','0~8 중 한 글자','LIKE ''[0-8]%'' → 0~8로 시작'),
                   JSON_ARRAY('[^0-8]','0~8이 아닌 한 글자','LIKE ''[^0-8]%'' → 0~8이 아닌 값으로 시작')
                 )
               ),

               JSON_OBJECT('type','paragraph','text',
                 '시험에서는 다양한 패턴 예시를 주고 어떤 문자열이 검색되는지를 묻는 문제가 자주 나온다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);


-- =========================================
-- 3.2.2 고급 SQL 작성 (concept 보강)
-- topic_id = 13202
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 13202,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              3.2.2.1 뷰(View)의 개념과 특징
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '3.2.2.1',
             'title', '뷰(View)의 개념과 특징',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','뷰(View)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '뷰는 하나 이상의 물리 테이블에서 유도되는 논리적(가상) 테이블이다. '
                 '실제 데이터는 기본 테이블에 저장되고, 뷰는 SELECT 문 정의만을 저장하여 필요한 컬럼·행만 선택적으로 노출한다.'
               ),

               JSON_OBJECT('type','heading','text','뷰의 특징'),
               JSON_OBJECT('type','list','items', JSON_ARRAY(
                 '뷰는 저장장치 내에 물리적으로 데이터가 저장되지 않는다.',
                 '뷰는 ALTER로 직접 변경할 수 없으며, 변경 시 DROP 후 CREATE를 다시 수행해야 한다.',
                 '뷰가 참조하는 기본 테이블이 삭제되면, 해당 뷰도 무효가 되거나 함께 삭제된다.'
               )),

               JSON_OBJECT('type','heading','text','뷰의 장점과 단점'),
               JSON_OBJECT(
                 'type','table',
                 'caption','뷰의 장단점 비교',
                 'headers', JSON_ARRAY('구분','내용'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('장점','논리적 데이터 독립성 제공, 사용자별 데이터 제한, 복잡한 질의 단순화, 보안 강화'),
                   JSON_ARRAY('단점','뷰 자체 인덱스 불가, 정의 변경의 제한, 일부 뷰에서 데이터 변경 제약 존재')
                 )
               )
             )
           ),

           /* -------------------------------------
              3.2.2.2 인덱스(Index)의 개념과 종류
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '3.2.2.2',
             'title', '인덱스 개념과 유형, 스캔 방식',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','인덱스(Index)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '인덱스는 검색을 빠르게 수행하기 위한 보조 데이터 구조로, 책의 색인과 같은 역할을 한다. '
                 '기본키(PK) 컬럼은 보통 자동으로 인덱스가 생성되지만, 일반 컬럼의 인덱스는 필요에 따라 직접 생성해야 한다.'
               ),

               JSON_OBJECT('type','heading','text','인덱스의 주요 유형'),
               JSON_OBJECT(
                 'type','table',
                 'caption','인덱스 종류 정리',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('순서 인덱스(Ordered Index)','B-Tree 기반으로 정렬된 순서 유지, 범위 검색에 유리'),
                   JSON_ARRAY('해시 인덱스(Hash Index)','해시 함수를 통해 키 값으로 바로 위치를 계산, 동등 비교에 유리'),
                   JSON_ARRAY('비트맵 인덱스(Bitmap Index)','가능한 값의 종류가 적은 컬럼에 적합, 비트 연산으로 빠른 검색'),
                   JSON_ARRAY('함수 기반 인덱스','함수나 수식 결과에 대해 인덱스를 생성하여 특정 패턴 검색 최적화'),
                   JSON_ARRAY('단일 인덱스','하나의 컬럼만으로 구성된 인덱스'),
                   JSON_ARRAY('결합 인덱스','두 개 이상의 컬럼으로 구성된 인덱스'),
                   JSON_ARRAY('클러스터드 인덱스','기본 키 기준으로 실제 데이터가 물리적으로 정렬·저장되는 방식')
                 )
               ),

               JSON_OBJECT('type','heading','text','인덱스 스캔 방식 (범·전·단·생)'),
               JSON_OBJECT('type','list','items', JSON_ARRAY(
                 '범위 스캔: 인덱스를 이용해 특정 범위의 키 값을 순차적으로 읽는 방식',
                 '전체 스캔: 인덱스 전체를 처음부터 끝까지 스캔',
                 '단일 스캔: 특정 키 값 하나를 찾기 위해 인덱스를 탐색',
                 '생략 스캔: 필요한 키 범위만 건너뛰며 읽는 최적화된 스캔 방식'
               ))
             )
           ),

           /* -------------------------------------
              3.2.2.3 집합 연산자(Set Operator)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '3.2.2.3',
             'title', '집합 연산자 개념과 유형',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','집합 연산자의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '집합 연산자는 두 개 이상의 SELECT 결과 집합을 하나의 결과 집합으로 결합하는 연산이다. '
                 '각 SELECT 문의 컬럼 개수와 데이터 타입이 호환되어야 한다.'
               ),

               JSON_OBJECT(
                 'type','table',
                 'caption','집합 연산자 유형 (유·유·인·마)',
                 'headers', JSON_ARRAY('연산자','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('UNION','중복을 제거한 합집합'),
                   JSON_ARRAY('UNION ALL','중복을 포함한 합집합'),
                   JSON_ARRAY('INTERSECT','교집합'),
                   JSON_ARRAY('MINUS','첫 번째 결과에서 두 번째 결과를 뺀 차집합')
                 )
               ),

               JSON_OBJECT('type','paragraph','text',
                 '시험에서는 어떤 결과가 나오는지 직접 집합을 계산하는 문제나, 각 연산자의 특성을 비교하는 문제가 자주 출제된다.'
               )
             )
           ),

           /* -------------------------------------
              3.2.2.4 조인(Join)과 스키마(Schema)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '3.2.2.4',
             'title', '조인 유형과 스키마 개념',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','조인(Join)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '조인은 두 개 이상의 테이블을 연결하여 하나의 결과 집합으로 조회하는 연산이다. '
                 '논리적 조인과 물리적 조인으로 나눌 수 있으며, 시험에서는 이름과 동작 차이를 묻는 문제가 많다.'
               ),

               JSON_OBJECT('type','heading','text','논리적 조인 유형 (내·외·교·셀)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','논리적 조인 종류',
                 'headers', JSON_ARRAY('조인','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('내부 조인(Inner Join)','양쪽 테이블에서 조인 조건을 만족하는 행만 반환'),
                   JSON_ARRAY('외부 조인(Outer Join)','한쪽 테이블의 모든 행과 다른 쪽 일치 행을 함께 반환'),
                   JSON_ARRAY('교차 조인(Cross Join)','조인 조건 없이 가능한 모든 조합을 반환'),
                   JSON_ARRAY('셀프 조인(Self Join)','하나의 테이블을 두 번 참조해 자기 자신과 조인')
                 )
               ),

               JSON_OBJECT('type','heading','text','물리적 조인 유형 (네·소·해)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','물리적 조인 방식',
                 'headers', JSON_ARRAY('조인 방식','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('중첩 반복 조인(Nested-Loop Join)','한 테이블을 기준으로 다른 테이블을 반복 탐색하는 방식'),
                   JSON_ARRAY('정렬 합병 조인(Sort-Merge Join)','조인 대상 컬럼을 정렬 후 병합해 조인하는 방식'),
                   JSON_ARRAY('해시 조인(Hash Join)','해시 테이블을 구성해 키 값으로 빠르게 매칭하는 방식')
                 )
               ),

               JSON_OBJECT('type','heading','text','스키마(Schema)의 정의'),
               JSON_OBJECT('type','paragraph','text',
                 '스키마는 데이터베이스의 구조와 제약 조건에 대한 전반적인 명세를 기술한 메타데이터 집합이다. '
                 '외부 스키마, 개념 스키마, 내부 스키마로 나뉘며, 각각 사용자 관점, 전체 논리 구조, 물리적 구조를 표현한다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);