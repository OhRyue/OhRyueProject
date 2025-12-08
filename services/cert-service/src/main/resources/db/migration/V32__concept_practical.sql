SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_cert;

-- =========================================
-- 실기 P.3 개념 (SELECT/집계/조인 + 인덱스/튜닝)
--  - topic_id = 31301 (P.3.1), 31302 (P.3.2)
-- =========================================

SET @tp_sql_query  := 31301; -- P.3.1 SELECT/집계/조인 쿼리 작성
SET @tp_sql_tuning := 31302; -- P.3.2 인덱스 설계 및 쿼리 튜닝

/* ========================================================
 * P.3.1 SELECT/집계/조인 쿼리 작성
 * topic_id = 31301
 * ======================================================== */
INSERT INTO concept (topic_id, sections_json)
VALUES (
  @tp_sql_query,
  JSON_OBJECT(
    'sections', JSON_ARRAY(

      /* -------------------------------------
         P.3.1.1 SELECT 기본 구조와 절의 순서
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 1,
        'subCode', 'P.3.1.1',
        'title', 'SELECT 기본 구조와 절의 순서를 먼저 잡기',
        'importance', 5,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','실기에서 가장 자주 쓰는 SELECT 구문 뼈대'),
          JSON_OBJECT(
            'type','paragraph',
            'text',
            '실기 문제의 대부분은 주어진 요구사항을 만족하는 SELECT 문을 작성하는 형태입니다. 먼저 SELECT 구문의 기본 구조와 절의 순서를 정확히 기억해 두면, 어떤 문제든 이 틀 위에 필요한 조건을 하나씩 얹어가는 방식으로 접근할 수 있습니다.'
          ),

          JSON_OBJECT(
            'type','table',
            'caption','SELECT 절의 논리적 실행 순서와 문법 순서 비교',
            'headers', JSON_ARRAY('구분','실행 순서(논리)','작성 순서(문법)'),
            'rows', JSON_ARRAY(
              JSON_ARRAY('1','FROM (어떤 테이블에서)','SELECT'),
              JSON_ARRAY('2','ON (조인 조건)','FROM'),
              JSON_ARRAY('3','JOIN (다른 테이블 결합)','JOIN'),
              JSON_ARRAY('4','WHERE (행 필터링)','ON'),
              JSON_ARRAY('5','GROUP BY (그룹 묶기)','WHERE'),
              JSON_ARRAY('6','HAVING (그룹 필터링)','GROUP BY'),
              JSON_ARRAY('7','SELECT (컬럼 선택)','HAVING'),
              JSON_ARRAY('8','DISTINCT (중복 제거)','ORDER BY'),
              JSON_ARRAY('9','ORDER BY (정렬)','LIMIT (DB에 따라)'),
              JSON_ARRAY('10','LIMIT/OFFSET (페이징)','-')
            )
          ),

          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '실제 SQL은 SELECT 로 시작하지만, 논리적으로는 FROM 에서 어떤 데이터를 가져오는지 먼저 결정된다.',
              'WHERE 는 그룹핑 전에 행을 필터링하고, HAVING 은 GROUP BY 이후 그룹에 조건을 거는 절이다.',
              'ORDER BY 와 LIMIT 은 화면에 보여줄 순서와 개수를 조절하는 단계로, 실기 시험에서 페이징 로직과 함께 자주 등장한다.'
            )
          )
        )
      ),

      /* -------------------------------------
         P.3.1.2 집계 함수, GROUP BY, HAVING 패턴
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 2,
        'subCode', 'P.3.1.2',
        'title', '집계 함수와 GROUP BY, HAVING 기본 패턴',
        'importance', 5,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','집계가 들어가면 항상 확인해야 할 세 가지'),
          JSON_OBJECT(
            'type','table',
            'caption','집계 쿼리 체크 포인트',
            'headers', JSON_ARRAY('항목','질문','예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                'GROUP BY 대상',
                '어떤 기준으로 묶을 것인가?',
                '고객별 매출: GROUP BY customer_id / 일자별 건수: GROUP BY order_date'
              ),
              JSON_ARRAY(
                '집계 함수',
                '어떤 통계를 보고 싶은가?',
                'COUNT(*), SUM(amount), AVG(score), MAX(created_at), MIN(created_at)'
              ),
              JSON_ARRAY(
                'HAVING 조건',
                '어떤 그룹만 남길 것인가?',
                '총 매출 100만 원 이상인 고객만: HAVING SUM(amount) >= 1000000'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','WHERE 와 HAVING 을 구분하는 요령'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              'WHERE: 개별 행에 대해 조건을 걸 때 사용. 예) 주문일자가 2024년인 행만 먼저 필터링.',
              'HAVING: GROUP BY 로 묶인 결과(그룹)에 조건을 걸 때 사용. 예) 고객별로 묶은 뒤, 주문 건수가 3건 이상인 고객만 남기기.',
              '실기에서 잘 나오는 함정: 집계 함수(COUNT, SUM 등)에 대한 조건은 HAVING 에 두어야 한다는 점.'
            )
          )
        )
      ),

      /* -------------------------------------
         P.3.1.3 JOIN 유형과 실무/시험에서의 쓰임새
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 3,
        'subCode', 'P.3.1.3',
        'title', 'INNER JOIN, LEFT JOIN 을 상황에 맞게 선택하기',
        'importance', 4,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','JOIN 이 필요한 대표적인 상황'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '주문 테이블과 고객 테이블을 합쳐서, 고객 이름과 주문 정보를 함께 보고 싶은 경우',
              '게시글 테이블과 카테고리 테이블을 합쳐서, 카테고리 이름을 함께 표시해야 하는 경우',
              '시험 시나리오에서 "코드값 대신 이름을 함께 출력하라"라는 말이 나오면 거의 항상 JOIN 을 의미한다.'
            )
          ),

          JSON_OBJECT(
            'type','table',
            'caption','JOIN 유형 비교 박스',
            'headers', JSON_ARRAY('JOIN 유형','설명','언제 쓰는가'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                'INNER JOIN',
                '양쪽 테이블에 모두 존재하는 행만 결합',
                '주문에 고객이 반드시 존재하는 경우처럼, 매칭이 되는 데이터만 보고 싶을 때 사용'
              ),
              JSON_ARRAY(
                'LEFT JOIN',
                '왼쪽 테이블의 모든 행을 기준으로, 오른쪽에 없으면 NULL 로 채움',
                '고객은 있지만 아직 주문이 없는 경우도 함께 보고 싶을 때 사용 (고객 기준 활동 분석 등)'
              ),
              JSON_ARRAY(
                'RIGHT JOIN',
                '오른쪽 기준(잘 쓰이지 않음)',
                '특정 DB나 기존 쿼리 구조 때문에 어쩔 수 없이 쓰는 경우가 있지만, 실기에서는 대부분 LEFT JOIN 으로 표현 가능'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','JOIN 조건을 작성할 때 자주 나오는 실수'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              'ON 절이 아닌 WHERE 절에 조인 조건을 잘못 넣어서, OUTER JOIN 이 사실상 INNER JOIN 처럼 동작하는 경우',
              '조인 키가 아닌 다른 컬럼(이름, 전화번호 등)을 조건으로 걸어서 중복 행이 발생하거나 성능이 떨어지는 경우',
              '별칭(A, B 등)을 지정해 놓고 사용하지 않아, 컬럼 출처가 헷갈리는 경우'
            )
          )
        )
      )

    )
  )
)
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);

/* ========================================================
 * P.3.2 인덱스 설계 및 쿼리 튜닝
 * topic_id = 31302
 * ======================================================== */
INSERT INTO concept (topic_id, sections_json)
VALUES (
  @tp_sql_tuning,
  JSON_OBJECT(
    'sections', JSON_ARRAY(

      /* -------------------------------------
         P.3.2.1 인덱스 기본 개념과 설계 포인트
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 1,
        'subCode', 'P.3.2.1',
        'title', '인덱스 기본 개념과 설계 시 체크 포인트',
        'importance', 5,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','인덱스란 무엇인가'),
          JSON_OBJECT(
            'type','paragraph',
            'text',
            '인덱스는 책의 목차처럼, 원하는 데이터를 더 빨리 찾기 위해 별도로 만들어 두는 자료 구조입니다. 일반적으로 B-Tree 구조를 사용하며, WHERE, JOIN, ORDER BY 등에 자주 등장하는 컬럼에 인덱스를 생성하면 성능이 크게 향상될 수 있습니다.'
          ),

          JSON_OBJECT(
            'type','table',
            'caption','인덱스 설계 시 꼭 확인해야 하는 요소',
            'headers', JSON_ARRAY('항목','설명','예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '선행 컬럼(Leading Column)',
                '복합 인덱스에서 가장 먼저 오는 컬럼. 검색 조건에 가장 자주 쓰이는 컬럼을 앞에 둔다.',
                '인덱스 (customer_id, order_date) 에서 customer_id = ? 조건이 자주 사용된다면 적절한 설계.'
              ),
              JSON_ARRAY(
                '카디널리티(Cardinality)',
                '서로 다른 값의 개수. 다양할수록 인덱스 효율이 좋다.',
                '성별 컬럼(남/여)은 값 종류가 적어 카디널리티가 낮으므로 인덱스 효율이 떨어진다.'
              ),
              JSON_ARRAY(
                '갱신 빈도',
                'INSERT/UPDATE 가 매우 자주 발생하는 컬럼에 인덱스를 많이 걸면 쓰기 성능이 떨어진다.',
                '로그 테이블에 불필요한 인덱스를 많이 두면 삽입 속도가 급격히 느려질 수 있다.'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','인덱스를 만들지 말아야 할 때'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '자주 조회되지 않는 컬럼에 인덱스를 만들어도 이득보다 관리 비용이 더 클 수 있다.',
              '값 종류가 거의 없는 컬럼(YN, 상태값 등)에 인덱스를 걸면 오히려 성능이 나빠질 수 있다.',
              '시험에서는 "인덱스를 과도하게 생성하여 DML 성능이 저하되는 문제"가 단골 포인트로 등장한다.'
            )
          )
        )
      ),

      /* -------------------------------------
         P.3.2.2 실행 계획(EXPLAIN) 읽기 기초
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 2,
        'subCode', 'P.3.2.2',
        'title', '실행 계획(EXPLAIN)으로 쿼리 동작 방식 이해하기',
        'importance', 4,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','EXPLAIN 으로 미리 쿼리 비용을 보는 이유'),
          JSON_OBJECT(
            'type','paragraph',
            'text',
            'EXPLAIN 은 쿼리를 실제로 실행하지 않고, DB 가 어떤 방식으로 테이블에 접근할지 미리 보여주는 도구입니다. 전체 테이블을 스캔하는지, 인덱스를 타는지, 어느 정도의 행을 읽을지 등을 파악하여 튜닝 방향을 정할 수 있습니다.'
          ),

          JSON_OBJECT(
            'type','table',
            'caption','실행 계획에서 자주 보는 컬럼 의미',
            'headers', JSON_ARRAY('컬럼','설명','튜닝 포인트'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                'type',
                '접근 방식. ALL 은 풀 스캔, ref/eq_ref 는 인덱스 기반 조인 등',
                'type 이 ALL 인 경우, WHERE 조건에 맞는 인덱스가 있는지 확인한다.'
              ),
              JSON_ARRAY(
                'key',
                '사용된 인덱스 이름',
                'key 가 NULL 이라면 인덱스를 사용하지 않는 것이므로, 인덱스 설계를 다시 검토해야 한다.'
              ),
              JSON_ARRAY(
                'rows',
                '예상으로 읽어야 하는 행의 수',
                'rows 값이 매우 크다면, 불필요한 범위를 읽고 있는지 WHERE 조건과 인덱스를 확인해야 한다.'
              ),
              JSON_ARRAY(
                'Extra',
                '추가 정보 (Using where, Using temporary, Using filesort 등)',
                'Using temporary, Using filesort 가 동시에 뜨는 경우, ORDER BY / GROUP BY 튜닝 필요성을 의심해 볼 수 있다.'
              )
            )
          )
        )
      ),

      /* -------------------------------------
         P.3.2.3 튜닝 체크리스트와 자주 나오는 함정
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 3,
        'subCode', 'P.3.2.3',
        'title', '실기에서 자주 등장하는 튜닝 체크리스트',
        'importance', 4,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','쿼리 튜닝 문제를 볼 때의 간단한 체크 박스'),
          JSON_OBJECT(
            'type','table',
            'caption','쿼리 튜닝 체크리스트',
            'headers', JSON_ARRAY('항목','점검 내용','예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                'SELECT * 사용',
                '실제로 필요한 컬럼만 조회하는지 확인',
                '대량 데이터를 다루는 화면이면, SELECT * 대신 필요한 컬럼만 선택하여 네트워크 트래픽과 메모리 사용을 줄인다.'
              ),
              JSON_ARRAY(
                '함수 적용 컬럼',
                'WHERE 조건에 함수가 적용되어 인덱스를 못 타고 있지 않은지 확인',
                'WHERE DATE(order_date) = ... 대신, order_date BETWEEN ... 형태로 바꾸어 인덱스를 활용하도록 한다.'
              ),
              JSON_ARRAY(
                '불필요한 서브쿼리',
                'JOIN 으로 바꿀 수 있는 상관 서브쿼리가 있는지 확인',
                '사용자별 최신 주문일을 구하려고, 사용자마다 SELECT MAX(order_date) 서브쿼리를 날리는 대신, GROUP BY 또는 윈도우 함수를 활용한다.'
              ),
              JSON_ARRAY(
                'WHERE vs HAVING',
                '행 필터링은 WHERE, 그룹 필터링은 HAVING 에 두었는지 확인',
                '집계와 상관없는 조건을 HAVING 에 작성하면, 불필요하게 많은 데이터를 먼저 그룹핑하게 되어 성능이 저하될 수 있다.'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','시험에서 잘 나오는 한 줄 요약 팁'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '인덱스는 자주 조회되고, 값 종류가 어느 정도 다양한 컬럼에 만든다.',
              '실행 계획에서 전체 스캔(ALL)과 큰 rows 값이 보이면 인덱스 설계를 가장 먼저 의심한다.',
              'WHERE 절에서 함수나 연산으로 감싼 컬럼은 인덱스를 제대로 사용하지 못할 수 있다.',
              'SELECT * 를 남발하기보다, 실제로 화면에 필요한 컬럼만 선택하는 습관이 중요하다.'
            )
          )
        )
      )

    )
  )
)
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);

SET FOREIGN_KEY_CHECKS = 1;
