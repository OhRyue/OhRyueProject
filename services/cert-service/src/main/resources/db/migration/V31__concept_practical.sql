SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_cert;

-- =========================================
-- 실기 P.2.x 개념 (모델링 / 정규화)
--  - topic_id = 31201 (P.2.1), 31202 (P.2.2)
-- =========================================

SET @tp_model_levels  := 31201; -- P.2.1 개념/논리/물리 모델링
SET @tp_normalization := 31202; -- P.2.2 정규화 및 반정규화 적용

/* ========================================================
 * P.2.1 개념/논리/물리 모델링
 * topic_id = 31201
 * ======================================================== */
INSERT INTO concept (topic_id, sections_json)
VALUES (
  @tp_model_levels,
  JSON_OBJECT(
    'sections', JSON_ARRAY(

      /* -------------------------------------
         P.2.1.1 데이터 모델링 3단계 한눈에 보기
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 1,
        'subCode', 'P.2.1.1',
        'title', '데이터 모델링 3단계(개념·논리·물리) 정리',
        'importance', 5,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','실기에서 자주 나오는 모델링 3단계'),
          JSON_OBJECT(
            'type','paragraph',
            'text',
            '실기 문제에서는 요구사항을 보고 개념 모델, 논리 모델, 물리 모델 중 어느 수준을 그려야 하는지 정확히 구분하는 것이 중요합니다. 단계마다 질문이 다르고, 산출물도 달라집니다.'
          ),

          JSON_OBJECT(
            'type','table',
            'caption','개념 / 논리 / 물리 모델 비교',
            'headers', JSON_ARRAY('단계','핵심 질문','대표 산출물'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '개념 모델',
                '어떤 개체들이 존재하는가?',
                '개념 ERD, 후보 엔터티 목록 (고객, 주문, 상품 등)'
              ),
              JSON_ARRAY(
                '논리 모델',
                '어떤 속성과 관계가 필요한가?',
                '정규화된 ERD, 기본키·외래키 정의, 관계 차수'
              ),
              JSON_ARRAY(
                '물리 모델',
                '어떻게 저장할 것인가?',
                '테이블·컬럼·인덱스 정의서, 파티셔닝/샤딩 설계'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','각 단계에서 놓치기 쉬운 포인트'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '개념 모델: 엔터티 이름은 현실 업무 용어에 맞게 명확하게 적는다. (예: 회원, 주문, 결제 이력 등)',
              '논리 모델: 정규화를 통해 중복과 이상현상을 줄이는 것이 핵심이다.',
              '물리 모델: DBMS 종류와 성능, 인덱스·파티션 전략을 함께 고려해야 한다.'
            )
          )
        )
      ),

      /* -------------------------------------
         P.2.1.2 ERD 작성 절차와 실기 접근 팁
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 2,
        'subCode', 'P.2.1.2',
        'title', '요구사항에서 ERD 로 이어지는 단계별 절차',
        'importance', 4,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','요구사항 → ERD 변환 4단계'),
          JSON_OBJECT(
            'type','table',
            'caption','요구사항에서 ERD 로 가는 절차',
            'headers', JSON_ARRAY('단계','설명','예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '1단계: 엔터티 후보 찾기',
                '시나리오에 등장하는 주요 명사를 기준으로 엔터티 후보를 뽑는다.',
                '고객, 주문, 주문상세, 상품, 쿠폰 등'
              ),
              JSON_ARRAY(
                '2단계: 속성 정의',
                '각 엔터티에 어떤 정보를 저장할지 속성을 정의한다.',
                '고객: 고객ID, 이름, 연락처, 등급, 가입일 등'
              ),
              JSON_ARRAY(
                '3단계: 관계 설정',
                '엔터티 사이의 관계와 카디널리티(1:1, 1:N, N:M)를 정의한다.',
                '고객 1명은 주문 여러 건을 가질 수 있다. (고객 1 : 주문 N)'
              ),
              JSON_ARRAY(
                '4단계: 키와 제약조건 결정',
                '기본키와 외래키, 필수 여부, 유일 조건 등을 정한다.',
                '주문: 기본키 order_id, 외래키 customer_id, 필수 컬럼 설정 등'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','실기에서 ERD 문제가 나왔을 때 체크할 것'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '엔터티 간 관계가 지나치게 복잡하게 얽혀 있지 않은지 확인한다.',
              'N:M 관계는 중간 엔터티를 두어 1:N, 1:N 구조로 풀어내는지 확인한다.',
              '식별자가 명확하지 않은 엔터티가 있는지, 공통 키로 묶을 수 있는지 검토한다.'
            )
          )
        )
      ),

      /* -------------------------------------
         P.2.1.3 실기에서 자주 나오는 모델링 키워드
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 3,
        'subCode', 'P.2.1.3',
        'title', '모델링 관련 자주 출제되는 용어 요약',
        'importance', 3,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','용어 요약 박스'),
          JSON_OBJECT(
            'type','table',
            'caption','모델링 핵심 용어 모음',
            'headers', JSON_ARRAY('용어','설명','시험 포인트'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '식별자(Identifier)',
                '엔터티 인스턴스를 고유하게 구분하는 속성',
                '기본키 선택 문제, 후보키와 대체키 구분'
              ),
              JSON_ARRAY(
                '카디널리티(Cardinality)',
                '엔터티 간 관계의 수적 제약',
                '1:1, 1:N, N:M 관계 설정과 해소 방법'
              ),
              JSON_ARRAY(
                '옵셔널리티(Optionality)',
                '관계에서 필수 여부를 나타내는 개념',
                '선택적 관계, 필수 관계 표현과 제약조건 연결'
              )
            )
          ),

          JSON_OBJECT(
            'type','paragraph',
            'text',
            '이 용어들은 필기에서도 자주 등장하지만, 실기에서는 실제 ERD 를 해석하거나 수정하는 문제와 함께 출제되는 경우가 많습니다.'
          )
        )
      )

    )
  )
)
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);

/* ========================================================
 * P.2.2 정규화 및 반정규화 적용
 * topic_id = 31202
 * ======================================================== */
INSERT INTO concept (topic_id, sections_json)
VALUES (
  @tp_normalization,
  JSON_OBJECT(
    'sections', JSON_ARRAY(

      /* -------------------------------------
         P.2.2.1 정규화의 목적과 이상현상
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 1,
        'subCode', 'P.2.2.1',
        'title', '정규화의 목표와 이상현상 이해하기',
        'importance', 5,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','왜 정규화를 하는가'),
          JSON_OBJECT(
            'type','paragraph',
            'text',
            '정규화의 핵심 목표는 데이터 중복을 줄이고, 삽입·갱신·삭제 시 발생하는 이상현상을 제거하는 것입니다. 실기에서는 주어진 테이블 구조를 보고 어떤 이상이 발생하는지 설명하고, 더 나은 테이블 구조를 설계하는 문제로 자주 출제됩니다.'
          ),

          JSON_OBJECT(
            'type','table',
            'caption','대표적인 이상현상 정리',
            'headers', JSON_ARRAY('이상현상','설명','예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '삽입 이상',
                '필요한 데이터를 삽입하기 위해 불필요한 데이터까지 함께 넣어야 하는 문제',
                '주문이 하나도 없어도, 고객 정보를 저장하기 위해 주문 테이블에 가짜 주문을 넣어야 하는 상황'
              ),
              JSON_ARRAY(
                '갱신 이상',
                '동일한 정보가 여러 곳에 중복 저장되어 있어, 하나만 수정하면 데이터가 불일치하게 되는 문제',
                '고객 주소가 여러 행에 중복 저장되어 있는데, 일부만 수정되어 서로 다른 주소가 되는 경우'
              ),
              JSON_ARRAY(
                '삭제 이상',
                '어떤 정보를 삭제할 때, 의도하지 않은 다른 정보까지 함께 사라지는 문제',
                '마지막 주문 행을 삭제했더니, 고객 기본 정보도 함께 삭제되는 경우'
              )
            )
          )
        )
      ),

      /* -------------------------------------
         P.2.2.2 1NF, 2NF, 3NF 요약 표
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 2,
        'subCode', 'P.2.2.2',
        'title', '1정규형, 2정규형, 3정규형 요약',
        'importance', 4,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','정규형 단계별 핵심 조건'),
          JSON_OBJECT(
            'type','table',
            'caption','정규형 단계별 요약',
            'headers', JSON_ARRAY('정규형','핵심 조건','정리 포인트'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '1NF',
                '모든 속성이 원자값을 가진다. 반복되는 그룹이 없다.',
                '한 셀에 여러 값이 들어있거나, 같은 종류의 컬럼이 수많이 반복되는 구조를 제거한다.'
              ),
              JSON_ARRAY(
                '2NF',
                '부분 함수 종속 제거 (기본키의 일부분에만 종속된 속성 제거)',
                '복합키의 일부에만 의존하는 속성을 분리하여 별도 테이블로 만든다.'
              ),
              JSON_ARRAY(
                '3NF',
                '이행 함수 종속 제거 (키가 아닌 컬럼이 다른 키가 아닌 컬럼에 의존하는 경우 제거)',
                '예: 학번 → 학과코드 → 학과명 구조라면, 학과 정보를 별도 테이블로 분리한다.'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','실기에서 자주 보이는 표현'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '학생별 수강 과목과 교수 정보를 하나의 테이블에 모두 넣어 둔 상태에서, 정규화를 요구하는 문제',
              '주문 테이블에 고객 등급, 등급명, 등급 혜택까지 한 번에 들어있는 경우',
              '정규화 전 테이블 구조를 보고, 정규화 후 엔터티 수와 관계를 묻는 문제'
            )
          )
        )
      ),

      /* -------------------------------------
         P.2.2.3 반정규화가 필요한 순간과 주의점
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 3,
        'subCode', 'P.2.2.3',
        'title', '반정규화 적용 기준과 주의사항',
        'importance', 4,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','반정규화는 언제 사용하는가'),
          JSON_OBJECT(
            'type','paragraph',
            'text',
            '정규화만으로는 성능 요구를 만족하기 어려울 때, 의도적으로 중복을 허용하여 조회 성능을 높이는 기법이 반정규화입니다. 단, 데이터 정합성 유지에 대한 추가 비용이 발생하므로 신중하게 적용해야 합니다.'
          ),

          JSON_OBJECT(
            'type','table',
            'caption','반정규화 적용이 고려되는 상황',
            'headers', JSON_ARRAY('상황','적용 예시','주의할 점'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '조인 테이블이 너무 많아 조회가 항상 느린 경우',
                '자주 조회되는 컬럼을 메인 테이블에 추가로 저장',
                '중복된 컬럼 업데이트를 위한 트리거, 배치 작업 등을 고려해야 한다.'
              ),
              JSON_ARRAY(
                '집계 결과를 자주 조회해야 하는 경우',
                '일/주/월 단위 통계 테이블을 별도로 두어 미리 누적 저장',
                '통계 테이블 갱신 시점과 원본 데이터 시점을 맞춰야 한다.'
              ),
              JSON_ARRAY(
                '대량의 히스토리 데이터를 자주 조회하는 경우',
                '최근 데이터와 오래된 데이터를 다른 테이블이나 파티션에 분리',
                '업무상 필요한 조회 범위를 명확히 정의해야 한다.'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','시험에서 나오는 한 줄 요약'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '정규화는 중복과 이상현상을 줄이기 위한 설계 원칙이다.',
              '반정규화는 성능 개선을 위해 의도적으로 중복을 허용하는 설계 기법이다.',
              '반정규화 문제는 보통 "조회 성능 향상을 위한 설계 방안"이라는 표현과 함께 출제된다.'
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
