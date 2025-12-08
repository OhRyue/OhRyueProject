-- =========================================
-- 3.3.1 관계 데이터베이스 모델 (concept 보강)
-- topic_id = 13301
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 13301,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              3.3.1.1 관계 데이터 모델 개념
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '3.3.1.1',
             'title', '관계 데이터 모델 개념과 특징',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','관계 데이터 모델(Relation Data Model)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '관계 데이터 모델은 실세계 데이터를 행(Row)과 열(Column)로 구성된 테이블 형태의 릴레이션으로 표현하는 데이터 모델이다. '
                 '수학자 E.F. Codd가 제안했으며, 현재 대부분의 상용 DBMS가 채택하고 있는 기본 모델이다.'
               ),
               JSON_OBJECT('type','paragraph','text',
                 '관계 데이터 모델의 핵심은 데이터를 2차원 표 형태로 단순하고 직관적으로 표현하면서도, '
                 '관계 대수와 관계 해석에 기반한 이론적 토대를 통해 강력한 질의 기능과 데이터 무결성을 제공하는 데 있다.'
               )
             )
           ),

           /* -------------------------------------
              3.3.1.2 관계 데이터 모델 구성 요소
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '3.3.1.2',
             'title', '관계 데이터 모델 구성 요소',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','관계 데이터 모델 구성 요소 정리'),
               JSON_OBJECT(
                 'type','table',
                 'caption','관계 데이터 모델 구성요소',
                 'headers', JSON_ARRAY('구성요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('릴레이션(Relation)','행과 열로 구성된 이론적 테이블로, 한 개의 개체 집합 또는 관계 집합을 의미한다.'),
                   JSON_ARRAY('튜플(Tuple)','릴레이션의 한 행(Row)에 해당하는 데이터 단위로, 한 개체 인스턴스를 나타낸다.'),
                   JSON_ARRAY('속성(Attribute)','릴레이션의 열(Column)에 해당하는 항목으로, 데이터의 성격과 도메인을 정의한다.'),
                   JSON_ARRAY('카디널리티(Cardinality)','릴레이션에 포함된 튜플(Row)의 개수이다.'),
                   JSON_ARRAY('차수(Degree)','릴레이션에 포함된 속성(Attribute, Column)의 개수이다.'),
                   JSON_ARRAY('스키마(Schema)','데이터베이스의 구조와 제약조건 등의 정보를 담고 있는 논리적 정의이다.'),
                   JSON_ARRAY('인스턴스(Instance)','스키마에 따라 실제로 테이블에 저장된 데이터 집합을 의미한다.')
                 )
               )
             )
           ),

           /* -------------------------------------
              3.3.1.3 관계 대수
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '3.3.1.3',
             'title', '관계 대수: 집합 연산자와 순수 관계 연산자',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','관계 대수(Relational Algebra)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '관계 대수는 하나 이상의 릴레이션을 입력으로 받아 또 다른 릴레이션을 결과로 반환하는 절차적 언어이다. '
                 '원하는 결과를 얻기 위해 어떤 연산을 어떤 순서로 수행할지를 명시한다(How에 초점).'
               ),

               JSON_OBJECT('type','heading','text','일반 집합 연산자 (합·교·차·카)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','관계 대수의 일반 집합 연산자',
                 'headers', JSON_ARRAY('연산자','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('∪ (합집합, UNION)','두 릴레이션에 존재하는 튜플을 모두 합쳐 중복 없이 반환한다.'),
                   JSON_ARRAY('∩ (교집합, INTERSECTION)','두 릴레이션에 공통으로 존재하는 튜플만 반환한다.'),
                   JSON_ARRAY('- (차집합, DIFFERENCE)','첫 번째 릴레이션에는 존재하지만 두 번째 릴레이션에는 없는 튜플을 반환한다.'),
                   JSON_ARRAY('× (카티션 프로덕트, CARTESIAN PRODUCT)','두 릴레이션의 가능한 모든 튜플 조합을 생성한다.')
                 )
               ),

               JSON_OBJECT('type','heading','text','순수 관계 연산자 (셀·프·조·디)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','관계 대수의 순수 관계 연산자',
                 'headers', JSON_ARRAY('연산자','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('σ (셀렉트, Select)','조건을 만족하는 튜플(행)을 수평적으로 선택한다.'),
                   JSON_ARRAY('π (프로젝트, Project)','필요한 속성(열)만 수직적으로 선택한다.'),
                   JSON_ARRAY('⋈ (조인, Join)','공통 속성을 기준으로 두 릴레이션을 결합해 새로운 릴레이션을 생성한다.'),
                   JSON_ARRAY('÷ (디비전, Division)','한 릴레이션에서 다른 릴레이션에 포함된 모든 값을 만족하는 튜플을 구한다.')
                 )
               )
             )
           ),

           /* -------------------------------------
              3.3.1.4 관계 해석과 논리 기호
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '3.3.1.4',
             'title', '관계 해석과 논리 기호',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','관계 해석(Relational Calculus)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '관계 해석은 수학의 프레디킷 해석(Predicate Calculus)에 기반한 비절차적 언어로, '
                 '원하는 정보가 무엇인지(What)를 명시하고, 실제로 어떻게 계산할지는 시스템이 결정한다.'
               ),
               JSON_OBJECT('type','paragraph','text',
                 '관계 해석에는 튜플 관계 해석과 도메인 관계 해석이 있으며, 관계 대수와 기능적으로는 동등한 표현 능력을 가진다. '
                 '시험에서는 관계 대수는 절차적, 관계 해석은 비절차적이라는 대비와, 논리 기호 의미를 함께 묻는 경우가 많다.'
               ),

               JSON_OBJECT('type','heading','text','논리 연산자와 정량자'),
               JSON_OBJECT(
                 'type','table',
                 'caption','관계 해석에서 사용하는 논리 기호',
                 'headers', JSON_ARRAY('구분','기호','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('OR 연산','∨','두 원자식을 또는 관계로 연결한다.'),
                   JSON_ARRAY('AND 연산','∧','두 원자식을 그리고 관계로 연결한다.'),
                   JSON_ARRAY('NOT 연산','ㄱ','원자식에 대한 부정을 나타낸다.'),
                   JSON_ARRAY('전칭 정량자','∀','모든 가능한 튜플에 대해 조건이 참임을 의미한다.'),
                   JSON_ARRAY('존재 정량자','ꓱ','어떤 튜플 하나라도 존재함을 나타낸다.')
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);


-- =========================================
-- 3.3.2 데이터 모델링 및 설계 (concept 보강)
-- topic_id = 13302
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 13302,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              3.3.2.1 데이터 모델 개념과 설계 절차
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '3.3.2.1',
             'title', '데이터 모델 개념과 설계 절차',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','데이터 모델(Data Model)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '데이터 모델은 현실 세계의 정보를 인간과 컴퓨터가 이해할 수 있도록 추상화해 표현한 모델이다. '
                 '업무에서 사용하는 개체와 관계, 제약조건을 구조적으로 정리하여 데이터베이스 설계의 기반을 제공한다.'
               ),

               JSON_OBJECT('type','heading','text','데이터 모델에 포함되어야 할 요소 (논·연·제)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '논리적 데이터 구조: 개체, 속성, 관계 등을 어떻게 구조화할 것인지',
                   '연산(Operation): 데이터에 대해 수행할 수 있는 연산(삽입, 삭제, 갱신, 검색 등)',
                   '제약조건(Constraint): 무결성, 비즈니스 룰 등 데이터가 만족해야 할 규칙'
                 )
               ),

               JSON_OBJECT('type','heading','text','데이터 모델링 절차 (개·논·물)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '개념적 데이터 모델: 업무 개념과 비즈니스 규칙을 개체·관계 수준에서 표현',
                   '논리적 데이터 모델: 특정 DBMS에 독립적으로 릴레이션 구조와 키, 제약조건을 설계',
                   '물리적 데이터 모델: 실제 저장 구조, 인덱스, 파티션, 파일 구조 등을 설계'
                 )
               ),

               JSON_OBJECT('type','heading','text','논리 설계 단계와 물리 설계 단계 작업'),
               JSON_OBJECT(
                 'type','table',
                 'caption','논리 설계 vs 물리 설계 주요 작업',
                 'headers', JSON_ARRAY('단계','주요 작업'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('논리 설계(논리 데이터 모델)','논리적 데이터베이스 구조로 매핑, 트랜잭션 인터페이스 설계, 스키마 평가 및 정제'),
                   JSON_ARRAY('물리 설계(물리 데이터 모델)','저장 레코드 양식 설계, 레코드 집중 분석 및 설계, 접근 경로(인덱스 등) 설계')
                 )
               )
             )
           ),

           /* -------------------------------------
              3.3.2.2 E-R 모델과 다이어그램
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '3.3.2.2',
             'title', '개체-관계(E-R) 모델과 다이어그램',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','개체-관계(E-R) 모델의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 'E-R 모델은 현실 세계를 개체(Entity), 속성(Attribute), 관계(Relationship)로 표현하는 개념적 데이터 모델이다. '
                 '데이터베이스 설계의 초기 단계에서 요구사항을 시각적으로 표현하는 데 사용된다.'
               ),

               JSON_OBJECT('type','heading','text','E-R 모델의 구성 요소 (개·속·관)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '개체(Entity): 독립적으로 식별 가능한 객체나 개념 (예: 학생, 과목, 주문)',
                   '속성(Attribute): 개체가 가지는 특성이나 성질 (예: 학번, 이름, 수강년도)',
                   '관계(Relationship): 개체 간의 연관성 (예: 학생-과목 수강 관계)'
                 )
               ),

               JSON_OBJECT('type','heading','text','E-R 다이어그램 기본 기호'),
               JSON_OBJECT(
                 'type','table',
                 'caption','개체-관계 다이어그램 기호',
                 'headers', JSON_ARRAY('구성','기호','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('개체(Entity)','□','사각형으로 표현한다.'),
                   JSON_ARRAY('관계(Relationship)','◇','마름모로 표현하며, 참여하는 개체들을 선으로 연결한다.'),
                   JSON_ARRAY('속성(Attribute)','○','타원으로 표현하며, 해당 개체나 관계와 선으로 연결한다.'),
                   JSON_ARRAY('다중 값 속성','◎','이중 타원으로 표현하여 하나의 개체에 여러 값이 올 수 있음을 나타낸다.'),
                   JSON_ARRAY('관계-속성 연결','ㅡ','개체, 관계, 속성을 연결하는 선')
                 )
               )
             )
           ),

           /* -------------------------------------
              3.3.2.3 정규화와 이상 현상
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '3.3.2.3',
             'title', '데이터베이스 정규화와 이상 현상',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','데이터베이스 정규화(DB Normalization)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '정규화는 관계형 데이터 모델에서 데이터의 중복성을 제거하고, 삽입·삭제·갱신 이상을 방지하기 위해 릴레이션을 무손실 분해하는 과정이다. '
                 '정규화를 통해 데이터 일관성과 저장 효율을 높일 수 있다.'
               ),

               JSON_OBJECT('type','heading','text','정규화의 목적'),
               JSON_OBJECT('type','list','items', JSON_ARRAY(
                 '중복 데이터를 최소화하여 테이블 불일치 위험을 줄인다.',
                 '삽입, 삭제, 갱신 이상을 최소화해 데이터 구조의 안정성을 높인다.',
                 '어떠한 릴레이션이라도 데이터베이스 내에서 표현 가능하도록 구조를 명확히 한다.',
                 '데이터 삽입 시 릴레이션 재구성 필요성을 줄이고, 효율적인 검색 알고리즘 설계를 돕는다.'
               )),

               JSON_OBJECT('type','heading','text','이상 현상(Anomaly)의 종류 (삽·삭·갱)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','이상 현상 분류',
                 'headers', JSON_ARRAY('종류','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('삽입 이상','불필요한 속성까지 함께 입력해야 하거나, 일부 정보만으로는 튜플을 삽입할 수 없는 문제'),
                   JSON_ARRAY('삭제 이상','특정 정보 삭제 시, 의도하지 않은 다른 정보까지 함께 삭제되는 문제'),
                   JSON_ARRAY('갱신 이상','한 곳의 데이터를 수정했는데 다른 튜플에 같은 정보가 남아 일관성이 깨지는 문제')
                 )
               ),

               JSON_OBJECT('type','heading','text','정규화 단계 (원·부·이·결·다·조)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','정규형별 조건',
                 'headers', JSON_ARRAY('정규형','조건'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('1정규형(1NF)','모든 속성 값이 더 이상 분해할 수 없는 원자 값으로만 구성된다.'),
                   JSON_ARRAY('2정규형(2NF)','1NF를 만족하면서, 기본키의 부분 집합에만 종속하는 부분 함수 종속이 제거된다.'),
                   JSON_ARRAY('3정규형(3NF)','2NF를 만족하면서, 기본키가 아닌 속성 간의 이행 함수 종속을 제거한다.'),
                   JSON_ARRAY('보이스-코드 정규형(BCNF)','모든 결정자가 후보키가 되도록 함수 종속을 재구성한다.'),
                   JSON_ARRAY('4정규형(4NF)','다치(다중 값) 종속성을 제거한다.'),
                   JSON_ARRAY('5정규형(5NF)','조인 종속성을 제거하여, 조인으로만 복원 가능한 최소 단위로 분해한다.')
                 )
               )
             )
           ),

           /* -------------------------------------
              3.3.2.4 논리 데이터 모델 품질 검증
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '3.3.2.4',
             'title', '논리 데이터 모델 품질 검증 기준',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','논리 데이터 모델 품질 검증의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '논리 데이터 모델 품질 검증은 설계된 데이터 모델이 실제 업무 환경의 요구사항을 시스템적으로 구현할 수 있는지를 객관적으로 평가하는 과정이다.'
               ),

               JSON_OBJECT('type','heading','text','데이터 모델이 갖추어야 할 요건'),
               JSON_OBJECT('type','list','items', JSON_ARRAY(
                 '완전성: 필요한 데이터와 관계가 빠짐없이 표현되어 있는가',
                 '중복 배제: 불필요한 중복 데이터가 최소화되어 있는가',
                 '비즈니스 룰 반영: 업무 규칙이 무결성 제약 등으로 정확히 표현되어 있는가',
                 '데이터 재사용성: 다양한 응용에서 공통 데이터를 재사용하기 용이한 구조인가',
                 '안정성 및 확장성: 요구사항 변경에 유연하게 대응 가능한가',
                 '간결성: 불필요하게 복잡하지 않고 이해하기 쉬운가',
                 '의사소통: 업무 담당자와 설계자 간 의사소통에 도움이 되는 표현인가',
                 '통합성: 시스템 간 통합과 연계를 고려한 구조인가'
               )),

               JSON_OBJECT('type','heading','text','데이터 모델 품질 검증 기준 (정·완·준·최·일·활)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','논리 데이터 모델 품질 기준',
                 'headers', JSON_ARRAY('기준','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('정확성','업무 규칙과 데이터 의미를 정확하게 반영하고 있는가'),
                   JSON_ARRAY('완전성','필요한 개체와 속성이 누락 없이 포함되어 있는가'),
                   JSON_ARRAY('준거성','표준 용어, 표준 코드 등 조직의 기준에 부합하는가'),
                   JSON_ARRAY('최신성','최근 업무 변경사항이 모델에 적시에 반영되었는가'),
                   JSON_ARRAY('일관성','중복이나 모순 없이 일관된 구조를 유지하는가'),
                   JSON_ARRAY('활용성','응용 프로그램, 리포트, 통계 등 실제 활용 측면에서 유용한가')
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);