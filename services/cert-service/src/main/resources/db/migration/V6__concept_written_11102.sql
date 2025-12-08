-- ============================
-- 1.1.2 UML (요구사항 확인 내 UML 개념)
-- topic_id = 11102
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 11102,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              1.1.2.1 UML 개념과 도입 효과
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.1.2.1',
             'title', 'UML의 개념과 도입 효과',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','UML(Unified Modeling Language)의 개념'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'UML은 객체지향 소프트웨어 개발 과정에서 시스템을 명세화·시각화·문서화하기 위해 사용되는 표준 모델링 언어로, 여러 객체지향 방법론의 모델링 기법을 통합해 만든 범용 표준이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '객체지향 분석·설계 산출물을 일관된 표기법으로 표현할 수 있다.',
                   '비즈니스 프로세스·시스템 구조·동작을 시각적으로 표현해 이해관계자 간 의사소통을 돕는다.',
                   '개발 방법론과 무관하게 공통 표기법을 제공해 표준화에 기여한다.'
                 )
               ),

               JSON_OBJECT('type','heading','text','UML 도입 효과'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '개발 기획과 산출물에 대한 이해관계자(전문가·비전문가) 간 의사소통을 용이하게 한다.',
                   '복잡한 요구사항을 구조화된 다이어그램으로 표현해 누락·모호성을 줄인다.',
                   '설계 모델을 기초로 코드 생성·역공학 등을 수행해 생산성과 유지보수성을 높일 수 있다.'
                 )
               ),

               JSON_OBJECT('type','heading','text','UML의 특징 (가구명문)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','UML의 네 가지 주요 특징 (가·구·명·문)',
                 'headers', JSON_ARRAY('특징','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('가시화 언어','개념 모델을 그림으로 표현해 오류를 줄이고 의사소통을 쉽게 한다.'),
                   JSON_ARRAY('구축 언어','모델을 기반으로 실제 실행 시스템을 예측·구축할 수 있으며, 코드 생성·역공학이 가능하다.'),
                   JSON_ARRAY('명세화 언어','정확하고 완전한 모델을 명세할 수 있도록 표준화된 표기법을 제공한다.'),
                   JSON_ARRAY('문서화 언어','시스템 구조와 동작을 문서화해 평가·검토·유지보수에 활용할 수 있다.')
                 )
               )
             )
           ),

           /* -------------------------------------
              1.1.2.2 UML 구성요소와 사물(Things)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.1.2.2',
             'title', 'UML 구성요소와 사물(Things)의 종류',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','UML의 기본 구성요소'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '사물(Things): 모델을 구성하는 기본 요소',
                   '관계(Relationships): 사물과 사물 간 연결',
                   '다이어그램(Diagrams): 사물과 관계를 특정 관점에서 모아 표현한 그림'
                 )
               ),

               JSON_OBJECT('type','heading','text','사물(Things)의 종류'),
               JSON_OBJECT(
                 'type','table',
                 'caption','UML 사물(Things) 분류',
                 'headers', JSON_ARRAY('구분','설명','예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '구조 사물(Structural Things)',
                     '시스템의 정적인 구조·구성 요소를 표현',
                     '클래스(Class), 유스케이스(Use Case), 컴포넌트(Component), 노드(Node)'
                   ),
                   JSON_ARRAY(
                     '행동 사물(Behavioral Things)',
                     '시간에 따른 행위·상호작용을 표현',
                     '상호작용(Interaction), 상태 머신(State Machine)'
                   ),
                   JSON_ARRAY(
                     '그룹 사물(Grouping Things)',
                     '요소들을 논리적으로 묶어 관리',
                     '패키지(Package)'
                   ),
                   JSON_ARRAY(
                     '주해 사물(Annotation Things)',
                     '모델 요소에 대한 부가 설명·제약조건 표현',
                     '노트(Note)'
                   )
                 )
               ),

               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '시험에서는 사물(Things)의 네 종류와 각각에 해당하는 예시(클래스, 패키지, 노트 등)를 매칭하는 문제가 자주 출제된다.'
               )
             )
           ),

           /* -------------------------------------
              1.1.2.3 UML 다이어그램 분류와 주요 다이어그램
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.1.2.3',
             'title', 'UML 다이어그램 분류와 핵심 다이어그램',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','UML 다이어그램의 두 가지 관점'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '구조적(정적) 다이어그램: 시스템의 정적인 구조(클래스, 구성요소, 배치 등)를 표현',
                   '행위적(동적) 다이어그램: 시간에 따른 행위·상호작용(유스케이스, 시퀀스, 상태, 활동 등)을 표현'
                 )
               ),

               JSON_OBJECT(
                 'type','table',
                 'caption','구조적(정적) 다이어그램 (클객 컴배 복패)',
                 'headers', JSON_ARRAY('다이어그램','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('클래스(Class)','클래스와 속성, 연산, 관계를 표현하는 대표적인 정적 구조 다이어그램'),
                   JSON_ARRAY('객체(Object)','실제 객체 인스턴스와 그 관계를 표현'),
                   JSON_ARRAY('컴포넌트(Component)','구성요소 간 의존 관계를 표현'),
                   JSON_ARRAY('배치(Deployment)','노드와 그 위에 배치된 컴포넌트를 표현'),
                   JSON_ARRAY('복합체 구조(Composite Structure)','클래스 내부 구조와 협력을 표현'),
                   JSON_ARRAY('패키지(Package)','패키지 간 의존 관계를 표현')
                 )
               ),

               JSON_OBJECT(
                 'type','table',
                 'caption','행위적(동적) 다이어그램 (유시커 상활타)',
                 'headers', JSON_ARRAY('다이어그램','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('유스케이스(Use Case)','사용자 관점에서 시스템이 제공하는 기능(사용 사례)을 표현'),
                   JSON_ARRAY('시퀀스(Sequence)','객체 간 메시지 흐름을 시간 순서대로 표현'),
                   JSON_ARRAY('커뮤니케이션(Communication)','객체 간 상호작용과 링크 중심으로 표현'),
                   JSON_ARRAY('상태(State)','이벤트에 따른 객체의 상태 변화 표현'),
                   JSON_ARRAY('활동(Activity)','처리 흐름·업무 절차를 단계적으로 표현'),
                   JSON_ARRAY('타이밍(Timing)','시간 축을 기준으로 상태 변화 타이밍을 상세히 표현')
                 )
               ),

               JSON_OBJECT('type','heading','text','대표 다이어그램 간단 요약'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '클래스 다이어그램: 객체지향 모델링 시 정적인 관계(클래스·속성·연산·연관)를 표현.',
                   '유스케이스 다이어그램: 사용자 목표·기능 요구사항을 액터와 유스케이스 관계로 표현.',
                   '시퀀스 다이어그램: 수직 축을 시간으로 보고, 객체 간 메시지 교환 순서를 강조.',
                   '상태 다이어그램: 이벤트에 따른 객체 상태 전이를 표현(동적 모델링).',
                   '활동 다이어그램: 처리 과정·업무 흐름을 단계적으로 표현(프로세스 관점).'
                 )
               )
             )
           ),

           /* -------------------------------------
              1.1.2.4 UML 관계와 스테레오타입
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '1.1.2.4',
             'title', 'UML 관계(Relationships)와 스테레오타입',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','UML 관계(Relationships)의 종류'),
               JSON_OBJECT(
                 'type','table',
                 'caption','주요 UML 관계와 표기',
                 'headers', JSON_ARRAY('관계','표기 특징','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '연관(Association)',
                     '실선(필요 시 화살표)',
                     '두 사물이 서로 관련되어 있음을 표현. 양방향일 경우 화살표 생략 가능.'
                   ),
                   JSON_ARRAY(
                     '집합(Aggregation)',
                     '포함하는 쪽에 속이 빈 마름모',
                     '부분과 전체 관계이지만, 부분이 독립적으로 존재할 수 있는 약한 포함 관계.'
                   ),
                   JSON_ARRAY(
                     '포함(Composition)',
                     '포함하는 쪽에 속이 찬(채워진) 마름모',
                     '전체가 사라지면 부분도 함께 사라지는 강한 포함 관계.'
                   ),
                   JSON_ARRAY(
                     '일반화(Generalization)',
                     '하위 → 상위 방향의 속이 빈 삼각형 화살표',
                     '상속 관계. 상위(일반) 개념과 하위(구체) 개념을 표현.'
                   ),
                   JSON_ARRAY(
                     '의존(Dependency)',
                     '점선 화살표',
                     '한 요소의 변화가 다른 요소에 영향을 미치는 느슨한 의존 관계 (매개변수, 지역변수 등).'
                   ),
                   JSON_ARRAY(
                     '실체화(Realization)',
                     '점선 + 속이 빈 삼각형 화살표',
                     '인터페이스 구현과 같이, 사물이 수행해야 할 기능을 실제로 제공하는 관계.'
                   )
                 )
               ),

               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '관계 유형은 영문 이름(Association, Aggregation, Composition, Generalization, Dependency, Realization)과 함께 표기법을 묻는 문제가 자주 나온다.'
               ),

               JSON_OBJECT('type','heading','text','UML 확장 모델 – 스테레오타입(Stereotype)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '스테레오타입은 UML 기본 요소를 확장해 새로운 의미를 부여하는 메커니즘으로, 길러멧(« ») 기호로 표현한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','대표 스테레오타입 예시',
                 'headers', JSON_ARRAY('스테레오타입','설명','예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('«include»','하나의 유스케이스 수행 시 반드시 다른 유스케이스를 포함해 실행','공통 로그인 절차를 여러 유스케이스에서 포함'),
                   JSON_ARRAY('«extend»','특정 조건에서만 추가 유스케이스를 확장 실행','결제 유스케이스에서만 실행되는 쿠폰 적용 기능'),
                   JSON_ARRAY('«interface»','모든 메서드가 추상인 인터페이스를 표현','서비스 인터페이스 정의 클래스'),
                   JSON_ARRAY('«entity»','데이터를 저장·관리하는 엔티티 클래스를 표현','주문, 회원, 상품 등 도메인 엔티티'),
                   JSON_ARRAY('«boundary»','시스템과 외부 액터 간 경계를 담당하는 클래스','화면, API 엔드포인트 등'),
                   JSON_ARRAY('«control»','비즈니스 로직·흐름 제어를 담당하는 클래스','서비스, 유스케이스 제어 객체')
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);