-- ============================
-- 1.3.2 객체지향 설계
-- topic_id = 11302 (@topic_oop_design)
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 11302,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              1.3.2.1 객체지향 기본 개념과 구성요소
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.3.2.1',
             'title', '객체지향 기본 개념과 구성요소',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','객체지향(Object Oriented) 개념'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '객체지향은 실세계의 개체를 “속성 + 메서드”가 결합된 객체로 표현하고, '
                 '이 객체들의 관계와 협력을 중심으로 소프트웨어를 설계·구현하는 기법이다.'
               ),

               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '현실 세계의 비즈니스(업무)를 객체, 속성, 클래스, 전체–부분 관계 등으로 나누어 분석·설계한다.',
                   '데이터(상태)와 이를 처리하는 연산(행위)을 하나의 객체 안에 묶어 모듈성을 높인다.',
                   '절차(프로시저) 중심이 아니라 “객체와 그 사이의 메시지 교환”을 중심으로 생각한다.'
                 )
               ),

               JSON_OBJECT('type','heading','text','객체지향 구성요소 (클객메 메인속)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','객체지향 핵심 구성요소',
                 'headers', JSON_ARRAY('구성요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '클래스 (Class)',
                     '공통된 속성과 연산을 가지는 객체들의 집합. 하나의 “타입” 또는 설계도(템플릿).'
                   ),
                   JSON_ARRAY(
                     '객체 (Object)',
                     '클래스에서 생성된 실체. 고유한 상태·행위·식별자를 가진 소프트웨어 모듈.'
                   ),
                   JSON_ARRAY(
                     '메서드 (Method)',
                     '객체가 수행할 수 있는 동작을 정의한 것. 객체 사용 방법, 메시지 처리 로직.'
                   ),
                   JSON_ARRAY(
                     '메시지 (Message)',
                     '객체에게 특정 연산을 수행하라고 “지시”하는 호출. 메서드 호출의 개념과 유사.'
                   ),
                   JSON_ARRAY(
                     '인스턴스 (Instance)',
                     '클래스에 속한 각각의 구체적인 객체. “클래스의 실제화(Instance화) 결과”.'
                   ),
                   JSON_ARRAY(
                     '속성 (Property)',
                     '객체가 가지는 데이터 항목. 객체의 상태를 구성하는 필드/변수.'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','객체지향 핵심 기법 (캡상다추정관)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','객체지향 6대 기법',
                 'headers', JSON_ARRAY('기법','핵심 포인트'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '캡슐화 (Encapsulation)',
                     '관련 데이터와 연산을 클래스 안에 묶고, 필요한 인터페이스만 외부에 공개해 내부 구현을 숨긴다. 정보 은닉과 밀접.'
                   ),
                   JSON_ARRAY(
                     '상속성 (Inheritance)',
                     '상위 클래스의 속성과 메서드를 하위 클래스가 물려받아 재사용하는 기법. 공통 기능 공유·확장에 사용.'
                   ),
                   JSON_ARRAY(
                     '다형성 (Polymorphism)',
                     '같은 메시지(메서드 호출)에 대해 객체 유형에 따라 다른 방식으로 응답하는 능력. 오버로딩/오버라이딩으로 구현.'
                   ),
                   JSON_ARRAY(
                     '추상화 (Abstraction)',
                     '공통 성질만 추출해 클래스로 정의하는 것. 불필요한 세부사항을 숨기고 핵심 개념만 남긴다.'
                   ),
                   JSON_ARRAY(
                     '정보 은닉 (Information Hiding)',
                     '내부 데이터·구현은 숨기고, 공개된 인터페이스로만 접근하게 하여 독립성과 변경 용이성을 높인다.'
                   ),
                   JSON_ARRAY(
                     '관계성 / 연관성 (Relationship)',
                     'is member of / is instance of / is part of / is a 와 같은 객체·클래스 간 관계를 모델링하는 기법.'
                   )
                 )
               ),

               JSON_OBJECT('type','callout',
                 'title','시험 포인트',
                 'body',
                 '· “객체 = 상태 + 행위 + 식별자” 정의\n'
                 '· 객체지향 6대 기법 암기(캡·상·다·추·정·관)\n'
                 '· 캡슐화–정보은닉–모듈 독립성 향상 효과를 함께 묻는 문제 자주 출제'
               )
             )
           ),

           /* -------------------------------------
              1.3.2.2 SOLID 객체지향 설계 원칙
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.3.2.2',
             'title', 'SOLID 객체지향 설계 원칙',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','SOLID 5대 원칙 개요'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'SOLID 원칙은 변경에 강하고 재사용성이 높은 객체지향 설계를 위한 5가지 기본 규칙으로, '
                 '정보처리기사에서 매우 자주 출제되는 영역이다.'
               ),

               JSON_OBJECT(
                 'type','table',
                 'caption','SOLID 5대 원칙',
                 'headers', JSON_ARRAY('약어','원칙명','핵심 내용'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'SRP',
                     '단일 책임 원칙 (Single Responsibility)',
                     '하나의 클래스는 “하나의 목적” 또는 “하나의 변경 이유”만 가져야 한다.'
                   ),
                   JSON_ARRAY(
                     'OCP',
                     '개방-폐쇄 원칙 (Open–Closed)',
                     '기능 확장에는 열려 있고(확장 가능), 기존 코드 변경에는 닫혀 있어야 한다(변경 최소화).'
                   ),
                   JSON_ARRAY(
                     'LSP',
                     '리스코프 치환 원칙 (Liskov Substitution)',
                     '하위 타입은 상위 타입으로 대체 가능해야 한다. (하위 클래스는 상위 클래스가 사용되는 곳에서 정상 동작해야 함).'
                   ),
                   JSON_ARRAY(
                     'ISP',
                     '인터페이스 분리 원칙 (Interface Segregation)',
                     '클라이언트는 자신이 사용하지 않는 메서드에 의존하면 안 된다. 인터페이스는 작고 역할별로 분리한다.'
                   ),
                   JSON_ARRAY(
                     'DIP',
                     '의존 역전 원칙 (Dependency Inversion)',
                     '구체(Concrete)가 아니라 추상(Abstraction)에 의존하도록 설계해, 상위·하위 모듈 모두 추상에 의존하게 만든다.'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','정보 은닉과 SOLID의 연결'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '정보 은닉은 내부 구현을 감추고 인터페이스만 노출해, SRP/OCP를 지키기 쉽도록 도와준다.',
                   'DIP를 적용하면 구현 클래스를 바꾸더라도 인터페이스(추상)에 의존한 코드 변경을 최소화할 수 있다.',
                   'ISP는 “굳이 필요 없는 메서드가 섞인 거대한 인터페이스”를 쪼개, 캡슐화·응집도를 높이는 효과가 있다.'
                 )
               ),

               JSON_OBJECT('type','callout',
                 'title','시험 포인트',
                 'body',
                 '· 약어 ↔ 한글 명칭 ↔ 정의를 정확히 매칭할 수 있어야 함\n'
                 '· “이 상황에서 위반된 원칙은?” 형태의 지문 문제 출제 빈도 높음\n'
                 '· DIP: “추상에 의존, 구체에 의존하지 않는다”라는 문구 그대로 기억해 두기'
               )
             )
           ),

           /* -------------------------------------
              1.3.2.3 객체지향 분석·설계 방법론 & 럼바우
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.3.2.3',
             'title', '객체지향 분석·설계 방법론 & 럼바우',
             'importance', 3,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','대표 객체지향 방법론 비교'),
               JSON_OBJECT(
                 'type','table',
                 'caption','주요 OO 방법론',
                 'headers', JSON_ARRAY('방법론','제안자','핵심 특징'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'OOSE',
                     '야콥슨 (Jacobson)',
                     '유스케이스 중심 방법론. 유스케이스를 모든 모델의 근간으로 활용, 기능 요구사항 위주.'
                   ),
                   JSON_ARRAY(
                     'OMT',
                     '럼바우 (Rumbaugh)',
                     '객체지향 분석–시스템 설계–오브젝트 설계–구현 4단계. 객체/동적/기능 모델링을 모두 사용.'
                   ),
                   JSON_ARRAY(
                     'OOD',
                     '부치 (Booch)',
                     '설계 문서화와 다이어그램 중심. Micro/Macro 프로세스를 모두 사용하는 설계 방법론.'
                   ),
                   JSON_ARRAY(
                     'OOA',
                     'Coad & Yourdon',
                     '객체 식별, 구조/주체/관계/서비스 정의 등으로 구성되는 분석 방법. ER 다이어그램 활용.'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','럼바우(Rumbaugh) 객체지향 분석 기법 (객·동·기)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','럼바우 3대 모델',
                 'headers', JSON_ARRAY('모델','사용 다이어그램','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '객체 모델링(Object Modeling)',
                     '객체 다이어그램',
                     '정보(데이터) 모델링. 시스템에서 필요한 객체, 속성, 연산, 관계를 규정하는 가장 핵심·선행 모델.'
                   ),
                   JSON_ARRAY(
                     '동적 모델링(Dynamic Modeling)',
                     '상태 다이어그램(상태도)',
                     '시간 흐름에 따른 객체 간 제어 흐름, 상태 변화, 동작 순서를 표현하는 모델.'
                   ),
                   JSON_ARRAY(
                     '기능 모델링(Functional Modeling)',
                     '자료 흐름도(DFD)',
                     '프로세스들 간 자료 흐름을 중심으로 처리 과정을 표현하는 모델.'
                   )
                 )
               ),

               JSON_OBJECT('type','callout',
                 'title','시험 포인트',
                 'body',
                 '· “럼바우 = 객체/동적/기능 모델링(객·동·기)” 세트로 통째로 기억\n'
                 '· 각 모델 ↔ 사용하는 다이어그램(객체/상태도/DFD) 매칭 문제 자주 출제'
               )
             )
           ),

           /* -------------------------------------
              1.3.2.4 디자인 패턴 & GoF 분류
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '1.3.2.4',
             'title', '디자인 패턴 & GoF 분류',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','디자인 패턴 개념과 장단점'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '디자인 패턴은 소프트웨어 설계에서 자주 반복되는 문제에 대한 검증된 해결 방법(재사용 가능한 설계 템플릿)이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '장점: 구조 파악이 쉽고, 재사용을 통한 개발 시간 단축, 설계 변경에 유연한 대응, 개발자 간 의사소통 용이.',
                   '단점: 객체지향 중심으로 쓰이므로 초기 학습·적용 비용이 들고, 절차적 설계에는 바로 적용하기 어렵다.'
                 )
               ),

               JSON_OBJECT('type','heading','text','디자인 패턴 구성요소 (패문솔 사결샘)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','디자인 패턴 정의 시 포함 요소',
                 'headers', JSON_ARRAY('구성요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('패턴 이름','패턴을 식별할 수 있는 이름. 팀 내 공통 언어 역할.'),
                   JSON_ARRAY('문제 및 배경','어떤 상황에서 어떤 문제를 해결하기 위해 사용하는지 기술.'),
                   JSON_ARRAY('솔루션','구조·참여 객체·협력 방식 등 해결 방법의 일반적인 구조.'),
                   JSON_ARRAY('사례','실제 적용 예시 및 사용 시점.'),
                   JSON_ARRAY('결과','장단점, 트레이드오프, 적용 결과.'),
                   JSON_ARRAY('샘플 코드','구체적인 구현 예.')
                 )
               ),

               JSON_OBJECT('type','heading','text','GoF 디자인 패턴 분류 (생·구·행)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','목적 기준 3분류',
                 'headers', JSON_ARRAY('분류','설명','대표 패턴 예'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '생성 패턴 (Creational)',
                     '객체 생성 과정을 캡슐화해, 어떤 객체를 어떻게 생성할지 유연하게 결정하는 패턴.',
                     'Builder, Prototype, Factory Method, Abstract Factory, Singleton'
                   ),
                   JSON_ARRAY(
                     '구조 패턴 (Structural)',
                     '클래스·객체를 더 큰 구조로 조합하는 방법에 대한 패턴.',
                     'Adapter, Facade, Decorator, Composite, Proxy, Bridge, Flyweight'
                   ),
                   JSON_ARRAY(
                     '행위 패턴 (Behavioral)',
                     '객체 간 책임 분배, 상호작용 방식, 알고리즘 캡슐화에 관한 패턴.',
                     'Strategy, Observer, State, Command, Template Method, Iterator, Visitor 등'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','대표 패턴 한 줄 정리'),
               JSON_OBJECT(
                 'type','table',
                 'caption','자주 출제되는 패턴 요약',
                 'headers', JSON_ARRAY('패턴','분류','요약'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'Singleton',
                     '생성',
                     '시스템에서 유일한 인스턴스 하나만 생성·공유하도록 보장하는 패턴.'
                   ),
                   JSON_ARRAY(
                     'Factory Method',
                     '생성',
                     '객체 생성을 서브클래스에 위임해, 어떤 구상 클래스가 생성될지 상위 클래스가 몰라도 되게 하는 패턴.'
                   ),
                   JSON_ARRAY(
                     'Adapter',
                     '구조',
                     '서로 다른 인터페이스를 가진 클래스를 연결해, 기존 코드를 수정하지 않고 재사용하도록 돕는 패턴.'
                   ),
                   JSON_ARRAY(
                     'Decorator',
                     '구조',
                     '원본 코드를 변경하지 않고 객체의 책임(기능)을 동적으로 추가하는 패턴.'
                   ),
                   JSON_ARRAY(
                     'Strategy',
                     '행위',
                     '알고리즘 군을 캡슐화하고, 실행 중에도 서로 교체 가능하게 만드는 패턴.'
                   ),
                   JSON_ARRAY(
                     'Template Method',
                     '행위',
                     '알고리즘 골격은 상위 클래스에 두고, 상세 단계는 하위 클래스에서 구현하도록 하는 패턴.'
                   )
                 )
               ),

               JSON_OBJECT('type','callout',
                 'title','시험 포인트',
                 'body',
                 '· “생성/구조/행위” 3분류에 각 패턴 이름을 정확히 매칭하는 문제가 자주 출제\n'
                 '· Singleton, Factory Method, Adapter, Strategy, Template Method는 개념·예시 모두 익혀 두기\n'
                 '· 패턴의 의도(언제 쓰는지)를 한 줄로 설명할 수 있도록 정리해 두면 응용형 문제에 대응하기 쉽다'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);
