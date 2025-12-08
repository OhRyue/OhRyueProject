-- =========================================
-- 4.2.1 기본문법 활용 (concept 보강)
-- topic_id = 14201
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 14201,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              4.2.1.1 데이터 타입과 변수
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '4.2.1.1',
             'title', '데이터 타입과 변수',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','데이터 타입(Data Type)의 기본 분류 (불·문·열·정·부)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','데이터 타입 유형',
                 'headers', JSON_ARRAY('유형','예시','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('블린(Boolean)','true / false','논리값 표현'),
                   JSON_ARRAY('문자(Char)','''A''','단일 문자 1개'),
                   JSON_ARRAY('문자열(String)','"Hello"','문자들의 연속'),
                   JSON_ARRAY('정수(Int)','10, -3','소수점 없는 정수 값'),
                   JSON_ARRAY('부동 소수점(Float/Double)','3.14','실수 표현, 근사값 저장')
                 )
               ),

               JSON_OBJECT('type','heading','text','변수(Variable)의 개념과 작성 규칙'),
               JSON_OBJECT('type','paragraph','text',
                 '변수는 값을 저장하기 위해 주기억장치에 확보하는 이름 있는 저장 공간이다. '
                 '프로그래밍 언어마다 문법은 다르지만, 공통적인 네이밍 규칙이 존재한다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '영문 대/소문자, 숫자, 밑줄(_)·달러($)만 사용 가능',
                   '첫 글자는 숫자로 시작할 수 없음',
                   '공백 사용 불가',
                   '예약어(for, if, while 등)는 변수명으로 사용할 수 없음'
                 )
               )
             )
           ),

           /* -------------------------------------
              4.2.1.2 배열과 선언 예시
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '4.2.1.2',
             'title', '배열(Array) 기본 개념과 C 배열 선언',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','배열(Array)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '배열은 같은 타입의 변수들을 연속된 메모리 공간에 모아 둔 집합으로, '
                 '인덱스를 이용해 각 요소에 접근한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','C/C++ 배열 선언 예시',
                 'headers', JSON_ARRAY('구분','선언 형태','초기화 결과 설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '초깃값 없음',
                     'int b[5];',
                     '길이 5의 int 배열 생성, 기본적으로 0으로 초기화된다고 가정(b = [0,0,0,0,0])'
                   ),
                   JSON_ARRAY(
                     '초깃값 있음',
                     'int b[9] = {1,2,3};',
                     '앞 3개는 1,2,3으로 초기화, 나머지 6개는 0으로 초기화'
                   )
                 )
               )
             )
           ),

           /* -------------------------------------
              4.2.1.3 연산자 종류와 삼항/비트 연산
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '4.2.1.3',
             'title', '연산자 분류와 비트·삼항 연산자',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','연산자(Operator)의 기본 분류'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '산술 연산자: +, -, *, /, % 등',
                   '시프트 연산자: <<, >>',
                   '관계 연산자: >, <, >=, <=, ==, !=',
                   '논리 연산자: &&, ||, !',
                   '비트 연산자: &, |, ^, ~',
                   '대입 연산자: =, +=, -=, ...',
                   '증감 연산자: ++, --',
                   '삼항 연산자: (조건식) ? A : B'
                 )
               ),
               JSON_OBJECT('type','heading','text','비트/삼항 연산자 상세'),
               JSON_OBJECT(
                 'type','table',
                 'caption','비트·삼항 연산자 정리',
                 'headers', JSON_ARRAY('연산자','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('&','두 비트가 모두 1이면 1, 아니면 0'),
                   JSON_ARRAY('|','두 비트 중 하나라도 1이면 1'),
                   JSON_ARRAY('^','두 비트가 다르면 1, 같으면 0'),
                   JSON_ARRAY('~','1은 0으로, 0은 1로 반전'),
                   JSON_ARRAY('(조건식) ? a : b','조건이 참이면 a 실행, 거짓이면 b 실행')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.2.1.4 명령문(조건/반복)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '4.2.1.4',
             'title', '명령문(Statement) – 조건문과 반복문',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','명령문(Statement)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '명령문은 프로그램을 구성하는 기본 실행 단위로, 조건 분기와 반복 제어 등을 통해 '
                 '프로그램의 흐름을 정의한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','대표적인 조건/반복문',
                 'headers', JSON_ARRAY('구분','예시','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('조건문','if 문','조건식이 참일 때 특정 블록 실행'),
                   JSON_ARRAY('조건문','switch/case 문','값에 따라 여러 분기 중 하나를 선택'),
                   JSON_ARRAY('반복문','while 문','조건이 참인 동안 반복 수행'),
                   JSON_ARRAY('반복문','for 문','초기식·조건식·증감식을 사용한 대표적인 반복 구조')
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);


-- =========================================
-- 4.2.2 언어특성 활용 (concept 보강)
-- topic_id = 14202
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 14202,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              4.2.2.1 절차적 프로그래밍 언어
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '4.2.2.1',
             'title', '절차적 프로그래밍 언어',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','절차적(Procedural) 프로그래밍 언어의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '절차적 프로그래밍 언어는 프로시저(함수) 호출을 중심으로, '
                 '명령어들이 순차적으로 실행되는 흐름을 중시하는 명령형 프로그래밍 언어이다.'
               ),
               JSON_OBJECT('type','heading','text','절차적 언어의 특징'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '프로그램 흐름 파악이 비교적 쉬움',
                   '함수 단위로 모듈화·구조화 가능',
                   '같은 코드를 여러 곳에서 재사용 가능',
                   '객체지향에 비해 유지보수 유연성은 다소 떨어질 수 있음'
                 )
               ),
               JSON_OBJECT('type','heading','text','대표적인 절차적 언어 예'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'ALGOL',
                   'C 언어',
                   'FORTRAN'
                 )
               )
             )
           ),

           /* -------------------------------------
              4.2.2.2 객체지향 프로그래밍 언어
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '4.2.2.2',
             'title', '객체지향 프로그래밍 언어',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','객체지향(Object Oriented) 언어의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '객체지향 언어는 프로그램을 명령어 목록이 아니라, 상태와 행위를 가진 '
                 '여러 개의 객체(Object)가 상호작용하는 모델로 보는 프로그래밍 언어이다.'
               ),
               JSON_OBJECT('type','heading','text','객체지향 프로그래밍의 주요 기능'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '자료 추상화(Data Abstraction)',
                   '상속(Inheritance)·다중 상속',
                   '다형성(Polymorphism)',
                   '동적 바인딩(Dynamic Binding)',
                   '접근 제어자(Access Modifier)를 통한 캡슐화'
                 )
               ),
               JSON_OBJECT('type','heading','text','Java의 접근 제어자와 허용 범위'),
               JSON_OBJECT(
                 'type','table',
                 'caption','Java 접근 제어자 비교',
                 'headers', JSON_ARRAY('범위','public','protected','default','private'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('클래스 내부','O','O','O','O'),
                   JSON_ARRAY('동일 패키지','O','O','O','X'),
                   JSON_ARRAY('하위 패키지(상속)','O','O','X','X'),
                   JSON_ARRAY('다른 패키지','O','X','X','X')
                 )
               ),
               JSON_OBJECT('type','heading','text','Java 기본 데이터 타입 크기'),
               JSON_OBJECT(
                 'type','table',
                 'caption','Java 기본 타입 정리',
                 'headers', JSON_ARRAY('분류','타입','크기'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('논리형','boolean','1 byte'),
                   JSON_ARRAY('문자형','char','2 byte'),
                   JSON_ARRAY('정수형','byte','1 byte'),
                   JSON_ARRAY('정수형','short','2 byte'),
                   JSON_ARRAY('정수형','int','4 byte'),
                   JSON_ARRAY('정수형','long','8 byte'),
                   JSON_ARRAY('실수형','float','4 byte'),
                   JSON_ARRAY('실수형','double','8 byte')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.2.2.3 스크립트 언어
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '4.2.2.3',
             'title', '스크립트 언어와 Python·Bash',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','스크립트 언어의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '스크립트 언어는 별도의 컴파일 과정 없이 인터프리터에 의해 바로 실행되는 언어로, '
                 '빠르게 배우고 쉽게 작성할 수 있다는 장점이 있다.'
               ),
               JSON_OBJECT('type','heading','text','대표적인 스크립트 언어'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'PHP',
                   'Perl',
                   'Python',
                   'JavaScript',
                   'Bash',
                   'Basic 등'
                 )
               ),
               JSON_OBJECT('type','heading','text','Python 리스트 슬라이싱 예'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'list[시작:끝:증가폭] 형식 사용',
                   '시작 생략 시 0, 끝 생략 시 마지막 인덱스까지',
                   '시작 인덱스부터 "끝 직전" 인덱스까지 요소 선택'
                 )
               ),
               JSON_OBJECT('type','heading','text','Bash에서 사용 가능한 제어문'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'if 문',
                   'for 문',
                   'while 문'
                 )
               )
             )
           ),

           /* -------------------------------------
              4.2.2.4 선언형 언어(함수형·논리형)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '4.2.2.4',
             'title', '선언형 언어와 함수형·논리형 언어',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','선언형(Declarative) 언어의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '선언형 언어는 어떻게(How) 해결할지를 기술하기보다, '
                 '무엇(What)을 원하는지를 기술하는 방식의 프로그래밍 언어이다.'
               ),
               JSON_OBJECT('type','heading','text','선언형 언어의 주요 유형'),
               JSON_OBJECT(
                 'type','table',
                 'caption','함수형·논리형·특수 분야 언어',
                 'headers', JSON_ARRAY('구분','언어','특징'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('함수형 언어','Haskell',
                              '패턴 매칭, 커링, 가드 등 지원, 순수 함수 중심'),
                   JSON_ARRAY('함수형 언어','LISP',
                              '수학 표기법 표현, 트리 구조·동적 타입·가비지 컬렉션 개념 제시'),
                   JSON_ARRAY('논리형 언어','Prolog',
                              '사실(Facts)과 규칙(Rules)을 기반으로 논리 추론 수행'),
                   JSON_ARRAY('특수 분야 언어','SQL',
                              '관계형 데이터베이스의 데이터 정의·조작·제어를 위한 특수 목적 언어')
                 )
               )
             )
           )

         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = 14202);



-- =========================================
-- 4.2.3 라이브러리 활용 (concept 보강)
-- topic_id = 14203
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 14203,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              4.2.3.1 라이브러리 개념·종류
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '4.2.3.1',
             'title', '라이브러리(Library)의 개념과 종류',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','라이브러리(Library)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '라이브러리는 자주 사용하는 기능을 재사용할 수 있도록 모듈화하여 제공하는 프로그램 묶음으로, '
                 '필요할 때 가져다 쓸 수 있는 코드 집합이다.'
               ),
               JSON_OBJECT('type','heading','text','라이브러리 구성 요소'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '도움말(Documentation)',
                   '설치 파일(Installer, 패키지)',
                   '샘플 코드 및 예제 프로젝트'
                 )
               ),
               JSON_OBJECT('type','heading','text','표준 라이브러리와 외부 라이브러리'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '표준 라이브러리: 언어 또는 플랫폼에서 기본 제공 (입출력, 문자열, 시간, 수학 등)',
                   '외부 라이브러리: 서드파티(Third-Party)에서 제공, 기능 확장에 활용'
                 )
               )
             )
           ),

           /* -------------------------------------
              4.2.3.2 데이터 입출력과 스트림
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '4.2.3.2',
             'title', '데이터 입출력과 표준 스트림',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','데이터 입출력(Data I/O)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '데이터 입출력은 프로그램과 외부 세계(키보드, 파일, 네트워크 등) 사이에서 '
                 '데이터를 입력받고 출력하는 기법이다.'
               ),
               JSON_OBJECT('type','heading','text','표준 스트림 구성'),
               JSON_OBJECT(
                 'type','table',
                 'caption','표준 입출력 스트림',
                 'headers', JSON_ARRAY('구성','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('표준 입력(Standard Input)','일반적으로 키보드에서 입력을 받는 스트림'),
                   JSON_ARRAY('표준 출력(Standard Output)','프로그램 결과를 화면 등에 출력하는 스트림'),
                   JSON_ARRAY('표준 오류(Standard Error)','에러 메시지 출력을 위한 별도 스트림')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.2.3.3 예외 처리(Exception Handling)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '4.2.3.3',
             'title', '예외 처리(Exception Handling) 기본',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','예외 처리의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '예외 처리는 프로그램 실행 중 발생하는 오류 상황을 정상적인 흐름으로 전환해, '
                 '프로그램이 비정상 종료되지 않도록 처리하는 기법이다.'
               ),
               JSON_OBJECT('type','heading','text','예외 처리 구성 요소'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'try 블록: 예외 발생 가능성이 있는 코드를 감싸는 영역',
                   'throw: 예외를 발생시키는 명령',
                   'catch 블록: 특정 타입의 예외를 받아 처리하는 영역'
                 )
               ),
               JSON_OBJECT('type','heading','text','예시 구조(의사 코드)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'try {\n'
                 '    if (예외조건) {\n'
                 '        throw 예외객체;\n'
                 '    }\n'
                 '} catch (타입 예외객체) {\n'
                 '    예외 처리 로직 수행;\n'
                 '}'
               )
             )
           ),

           /* -------------------------------------
              4.2.3.4 프로토타입과 재사용
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '4.2.3.4',
             'title', '프로토타입(Prototype)과 재사용',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','프로토타입(Prototype) 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '프로토타입은 속성과 메서드를 공유하기 위해, '
                 '기준이 되는 마스터 객체를 바탕으로 다른 객체를 복제하거나 확장하는 스타일을 말한다. '
                 '특히 JavaScript 등에서 객체 간 재사용을 지원하는 핵심 메커니즘이다.'
               ),
               JSON_OBJECT('type','heading','text','프로토타입 구성 요소'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '마스터 객체(Master Object): 공통 속성과 메서드를 가진 기준 객체',
                   '복제(Cloning): 마스터 객체를 복사해 새 객체를 생성',
                   '객체 생성: 프로토타입 체인을 이용해 동적으로 객체 확장'
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);