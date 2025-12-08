SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

------------------------------------------------------------
-- Q1. 레코드 접근 방법 - 색인(index) 접근
--  - Topic: P.2.1 개념/논리/물리 모델링 (31201)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31201,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음은 파일 구조와 관련된 설명이다. 괄호 안에 들어갈 가장 알맞은 용어를 작성하시오.

데이터베이스의 물리 설계 시, 레코드에 접근하는 방법에는
순차(sequential) 접근, (        ) 접근, 해싱(hashed) 접근 등이 있다.

이 중 (        ) 접근 방법은 레코드의 키 값과 포인터를 쌍으로 묶어 저장하고,
검색 시 키 값을 기준으로 빠르게 탐색하도록 설계된 방식이다.

키와 포인터의 쌍으로 구성된 색인 구조를 사용하여
해당 키가 가리키는 주소를 통해 원하는 레코드를 직접 찾을 수 있는
이 접근 방법의 이름을 쓰시오.',
  '색인 접근(Indexed Access, 인덱스)',
  '순차 접근은 레코드를 처음부터 끝까지 차례대로 읽는 방식이라,
원하는 레코드를 찾기 위해 전체 또는 상당 부분을 스캔해야 합니다.

반면 (        ) 접근은 “키 값과 포인터를 쌍으로 저장”해 두었다가,
키 값으로 색인을 먼저 찾고, 색인이 가리키는 주소로 바로 가서 레코드를 읽는 방식입니다.
이것이 바로 색인(index)을 이용한 **색인 접근(Indexed Access)** 입니다.

색인 접근의 특징은 다음과 같습니다.
- 레코드의 “키 값 + 포인터”를 별도의 색인 구조에 저장
- 검색 시 전체 파일을 순차적으로 읽지 않고 색인만 먼저 탐색
- 키에 대한 검색 속도가 매우 빠름

따라서 괄호 안에 들어갈 용어는
“색인 접근(Indexed Access, 인덱스)” 입니다.',
  'past:2025-2:Q01'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_MODEL_ER_PHYSICAL');

------------------------------------------------------------
-- Q2. 릴레이션에서 열(Column, 필드)에 해당하는 개념
--  - Topic: P.2.1 개념/논리/물리 모델링 (31201)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31201,
  'PRACTICAL',
  'SHORT',
  'EASY',
  '다음은 데이터베이스 릴레이션의 구성 요소 중 하나에 대한 설명이다.
설명을 읽고 보기에서 알맞은 기호를 골라 작성하시오.

릴레이션(Relation)에서 열(Column)을 의미하며
데이터 항목의 속성 또는 특성을 나타낸다.
각 열은 고유한 이름을 가지며 특정 도메인(Domain)에서 정의된 값을 갖는다.

예를 들어 "학생" 릴레이션에서 학번, 이름, 전공 등은 각각 하나의 열이며
이 열들은 학생의 고유한 속성을 나타낸다.

이 개념은 파일 구조에서의 필드(Field)에 해당하며
릴레이션에서 행(Row, Tuple)의 구성 요소가 된다.

[보기]
ㄱ. Cardinality
ㄴ. Domain
ㄷ. Attribute
ㅁ. Degree
ㅂ. Schema
ㅅ. Tuple

알맞은 기호를 쓰시오.',
  'ㄷ. Attribute',
  '관계형 데이터베이스에서

- **행(Row)** 은 튜플(Tuple)
- **열(Column)** 은 속성(Attribute)
- **도메인(Domain)** 은 속성이 가질 수 있는 값의 집합

으로 대응됩니다.

문제에서 묘사한 특징은 다음과 같습니다.
- 릴레이션에서 “열(Column)”을 의미
- 데이터 항목의 “속성/특성”을 나타냄
- 파일 구조의 “필드(Field)”에 대응

이는 곧 **속성(Attribute)** 의 정의에 해당합니다.

따라서 보기에서 올바른 선택지는
“ㄷ. Attribute” 입니다.',
  'past:2025-2:Q02'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_MODEL_ER_PHYSICAL');

------------------------------------------------------------
-- Q3. SSH 보안 접속 프로토콜
--  - Topic: P.5.2 장애 분석 및 운영 보안 (31502)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음은 원격 접속과 관련된 보안 프로토콜에 대한 설명이다.
설명을 읽고 알맞은 용어를 작성하시오.

> 원격 접속 시 암호화된 통신을 제공하는 보안 접속용 프로토콜이다.
> 공개키 기반의 인증 방식을 사용하며, 데이터 전송 구간을 암호화한다.
> 주로 원격 서버에 안전하게 접속할 때 사용되고, 기본 포트 번호는 22번이다.
> Telnet의 보안 취약점을 보완한 대안으로 널리 사용된다.

위 설명에 해당하는 프로토콜의 이름을 쓰시오.',
  'SSH',
  '문제에서 제시한 특징을 정리하면 다음과 같습니다.

- 원격 접속(Remote Login)에 사용
- 통신 구간을 암호화하여 기밀성 보장
- 공개키 기반 인증 방식 지원
- 기본 포트 번호 22번
- Telnet을 대체하는 보안 접속 프로토콜

이는 모두 **SSH(Secure Shell)** 의 특징과 일치합니다.

따라서 정답은 “SSH” 입니다.
(대소문자는 일반적으로 SSH, ssh 형태 모두 허용될 수 있습니다.)',
  'past:2025-2:Q03'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

------------------------------------------------------------
-- Q4. 스케줄링 알고리즘 SJF / SRT
--  - Topic: P.5.2 장애 분석 및 운영(스케줄링) (31502)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '스케줄링 알고리즘에 관한 다음 설명을 읽고
(1)과 (2)에 알맞은 스케줄링 알고리즘의 명칭을 각각 쓰시오.

(1) CPU burst 시간이 짧은 프로세스를 우선적으로 처리하는 스케줄링 방식이다.
    "Shortest Next CPU Burst" 라고도 불리며,
    선점형 또는 비선점형으로 구현될 수 있다.

(2) 위의 스케줄링 방식을 “선점형”으로 구현한 형태로,
    실행 중인 프로세스보다 더 짧은 burst 시간을 가진 프로세스가 도착하면
    현재 CPU를 선점하는 알고리즘이다.

(1), (2)의 알고리즘 이름을 순서대로 쓰시오.
(예: FCFS, RR 형태로 작성)',
  'SJF, SRT',
  '문제에서 설명하는 알고리즘은 다음과 같습니다.

(1) CPU burst 시간이 “가장 짧은” 프로세스를 먼저 선택하는 방식
    - Shortest Job First, **SJF(Shortest Job First)** 또는
      Shortest Next CPU Burst 라고 부름
    - 비선점형 SJF와 선점형 SJF(=SRT)가 존재

(2) 위 방식을 “선점형”으로 구현한 형태
    - 더 짧은 CPU burst 시간을 가진 프로세스가 도착하면
      현재 실행 중인 프로세스를 중단하고 새 프로세스에 CPU를 할당
    - 이를 **SRT(Shortest Remaining Time)** 또는 선점형 SJF라고 부름

따라서 정답은
- (1) SJF
- (2) SRT

이므로, “SJF, SRT” 와 같이 작성할 수 있습니다.',
  'past:2025-2:Q04'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

------------------------------------------------------------
-- Q5. Java - 참조 타입과 매개변수 전달
--  - Topic: P.1.1 업무 시나리오/코드 해석 (31101)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 Java 코드를 실행했을 때 출력되는 결과를 쓰시오.

```java
public class Main {

    public static void change(String[] data, String s) {
        data[0] = s;
        s = "Z";
    }

    public static void main(String[] args) {
        String[] data = { "A" };
        String s = "B";

        change(data, s);

        System.out.print(data[0] + s);
    }
}
```',
  'BB',
  '코드를 단계별로 살펴보겠습니다.

1) main 시작 시
   - data → {"A"}
   - s → "B"

2) change(data, s) 호출
   - data 매개변수는 “배열 참조”가 값 복사되어 넘어옴
     (배열 객체 자체는 공유됨)
   - s 매개변수는 문자열 참조가 값 복사되어 넘어옴

3) change 내부에서
   - data[0] = s;
     → 현재 s는 "B" 이므로, data[0] 은 "B" 가 된다.
       (공유된 배열의 0번 요소가 "B"로 변경)
   - s = "Z";
     → 여기서 s는 지역 매개변수이므로,
       main의 s에는 아무 영향이 없다.

4) change 종료 후 main으로 복귀
   - data 배열의 0번 요소는 "B"
   - main 지역 변수 s 는 여전히 "B"

5) 출력
   - data[0] + s  → "B" + "B" = "BB"

따라서 정답은 **BB** 입니다.',
  'past:2025-2:Q05'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q6. IPv4 주소와 서브넷 마스크 - 네트워크 주소와 호스트 수
--  - Topic: P.5.2 운영/네트워크 분석 (31502)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '호스트의 IP 주소가 223.13.234.132 이고
서브넷 마스크가 255.255.255.192 일 때 다음 물음에 답하시오.

① 이 호스트가 속한 네트워크 주소는 223.13.234.(   ) 이다.
② 이 네트워크에서 사용 가능한 호스트 수는 (   )개이다.
   (단, 네트워크 주소와 브로드캐스트 주소는 제외한다.)

①, ②에 들어갈 값을 순서대로 쓰시오.
(예: 128, 62 형태로 작성)',
  '128, 62',
  '서브넷 마스크 255.255.255.192 는 마지막 옥텟이 /26 에 해당합니다.
- 마지막 옥텟 비트: 11000000
- 블록 크기: 256 - 192 = 64

따라서 네트워크 주소 범위는
- 223.13.234.0
- 223.13.234.64
- 223.13.234.128
- 223.13.234.192

중에서, 호스트 IP 223.13.234.132 가 속한 구간은
**223.13.234.128 ~ 223.13.234.191** 입니다.
→ 네트워크 주소: 223.13.234.128

호스트 수는
- 전체 주소 수: 2^(32-26) = 2^6 = 64개
- 네트워크 주소 1개, 브로드캐스트 주소 1개 제외
→ 사용 가능한 호스트 수: 64 - 2 = 62개

따라서 정답은

① 128
② 62

이므로, “128, 62” 입니다.',
  'past:2025-2:Q06'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

------------------------------------------------------------
-- Q7. 디자인 패턴 - Proxy 패턴
--  - Topic: P.1.1 시나리오/설계 개념 해석 (31101)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음은 어떤 디자인 패턴에 대한 설명이다.
설명을 보고 알맞은 디자인 패턴의 이름을 작성하시오.

어떤 객체에 대한 접근을 제어하거나
추가적인 기능을 부여하기 위해
해당 객체의 “대리 객체”를 두고 사용하는 방식의 패턴이다.

실제 객체에 대한 접근 전에 필요한 검증, 로깅, 캐싱 등의 작업을 수행할 수 있으며,
실제 객체의 생성을 지연시켜 메모리와 자원을 절약할 수 있다.

또한 실제 객체를 감추어 정보 은닉을 강화할 수 있다는 장점이 있다.

위 설명에 해당하는 디자인 패턴의 이름을 쓰시오.',
  'Proxy 패턴(Proxy)',
  '문제의 핵심 키워드는 다음과 같습니다.

- 어떤 객체에 대한 “대리 객체(Proxy)”를 둔다.
- 실제 객체에 대한 접근을 제어하거나, 그 전에 부가 작업 수행
  (검증, 로깅, 캐싱, 접근 제어 등)
- 실제 객체의 생성을 지연(Lazy Initialization)하여 자원 절약
- 실제 객체를 감추어 정보 은닉을 강화

이러한 특징을 가지는 대표적인 디자인 패턴은
**Proxy 패턴(프록시 패턴)** 입니다.

따라서 정답은 “Proxy” 또는 “Proxy 패턴”입니다.',
  'past:2025-2:Q07'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q8. 웹 비동기 통신 기법 - AJAX
--  - Topic: P.1.1 시나리오/웹 동작 해석 (31101)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음은 웹 데이터 교환 방식에 관한 설명이다.
괄호 안에 들어갈 알맞은 용어를 작성하시오.

(        ) 은/는 웹 페이지 전체를 다시 불러오지 않고,
JavaScript와 XML(또는 JSON)을 이용하여
일부 콘텐츠만 비동기적으로 갱신할 수 있는 기술이다.

(        ) 을/를 사용하면 HTML만으로는 구현하기 어려운
동적인 기능들을 구현할 수 있으며,
사용자가 웹 페이지와 보다 자유롭게 상호작용할 수 있도록 해주는
대표적인 웹 개발 기법이다.

위 설명에 해당하는 용어를 쓰시오.',
  'AJAX',
  '문제에서 설명하는 기술의 특징은 다음과 같습니다.

- 페이지 전체를 새로고침하지 않고, “일부 영역만” 비동기 갱신
- JavaScript + XML 또는 JSON 기반 통신
- 동적인 웹 애플리케이션 구현
- 사용자 경험(UX)을 향상시키는 대표적인 웹 개발 기법

이는 **AJAX(Asynchronous JavaScript and XML)** 의 특징과 정확히 일치합니다.

따라서 정답은 “AJAX” 입니다.
(대소문자는 일반적으로 AJAX, Ajax 등으로 허용될 수 있습니다.)',
  'past:2025-2:Q08'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q9. Java 람다, 예외 처리 - run(f) + run((n) -> n+9)
--  - Topic: P.1.1 코드/예외 흐름 해석 (31101)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음 Java 코드를 실행했을 때 출력되는 결과를 쓰시오.

```java
public class Gisafirst {

    static interface F {
        int apply(int x) throws Exception;
    }

    public static int run(F f) {
        try {
            return f.apply(3);
        } catch (Exception e) {
            return 7;
        }
    }

    public static void main(String[] args) {
        F f = (x) -> {
            if (x > 2) {
                throw new Exception();
            }
            return x * 2;
        };

        System.out.print(run(f) + run((int n) -> n + 9));
    }
}
```',
  '19',
  '각 호출을 분리해서 생각하면 쉽습니다.

1) run(f)
   - run은 f.apply(3)을 호출하고, 예외가 나면 7을 리턴
   - f 구현:
     - x > 2 이면 Exception 발생
     - 그렇지 않으면 x * 2 반환
   - 여기서 x=3 이므로 3 > 2 → 예외 발생
   - 따라서 run(f)는 catch 블록으로 가서 **7** 을 반환

2) run((int n) -> n + 9)
   - 두 번째 인자로 새 람다식을 전달
   - 이 람다식은 “입력값 + 9” 를 반환하며 예외를 던지지 않음
   - run 호출 시 f.apply(3)을 수행
     → 3 + 9 = 12
   - 예외가 없으므로 그대로 12 반환

3) 최종 출력
   - run(f) + run((int n) -> n + 9)
   = 7 + 12
   = 19

따라서 출력 결과는 **19** 입니다.',
  'past:2025-2:Q09'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q10. Java - 정적 메서드 숨기기 vs 인스턴스 메서드 오버라이딩
--  - Topic: P.1.1 코드/다형성 해석 (31101)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 Java 코드를 실행했을 때 출력되는 결과를 쓰시오.

```java
class P {
    static String print() {
        return "P";
    }

    int sum(int a) {
        return a + 1;
    }
}

class C extends P {
    static String print() {
        return "C";
    }

    int sum(int a) {
        return a + 2;
    }
}

public class Gisafirst {
    public static void main(String[] args) {
        P tmp = new C();
        System.out.println(tmp.sum(3) + tmp.print());
    }
}
```',
  '5P',
  '여기서 중요한 점은

- **인스턴스 메서드 sum** 은 오버라이딩 대상
- **정적 메서드 print** 는 “오버라이딩이 아니라 숨기기(hiding)” 라는 점입니다.

1) P tmp = new C();
   - 참조 타입은 P, 실제 인스턴스는 C

2) tmp.sum(3)
   - sum은 인스턴스 메서드이므로 “동적 바인딩” 적용
   - 실제 객체 타입 C 의 sum이 호출
   - C.sum(3) → 3 + 2 = 5

3) tmp.print()
   - print는 static 메서드이므로 “클래스 타입 기준”으로 정적 바인딩
   - 참조 타입이 P 이므로 P.print() 가 호출
   - 결과는 "P"

4) 최종 출력
   - tmp.sum(3) + tmp.print()
   = 5 + "P"
   = "5P"

따라서 출력 결과는 **5P** 입니다.',
  'past:2025-2:Q10'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');
