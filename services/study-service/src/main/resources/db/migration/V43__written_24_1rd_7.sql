-- =========================================
-- 2024년 1회 – 프로그래밍 언어 활용 (4과목)
-- Q61 ~ Q70  (cert_id = 1)
-- =========================================

------------------------------------------------------------
-- Q61. C 문자열 처리 함수
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14203,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음은 C 언어의 문자열 처리 함수입니다. 서식과 기능의 연결로 틀린 것은 무엇인가요?',
  'C',
  '표준 C 문자열 함수의 기본 기능은 다음과 같습니다.
  - strlen(s): 문자열 s 의 길이를 구합니다.
  - strcpy(s1, s2): s2 의 내용을 s1 에 복사합니다.
  - strcmp(s1, s2): 두 문자열을 사전식으로 비교하여, 같으면 0, 크고 작음에 따라 음수/양수를 반환합니다.
  - strrev(s): 구현에 따라 다르지만, 문자열 s 의 문자를 역순으로 뒤집는 함수로 제공되기도 합니다(표준 C가 아닌 확장 함수인 경우가 많습니다).

  보기 ③처럼 strcmp 를 “문자열 연결” 기능으로 설명한 것은 잘못입니다. 문자열 연결은 strcat 이 담당하므로, 틀린 연결은 ③입니다.',
  'past:2024-1:Q61'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'strlen(s)  – 문자열 s 의 길이를 구한다.',            0),
  (@q_id, 'B', 'strcpy(s1, s2)  – 문자열 s2 를 s1 에 복사한다.',      0),
  (@q_id, 'C', 'strcmp(s1, s2)  – 문자열 s1 과 s2 를 연결한다.',      1),
  (@q_id, 'D', 'strrev(s)  – 문자열 s 를 거꾸로 변환한다.',           0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');

------------------------------------------------------------
-- Q62. C 배열/피보나치
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14201,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 C 언어 프로그램의 실행 결과로 옳은 것은 무엇인가요?

```c
#include <stdio.h>

int main(void) {
    int a[10];
    a[0] = 0;
    a[1] = 1;
    for (int i = 0; i < 8; i++)
        a[i + 2] = a[i + 1] + a[i];
    printf("%d", a[9]);
}
```',
  'D',
  '배열 a는 피보나치 수열처럼 채워집니다.
- a[0] = 0, a[1] = 1
- a[i+2] = a[i+1] + a[i]

순서대로 값은 다음과 같습니다.
a[2] = 1, a[3] = 2, a[4] = 3, a[5] = 5, a[6] = 8, a[7] = 13, a[8] = 21, a[9] = 34

그러나 for문이 i < 8 까지만 반복되므로 실제 마지막 계산은
a[9] = a[7] + a[6] = 13 + 8 = 21 입니다.

따라서 출력 값은 21이며 정답은 ④입니다.',
  'past:2024-1:Q62'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '13', 0),
  (@q_id, 'B', '8',  0),
  (@q_id, 'C', '34', 0),
  (@q_id, 'D', '21', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');


------------------------------------------------------------
-- Q63. IPv6 설명
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14302,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'IPv6 에 대한 설명으로 틀린 것은 무엇인가요?',
  'C',
  'IPv6 의 특징을 정리하면 다음과 같습니다.
  - 128비트 주소 공간을 제공하여 IPv4 보다 훨씬 많은 주소를 표현할 수 있습니다.
  - 확장 헤더(Extension Header)를 사용해 보안, QoS 등 다양한 기능을 유연하게 확장할 수 있습니다.
  - 기본 헤더에 인증/보안 관련 확장 기능을 포함하도록 설계되어 있습니다.

  반면 패킷 크기는 64KByte 로 “고정”이 아니라, 페이로드 길이를 지정하는 방식으로 가변이며, 확장 메커니즘을 통해 더 큰 패킷도 처리할 수 있습니다. 따라서 “64KByte 로 고정”이라는 ③번 설명은 틀렸습니다.',
  'past:2024-1:Q63'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '128비트의 주소 공간을 제공한다.',                           0),
  (@q_id, 'B', '인증 및 보안 기능을 프로토콜 자체에 포함하고 있다.',          0),
  (@q_id, 'C', '패킷 크기가 64KByte 로 고정되어 있다.',                      1),
  (@q_id, 'D', '확장 헤더를 통해 네트워크 기능 확장이 용이하다.',              0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'NET_PROTO_IP');

------------------------------------------------------------
-- Q64. 파이썬 변수 작성 규칙
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14201,
  'WRITTEN',
  'MCQ',
  'EASY',
  '파이썬(Python)에서 변수 이름 작성 규칙에 대한 설명으로 옳지 않은 것은 무엇인가요?',
  'C',
  '파이썬 식별자(변수 이름) 규칙은 다음과 같습니다.
  - 첫 글자는 반드시 영문자나 밑줄(_)이어야 하며, 숫자로 시작할 수 없습니다.
  - 사용할 수 있는 문자는 영문 대소문자, 숫자, 밑줄(_)입니다.
  - 중간에 공백, 하이픈(-), 특수기호 등을 사용할 수 없습니다.
  - if, for, class 같은 예약어는 변수 이름으로 사용할 수 없습니다.

  따라서 “변수 이름의 중간에 공백을 사용할 수 있다”라고 한 ③번 설명은 규칙에 어긋나므로 틀렸습니다.',
  'past:2024-1:Q64'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '변수 이름의 첫 글자로 숫자를 사용할 수 없다.',          0),
  (@q_id, 'B', '영문 대소문자, 숫자, 밑줄(_)을 사용할 수 있다.',         0),
  (@q_id, 'C', '변수 이름의 중간에 공백을 사용할 수 있다.',              1),
  (@q_id, 'D', '이미 사용되고 있는 예약어는 변수 이름으로 사용할 수 없다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');

------------------------------------------------------------
-- Q65. 스레드 설명
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14301,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '스레드(Thread)에 대한 설명으로 옳지 않은 것은 무엇인가요?',
  'A',
  '스레드는 프로세스 내에서 실행되는 “경량 프로세스”로, 다음과 같은 특징이 있습니다.
  - 하나의 프로세스 안에 여러 개의 스레드를 둘 수 있으며, 코드/데이터/파일 등 자원을 공유합니다.
  - 커널 스레드는 운영체제가 직접 스케줄링하고 관리합니다.
  - 사용자 스레드는 사용자 수준 라이브러리에서 스케줄링을 담당할 수 있습니다.
  - 스레드를 사용하면 컨텍스트 스위칭 비용을 줄이고, I/O 와 연산을 겹쳐 수행할 수 있어 전체 처리율을 높일 수 있습니다.

  따라서 “한 개의 프로세스는 여러 개의 스레드를 가질 수 없다”라는 ①번 설명은 사실과 반대이므로 옳지 않습니다.',
  'past:2024-1:Q65'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '한 개의 프로세스는 여러 개의 스레드를 가질 수 없다.',   1),
  (@q_id, 'B', '커널 스레드는 운영체제가 직접 스케줄링하고 관리한다.',   0),
  (@q_id, 'C', '사용자 스레드는 사용자 수준 라이브러리를 통해 관리될 수 있다.', 0),
  (@q_id, 'D', '스레드를 사용하면 시스템 자원 활용과 응용 프로그램 처리율을 향상시킬 수 있다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OS_PROC_MEM');

------------------------------------------------------------
-- Q66. HRN 스케줄링
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14301,
  'WRITTEN',
  'MCQ',
  'HARD',
  'HRN(Highest Response Ratio Next) 방식으로 스케줄링할 때,  
다음과 같이 대기 시간과 서비스 실행 시간이 주어진 작업 A, B, C, D의 처리 순서로 옳은 것은 무엇인가요?

| 작업 | 대기 시간 | 서비스 실행 시간 |
|------|-----------|------------------|
| A    | 5         | 20               |
| B    | 40        | 20               |
| C    | 15        | 45               |
| D    | 20        | 2                |',
  'C',
  'HRN 스케줄링은 다음과 같은 응답비율(Response Ratio)로 우선순위를 결정합니다.

  응답비율 = (대기시간 + 서비스시간) / 서비스시간  
           = 1 + (대기시간 / 서비스시간)

초기 상태에서 작업들의 응답비율은 다음과 같습니다.
- A: (5 + 20) / 20  = 1.25
- B: (40 + 20) / 20 = 3.0
- C: (15 + 45) / 45 ≈ 1.33
- D: (20 + 2) / 2   = 11.0

가장 커서 먼저 실행되는 작업은 D이며, 이후 계산을 반복하면  
최종 순서는 **D → A → B → C**가 됩니다.

따라서 정답은 ③번입니다.',
  'past:2024-1:Q66'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'A → C → B → D', 0),
  (@q_id, 'B', 'A → B → C → D', 0),
  (@q_id, 'C', 'D → A → B → C', 1),
  (@q_id, 'D', 'D → B → C → A', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OS_PROC_MEM');


------------------------------------------------------------
-- Q67. Java while(y--)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14202,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 자바(Java) 코드의 실행 결과로 옳은 것은 무엇인가요?

```java
int x = 1, y = 6;
while (y--) {
    x++;
}
System.out.println("x=" + x + " y=" + y);
```',
  'D',
  '자바에서는 while 문의 조건식에 boolean 타입이 와야 합니다.

하지만 위 코드에서 while (y--) 는
- y-- 의 결과 값(정수)을 조건으로 사용하고 있어, C 언어처럼 정수를 true/false 로 간주하는 방식을 사용할 수 없습니다.
- 즉, int 타입을 boolean 으로 자동 변환하지 않기 때문에 컴파일 단계에서 오류가 발생합니다.

따라서 이 코드는 실행까지 가지 못하고 컴파일 오류가 발생하므로 ④번이 정답입니다.
(자바에서 이런 로직을 쓰려면 while (y-- > 0) 처럼 명시적으로 비교 연산을 사용해야 합니다.)',
  'past:2024-1:Q67'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'x=7 y=0',   0),
  (@q_id, 'B', 'x=6 y=-1',  0),
  (@q_id, 'C', 'x=7 y=-1',  0),
  (@q_id, 'D', '컴파일 오류(Unresolved compilation problem) 발생', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');


------------------------------------------------------------
-- Q68. C 산술 연산자
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14201,
  'WRITTEN',
  'MCQ',
  'EASY',
  'C 언어에서 다음 중 산술 연산자가 아닌 것은 무엇인가요?',
  'D',
  'C 언어의 대표적인 산술 연산자는 다음과 같습니다.
  - 덧셈: +
  - 뺄셈: -
  - 곱셈: *
  - 나눗셈: /
  - 나머지: %

  반면, “=” 연산자는 대입(할당) 연산자로, 변수에 값을 저장할 때 사용합니다.
  산술 연산을 수행하는 연산자가 아니므로, 산술 연산자가 아닌 것은 ④번 “=”입니다.',
  'past:2024-1:Q68'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '+', 0),
  (@q_id, 'B', '-', 0),
  (@q_id, 'C', '/', 0),
  (@q_id, 'D', '=', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');

------------------------------------------------------------
-- Q69. Java 전위/후위 연산
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14202,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 Java 프로그램이 실행되었을 때의 출력 결과는 무엇인가요?

```java
public class Operator {
    public static void main(String[] args) {
        int x = 5, y = 0, z = 0;
        y = ++x;
        z = --x;
        System.out.print(x + ", " + y + ", " + z);
    }
}
```',
  'A',
  '코드를 한 줄씩 따라가 보겠습니다.
1) 초기값: x = 5, y = 0, z = 0
2) y = ++x;
   - 전위 증가(++x)이므로, x 를 먼저 6 으로 증가한 뒤 그 값을 y 에 대입합니다.
   - 결과: x = 6, y = 6, z = 0
3) z = --x;
   - 전위 감소(--x)이므로, x 를 5 로 줄인 뒤 그 값을 z 에 대입합니다.
   - 결과: x = 5, y = 6, z = 5

따라서 최종 출력은 "5, 6, 5" 가 되므로 ①번이 정답입니다.',
  'past:2024-1:Q69'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '5, 6, 5', 1),
  (@q_id, 'B', '5, 5, 5', 0),
  (@q_id, 'C', '5, 6, 4', 0),
  (@q_id, 'D', '6, 5, 5', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');


------------------------------------------------------------
-- Q70. Class C IP 주소
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14302,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 Class C IP 주소에 해당하는 것은 어느 것인가요?
  (단, 전통적인 클래스풀(Classful) 주소 체계를 기준으로 합니다.)',
  'A',
  '클래스풀 IP 주소 체계를 기준으로 할 때, 첫 옥텟 범위는 다음과 같습니다.
  - Class A: 0 ~ 127
  - Class B: 128 ~ 191
  - Class C: 192 ~ 223
  - Class D: 224 ~ 239 (멀티캐스트)

  각 보기의 첫 옥텟을 보면
  - 10.3.2.1 → 10 (Class A)
  - 172.16.98.3 → 172 (Class B, 사설 대역)
  - 200.168.30.1 → 200 (192~223 사이이므로 Class C)
  - 225.2.4.1 → 225 (Class D)

  따라서 Class C 에 해당하는 주소는 ①번 200.168.30.1 입니다.',
  'past:2024-1:Q70'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '200.168.30.1', 1),
  (@q_id, 'B', '10.3.2.1',     0),
  (@q_id, 'C', '172.16.98.3',  0),
  (@q_id, 'D', '225.2.4.1',    0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'NET_PROTO_IP');
