USE certpilot_study;

SET @cert_id := 1;

-- =========================================
-- 2024년 3회 – 실기 (11~20)
-- mode: PRACTICAL, type: SHORT
-- =========================================


/* Q11. URL 구성 요소 – 번호 매칭 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source, image_url
) VALUES (
  @cert_id, 31502,
  'PRACTICAL', 'SHORT', 'EASY',
  '다음은 URI 구조를 나타낸 그림이다. 각 설명에 해당하는 번호를 순서대로 쓰시오.

1. 추가적인 질의를 제공하는 부분으로, 웹 서버에 전달할 추가 데이터를 나타낸다. ( )

2. 자원 경로를 나타내며, 서버 상에서 해당 리소스의 위치를 지정한다. ( )

3. 웹 프로토콜을 정의하는 부분으로, HTTP, FTP 등의 방식을 나타낸다. ( )

4. 호스트와 포트 번호를 나타내는 부분으로, 서버 주소와 포트 번호를 포함한다. ( )

5. 특정 페이지 내의 위치를 가리키며, 문서 내 특정 섹션으로 이동하는 데 사용된다. ( )',
NULL,
'43125',
'그림에서 query는 ④, path는 ③, scheme은 ①, authority는 ②, fragment는 ⑤이므로 순서대로 4 3 1 2 5가 된다.',
'practical:2024-3:Q11',
'https://api.mycertpilot.com/static/images/questions/pq_2024_03_11.png'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
(@q_id, 'P_DB_OPERATION_MONITOR');

/* Q12. C 구조체 / 단일 연결 리스트 값 교환 */
INSERT INTO question (
cert_id, topic_id, mode, type, difficulty,
stem, payload_json, answer_key, solution_text, source
) VALUES (
@cert_id, 31101,
'PRACTICAL', 'SHORT', 'NORMAL',
'다음 C 프로그램을 실행했을 때 출력되는 값을 쓰시오.

```c
#include <stdio.h>

struct Node {
    int value;
    struct Node* next;
};

void func(struct Node* node) {
    while (node != NULL && node->next != NULL) {
        int t = node->value;
        node->value = node->next->value;
        node->next->value = t;
        node = node->next->next;
    }
}

int main() {
    struct Node n1 = {1, NULL};
    struct Node n2 = {2, NULL};
    struct Node n3 = {3, NULL};

    n1.next = &n3;
    n3.next = &n2;

    func(&n1);

    struct Node* current = &n1;
    while (current != NULL) {
        printf("%d", current->value);
        current = current->next;
    }
    return 0;
}
```',
  NULL,
  '312',
  '연결 리스트는 처음에 1→3→2 순으로 연결되어 있고, func에서 (1,3)을 교환한 뒤 다음 쌍이 없어 종료되므로 최종 값은 3 1 2가 되어 "312"가 출력된다.',
  'practical:2024-3:Q12'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


/* Q13. 개체 무결성 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31201,
  'PRACTICAL', 'SHORT', 'EASY',
  '다음 설명에 해당하는 무결성의 종류를 쓰시오.

기본 키(Primary Key)를 구성하는 모든 속성은 NULL 값을 가질 수 없고, 각 튜플의 기본 키 값은 서로 달라야 한다는 규칙을 의미한다.',
  NULL,
  '개체',
  '기본 키 값의 유일성과 NOT NULL을 요구하는 규칙은 개체 무결성(Entity Integrity)이다.',
  'practical:2024-3:Q13'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_MODEL_ER_PHYSICAL');


/* Q14. Python type 판별 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
@cert_id, 31101,
  'PRACTICAL', 'SHORT', 'EASY',
  '다음 Python 코드를 실행했을 때 출력되는 값을 쓰시오.

```python
def test(v):
    if type(v) == type(""):
        return len(v)
    elif type(v) == type(100):
        return 101
    else:
        return 20

a = "100.0"
b = 100.0
c = (100.0, 200.0)
print(test(a) + test(b) + test(c))
```',
  NULL,
  '45',
  'a는 문자열이므로 길이 5를 반환하고, b는 실수형이어서 else 분기에서 20, c는 튜플이라 20을 반환하므로 합은 5+20+20=45가 된다.',
  'practical:2024-3:Q14'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


/* Q15. UML 관계 – 연관/일반화/의존 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source, image_url
) VALUES (
@cert_id, 31101,
  'PRACTICAL', 'SHORT', 'EASY',
  '다음 UML 다이어그램에서 1, 2, 3에 들어갈 관계를 보기에서 골라 기호로 쓰시오.

1. (    ) 관계
   상위에 "차" 클래스가 있고, 아래에 "엔진", "부품", "타이어" 등 구성요소들이 연결되어 있는 구조이다.
   전체와 부분 객체 사이의 관계를 표현한다.

2. (    ) 관계
   상위에 "차" 클래스가 있고, 아래에 "트럭", "버스", "택시" 등 하위 종류들이 삼각형 화살표로 연결된 구조이다.
   상위/하위, 일반/특수 개념 사이의 상속 관계를 표현한다.

3. (    ) 관계
   "텔레비전"과 "리모콘" 사이가 점선 화살표로 연결되어 있어, 한 객체가 다른 객체를 일시적으로 사용함을 나타낸다.

[보기]
㉠ 의존   ㉡ 연관   ㉢ 일반화',
  NULL,
  '㉡㉢㉠',
  '1은 전체와 부분 사이를 나타내는 연관(또는 집합) 관계이므로 ㉡, 2는 상속 구조를 나타내는 일반화 관계로 ㉢, 3은 점선 화살표로 나타내는 의존 관계이므로 ㉠이 되어 순서는 ㉡㉢㉠이다.',
  'practical:2024-3:Q15',
  'https://api.mycertpilot.com/static/images/questions/pq_2024_03_15.png'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


/* Q16. Java 예외 처리 – try/catch/finally */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
@cert_id, 31101,
  'PRACTICAL', 'SHORT', 'EASY',
  '다음 Java 코드를 실행했을 때 출력되는 값을 쓰시오.

```java
public class Main {
    public static void main(String[] args) {
        int sum = 0;
        try {
            func();
        } catch (NullPointerException e) {
            sum = sum + 1;
        } catch (Exception e) {
            sum = sum + 10;
        } finally {
            sum = sum + 100;
        }
        System.out.print(sum);
    }

    static void func() throws Exception {
        throw new NullPointerException();
    }
}
```',
  NULL,
  '101',
  'func에서 NullPointerException이 발생하면 첫 번째 catch가 실행되어 sum이 1이 되고, finally 블록에서 100이 더해져 최종적으로 101이 출력된다.',
  'practical:2024-3:Q16'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


/* Q17. Java 상속과 오버라이딩 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
@cert_id, 31101,
  'PRACTICAL', 'SHORT', 'NORMAL',
  '다음 Java 코드를 실행했을 때 출력되는 값을 쓰시오.

```java
class B {
    int x = 3;
    int getX() {
        return x * 2;
    }
}

class A extends B {
    int x = 7;
    @Override
    int getX() {
        return x * 3;
    }
}

public class Annotation {
    public static void main(String[] args) {
        B b1 = new A();
        A b2 = new A();
        System.out.print(b1.getX() + b1.x + b2.getX() + b2.x);
    }
}
```',
  NULL,
  '52',
  'b1은 A 타입 인스턴스를 B 참조로 가리키지만 getX는 오버라이딩된 A의 메서드가 호출되어 7*3=21이 되고, b1.x는 B의 필드 3이다. b2.getX()는 21, b2.x는 7이므로 21+3+21+7 = 52가 출력된다.',
  'practical:2024-3:Q17'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


/* Q18. Java 오버로딩 / Object 선택 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31101,
  'PRACTICAL', 'SHORT', 'NORMAL',
  '다음 Java 코드를 실행했을 때 출력되는 값을 쓰시오.

```java
class Printer {
    void print(Integer a) {
        System.out.print("A" + a);
    }
    void print(Object a) {
        System.out.print("B" + a);
    }
    void print(Number a) {
        System.out.print("C" + a);
    }
}

public class Main {
    public static void main(String[] args) {
        new Container(0).print();
    }

    public static class Container {
        Object value;
        public Container(Object t) {
            value = t;
        }
        public void print() {
            new Printer().print(value);
        }
    }
}
```',
  NULL,
  'B0',
  'Container의 value 필드 타입은 Object이므로 Printer.print 호출 시 컴파일러는 매개변수 타입이 Object인 메서드를 선택한다. 따라서 "B0"가 출력된다.',
  'practical:2024-3:Q18'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


/* Q19. C 이중 포인터와 배열 갱신 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31101,
  'PRACTICAL', 'SHORT', 'HARD',
  '다음 C 프로그램을 실행했을 때 출력되는 값을 쓰시오.

```c
#include <stdio.h>

void func1(int** arr, int size) {
    for (int i = 0; i < size; i++) {
        *(*arr + i) = (*(*arr + i) + i) % size;
    }
}

void func2(int* num, int* arr) {
    *num = arr[*num % 4];
}

int main() {
    int arr[] = {3, 1, 4, 1, 5};
    int* p = arr;
    int** pp = &p;
    int num = 6;

    func1(pp, 5);
    func2(&num, arr);

    printf("%d", num);
    return 0;
}
```',
  NULL,
  '1',
  'func1에서 배열 요소가 인덱스를 더한 뒤 size(5)로 나머지 연산되어 새 값으로 갱신되고, 이후 func2에서 num은 갱신된 배열에서 num%4 위치의 값으로 바뀐다. 연산 과정을 모두 따르면 최종적으로 num은 1이 되어 출력값은 1이 된다.',
  'practical:2024-3:Q19'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


/* Q20. 애드혹 네트워크 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31502,
  'PRACTICAL', 'SHORT', 'EASY',
  '다음 설명에 해당하는 네트워크 용어를 쓰시오.

중앙 집중식 인프라 없이, 사전에 구축된 네트워크 기반 시설 없이 구성되는 자율형 무선 네트워크를 의미한다.
각 노드(무선 장치)가 직접 서로 연결되어 라우팅을 수행하며, 군사 작전 중 실시간 정보 공유 등에서 사용된다.',
  NULL,
  '애드혹 네트워크',
  '기지국 없이 단말들끼리 직접 연결해 구성하는 자율형 네트워크를 애드혹 네트워크(Ad-hoc network)라고 한다.',
  'practical:2024-3:Q20'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');
