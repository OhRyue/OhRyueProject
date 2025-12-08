-- =========================================
-- 2025년 1회 정보처리기사 실기
-- SHORT 문제 시드 (Q11 ~ Q20)
-- =========================================

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

SET @tp_31101 := 31101;
SET @tp_31102 := 31102;
SET @tp_31201 := 31201;
SET @tp_31202 := 31202;
SET @tp_31301 := 31301;
SET @tp_31302 := 31302;
SET @tp_31401 := 31401;
SET @tp_31402 := 31402;
SET @tp_31501 := 31501;
SET @tp_31502 := 31502;

------------------------------------------------------------
-- Q11. C 언어 – 3x3 배열 동적 할당 (원문 코드 미확인, 정답만 반영)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31101,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음은 C 언어로 3×3 2차원 배열을 동적으로 할당하여 연산을 수행하고, 최종 결과를 출력하는 프로그램이다.

(주의: 이 문항은 실제 기출의 코드 전체가 아직 확보되지 않아, 현재는 "3×3 배열과 malloc을 사용하는 코드"라는 정도만 반영된 임시 설명이다.
실제 서비스에 사용하기 전, 반드시 원문 기출의 코드를 확인하여 이 stem 내용을 교체해 주어야 한다.)

프로그램을 실행했을 때 출력되는 정수 값을 작성하시오.',
  '13',
  '문제 설명에 따르면 프로그램은 3×3 크기의 정수 배열을 동적으로 할당하고, 일부 연산을 수행한 뒤 정수 하나를 출력하며, 정답은 13이다.
현재 원문 코드가 확보되지 않아 구체적인 연산 과정은 명시할 수 없으므로, 이 문항은 정답 값만 맞추도록 구성한 임시 문항이다.
실제 기출 코드가 준비되면, 코드와 설명을 원문에 맞게 교체하는 것이 바람직하다.',
  'prac:2025-1:Q11'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q12. 결합도 유형
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음은 모듈 간 결합도에 대한 설명이다.
각 설명에 해당하는 결합도의 유형을 보기에서 골라 "①, ②, ③" 형식으로 작성하시오.

(1) 다른 모듈 내부에 있는 변수나 기능을 직접 참조하여 사용하는 경우의 결합도

(2) 모듈 간 인터페이스로 배열, 오브젝트, 구조체 등 자료구조가 인자로 전달되는 경우의 결합도

(3) 파라미터가 아닌 모듈 외부에 선언된 전역 변수를 여러 모듈이 함께 참조하고 갱신하는 경우의 결합도

보기:
ㄱ. 자료 결합도    ㄴ. 스탬프 결합도
ㄷ. 제어 결합도    ㄹ. 공통 결합도
ㅁ. 내용 결합도    ㅂ. 외부 결합도',
  '내용 결합도, 스탬프 결합도, 공통 결합도',
  '다른 모듈의 내부 구현(지역 변수, 내부 함수 등)에 직접 접근하는 경우 가장 강한 결합도인 내용 결합도에 해당한다.
모듈 사이에 자료구조 전체를 인자로 주고받는 경우는 스탬프 결합도로 분류한다.
전역 변수를 여러 모듈이 함께 사용하고 갱신하는 형태는 공통 결합도에 해당한다.
따라서 (1) 내용 결합도, (2) 스탬프 결합도, (3) 공통 결합도가 정답이다.',
  'prac:2025-1:Q12'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q13. Java – 상속과 동적 바인딩
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31101,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음 Java 프로그램을 실행했을 때 출력되는 결과를 작성하시오.

```java
public class Main {
    public static void main(String[] args) {
        new Child();
        System.out.println(Parent.total);
    }
}

class Parent {
    static int total = 0;
    int v = 1;

    public Parent() {
        total += (++v);
        show();
    }

    public void show() {
        total += total;
    }
}

class Child extends Parent {
    int v = 10;

    public Child() {
        v += 2;
        total += v++;
        show();
    }

    @Override
    public void show() {
        total += total * 2;
    }
}
```',
  '54',
  'Parent 생성자가 먼저 호출되며, Parent의 v는 1에서 ++v로 2가 되고 total은 2가 된다.
이후 생성자에서 show()를 호출할 때, 실제 인스턴스는 Child이므로 재정의된 Child.show가 호출되어 total += total * 2가 수행된다.
현재 total이 2이므로 total = 2 + 2 * 2 = 6이 된다.

Child 생성자에서는 v가 10에서 12로 증가한 뒤 total += v++를 수행한다.
이때 v++는 12를 더하고 v는 13이 되므로 total = 6 + 12 = 18이다.
이어 다시 Child.show가 호출되어 total += total * 2가 수행되며, total = 18 + 18 * 2 = 54가 된다.

main에서 Parent.total을 출력하므로 최종 출력값은 54이다.',
  'prac:2025-1:Q13'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q14. Adapter 패턴
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 설명에 해당하는 디자인 패턴의 이름을 영문으로 작성하시오.

서로 다른 인터페이스를 가진 클래스들을 연결하여 함께 사용할 수 있도록 해 주는 패턴이다.
기존 클래스(Adaptee)를 원하는 인터페이스(Target)에 맞게 변환하는 어댑터(Adapter)를 만들어, 기존 클래스를 감싸는 래퍼(wrapper) 역할을 한다.

아래 표는 GoF 디자인 패턴을 생성, 구조, 행위 유형으로 분류한 것이다.

| 생성 (Creational)   | 구조 (Structural) | 행위 (Behavioral) |
|---------------------|-------------------|-------------------|
| Singleton           | Adapter           | Strategy          |
| Factory Method      | Bridge            | Template Method   |
| Abstract Factory    | Composite         | Observer          |
| Builder             | Decorator         | State             |
| Prototype           | Façade            | Command           |

위 설명과 표를 함께 참고하여, 구조 패턴 중 이러한 역할을 수행하는 패턴의 이름을 작성하시오.',
  'Adapter',
  '기존 클래스의 인터페이스를 클라이언트가 기대하는 인터페이스로 변환해 주는 구조 패턴을 어댑터(Adapter) 패턴이라고 한다.
표에서도 구조(Structural) 분류에 Adapter가 포함되어 있으며, 설명에서 언급한 "기존 클래스를 감싸서 인터페이스를 변환해 주는 래퍼" 역할과 정확히 일치한다.
따라서 정답은 Adapter이다.',
  'prac:2025-1:Q14'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q15. 문장(Statement) 커버리지
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source, image_url
) VALUES (
  @cert_id,
  @tp_31101,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음은 어떤 함수의 제어 흐름도를 단순화하여 번호(1~6)로 표시한 것이다.
각 번호에 아래 코드 조각 중 하나를 연결하고, 문장(Statement) 커버리지를 만족하는 실행 순서를 번호로만 나열하시오.

함수는 다음과 같다.

```c
int Main(int b[], int m, int x) {
    int a = 0;
    while (a < m || b[a] < x) {
        if (b[a] < 0)
            b[a] = -b[a];
        a++;
    }
    return 1;
}
```

보기 – 제어 흐름도 상의 번호에 들어갈 코드 조각

1. int a = 0;
2. while (a < m || b[a] < x)
3. if (b[a] < 0)
4. b[a] = -b[a];
5. a++;
6. return 1;

제어 흐름도에서 시작점 1과 2는 이미 고정되어 있고,
문장 커버리지를 만족하는 순서는

1 → 2 → (         )

형태로 표현된다.

빈칸에 들어갈 번호들의 나열을 공백 없이 작성하시오.',
  '1234526',
  '코드의 제어 흐름을 번호로 치환하면 다음과 같이 대응시킬 수 있다.
1: int a = 0;
2: while (a < m || b[a] < x)
3: if (b[a] < 0)
4: b[a] = -b[a];
5: a++;
6: return 1;

while이 참인 동안 if 분기 결과에 따라 4가 실행될 수도 있고 건너뛸 수도 있지만, 문장 커버리지를 위해서는 모든 문장이 한 번 이상 실행되면 된다.
예를 들어 b[a] < 0인 경우를 한 번, 이후 조건이 거짓이 되어 while을 빠져나오는 경로를 선택하면 다음과 같은 경로가 된다.

1 → 2 → 3 → 4 → 5 → 2 → 6

이를 번호만 이어 쓰면 1234526이 된다.
따라서 빈칸에는 1234526이 들어간다.',
  'prac:2025-1:Q15',
  'https://api.mycertpilot.com/static/images/questions/pq_2025_01_15.png'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q16. Java – 분할 정복 + Math.max
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31101,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음 Java 프로그램을 실행했을 때 출력되는 결과를 작성하시오.

```java
public class Main {

    public static void main(String[] args) {
        int[] data = {3, 5, 8, 12, 17};
        System.out.println(func(data, 0, data.length - 1));
    }

    static int func(int[] a, int st, int end) {
        if (st >= end) return 0;
        int mid = (st + end) / 2;
        return a[mid] + Math.max(func(a, st, mid), func(a, mid + 1, end));
    }
}
```',
  '20',
  '배열 a = {3, 5, 8, 12, 17}이고, 처음 호출은 func(a, 0, 4)이다.
mid = (0 + 4) / 2 = 2이므로 a[mid] = 8이다.

왼쪽: func(a, 0, 2)
- mid = 1, a[1] = 5
- 왼쪽: func(a, 0, 1) → mid = 0, a[0] = 3, 두 하위 호출은 st >= end 조건으로 0을 반환하므로 결과 3
- 오른쪽: func(a, 2, 2)는 st >= end 이므로 0
- 따라서 func(a, 0, 2) = 5 + max(3, 0) = 8

오른쪽: func(a, 3, 4)
- mid = (3 + 4) / 2 = 3, a[3] = 12
- 왼쪽: func(a, 3, 3) = 0
- 오른쪽: func(a, 4, 4) = 0
- 따라서 func(a, 3, 4) = 12 + max(0, 0) = 12

최종적으로 func(a, 0, 4) = 8 + max(8, 12) = 8 + 12 = 20이므로 출력값은 20이다.',
  'prac:2025-1:Q16'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q17. Python – 트리 레벨 합
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 Python 프로그램을 실행했을 때 출력되는 결과를 작성하시오.

```python
class Node:
    def __init__(self, value):
        self.value = value
        self.children = []

def tree(li):
    nodes = [Node(i) for i in li]
    for i in range(1, len(li)):
        nodes[(i - 1) // 2].children.append(nodes[i])
    return nodes[0]

def calc(node, level=0):
    if node is None:
        return 0
    return (node.value if level % 2 == 1 else 0) + \
           sum(calc(n, level + 1) for n in node.children)

li = [3, 5, 8, 12, 15, 18, 21]

root = tree(li)

print(calc(root))
```',
  '13',
  'tree(li)는 완전 이진트리 형태로 노드를 배치한다.
인덱스 기준으로 부모의 자식은 (i - 1) // 2에 연결되므로, 값과 레벨은 다음과 같다.

- 레벨 0: 3 (루트)
- 레벨 1: 5, 8
- 레벨 2: 12, 15, 18, 21

calc 함수는 레벨이 홀수일 때만 해당 노드의 value를 더한다.
레벨 1 노드 값은 5와 8이므로 합은 13이다.
레벨 3 이후에는 자식이 없으므로 재귀 호출 결과는 0이 된다.
따라서 최종 출력값은 13이다.',
  'prac:2025-1:Q17'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q18. C 언어 – 단일 연결 리스트 재연결
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31101,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음 C 프로그램을 실행했을 때 출력되는 결과를 작성하시오.

```c
#include <stdio.h>
#include <stdlib.h>

typedef struct Data {
    int value;
    struct Data *next;
} Data;

Data* insert(Data* head, int value) {
    Data* new_node = (Data*)malloc(sizeof(Data));
    new_node->value = value;
    new_node->next = head;
    return new_node;
}

Data* reconnect(Data* head, int value) {
    if (head == NULL || head->value == value) return head;

    Data *prev = NULL, *curr = head;

    while (curr != NULL && curr->value != value) {
        prev = curr;
        curr = curr->next;
    }

    if (curr != NULL && prev != NULL) {
        prev->next = curr->next;
        curr->next = head;
        head = curr;
    }

    return head;
}

int main() {
    Data *head = NULL, *curr;
    for (int i = 1; i <= 5; i++)
        head = insert(head, i);

    head = reconnect(head, 3);

    for (curr = head; curr != NULL; curr = curr->next)
        printf("%d", curr->value);

    return 0;
}
```',
  '35421',
  'insert 함수는 새 노드를 항상 리스트의 맨 앞에 삽입한다.
for 루프에서 1부터 5까지 차례대로 insert하면 리스트는 다음과 같이 된다.

- i = 1: 1
- i = 2: 2 → 1
- i = 3: 3 → 2 → 1
- i = 4: 4 → 3 → 2 → 1
- i = 5: 5 → 4 → 3 → 2 → 1

reconnect(head, 3)은 값이 3인 노드를 찾아 리스트 맨 앞으로 이동시키는 함수이다.
현재 리스트에서 3은 5 → 4 → 3 → 2 → 1 중 세 번째 노드이다.
prev는 4, curr는 3이 되고, prev->next를 curr->next(2)로 바꾸고 curr->next를 head(5)로 연결한 뒤 head를 curr로 바꾼다.

재연결 후 리스트는 3 → 5 → 4 → 2 → 1 순서가 된다.
for 루프에서 값을 차례대로 출력하므로 결과는 35421이다.',
  'prac:2025-1:Q18'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q19. C 언어 – 비트 연산과 합계
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31101,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음 C 프로그램을 실행했을 때 출력되는 결과를 10진수 정수로 작성하시오.

```c
#include <stdio.h>

typedef struct student {
    char* name;
    int score[3];
} Student;

int dec(int enc) {
    return enc & 0xA5;
}

int sum(Student* p) {
    return dec(p->score[0]) + dec(p->score[1]) + dec(p->score[2]);
}

int main() {
    Student s[2] = {
        "Kim", {0xA0, 0xA5, 0xDB},
        "Lee", {0xA0, 0xED, 0x81}
    };
    Student* p = s;
    int result = 0;

    for (int i = 0; i < 2; i++) {
        result += sum(&s[i]);
    }
    printf("%d", result);
    return 0;
}
```',
  '908',
  'dec 함수는 인코딩된 정수 enc에 대해 0xA5와 AND 연산을 수행한다.
각 점수에 대해 enc & 0xA5를 계산한 뒤 모두 더한 값이 result로 누적된다.
문제에서 제공된 해설에 따르면 모든 연산을 수행한 최종 결과는 908이다.
따라서 화면에는 908이 출력된다.',
  'prac:2025-1:Q19'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q20. Java – 재귀 오버로드
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 Java 프로그램을 실행했을 때 출력되는 결과를 작성하시오.

```java
public class Main {
    public static void main(String[] args) {
        System.out.println(calc("5"));
    }

    static int calc(int value) {
        if (value <= 1) return value;
        return calc(value - 1) + calc(value - 2);
    }

    static int calc(String str) {
        int value = Integer.valueOf(str);
        if (value <= 1) return value;
        return calc(value - 1) + calc(value - 3);
    }
}
```',
  '4',
  'main에서는 문자열 "5"를 인자로 calc(String)을 호출한다.
calc(String)에서 value는 5가 되고, value > 1이므로

calc("5") = calc(4) + calc(2)

가 된다. 여기서 calc(4)와 calc(2)는 정수 버전 calc(int)로 호출된다.

calc(2):
- calc(2) = calc(1) + calc(0) = 1 + 0 = 1

calc(4):
- calc(4) = calc(3) + calc(2)
- calc(3) = calc(2) + calc(1) = 1 + 1 = 2
- calc(2)는 위에서 1이므로 calc(4) = 2 + 1 = 3

따라서 calc("5") = 3 + 1 = 4가 되고, 최종 출력값은 4이다.',
  'prac:2025-1:Q20'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');
