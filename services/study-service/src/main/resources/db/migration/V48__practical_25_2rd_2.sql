SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

------------------------------------------------------------
-- Q11. 분기(Branch) 커버리지를 만족하는 테스트 경로
--  - Topic: P.1.1 업무 시나리오 해석 (31101)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source, image_url
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음 아래 제어 흐름 그래프가 분기 커버리지를 만족하기 위한 테스팅 순서를 쓰시오.
,
  '1234567, 124561',
  '분기(Branch) 커버리지는 프로그램 내의 모든 분기(조건문의 YES/NO)를
최소 한 번 이상 수행하는 것을 목표로 합니다.

이 제어 흐름 그래프에는 두 개의 주요 분기가 있습니다.
- ②번 노드: X > K ?  (YES / NO)
- ⑥번 노드: RESULT > 1 ? (YES / NO)

따라서 모든 분기를 커버하려면
- ②에서 YES/NO 모두 한 번 이상,
- ⑥에서 YES/NO 모두 한 번 이상
나오도록 경로를 설계해야 합니다.

예를 들어 다음 두 경로를 사용하면 됩니다.

1) 1 → 2 → 4 → 5 → 6 → 7
   - ②: X > K ? 에서 NO 분기
   - ⑥: RESULT > 1 ? 에서 NO 분기 → ⑦로 종료

2) 1 → 2 → 3 → 2 → 4 → 5 → 6 → 1
   - ②: 처음에는 YES 분기로 ③ 이동
   - POINTER = TRUE 설정 후 다시 ②로 돌아와 NO 분기로 ④ 이동
   - ⑥: RESULT > 1 ? 에서 YES 분기로 다시 ①/②로 루프

이 두 경로를 합치면
- ②의 YES/NO,
- ⑥의 YES/NO
모든 분기를 최소 한 번씩 통과하게 되어 분기 커버리지를 만족합니다.,
  'past:2025-2:Q11',
  'https://api.mycertpilot.com/static/images/questions/pq_2025_02_11.png'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q12. C 언어 원형 큐(Circular Queue) 동작
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
  '다음 C 프로그램을 실행했을 때의 출력 결과를 쓰시오.

```c
#define SIZE 3

typedef struct {
    int a[SIZE];
    int front;
    int rear;
} Queue;

void enq(Queue* q, int val) {
    q->a[q->rear] = val;
    q->rear = (q->rear + 1) % SIZE;
}

int deq(Queue* q) {
    int val = q->a[q->front];
    q->front = (q->front + 1) % SIZE;
    return val;
}

int main() {
    Queue q = {{0}, 0, 0};

    enq(&q, 1);
    enq(&q, 2);
    deq(&q);
    enq(&q, 3);

    printf("%d 그리고 %d", deq(&q), deq(&q));
    return 0;
}
```',
  '2 그리고 3',
  '원형 큐의 front, rear 움직임을 순서대로 따라가 보겠습니다.

초기 상태
- a = {0, 0, 0}, front = 0, rear = 0

1) enq(&q, 1)
   - a[rear] = a[0] = 1
   - rear = (0 + 1) % 3 = 1
   → a = {1, 0, 0}, front = 0, rear = 1

2) enq(&q, 2)
   - a[rear] = a[1] = 2
   - rear = (1 + 1) % 3 = 2
   → a = {1, 2, 0}, front = 0, rear = 2

3) deq(&q)
   - 반환값 val = a[front] = a[0] = 1
   - front = (0 + 1) % 3 = 1
   → a = {1, 2, 0}, front = 1, rear = 2

4) enq(&q, 3)
   - a[rear] = a[2] = 3
   - rear = (2 + 1) % 3 = 0
   → a = {1, 2, 3}, front = 1, rear = 0

5) printf에서 deq 두 번 호출
   - 첫 번째 deq: val = a[1] = 2, front = 2
   - 두 번째 deq: val = a[2] = 3, front = 0

따라서 화면에는
“2 그리고 3”
이 출력됩니다.',
  'past:2025-2:Q12'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q13. RR(Round Robin) 스케줄링 평균 대기시간
--  - Topic: P.5.2 운영/스케줄링 분석 (31502)
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
  '운영체제에서 라운드로빈(Round Robin, RR) 스케줄링은
각 프로세스에 동일한 시간 할당량(타임 퀀텀)을 순차적으로 부여하며
CPU를 할당하는 방식이다.

다음은 4개의 프로세스가 서로 다른 시간에 도착하며
각기 다른 실행 시간을 가지는 상황이다.
시간 할당량은 4ms 이고, 컨텍스트 스위칭 시간은 무시한다.

| 프로세스 | 도착 시간(ms) | 실행 시간(ms) |
|---------|---------------|---------------|
| P1      | 0             | 8             |
| P2      | 1             | 4             |
| P3      | 2             | 9             |
| P4      | 3             | 5             |

위 정보를 바탕으로 RR 방식으로 CPU 스케줄링을 수행할 경우,
모든 프로세스의 평균 대기시간(Average Waiting Time)은 얼마인가?

평균 대기시간을 ms 단위 실수로 쓰시오.',
  '11.75',
  'RR 스케줄링에서 각 프로세스는 차례대로 4ms씩 CPU를 배정받습니다.
도착 시간을 고려하여 타임라인을 구성하면 다음과 같은 실행 순서를 얻을 수 있습니다.

1) 0~4ms: P1 (남은 실행 시간 4ms)
2) 4~8ms: P2 (총 4ms 필요 → 여기서 종료)
3) 8~12ms: P3 (남은 5ms)
4) 12~16ms: P4 (남은 1ms)
5) 16~20ms: P1 (나머지 4ms 실행 후 종료)
6) 20~24ms: P3 (4ms 실행, 남은 1ms)
7) 24~25ms: P4 (1ms 실행 후 종료)
8) 25~26ms: P3 (마지막 1ms 실행 후 종료)

각 프로세스의 “대기시간”을 계산하면 다음과 같습니다.
- P1: 도착 0, 완료 20, 실행시간 8 → 대기시간 = 20 - 0 - 8 = 12
- P2: 도착 1, 완료 8,  실행시간 4 → 대기시간 = 8  - 1 - 4 = 3
- P3: 도착 2, 완료 26, 실행시간 9 → 대기시간 = 26 - 2 - 9 = 15
- P4: 도착 3, 완료 25, 실행시간 5 → 대기시간 = 25 - 3 - 5 = 17

대기시간 합계는
12 + 3 + 15 + 17 = 47(ms) 이고,
프로세스는 4개이므로

평균 대기시간 = 47 / 4 = 11.75(ms)

따라서 정답은 **11.75** 입니다.',
  'past:2025-2:Q13'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

------------------------------------------------------------
-- Q14. C 구조체 배열 및 포인터
--  - Topic: P.1.1 코드/자료구조 해석 (31101)
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
  '다음 C 프로그램을 실행했을 때의 출력 결과를 쓰시오.

```c
#include <stdio.h>

struct dat {
    int x;
    int y;
};

int main() {
    struct dat a[] = {{1, 2}, {3, 4}, {5, 6}};
    struct dat* ptr = a;
    struct dat** pptr = &ptr;

    (*pptr)[1] = (*pptr)[2];
    printf("%d 그리고 %d", a[1].x, a[1].y);

    return 0;
}
```',
  '5 그리고 6',
  '코드에서 중요한 부분은

- a는 구조체 배열: a[0]={1,2}, a[1]={3,4}, a[2]={5,6}
- ptr = a;  → ptr은 a[0]을 가리키는 포인터
- pptr = &ptr; → pptr은 ptr의 주소를 가리키는 이중 포인터

문장
(*pptr)[1] = (*pptr)[2];

을 해석해 보면,
- (*pptr)는 ptr과 동일
- (*pptr)[1] 은 a[1]
- (*pptr)[2] 은 a[2]

따라서 a[1] = a[2] 대입이 일어나고,
배열 a는 다음과 같이 변합니다.
- a[0] = {1, 2}
- a[1] = {5, 6}
- a[2] = {5, 6}

마지막 printf는 a[1].x, a[1].y 를 출력하므로

“5 그리고 6”

이 출력됩니다.',
  'past:2025-2:Q14'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q15. Java 참조 타입과 배열 요소 변경
--  - Topic: P.1.1 코드/객체 참조 해석 (31101)
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
  '다음 Java 프로그램을 실행했을 때의 출력 결과를 쓰시오.

```java
public class Gisafirst {
    public static class BO {
        public int v;
        public BO(int v) {
            this.v = v;
        }
    }

    public static void main(String[] args) {
        BO a = new BO(1);
        BO b = new BO(2);
        BO c = new BO(3);
        BO[] arr = {a, b, c};
        BO t = a;
        arr[0] = c;
        arr[2] = t;
        arr[1].v = arr[0].v;
        System.out.println(a.v + "a" + b.v + "b" + c.v);
    }
}
```',
  '1a3b3',
  '객체 참조의 이동과 필드 값 변경 순서를 추적해 보겠습니다.

초기 생성
- a.v = 1
- b.v = 2
- c.v = 3
- arr = {a, b, c}

1) BO t = a;
   - t는 a 객체를 가리킴

2) arr[0] = c;
   - arr = {c, b, c}

3) arr[2] = t;
   - t는 a를 가리키므로 arr[2] = a
   - arr = {c, b, a}

4) arr[1].v = arr[0].v;
   - arr[0]는 c, arr[1]은 b
   - arr[1].v = arr[0].v → b.v = c.v = 3
   - 이제 값은
     - a.v = 1
     - b.v = 3
     - c.v = 3

출력문
- a.v + "a" + b.v + "b" + c.v
= "1a3b3"

따라서 정답은 **1a3b3** 입니다.',
  'past:2025-2:Q15'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q16. C 단순 연결 리스트 구조
--  - Topic: P.1.1 코드/자료구조 해석 (31101)
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
  '다음 C 프로그램을 실행했을 때의 출력 결과를 쓰시오.

```c
#include <stdio.h>

struct node {
    int p;
    struct node* n;
};

int main() {
    struct node a = {1, NULL};
    struct node b = {2, NULL};
    struct node c = {3, NULL};

    c.n = &a;
    a.n = &b;
    b.n = NULL;

    struct node* head = &c;
    printf("%d %d %d", head->p, head->n->p, head->n->n->p);
    return 0;
}
```',
  '3 1 2',
  '연결 관계를 정리하면 다음과 같습니다.

- c.n = &a;  → c → a
- a.n = &b;  → a → b
- b.n = NULL → b에서 끝

따라서 리스트 구조는

c(3) → a(1) → b(2)

가 됩니다.

head는 &c 이므로,
- head->p        = 3
- head->n->p     = a.p = 1
- head->n->n->p  = b.p = 2

따라서 출력 결과는

“3 1 2”

입니다.',
  'past:2025-2:Q16'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q17. Python 딕셔너리/집합 연산
--  - Topic: P.1.1 코드/파이썬 집합 해석 (31101)
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
  '다음 Python 코드를 실행했을 때 출력되는 결과를 쓰시오.

```python
def gisafirst():
    lst = [1, 2, 3]
    dic = {x: x * 2 for x in lst}
    s = set(dic.values())
    lst[0] = 99
    dic[2] = 7
    s.add(99)
    return len(s & set(dic.values()))

print(gisafirst())
```',
  '2',
  '코드를 순서대로 따라가 보겠습니다.

1) lst = [1, 2, 3]

2) dic = {x: x * 2 for x in lst}
   - lst 값 기준으로
   - dic = {1: 2, 2: 4, 3: 6}

3) s = set(dic.values())
   - dic.values() = {2, 4, 6}
   - s = {2, 4, 6}

4) lst[0] = 99
   - lst는 이제 [99, 2, 3] 이지만,
   - dic 은 이미 만들어진 상태라 변화 없음

5) dic[2] = 7
   - key 2의 value가 4에서 7로 변경
   - dic = {1: 2, 2: 7, 3: 6}

6) s.add(99)
   - s = {2, 4, 6, 99}

7) s & set(dic.values())
   - dic.values() = {2, 7, 6}
   - s ∩ dic.values() = {2, 6}

집합의 크기 len({2, 6}) = 2 이므로
최종 반환값은 2, 출력은

“2”

가 됩니다.',
  'past:2025-2:Q17'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q18. C 단순 연결 리스트를 이용한 문자열 역순 출력
--  - Topic: P.1.1 코드/포인터 해석 (31101)
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
  '다음 C 프로그램을 실행했을 때의 출력 결과를 쓰시오.

```c
#include <stdio.h>
#include <stdlib.h>

struct node {
    char c;
    struct node* p;
};

struct node* func(char* s) {
    struct node* h = NULL, *n;

    while (*s) {
        n = malloc(sizeof(struct node));
        n->c = *s++;
        n->p = h;
        h = n;
    }

    return h;
}

int main() {
    struct node* n = func("BEST");

    while (n) {
        putchar(n->c);
        struct node* t = n;
        n = n->p;
        free(t);
    }

    return 0;
}
```',
  'TSEB',
  '문자열 "BEST" 를 하나씩 읽어가며 단순 연결 리스트를 생성하는 코드입니다.

func 함수에서 while(*s) 루프 동작:
- 처음 문자 ''B'' 처리
  - 새 노드에 c = ''B'', p = NULL
  - h = 이 노드
- 다음 문자 ''E'' 처리
  - 새 노드에 c = ''E'', p = 이전 h (''B'')
  - h = 이 새 노드
- 다음 문자 ''S'' 처리
  - c = ''S'', p = ''E'' 노드
  - h = ''S'' 노드
- 마지막 문자 ''T'' 처리
  - c = ''T'', p = ''S'' 노드
  - h = ''T'' 노드

결과적으로 리스트 구조는

T → S → E → B → NULL

main 에서 while(n) 루프는
- 현재 노드의 c를 출력한 뒤
- 다음 노드로 n = n->p 로 이동

따라서 출력 순서는

T, S, E, B

가 되어

“TSEB”

가 화면에 출력됩니다.',
  'past:2025-2:Q18'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

------------------------------------------------------------
-- Q19. TCP 3-way 핸드셰이크 취약점을 이용한 공격
--  - Topic: P.5.2 장애 분석 및 보안 사고 대응 (31502)
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
  '다음은 TCP 통신 과정에서 발생할 수 있는 보안 취약점에 대한 설명이다.
이를 이용한 공격 기법의 이름을 쓰시오.

TCP는 연결을 수립하기 위해
클라이언트가 서버에 SYN 패킷을 보내고,
서버는 SYN-ACK 패킷으로 응답한 뒤,
클라이언트가 다시 ACK 패킷을 보내는
3-way 핸드셰이크 과정을 거친다.

공격자는 클라이언트 역할로 수많은 SYN 패킷을 서버에 전송한 뒤,
마지막 ACK를 고의로 보내지 않아
서버가 연결 대기 상태를 계속 유지하게 만든다.

이로 인해 서버의 연결 대기 큐가 가득 차면서
정상적인 접속 요청을 처리하지 못하게 되어
서비스 거부(DoS) 상태가 발생한다.

위와 같은 방식의 공격 기법 이름을 쓰시오.',
  'SYN 플러딩(SYN Flooding)',
  '설명에서 핵심 키워드는 다음과 같습니다.

- TCP 3-way 핸드셰이크 과정(SYN → SYN-ACK → ACK)을 악용
- 클라이언트가 SYN만 잔뜩 보내고 마지막 ACK를 보내지 않음
- 서버는 半-연결 상태(half-open)를 유지하며 대기
- 연결 대기 큐가 가득 차 정상 요청을 처리하지 못함
- 결과적으로 서비스 거부(DoS) 상황 유발

이는 TCP의 연결 설정 단계에서 “SYN 패킷”을 과도하게 보내는
**SYN 플러딩(SYN Flooding)** 공격의 전형적인 설명입니다.

따라서 정답은
“SYN 플러딩” 또는 “SYN Flooding” 입니다.',
  'past:2025-2:Q19'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

------------------------------------------------------------
-- Q20. 관계 대수 πTTL(employee) 연산 결과
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
  '다음은 employee 테이블의 내용이다.

| Index | AGE | TTL  |
|-------|-----|------|
| 1     | 55  | 부장 |
| 2     | 35  | 대리 |
| 3     | 42  | 과장 |
| 4     | 45  | 차장 |

이때 관계 대수 연산
πTTL(employee)
의 결과로 얻어지는 값들을 모두 쓰시오.

(중복이 제거된 TTL 값들을 쉼표로 구분하여 작성하시오.)',
  '부장, 대리, 과장, 차장',
  '관계 대수에서 π (프로젝션, Projection) 연산은
특정 속성(열)만 선택하여 보여주는 연산이며,
동일한 속성 값은 “중복을 제거”한 집합 형태로 취급합니다.

테이블에서 TTL 열만 보면

- 부장
- 대리
- 과장
- 차장

네 가지 직급이 모두 서로 다른 값이므로,
중복 제거 여부와 상관없이 그대로 네 값이 결과에 포함됩니다.

따라서
πTTL(employee)
의 결과는

부장, 대리, 과장, 차장

입니다.',
  'past:2025-2:Q20'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_MODEL_ER_PHYSICAL');
