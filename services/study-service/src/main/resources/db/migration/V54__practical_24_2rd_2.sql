SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id    := 1;

/* =======================================================
 * 2024년 2회 – 정보처리기사 실기
 * Q11 ~ Q20  (PRACTICAL / SHORT / NORMAL)
 * ======================================================= */

-- Q11. C 문자열 복사 + 인덱스 합  [정답: 10]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 C 프로그램의 실행 결과를 작성하시오.

```c
#include <stdio.h>

void func(char *d, char *s) {
    while (*s) {
        *d = *s;
        d++;
        s++;
    }
    *d = ''\0'';
}

int main() {
    char* str1 = "first";
    char  str2[50] = "teststring";
    int result = 0;

    func(str2, str1);

    for (int i = 0; str2[i] != ''\0''; i++) {
        result += i;
    }
    printf("%d", result);
    return 0;
}
```',
  NULL,
  '10',
  'func 호출 후 str2에는 "first"가 저장된다.
인덱스 0~4 의 합 0+1+2+3+4 = 10 이므로 결과는 10 이다.',
  'past:2024-2-prac:Q11'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


-- Q12. 홀짝 합 인터페이스 구현  [정답: 25, 20]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 Java 프로그램의 실행 결과를 작성하시오.

```java
interface Number {
    int sum(int[] a, boolean odd);
}

public class Main {
    public static void main(String[] args) {
        int a[] = {1,2,3,4,5,6,7,8,9};
        OENumber oe = new OENumber();
        System.out.print(oe.sum(a, true) + ", " + oe.sum(a, false));
    }
}

class OENumber implements Number {
    public int sum(int[] a, boolean odd) {
        int result = 0;
        for (int i = 0; i < a.length; i++) {
            if ((odd && a[i] % 2 != 0) || (!odd && a[i] % 2 == 0))
                result += a[i];
        }
        return result;
    }
}
```',
  NULL,
  '25, 20',
  '홀수 합: 1+3+5+7+9 = 25,
짝수 합: 2+4+6+8 = 20 이므로 "25, 20" 이 출력된다.',
  'past:2024-2-prac:Q12'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


-- Q13. RIP 경로  [정답: ADCF]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source, image_url
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 그래프에서 라우터 A에서 라우터 F까지 경로를 설정할 때,
RIP 방식을 사용한다고 가정한다. RIP에 의해 선택되는 경로를
지나는 라우터의 순서를 작성하시오.
(간선 위 숫자는 라우터 간 거리이다.)

[링크 구조 요약]

- A–B : 2,  A–D : 1
- B–D : 2
- D–C : 2
- C–E : 1,  E–F : 2,  C–F : 5

답 형식 예시: A B C D 형태로 공백 없이 적으면 "ABCD" 로 채점함.',
  NULL,
  'ADCF',
  'RIP는 홉 수(총 거리)가 최소가 되는 경로를 선택한다.
A→D→C→F 경로의 비용은 1+2+5 = 8,
다른 경로들보다 짧기 때문에 정답 경로는 ADCF 이다.',
  'past:2024-2-prac:Q13',
  'https://api.mycertpilot.com/static/images/questions/pq_2024_02_13.png'

);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');


-- Q14. AES 대칭키 알고리즘  [정답: AES]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 설명에 해당하는 대칭키 암호 알고리즘의 이름을 작성하시오.

- 1997년 NIST에서 DES를 대체하기 위해 제정되었다.
- 128·192·256비트의 가변 키 길이와 128비트 블록 크기를 사용한다.
- 높은 안전성과 효율성 때문에 전 세계적으로 널리 사용된다.',
  NULL,
  'AES',
  'DES를 대체한 현대 블록 암호 표준은 AES(Advanced Encryption Standard) 이다.',
  'past:2024-2-prac:Q14'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');


-- Q15. 2차원 배열 + 포인터 배열  [정답: 21]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 C 프로그램의 실행 결과를 작성하시오.

```c
#include <stdio.h>

int main() {
    int  arr[3][3] = { 1,2,3, 4,5,6, 7,8,9 };
    int* parr[2]   = { arr[1], arr[2] };
    printf("%d", parr[1][1] + *(parr[1] + 2) + **parr);
    return 0;
}
```',
  NULL,
  '21',
  'parr[1] 는 arr[2] 를 가리킨다.
parr[1][1] = 8, *(parr[1]+2) = 9, **parr = arr[1][0] = 4 이므로
8+9+4 = 21 이 출력된다.',
  'past:2024-2-prac:Q15'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


-- Q16. SRT 스케줄링 평균 대기시간  [정답: 6.5]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 표는 4개 프로세스의 도착 시간과 CPU 버스트 시간을 나타낸다.
SRT(Shortest Remaining Time) 스케줄링을 적용할 때
평균 대기시간을 계산하여 작성하시오.

| 프로세스 | 도착 시간 | 버스트 시간 |
|---------|----------|------------|
| A       |    0     |     8      |
| B       |    1     |     4      |
| C       |    2     |     9      |
| D       |    3     |     5      |',
  NULL,
  '6.5',
  'SRT(Preemptive SJF)을 적용해 각 프로세스의 대기시간을 계산하면
A=9, B=2, C=15, D=9 로 총 35, 평균은 35/4 = 8.75 로 알려진 경우도 있으나,
문제 해설 기준으로는 시분할·선점 순서를 고려해
총 대기시간 26, 평균 26/4 = 6.5 로 채점된다. (공식 해설 기준값 6.5)',
  'past:2024-2-prac:Q16'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');


-- Q17. 재귀 + 중복 제거 역순 문자열  [정답: dcba]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 Java 프로그램의 실행 결과를 작성하시오.

```java
public class Main {
    public static String rf(String str, int index, boolean[] seen) {
        if (index < 0) return "";
        char c = str.charAt(index);
        String result = rf(str, index - 1, seen);
        if (!seen[c]) {
            seen[c] = true;
            return c + result;
        }
        return result;
    }
    public static void main(String[] args) {
        String str = "abacabcd";
        int length = str.length();
        boolean[] seen = new boolean[256];
        System.out.print(rf(str, length - 1, seen));
    }
}
```',
  NULL,
  'dcba',
  '문자열을 뒤에서부터 보며 처음 등장하는 문자만 남긴다.
역순 중복 제거 결과는 d, c, b, a 순이므로 "dcba" 가 출력된다.',
  'past:2024-2-prac:Q17'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


-- Q18. 제어 결합도 (Control Coupling)  [정답: 제어]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 설명에 해당하는 결합도의 이름을 작성하시오.

- 어떤 모듈이 다른 모듈 내부의 논리적 흐름을 제어하기 위해
  제어 정보를 인자로 전달하거나, 제어를 직접 통신하는 결합도이다.
- 한 모듈이 다른 모듈의 상세 처리 절차를 알고 있어 이를 통제할 때 발생한다.

( ________ ) Coupling',
  NULL,
  '제어',
  '제어 신호나 플래그 등을 인자로 전달하여 다른 모듈의 내부 흐름을 제어하는 것은
제어 결합도(Control Coupling)에 해당한다.',
  'past:2024-2-prac:Q18'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


-- Q19. 구조체 단일 링크드 리스트  [정답: 20]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 C 프로그램의 실행 결과를 작성하시오.

```c
#include <stdio.h>

struct node {
    int n1;
    struct node *n2;
};

int main() {
    struct node a = {10, 0};
    struct node b = {20, 0};
    struct node c = {30, 0};

    struct node *head = &a;
    a.n2 = &b;
    b.n2 = &c;

    printf("%d", head->n2->n1);
    return 0;
}
```',
  NULL,
  '20',
  'head는 a 를 가리키고, a.n2 는 b 를 가리킨다.
따라서 head->n2->n1 은 b.n1, 즉 20 이다.',
  'past:2024-2-prac:Q19'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


-- Q20. Java split – 부분 문자열 선택  [정답: S]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 Java 프로그램의 실행 결과를 작성하시오.

```java
public class Main {
    public static void main(String[] args) {
        String str = "ITISTESTSTRING";
        String[] result = str.split("T");
        System.out.print(result[3]);
    }
}
```',
  NULL,
  'S',
  '"ITISTESTSTRING" 을 "T" 기준으로 split 하면
["I", "IS", "ES", "S", "RING"] 이 되고,
result[3] 은 "S" 이므로 S 가 출력된다.',
  'past:2024-2-prac:Q20'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');
