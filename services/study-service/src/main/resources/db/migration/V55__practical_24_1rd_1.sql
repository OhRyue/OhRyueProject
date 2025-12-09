SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id    := 1;

/* =======================================================
 * 2024년 1회 – 정보처리기사 실기
 * Q1 ~ Q10 (모두 PRACTICAL / SHORT)
 * ======================================================= */

-- Q1. Java 싱글톤(static) 공유 인스턴스 카운트  [정답: 4]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 Java 코드의 실행 결과를 작성하시오.

```java
class Connection {

    private static Connection _inst = null;
    private int count = 0;

    static public Connection get() {
        if (_inst == null) {
            _inst = new Connection();
            return _inst;
        }
        return _inst;
    }

    public void count() {
        count++;
    }

    public int getCount() {
        return count;
    }
}

public class Main {
    public static void main(String[] args) {
        Connection conn1 = Connection.get();
        conn1.count();

        Connection conn2 = Connection.get();
        conn2.count();

        Connection conn3 = Connection.get();
        conn3.count();

        conn1.count();
        System.out.print(conn1.getCount());
    }
}
```',
  NULL,
  '4',
  'Connection 클래스는 정적 필드 _inst 에 단 하나의 인스턴스를 저장하는 싱글톤 패턴 구현이다.
get() 메서드를 호출하면 항상 동일한 _inst 인스턴스를 반환하므로 conn1, conn2, conn3 는 모두 같은 객체를 참조한다.
count() 메서드는 인스턴스 필드 count 를 1씩 증가시키므로,
호출 순서를 따라가면 총 네 번(count, count, count, count) 호출되어 최종 count 값은 4가 된다.
따라서 출력 결과는 4 이다.',
  'past:2024-1-prac:Q1'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

-- Q2. C 언어 삼항 연산자 + 비트 시프트  [정답: 151]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 C 언어 코드의 실행 결과를 작성하시오.

```c
int main() {
    int v1 = 0, v2 = 35, v3 = 29;

    if (v1 > v2 ? v2 : v1) {
        v2 = v2 << 2;
    } else {
        v3 = v3 << 2;
    }

    printf("%d", v2 + v3);
}
```',
  NULL,
  '151',
  '삼항 연산자 (조건식) ? v2 : v1 에서 조건식은 v1 > v2 이므로 0 > 35 가 되어 false 이다.
따라서 전체 표현식 값은 v1(0) 이고, if(0) 이므로 else 절이 실행된다.
else 블록에서 v3 = v3 << 2 가 되어 v3 는 29 × 2² = 116 으로 변경된다.
v2 는 35 그대로이므로 최종 v2 + v3 = 35 + 116 = 151 이 출력된다.',
  'past:2024-1-prac:Q2'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

-- Q3. 응집도 높은 순서 정렬  [정답: ㄱㄴㄹㄷ]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음은 응집도와 관련된 설명이다. 보기에서 응집도가 높은 순으로 나열하시오.

보기
ㄱ. 기능
ㄴ. 교환
ㄷ. 우연
ㄹ. 시간',
  NULL,
  'ㄱㄴㄹㄷ',
  '모듈 응집도는 강한 순서대로
기능적(기능) → 순차적 → 교환적 → 절차적 → 시간적 → 논리적 → 우연적 으로 본다.
보기에는 기능, 교환, 시간, 우연 네 가지가 주어졌으므로
가장 강한 기능적 응집(ㄱ), 그 다음 교환적 응집(ㄴ), 그 다음 시간적 응집(ㄹ), 마지막으로 가장 약한 우연적 응집(ㄷ) 순서가 된다.
따라서 정답은 ㄱㄴㄹㄷ 이다.',
  'past:2024-1-prac:Q3'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

-- Q4. C 문자열 뒤집기 후 홀수 인덱스 문자 출력  [정답: GECA]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음 C 언어 코드의 실행 결과를 작성하시오.

```c
#include <stdio.h>
#include <string.h>

void reverse(char* str) {
    int len = strlen(str);
    char temp;
    char *p1 = str;
    char *p2 = str + len - 1;

    while (p1 < p2) {
        temp = *p1;
        *p1 = *p2;
        *p2 = temp;
        p1++;
        p2--;
    }
}

int main(void) {
    char str[100] = "ABCDEFGH";

    reverse(str);

    int len = strlen(str);

    for (int i = 1; i < len; i += 2) {
        printf("%c", str[i]);
    }

    printf("\\n");
    return 0;
}
```',
  NULL,
  'GECA',
  '초기 문자열은 "ABCDEFGH" 이다.
reverse 함수는 포인터 p1, p2 를 사용하여 문자열 전체를 뒤집으므로, reverse 호출 이후 str 은 "HGFEDCBA" 가 된다.
len = 8 이고 for 문에서는 i = 1, 3, 5, 7 인 인덱스의 문자만 출력한다.
"HGFEDCBA" 에서 인덱스별 문자는
0:H, 1:G, 2:F, 3:E, 4:D, 5:C, 6:B, 7:A 이므로
순서대로 G, E, C, A 가 출력되어 결과는 "GECA" 가 된다.',
  'past:2024-1-prac:Q4'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

-- Q5. 서브넷/라우팅 – 각 네트워크에서 할당 가능한 IP 선택  [정답: 192.168.35.72 / 129.200.8.249 / 192.168.36.249]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source, image_url
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '아래 그림과 같이 라우터를 기준으로 여러 네트워크가 구성되어 있다.
각 네트워크에서 할당 가능한 2번, 4번, 5번의 IP 주소를 보기에서 골라 각각 작성하시오.

주어진 네트워크(예시)
1) 192.168.35.3/24
3) 129.200.10.16/22
6) 192.168.36.24/24

[보기]
- 192.168.35.0
- 192.168.35.72
- 192.168.36.0
- 192.168.36.249
- 129.200.8.0
- 129.200.8.249',
  NULL,
  '① 192.168.35.72  ② 129.200.8.249  ③ 192.168.36.249',
  '각 네트워크의 주소 범위를 CIDR 마스크로 계산한다.
/24 네트워크(192.168.35.0/24, 192.168.36.0/24) 에서는
네트워크 주소(.0)와 브로드캐스트 주소를 제외한 나머지 호스트 주소가 할당 가능하다.
따라서 192.168.35.72, 192.168.36.249 는 유효한 호스트 주소이지만 192.168.35.0, 192.168.36.0 은 네트워크 주소이므로 사용할 수 없다.
129.200.10.16/22 는 129.200.8.0~129.200.11.255 범위를 갖는 서브넷이므로
129.200.8.0 은 네트워크 주소이고, 129.200.8.249 는 유효한 호스트 주소가 된다.
따라서 2번, 4번, 5번에 들어갈 IP 는 각각 192.168.35.72, 129.200.8.249, 192.168.36.249 이다.',
  'past:2024-1-prac:Q5',
  'https://api.mycertpilot.com/static/images/questions/pq_2024_01_05.png'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

-- Q6. 정규형 판별 – 고객/강좌/강사 관계  [정답: 제3정규형]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31202,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음과 같은 릴레이션에서 만족하는 정규형을 작성하시오.

[테이블]

고객아이디 | 강좌명   | 강사번호
-----------+----------+---------
apple      | 영어회화 | P001
banana     | 기초토익 | P002
carrot     | 영어회화 | P001
carrot     | 기초토익 | P004
orange     | 영어회화 | P003
orange     | 기초토익 | P004',
  NULL,
  '제3정규형',
  '속성은 고객아이디, 강좌명, 강사번호 세 개이며, 한 고객이 여러 강좌를 수강할 수 있는 구조이다.
(고객아이디, 강좌명) 이 기본키가 되고, 강사번호는 해당 강좌를 담당하는 강사를 나타내는 종속 속성으로 볼 수 있다.
부분 함수 종속이나 이행 함수 종속 없이, 기본키에 대해서만 완전 함수 종속을 만족하므로 이미 제2정규형/제3정규형 조건을 충족한다.
따라서 이 릴레이션은 제3정규형에 해당한다.',
  'past:2024-1-prac:Q6'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_MODEL_NORMAL_DENORMAL');

-- Q7. 링크 상태 라우팅 프로토콜  [정답: OSPF]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 설명에 해당하는 네트워크 용어를 영문 약자로 작성하시오.

1. 대표적인 링크 상태 라우팅 프로토콜이다. 인터넷에서 연결된 링크의 상태를 감시하여 최적의 경로를 선택한다.
2. 단일 자율 시스템(AS) 내에서 라우팅 정보를 배포하는 내부 게이트웨이 프로토콜(IGP)이다.
3. 모든 대상에 도달하기 위한 최단 경로를 구축하며, 최단 경로 계산에 Dijkstra 알고리즘을 사용한다.',
  NULL,
  'OSPF',
  '링크 상태 기반 라우팅 프로토콜로, 자율 시스템 내부에서 동작하는 IGP 이며,
각 라우터가 링크 상태 데이터베이스를 가지고 Dijkstra 알고리즘으로 최단 경로 트리를 계산하는 프로토콜은 OSPF(Open Shortest Path First) 이다.
따라서 정답은 OSPF 이다.',
  'past:2024-1-prac:Q7'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

-- Q8. 조인 종류 – 세타/동등/자연 조인  [정답: 세타 조인 / 동등 조인 / 자연 조인]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31301,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 각 설명에 해당하는 조인의 이름을 순서대로 작성하시오.

(1) 조인에 참여하는 두 릴레이션의 속성 값을 비교하여 조건을 만족하는 튜플만 반환한다.

(2) (1)의 조인 중에서 조건이 정확하게 "=" 등호로 일치하는 결과만 반환한다.

(3) (2)에서 조인에 사용된 속성이 결과에 두 번 나타나지 않도록 중복된 속성을 제거한 조인이다.',
  NULL,
  '① 세타 조인  ② 동등 조인  ③ 자연 조인',
  '두 릴레이션의 속성 값을 비교하여 주어진 조건(=, >, <, BETWEEN 등)을 만족하는 튜플만 반환하는 일반적인 조인을 세타(θ) 조인이라고 한다.
이 중 조건이 "=" 인 경우만을 동등 조인(Equi Join)이라고 부르며,
동등 조인의 결과에서 조인에 사용된 같은 이름의 속성 중 하나를 제거하여 중복을 없앤 형태가 자연 조인(Natural Join)이다.
따라서 (1) 세타 조인, (2) 동등 조인, (3) 자연 조인 이 정답이다.',
  'past:2024-1-prac:Q8'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_SQL_SELECT_JOIN');

-- Q9. 페이지 교체 – LRU / LFU 페이지 부재 횟수  [정답: LRU 6, LFU 6]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음 페이지 참조 순서에서 할당된 프레임 수가 3개일 때,
LRU와 LFU 알고리즘의 페이지 부재(page fault) 횟수를 각각 작성하시오.

페이지 참조 순서 : 1, 2, 3, 1, 2, 4, 1, 2, 5, 7

(1) LRU :
(2) LFU :',
  NULL,
  '(1) 6  (2) 6',
  '프레임 3개, 초기에는 모두 비어 있다.

[LRU]
1, 2, 3 은 모두 미스이므로 3회 부재 후 [1,2,3].
이후 1, 2 참조는 히트.
4 참조 시 가장 오래 사용되지 않은 3 을 교체하여 [1,2,4] (부재 4번째).
다시 1, 2 는 히트.
5 참조 시 가장 오래 사용되지 않은 4 를 교체하여 [1,2,5] (부재 5번째).
7 참조 시 가장 오래 사용되지 않은 1 을 교체하여 [7,2,5] (부재 6번째).
따라서 LRU 의 페이지 부재 횟수는 6회이다.

[LFU]
참조될 때마다 각 페이지의 사용 횟수를 센다.
1, 2, 3 은 모두 1회씩 사용 후 [1,2,3] (3회 부재).
1, 2 재참조로 빈도는 1:3회, 2:3회, 3:1회.
4 참조 시 가장 사용 빈도가 적은 3 을 교체하여 [1,2,4] (4번째 부재).
이후 1, 2 재참조로 1:4회, 2:4회, 4:1회.
5 참조 시 가장 적은 4 를 교체하여 [1,2,5] (5번째 부재).
7 참조 시 가장 적은 5 를 교체하여 [1,2,7] (6번째 부재).
따라서 LFU 의 페이지 부재 횟수도 6회이다.',
  'past:2024-1-prac:Q9'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

-- Q10. Java 상속/생성자/메서드 호출 순서  [정답: 6 3 1 7 2]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음 Java 코드에서 주석 ①~⑦ 의 실행 순서를,
이미 주어진 ⑤ 다음에 올 순서(6개)를 번호만 공백으로 구분하여 작성하시오.

```java
class Parent {
    int x, y;

    Parent(int x, int y) {   // ①
        this.x = x;
        this.y = y;
    }

    int getT() {             // ②
        return x * y;
    }
}

class Child extends Parent {
    int x;

    Child(int x) {           // ③
        super(x + 1, x);
        this.x = x;
    }

    int getT(int n) {        // ④
        return super.getT() + n;
    }
}

class Main {
    public static void main(String[] args) {  // ⑤
        Parent parent = new Child(3);        // ⑥
        System.out.println(parent.getT());   // ⑦
    }
}
실행 순서 : ⑤ → ( ) → ( ) → ( ) → ( ) → ( )',
NULL,
'6 3 1 7 2',
'프로그램이 시작되면 먼저 main 메서드(⑤) 가 호출된다.
main 내부에서 new Child(3) 연산(⑥) 이 수행되며,
객체 생성 과정에서 Child 생성자(③) 가 먼저 실행되고,
super(x + 1, x) 호출을 통해 Parent 생성자(①) 가 이어서 실행된다.
객체 생성이 끝난 뒤 println 호출(⑦) 에서 parent.getT() 를 호출하는데,
Parent 에는 매개변수 없는 getT(), Child 에는 int 매개변수 하나를 받는 getT(int) 가 존재하므로
동적 디스패치 결과 Parent 의 getT() (②) 가 호출된다(오버로딩이지 오버라이딩이 아님).
따라서 전체 실행 순서는 ⑤ → ⑥ → ③ → ① → ⑦ → ② 이고,
문제에서 요구하는 ⑤ 이후의 순서는 6 3 1 7 2 이다.',
'past:2024-1-prac:Q10'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');
