SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id    := 1;

/* =======================================================
 * 2024년 2회 – 정보처리기사 실기
 * Q1 ~ Q10 (모두 PRACTICAL / SHORT / NORMAL)
 * ======================================================= */

-- Q1. Java 배열 동일성 비교  [정답: NNN]
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
public class Main {
    public static void check(int[] x, int[] y) {
        if (x == y) System.out.print("O");
        else System.out.print("N");
    }
    public static void main(String[] args) {
        int a[] = new int[] {1, 2, 3, 4};
        int b[] = new int[] {1, 2, 3, 4};
        int c[] = new int[] {1, 2, 3};
        check(a, b);
        check(b, c);
        check(a, c);
    }
}
```',
  NULL,
  'NNN',
  '배열 리터럴 내용이 아니라 참조(주소)를 비교하므로, a·b·c 는 모두 서로 다른 배열 객체이다.
따라서 세 번 모두 조건식이 false 가 되어 "N" 이 세 번 출력된다.',
  'past:2024-2-prac:Q1'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


-- Q2. 반정규화 정의  [정답: 반정규화]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31202,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 내용에 해당하는 데이터베이스 용어를 작성하시오.

시스템의 성능을 향상시키고 개발 및 운영의 편의성을 높이기 위해,
정규화된 데이터 모델을 의도적으로 통합·중복·분리하여
정규화 원칙을 일부러 위배하는 설계 기법이다.',
  NULL,
  '반정규화',
  '정규화를 깨고 성능·편의성을 위해 테이블을 통합하거나 중복시키는 것을 반정규화(비정규화, 역정규화)라고 한다.',
  'past:2024-2-prac:Q2'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_MODEL_NORMAL_DENORMAL');


-- Q3. SQL DML/조회 구문 빈칸  [정답: VALUES / SELECT / FROM / SET]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31301,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 조건을 만족하도록 괄호 안에 들어갈 SQL 키워드를 작성하시오.

[테이블 구조 요약]
사원(사원번호(PK), 이름, 나이, 부서)
부서(사원번호(PK), 이름, 주소, 나이)

1) 신입사원이 들어와서 부서 테이블에 새로운 사람을 추가한다.
INSERT INTO 부서 (사원번호, 이름, 주소, 나이)
( ① ) (240728, ''홍길동'', ''서울'', 30);

2) 부서 테이블에서 추가한 사원을 검색한 후 사원 테이블에 추가한다.
INSERT INTO 사원 (사원번호, 이름, 나이, 부서)
( ② ) 사원번호, 이름, 나이, ''영업'' FROM 부서 WHERE 이름 = ''홍길동'';

3) 전체 사원 테이블을 조회한다.
SELECT * ( ③ ) 사원;

4) 사원의 퇴사로 인해 부서 값을 ''퇴사''로 변경한다.
UPDATE 사원 ( ④ ) 부서 = ''퇴사'' WHERE 사원번호 = 240728;

각 번호 ①~④에 들어갈 키워드를 순서대로 작성하시오.',
  NULL,
  'VALUES / SELECT / FROM / SET',
  'INSERT VALUES, INSERT … SELECT, SELECT … FROM, UPDATE … SET 구조를 묻는 문제이다.
정답은 순서대로 VALUES, SELECT, FROM, SET 이다.',
  'past:2024-2-prac:Q3'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_SQL_SELECT_JOIN');


-- Q4. Cardinality / Degree  [정답: 카디널리티 5, 디그리 4]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31201,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 테이블에서 카디널리티(Cardinality)와 디그리(Degree)를 작성하시오.

| 학번 | 이름 | 성적 | 지도교수 |
|-----|------|------|----------|
| 100 | Kim  | 92   | P1       |
| 200 | Park | 88   | P2       |
| 300 | Lee  | 95   | P3       |
| 400 | Choi | 73   | P1       |
| 500 | Kang | 82   | P1       |

카디널리티와 디그리를 각각 얼마인지 서술하시오.',
  NULL,
  '카디널리티 5, 디그리 4',
  '튜플(행)의 개수는 5개이므로 카디널리티는 5,
속성(열)의 개수는 4개이므로 디그리는 4이다.',
  'past:2024-2-prac:Q4'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_MODEL_ER_PHYSICAL');


-- Q5. IPsec 프로토콜  [정답: IPsec]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 설명에 해당하는 네트워크 보안 프로토콜의 이름을 작성하시오.

- 네트워크 계층에서 IP 패킷을 암호화하고 인증하기 위한 표준이다.
- 기업에서 사설 인터넷망(VPN)을 구현하는 데 사용된다.
- AH(Authentication Header)와 ESP(Encapsulating Security Payload) 두 가지 보안 프로토콜을 사용한다.',
  NULL,
  'IPsec',
  'IP 계층에서 동작하며 AH·ESP로 암호화·인증을 제공하는 VPN 표준 프로토콜은 IPsec 이다.',
  'past:2024-2-prac:Q5'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');


-- Q6. 응집도 유형 (순차적 응집도)  [정답: 순차적 응집도]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 설명에 해당하는 모듈 응집도 유형을 보기에서 골라 작성하시오.

실행 순서가 밀접한 관계를 갖는 기능들을 모아 하나의 모듈로 구성한다.
한 기능의 출력 자료가 다음 기능의 입력 자료로 사용되는 형태의 응집도이다.

[보기]
㉠ 기능적 응집도  ㉡ 논리적 응집도  ㉢ 교환적 응집도
㉣ 절차적 응집도  ㉤ 시간적 응집도  ㉥ 순차적 응집도
㉦ 우연적 응집도',
  NULL,
  '순차적 응집도',
  '앞 단계의 출력이 다음 단계의 입력으로 순차적으로 흐르는 형태이므로 순차적 응집도(Sequential Cohesion)에 해당한다.',
  'past:2024-2-prac:Q6'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


-- Q7. Iterator 패턴  [정답: Iterator]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source, image_url
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 설명에 해당하는 디자인 패턴 이름을 작성하시오.

- 컬렉션 객체의 내부 구조를 노출하지 않고, 원소에 순차적으로 접근할 수 있게 하는 패턴이다.
- 객체의 내부 표현 방식과 독립적으로 요소에 접근할 수 있다.
- 반복 과정을 캡슐화해 클라이언트 코드가 컬렉션의 구체 구현에 의존하지 않게 한다.
- Cursor 라고도 불린다.',
  NULL,
  'Iterator',
  '컬렉션의 순회 방법을 캡슐화하는 GoF 행위 패턴은 Iterator 패턴이다.',
  'past:2024-2-prac:Q7',
  'https://api.mycertpilot.com/static/images/questions/pq_2024_02_07.png'

);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


-- Q8. Python 부분 문자열 카운트  [정답: ab3 ca3]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 Python 코드의 실행 결과를 작성하시오.

```python
def cnt(s, p):
    result = 0
    for i in range(len(s)):
        sub = s[i:i+len(p)]
        if sub == p:
            result += 1
    return result

s  = "abdcabcabca"
p1 = "ca"
p2 = "ab"

print(f"ab{cnt(s, p1)} ca{cnt(s, p2)}")
```',
  NULL,
  'ab3 ca3',
  '"ca" 와 "ab" 가 각각 3번씩 등장하므로 출력 문자열은 ab3 ca3 이 된다.',
  'past:2024-2-prac:Q8'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


-- Q9. 가상회선 / 데이터그램 교환 방식  [정답: ① 가상회선 ② 데이터그램]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '데이터 교환 방식에 대한 다음 설명에서 괄호 ①, ②에 들어갈 용어를 작성하시오.

① 연결형 통신에서 주로 사용되며, 출발지와 목적지 사이의 전송 경로를 미리 설정하여
   논리적으로 고정한 후 통신하는 방식

② 비연결형 통신에서 주로 사용되며, 사전 접속 절차 없이
   각 패킷에 목적지까지의 경로 정보 등을 포함하여 개별적으로 전달하는 방식',
  NULL,
  '① 가상회선 ② 데이터그램',
  '연결을 미리 설정하는 방식은 가상회선 방식,
패킷 단위로 독립 전달하는 방식은 데이터그램 방식이다.',
  'past:2024-2-prac:Q9'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');


-- Q10. C switch / break  [정답: -13]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 C 코드의 실행 결과를 작성하시오.

```c
#include <stdio.h>

void swap() {
    int a = 11;
    int b = 19;
    int t = a;
    a = b;
    b = t;
}

int main() {
    int a = 11;
    int b = 19;
    swap();
    switch (a) {
        case 1:
            b += 1;
        case 11:
            b += 2;
        default:
            b += 3;
            break;
    }
    printf("%d", a - b);
}
```',
  NULL,
  '-13',
  'swap 함수는 지역 변수만 바꾸므로 main 의 a, b 값은 그대로 11, 19 이다.
switch 에서 case 11 에 매치되고 break 가 없어서 default 까지 실행되어
b = 19 + 2 + 3 = 24, a - b = 11 - 24 = -13 이 출력된다.',
  'past:2024-2-prac:Q10'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');
