-- =========================================
-- 2025년 1회 정보처리기사 실기
-- PRACTICAL SHORT 문제 시드 (Q1 ~ Q10)
-- =========================================

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

-- PRACTICAL 토픽 상수
SET @tp_31101 := 31101; -- P.1.1 업무 시나리오 해석
SET @tp_31102 := 31102; -- P.1.2 업무/데이터 요구 도출
SET @tp_31201 := 31201; -- P.2.1 개념/논리/물리 모델링
SET @tp_31202 := 31202; -- P.2.2 정규화/반정규화 적용
SET @tp_31301 := 31301; -- P.3.1 SELECT/집계/조인 쿼리 작성
SET @tp_31302 := 31302; -- P.3.2 인덱스 설계 및 쿼리 튜닝
SET @tp_31401 := 31401; -- P.4.1 트랜잭션 특성/격리수준
SET @tp_31402 := 31402; -- P.4.2 동시성 제어/락 전략 설계
SET @tp_31501 := 31501; -- P.5.1 백업/복구 전략 수립
SET @tp_31502 := 31502; -- P.5.2 장애 분석 및 개선안 도출

/* =======================================================
 * Q1. 세션 하이재킹 (TCP 세션 탈취)
 *  - Topic: P.1.1 업무 시나리오 해석 (@tp_31101 / 31101)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음은 네트워크 보안과 관련된 설명이다.

(  )은/는 "세션을 가로채다"라는 의미로, 정당한 사용자의 세션 상태를 훔치거나 도용하여 액세스하는 공격 기법이다.

TCP (  ) 공격은 클라이언트와 서버 사이에 이미 수립된 TCP 세션에서 공격자가 시퀀스 번호나 제어 플래그 등을 조작하여 정상 세션을 끊거나 탈취하고, 인증 없이 통신을 이어 가는 방식이다.

괄호 (  ) 안에 들어갈 알맞은 용어를 작성하시오.',
  '세션 하이재킹',
  '정상 사용자와 서버 사이의 통신이 이미 설정된 상태에서 공격자가 세션 식별자나 시퀀스 번호를 탈취해 통신을 가로채는 공격을 세션 하이재킹(Session Hijacking)이라고 한다.
TCP 세션 하이재킹은 이미 3-way 핸드셰이크가 끝난 연결을 노려, 시퀀스 번호를 예측하거나 RST 패킷 등을 이용해 정상 세션을 중단시키고 공격자가 세션을 이어받는 형태로 이루어진다.
따라서 빈칸에는 세션 하이재킹이 들어간다.',
  'prac:2025-1:Q01'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

/* =======================================================
 * Q2. 무결성 제약조건 비교 (도메인/개체/참조)
 *  - Topic: P.4.1 트랜잭션 특성/격리수준 (@tp_31401 / 31401)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31401,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 표는 세 가지 무결성 제약조건 (ㄱ), (ㄴ), (ㄷ)에 대한 특성을 정리한 것이다.
표를 보고 각 기호에 해당하는 무결성 제약조건 이름을 바르게 나열하시오.

| 구분                     | (ㄱ) 무결성 제약조건 | (ㄴ) 무결성 제약조건 | (ㄷ) 무결성 제약조건 |
|--------------------------|----------------------|----------------------|----------------------|
| 제약 대상               | 속성                 | 튜플                 | 속성과 튜플          |
| NULL 값                 | -                    | 기본키               | 외래키               |
| 릴레이션 내 제약조건 개수 | 속성의 개수와 동일  | 1개                  | 0~여러 개            |
| 기타                     | - 튜플 삽입·수정 시 제약사항 우선 확인 | - 튜플 삽입·수정 시 제약사항 우선 확인 | - 튜플 삽입·수정 시 제약사항 우선 확인<br>- 부모 릴레이션 튜플 수정·삭제 시 제약사항 우선 확인 |

보기: 개체, 참조, 도메인

괄호 (ㄱ), (ㄴ), (ㄷ)에 들어갈 제약조건 이름을 순서대로 "A, B, C" 형식으로 작성하시오.',
  '도메인, 개체, 참조',
  '속성 수준에서 허용되는 값의 범위를 제어하는 제약은 도메인 무결성으로, 컬럼의 타입·허용 범위 등을 정의한다.
튜플 전체가 기본키를 통해 유일하게 식별되도록 강제하는 것은 개체 무결성으로, 기본키에는 NULL이 허용되지 않는다.
외래키를 통해 다른 릴레이션의 기본키를 참조하는 조건을 유지하는 것은 참조 무결성이다.
표에서 (ㄱ)은 속성·NULL 미사용·속성 개수와 동일 조건이므로 도메인 무결성, (ㄴ)은 튜플·기본키·1개이므로 개체 무결성, (ㄷ)은 속성과 튜플·외래키·0~여러 개이므로 참조 무결성에 해당한다.
따라서 정답은 도메인, 개체, 참조이다.',
  'prac:2025-1:Q02'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_TX_ISOLATION');

/* =======================================================
 * Q3. CRC 오류 검출 코드
 *  - Topic: P.1.1 업무 시나리오 해석 (@tp_31101 / 31101)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 설명에 공통으로 들어갈 오류 검출 코드를 영문 약자로 작성하시오.

(        )은/는 3글자의 영문 약자로 이루어진 오류 검출 코드로, 데이터를 전송하거나 저장할 때 발생할 수 있는 오류를 감지하는 데 사용된다.

(        )은/는 데이터에 체크섬을 추가한 뒤, 전송 또는 저장 후 다시 같은 연산을 수행하여 데이터가 변경되었는지를 확인하는 기법이다.

데이터는 이진수 0과 1로 표현되며, 정해진 다항식 x³ + x + 1을 기반으로 데이터를 2진수 나눗셈하고 그 나머지를 (        ) 값으로 사용한다.',
  'CRC',
  '설명에서 다항식 기반 나눗셈으로 나머지를 구해 체크섬처럼 사용하는 오류 검출 코드는 순환 중복 검사(Cyclic Redundancy Check)이다.
이는 일반적으로 CRC라고 부르며, 3글자의 영문 약자로 표현된다.
따라서 괄호에는 CRC가 들어간다.',
  'prac:2025-1:Q03'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

/* =======================================================
 * Q4. Scareware (스캐어웨어)
 *  - Topic: P.5.2 장애 분석 및 보안 사고 대응 (@tp_31502 / 31502)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31502,
  'PRACTICAL',
  'SHORT',
  'EASY',
  '다음은 악성코드에 대한 설명이다.
내용을 읽고 알맞은 용어를 보기에서 골라 한글로 작성하시오.

사용자가 원치 않는 소프트웨어를 구매하도록 조작하기 위해 사회 공학을 사용하여 충격, 불안 또는 위협에 대한 인식을 유발하는 악성 소프트웨어의 한 형태이다.

"겁을 주다"라는 의미의 영어 단어에서 유래하였으며, 공포를 이용해 사용자를 속여 대가를 지불하게 하거나 특정 행동을 유도하는 랜섬웨어·악성코드이다.

가짜 바이러스 경고나 시스템 오류 메시지를 띄워 사용자가 돈을 지불하거나 특정 소프트웨어를 설치하도록 속이는 방식으로 동작한다.

보기:
ㄱ. 컴포넌트 웨어   ㄴ. 유즈웨어   ㄷ. 셔블웨어   ㄹ. 스캐어 웨어
ㅁ. 안티 스파이 웨어  ㅂ. 네트웨어   ㅅ. 그룹웨어   ㅇ. 애드웨어',
  '스캐어웨어',
  '가짜 보안 경고나 시스템 오류 창을 띄워 사용자를 겁주고, 유료 결제나 불필요한 프로그램 설치를 유도하는 악성코드를 스캐어웨어(Scareware)라고 한다.
설명에 공포를 유발하여 대가 지불이나 행동을 유도한다는 내용이 있으므로 보기 중 "스캐어 웨어"가 정답이다.',
  'prac:2025-1:Q04'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

/* =======================================================
 * Q5. Java 예외 처리 흐름
 *  - Topic: P.5.2 장애 분석 및 개선안 도출 (@tp_31502 / 31502)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31502,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 Java 프로그램을 실행했을 때 출력되는 결과를 작성하시오.

```java
public class Main {

    public static void main(String[] args) {

        int a = 5, b = 0;

        try {
            System.out.print(a / b);
        } catch (ArithmeticException e) {
            System.out.print("출력1");
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.print("출력2");
        } catch (NumberFormatException e) {
            System.out.print("출력3");
        } catch (Exception e) {
            System.out.print("출력4");
        } finally {
            System.out.print("출력5");
        }
    }
}
```',
  '출력1출력5',
  'a / b 연산에서 b가 0이므로 실행 시 ArithmeticException이 발생한다.
try 블록에서 예외가 발생하면 이후 문장은 실행되지 않고, 가장 먼저 해당 타입에 일치하는 catch 블록이 수행된다.
따라서 ArithmeticException을 처리하는 첫 번째 catch 블록에서 "출력1"이 출력된다.
예외 발생 여부와 상관없이 finally 블록은 항상 수행되므로 이어서 "출력5"가 출력된다.
결과적으로 화면에는 "출력1출력5"가 출력된다.',
  'prac:2025-1:Q05'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

/* =======================================================
 * Q6. ARP / RARP
 *  - Topic: P.5.2 장애 분석 및 개선안 도출 (@tp_31502 / 31502)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31502,
  'PRACTICAL',
  'SHORT',
  'EASY',
  '다음은 네트워크 프로토콜에 대한 설명이다.
각 설명에 해당하는 프로토콜 이름을 순서대로 작성하시오.

(1) IP 주소를 이용해 같은 네트워크 상의 MAC 주소를 알아내기 위해 사용하는 프로토콜

(2) 네트워크 카드의 MAC 주소를 이용해 자신의 IP 주소를 알아내기 위해 사용하는 프로토콜',
  'ARP, RARP',
  'IP 주소에서 MAC 주소를 역으로 찾는 프로토콜은 주소 결정 프로토콜인 ARP(Address Resolution Protocol)이며,
MAC 주소에서 IP 주소를 얻기 위해 사용하는 프로토콜은 RARP(Reverse ARP)이다.
따라서 (1)은 ARP, (2)는 RARP이므로 정답은 "ARP, RARP"이다.',
  'prac:2025-1:Q06'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

/* =======================================================
 * Q7. SQL JOIN 결과 (emp / sal)
 *  - Topic: P.3.1 SELECT/집계/조인 쿼리 작성 (@tp_31301 / 31301)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31301,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 두 테이블 emp, sal이 있다.

[emp 테이블]

| id   | name   |
|------|--------|
| 1001 | 김철수 |
| 1002 | 홍길동 |
| 1004 | 강감찬 |
| 1008 | 이순신 |

[sal 테이블]

| id   | incentives |
|------|------------|
| 1002 | 300        |
| 1004 | 300        |
| 1008 | 1000       |
| 1009 | 500        |

아래 SQL 쿼리를 실행했을 때 출력되는 name과 incentive 값을
"이름 금액" 형식으로 작성하시오.

```sql
SELECT name, incentives
FROM emp, sal
WHERE emp.id = sal.id
  AND incentives >= 500;
```',
  '이순신 1000',
  '조건 emp.id = sal.id에 의해 두 테이블을 동일한 id로 조인하고, incentives가 500 이상인 행만 남긴다.
sal 테이블에서 incentives가 500 이상인 행은 id 1008(1000), 1009(500)이다.
그러나 emp 테이블에는 id 1009가 없으므로 조인 결과에는 id 1008 한 건만 남고, name은 이순신, incentives는 1000이 된다.
따라서 출력 결과는 "이순신 1000"이다.',
  'prac:2025-1:Q07'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_SQL_SELECT_JOIN');

/* =======================================================
 * Q8. 릴레이션 용어 (degree / cardinality / foreign key / domain)
 *  - Topic: P.2.1 개념/논리/물리 모델링 (@tp_31201 / 31201)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31201,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음은 관계형 데이터베이스의 기본 용어에 대한 설명이다.
각 번호에 해당하는 용어를 올바르게 연결하여 "1, 2, 3, 4" 형식으로 작성하시오.

1. 릴레이션에서 속성(열, 컬럼)의 개수를 의미한다. : ( 1 )
2. 릴레이션에서 튜플(행, 로우)의 개수를 의미한다. : ( 2 )
3. 한 릴레이션의 속성이 다른 릴레이션의 기본키를 참조할 때, 참조하는 그 속성을 의미한다. : ( 3 )
4. 특정 속성에 대해 입력될 수 있는 값의 유형이나 범위를 의미하며, 무결성을 보장하는 기준이 된다. : ( 4 )

보기:
ㄱ. domain   ㄴ. primary   ㄷ. degree   ㄹ. candidate   ㅁ. cardinality   ㅂ. attribute   ㅅ. foreign

번호 (1)~(4)에 들어갈 알맞은 용어를 영어로 "degree, cardinality, foreign key, domain" 형식으로 작성하시오.',
  'degree, cardinality, foreign key, domain',
  '릴레이션의 속성 개수는 차수라고 하며, 영문으로 degree라고 부른다.
튜플(행)의 개수는 카디널리티(cardinality)이다.
다른 릴레이션의 기본키를 참조하는 속성은 외래키(foreign key)이다.
특정 속성에 입력될 수 있는 값의 집합을 도메인(domain)이라고 한다.
따라서 (1) degree, (2) cardinality, (3) foreign key, (4) domain의 순서가 정답이다.',
  'prac:2025-1:Q08'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_MODEL_ER_PHYSICAL');

/* =======================================================
 * Q9. 서브넷 브로드캐스트 수신 가능한 IP
 *  - Topic: P.5.2 장애 분석 및 개선안 도출 (@tp_31502 / 31502)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31502,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '어떤 PC의 IP 주소가 192.168.35.10이고 서브넷 마스크가 255.255.252.0이라고 하자.
이 PC에서 브로드캐스팅으로 다른 IP로 정보를 전송할 때, 다음 보기 중 같은 서브넷에서 브로드캐스트를 수신할 수 있는 IP를 모두 고르시오.

[보기]

ㄱ. 192.168.34.1
ㄴ. 192.168.32.19
ㄷ. 192.168.35.200
ㄹ. 192.168.33.138
ㅁ. 192.168.35.50

보기의 기호를 모두 포함하여 "ㄱ, ㄴ, ..." 형식으로 작성하시오.',
  'ㄱ, ㄴ, ㄷ, ㄹ, ㅁ',
  '서브넷 마스크 255.255.252.0은 마지막 3옥텟에서 252(11111100)를 사용하므로, 네트워크 주소 범위는 4 단위로 묶인다.
IP 192.168.35.10을 3옥텟 기준으로 보면 35는 32~35 블록에 속하며, 이 블록의 네트워크 주소는 192.168.32.0, 브로드캐스트 주소는 192.168.35.255이다.
따라서 192.168.32.1부터 192.168.35.254까지가 같은 서브넷의 호스트 주소이다.
보기의 모든 IP(34.1, 32.19, 35.200, 33.138, 35.50)는 이 범위 안에 있으므로 모두 브로드캐스트를 수신할 수 있다.
따라서 정답은 ㄱ, ㄴ, ㄷ, ㄹ, ㅁ이다.',
  'prac:2025-1:Q09'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');

/* =======================================================
 * Q10. C 언어 – 문자 삽입 후 정렬된 배열
 *  - Topic: P.5.2 장애 분석 및 개선안 도출 (@tp_31502 / 31502)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  @cert_id,
  @tp_31502,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음 C 프로그램을 실행했을 때의 출력 결과를 작성하시오.

```c
#include <stdio.h>

char Data[5] = {''B'', ''A'', ''D'', ''E''};
char c;

int main() {
    int i, temp, temp2;

    c = ''C'';

    printf("%d ", Data[3] - Data[1]);

    for (i = 0; i < 5; ++i) {
        if (Data[i] > c)
            break;
    }

    temp = Data[i];
    Data[i] = c;
    i++;

    for (; i < 5; ++i) {
        temp2 = Data[i];
        Data[i] = temp;
        temp = temp2;
    }

    for (i = 0; i < 5; ++i) {
        printf("%c", Data[i]);
    }

    return 0;
}
```',
  '4 BACDE',
  '초기 배열은 {''B'', ''A'', ''D'', ''E''}이다.
먼저 Data[3] - Data[1]은 ''E'' - ''A''로, 아스키 코드 기준 69 - 65 = 4이므로 "4 "가 먼저 출력된다.
그 다음 반복문에서 Data[i] > ''C''인 첫 위치를 찾는다.
정렬 순서상 ''C''는 ''B'' 다음에 와야 하므로 i는 ''D''가 있는 인덱스 2에서 멈춘다.
이 위치에 ''C''를 삽입하고, 이후 원소들을 한 칸씩 뒤로 밀어 넣으면 최종 배열은 {''B'', ''A'', ''C'', ''D'', ''E''}가 된다.
마지막 for 문에서 이 배열의 문자들을 순서대로 출력하므로 "BACDE"가 이어서 출력된다.
따라서 전체 출력 결과는 "4 BACDE"이다.',
  'prac:2025-1:Q10'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');
