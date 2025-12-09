SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id    := 1;

/* =======================================================
 * 2024년 1회 – 정보처리기사 실기
 * Q11 ~ Q20 (모두 PRACTICAL / SHORT)
 * ======================================================= */

-- Q11. C 구조체/포인터 – 단리 이자 계산  [정답: 9981 and 2795.10]
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

typedef struct {
    int accNum;
    double bal;
} BankAcc;

double sim_pow(double base, int year) {
    int i;
    double r = 1.0;
    for (i = 0; i < year; i++) {
        r = r * base;
    }
    return r;
}

void initAcc(BankAcc *acc, int x, double y) {
    acc->accNum = x;
    acc->bal = y;
}

void xxx(BankAcc *acc, double *en) {
    if (*en > 0 && *en < acc->bal) {
        acc->bal = acc->bal - *en;
    } else {
        acc->bal = acc->bal + *en;
    }
}

void yyy(BankAcc *acc) {
    acc->bal = acc->bal * sim_pow((1 + 0.1), 3);
}

int main(void) {
    BankAcc myAcc;
    initAcc(&myAcc, 9981, 2200.0);
    double amount = 100.0;
    xxx(&myAcc, &amount);
    yyy(&myAcc);
    printf("%d and %.2f", myAcc.accNum, myAcc.bal);
    return 0;
}
```',
  NULL,
  '9981 and 2795.10',
  'initAcc 호출 후 계좌 번호는 9981, 잔액은 2200.0 이다.
xxx 함수에서 인출 금액 amount 는 100.0 으로 0보다 크고 잔액 2200.0 보다 작으므로 잔액에서 차감되어 bal = 2100.0 이 된다.
yyy 함수는 3년 동안 연 10% 단리(여기서는 sim_pow 로 1.1³ 을 곱하는 형태)를 적용하므로
2100.0 × 1.1³ = 2100.0 × 1.331 = 2795.1 이 된다.
printf 는 "%.2f" 포맷을 사용하므로 소수 둘째 자리까지 2795.10 으로 출력되고,
계좌 번호와 함께 "9981 and 2795.10" 이 출력된다.',
  'past:2024-1-prac:Q11'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

-- Q12. Python 리스트/문자열 결합  [정답: Seynaau]
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
a = ["Seoul", "Kyeonggi", "Incheon", "Daejun", "Daegu", "Pusan"]
s = "S"

for i in a:
    s = s + i[1]

print(s)
```',
  NULL,
  'Seynaau',
  '리스트 a 의 각 원소에서 인덱스 1(두 번째 문자)을 차례대로 꺼내어 문자열 s 뒤에 붙인다.
각 도시 이름의 두 번째 문자는
Seoul → e, Kyeonggi → y, Incheon → n, Daejun → a, Daegu → a, Pusan → u 이다.
초기 s 가 "S" 이므로 반복 후 s 는 "S" + "e" + "y" + "n" + "a" + "a" + "u" 가 되어
최종 결과는 "Seynaau" 이다.',
  'past:2024-1-prac:Q12'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

-- Q13. SQL 서브쿼리 + IN 결과 집합  [정답: a, b]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31301,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '아래 R1, R2 테이블과 SQL 문장을 참고하여 실행 결과로 출력되는 B 열의 값을 표 형태로 작성하시오.

R1

| A | B | C |
|---|---|---|
| 1 | a | x |
| 2 | b | x |
| 1 | c | w |
| 3 | d | w |

R2

| C | D | E |
|---|---|---|
| x | k | 3 |
| y | k | 3 |
| z | s | 2 |

```sql
SELECT
    B
FROM
    R1
WHERE
    C IN (SELECT C FROM R2 WHERE D = "k");
```',
  NULL,
  'a, b',
  '서브쿼리 (SELECT C FROM R2 WHERE D = "k") 의 결과를 먼저 구하면,
R2 에서 D 가 "k" 인 튜플의 C 값은 x, y 두 개이므로 집합 {x, y} 가 된다.
메인 쿼리의 WHERE 조건 C IN {x, y} 를 R1 에 적용하면
C 가 x 인 첫 번째 행(1, a, x) 과 두 번째 행(2, b, x) 만 선택된다.
따라서 B 열 기준으로 결과는 a, b 두 행이 된다.',
  'past:2024-1-prac:Q13'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_SQL_SELECT_JOIN');

-- Q14. 테스트 커버리지 – 변경 조건/결정(MC/DC)  [정답: 변경 조건/결정 커버리지]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음은 애플리케이션 테스트 관리에 대한 설명이다.
설명에 해당하는 테스트 커버리지 기법을 보기에서 골라 한글 명칭으로 작성하시오.

1. 모든 분기와 조건의 조합을 고려하지만, 모든 조합을 전부 테스트하기보다는
   테스트가 필요한 중요한 조합을 찾아내는 데 중점을 둔다.
2. 특정 조건을 수행할 때 다른 조건과는 상관없이 전체 결과(결정)에 영향을 미치는지를 확인한다.
3. 각각의 개별 조건은 적어도 한 번은 전체 결과에 독립적으로 영향을 주어야 한다.

[보기]
ㄱ. 구문 커버리지
ㄴ. 결정 커버리지
ㄷ. 조건 커버리지
ㄹ. 변경 조건/결정 커버리지
ㅁ. 다중 조건 커버리지
ㅂ. 경로 커버리지
ㅅ. 조건/결정 커버리지',
  NULL,
  '변경 조건/결정 커버리지',
  '각 개별 조건이 다른 조건과 무관하게 전체 결정 결과에 독립적으로 영향을 주도록
테스트 케이스를 설계하는 기법은 변경 조건/결정 커버리지(MC/DC: Modified Condition/Decision Coverage) 이다.
다중 조건의 전체 조합을 모두 테스트하는 다중 조건 커버리지보다 테스트 수를 줄이면서도,
각 조건의 독립적인 영향력을 확인할 수 있다는 특징을 가진다.
설명 1~3 이 모두 MC/DC 의 정의와 일치하므로 정답은 변경 조건/결정 커버리지이다.',
  'past:2024-1-prac:Q14'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_SW_TEST');

-- Q15. 보안 악성 코드 – Rootkit  [정답: Rootkit]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 설명에 해당하는 보안 용어를 보기에서 골라 영문 명칭으로 작성하시오.

- 인터넷 공격자의 존재를 숨기면서 시스템에 대한 무제한 접근 권한을 부여하는 악성 프로그램이다.
- 해커가 자신의 존재를 숨기면서 허가되지 않은 컴퓨터나 소프트웨어에 접근할 수 있도록 설계된 도구이다.
- 일반적으로 펌웨어, 가상화 계층 등 다양한 시스템 영역에서 작동하며,
  운영체제의 시스템 콜을 가로채 악성 코드 실행 여부를 숨겨 안티바이러스 탐지를 우회할 수 있다.

[보기]
ㄱ. Worm
ㄴ. Trojan horse
ㄷ. Backdoor
ㄹ. Virus
ㅁ. Ransomware
ㅂ. Spyware
ㅅ. Rootkit',
  NULL,
  'Rootkit',
  '시스템에 은밀히 설치되어 관리자 권한을 탈취하고,
프로세스·파일·네트워크 연결 등을 숨겨 보안 프로그램의 탐지를 회피하도록 하는 악성 도구 모음을 루트킷(Rootkit) 이라고 한다.
설명에서 공격자의 존재를 숨기고, 시스템 콜을 조작하여 악성 활동을 감추는 특징을 언급하고 있으므로
정답은 Rootkit 이다.',
  'past:2024-1-prac:Q15'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_SECURITY');

-- Q16. Java 상속/오버라이딩 – 동적 바인딩  [정답: 9]
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
class classOne {
    int a, b;

    public classOne(int a, int b) {
        this.a = a;
        this.b = b;
    }

    public int getP() {
        return a + b;
    }
}

class classTwo extends classOne {
    int po = 3;

    public classTwo(int i) {
        super(i, i + 1);
    }

    public int getP() {
        return po * po;
    }
}

public class Main {
    public static void main(String[] args) {
        classOne one = new classTwo(10);
        System.out.println(one.getP());
    }
}
```',
  NULL,
  '9',
  'classTwo 는 classOne 을 상속받고 getP 메서드를 오버라이드한다.
main 메서드에서 변수 타입은 classOne 이지만,
실제 인스턴스는 new classTwo(10) 이므로 런타임 시 동적 바인딩에 의해
자식 클래스 classTwo 의 getP 가 호출된다.
classTwo 의 getP 는 po * po 를 반환하고, po 의 값은 3 이므로 3×3 = 9 가 출력된다.',
  'past:2024-1-prac:Q16'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');

-- Q17. 보안 공격 – APT(지능형 지속 공격)  [정답: APT]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 설명에 해당하는 보안 공격 기법의 약자를 영문으로 작성하시오.

1. 불특정 다수가 아닌 명확한 표적을 정하여, 장기간 정보 수집 후 공격을 감행한다.
2. 표적 시스템뿐 아니라 내부 직원이 사용하는 다양한 단말도 공격 대상이 된다.
3. 제로데이 취약점, 악성 코드 등 여러 보안 위협 기술을 복합적으로 사용한다.
4. 일반적으로 공격은 침투, 검색, 수집 및 유출의 4단계로 진행되며, 각 단계마다 다양한 기술이 사용된다.',
  NULL,
  'APT',
  '특정 조직이나 기관을 겨냥해 장기간에 걸쳐 정교하게 수행되는 지능형 지속 공격을 APT(Advanced Persistent Threat) 라고 한다.
설명에서 표적 지정, 장기적 정보 수집, 다양한 공격 기법의 조합, 침투-검색-수집-유출 4단계 등
APT 의 특징을 모두 언급하고 있으므로 정답은 APT 이다.',
  'past:2024-1-prac:Q17'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_SECURITY');

-- Q18. SQL WHERE 절 AND/OR 우선순위  [정답: 1]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31301,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 TABLE 과 SQL 문장을 참고하여 실행 결과로 반환되는 COUNT(*) 값을 작성하시오.

TABLE

| EMPNO |  SAL |
|-------|------|
|  100  | 1000 |
|  200  | 3000 |
|  300  | 1500 |

```sql
SELECT
    COUNT(*)
FROM
    TABLE
WHERE
    EMPNO > 100
    AND SAL >= 3000
    OR EMPNO = 200;
```',
  NULL,
  '1',
  'SQL 에서 AND 는 OR 보다 우선순위가 높으므로
WHERE 절은 (EMPNO > 100 AND SAL >= 3000) OR EMPNO = 200 로 해석된다.

각 행에 대해 조건을 평가하면,
- EMPNO = 100, SAL = 1000:
  EMPNO > 100 : 거짓, SAL >= 3000 : 거짓 → AND 결과 거짓, OR EMPNO = 200 도 거짓 → 전체 거짓.
- EMPNO = 200, SAL = 3000:
  EMPNO > 100 : 참, SAL >= 3000 : 참 → AND 결과 참, OR EMPNO = 200 도 참 → 전체 참.
- EMPNO = 300, SAL = 1500:
  EMPNO > 100 : 참, SAL >= 3000 : 거짓 → AND 결과 거짓, OR EMPNO = 200 도 거짓 → 전체 거짓.

따라서 조건을 만족하는 튜플은 EMPNO 200 한 행뿐이므로 COUNT(*) 결과는 1 이다.',
  'past:2024-1-prac:Q18'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_SQL_SELECT_JOIN');

-- Q19. C 시저 암호 변환  [정답: Nd sc 1]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31502,
  'PRACTICAL',
  'SHORT',
  'HARD',
  '다음 C 언어 코드의 실행 결과를 작성하시오.

```c
#include <stdio.h>
#include <ctype.h>

int main(void) {
    char *p = "It is 8";
    char result[100];
    int i;

    for (i = 0; p[i] != ''\0''; i++) {
        if (isupper(p[i]))
            result[i] = (p[i] - ''A'' + 5) % 26 + ''A'';
        else if (islower(p[i]))
            result[i] = (p[i] - ''a'' + 10) % 26 + ''a'';
        else if (isdigit(p[i]))
            result[i] = (p[i] - ''0'' + 3) % 10 + ''0'';
        else
            result[i] = p[i];
    }

    result[i] = ''\0'';
    printf("%s\n", result);
    return 0;
}
```',
  NULL,
  'Nd sc 1',
  '문자열은 "It is 8" 이고, 반복문에서 각 문자를 종류별로 다음과 같이 치환한다.
- 대문자: (문자 - ''A'' + 5) % 26 + ''A''
- 소문자: (문자 - ''a'' + 10) % 26 + ''a''
- 숫자:   (문자 - ''0'' + 3) % 10 + ''0''
- 그 외 공백 등은 그대로 유지한다.

각 문자별로 계산하면
I → 대문자: ''I''(8번째, 0부터) 에 5 를 더하면 13 → N
t → 소문자: ''t'' 에 10 을 더해 알파벳을 한 바퀴 돌아 d
공백은 그대로 공백
i → 소문자: s
s → 소문자: c
공백은 그대로 공백
8 → 숫자: (8 + 3) % 10 = 1

따라서 최종 결과 문자열은 "Nd sc 1" 이 된다.',
  'past:2024-1-prac:Q19'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_SECURITY');

-- Q20. GoF 생성 패턴 – Abstract Factory  [정답: Abstract Factory]
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id,
  31101,
  'PRACTICAL',
  'SHORT',
  'NORMAL',
  '다음 설명에 해당하는 디자인 패턴의 이름을 작성하시오.

- 구체적인 클래스에 직접 의존하지 않고,
  서로 연관되거나 의존적인 객체들의 조합을 만드는 인터페이스를 제공하는 패턴이다.
- 관련성이 있는 객체 군이 여러 개 있을 때, 이들을 하나의 제품군으로 묶어 추상화하고,
  특정 상황에 따라 팩토리 객체가 해당 제품군 전체를 생성한다.
- 서로 관련된 여러 종류의 객체를 일관된 방식으로 생성해야 할 때 유용하며,
  kit 패턴이라고도 불린다.',
  NULL,
  'Abstract Factory',
  '관련된 여러 객체를 한 번에 생성하는 인터페이스를 제공하면서
구체적인 클래스에 대한 의존성을 낮추는 GoF 생성 패턴은 추상 팩토리(Abstract Factory) 패턴이다.
클라이언트는 구체 클래스 대신 팩토리 인터페이스만 알고,
구체 팩토리를 교체함으로써 한 제품군 전체를 쉽게 바꿀 수 있다.
설명에서 연관 객체의 군, 제품군 단위 생성, kit 라는 별칭 등을 언급하고 있으므로
정답은 Abstract Factory 이다.',
  'past:2024-1-prac:Q20'
);
SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_SW_DESIGN_PATTERN');
