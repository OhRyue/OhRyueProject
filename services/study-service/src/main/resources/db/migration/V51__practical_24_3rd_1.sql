USE certpilot_study;

SET @cert_id := 1;

/* =======================================================
 * 2024년 3회 – 실기 (1~10)
 *  - mode: PRACTICAL, type: SHORT
 * ======================================================= */

/* Q1. Java equals / 배열 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31101,
  'PRACTICAL', 'SHORT', 'NORMAL',
  '다음 Java 코드를 실행했을 때 출력값을 쓰시오.

```java
public class Main {
    static void func(String[] m, int n) {
        for (int i = 1; i < n; i++) {
            if (m[i-1].equals(m[i])) {
                System.out.print("O");
            } else {
                System.out.print("N");
            }
        }

        for (String mo : m) {
            System.out.print(mo);
        }
    }

    public static void main(String[] args) {
        String[] m = new String[3];
        m[0] = "A";
        m[1] = "A";
        m[2] = new String("A");
        func(m, 3);
    }
}
```',
  NULL,
  'OOAAA',
  'equals 메서드는 문자열 내용을 비교하므로 m[0]과 m[1], m[1]과 m[2]가 모두 같아서 "OO"가 출력되고, 이어서 배열의 원소 "A", "A", "A"가 그대로 이어져 "OOAAA"가 된다.',
  'practical:2024-3:Q1'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


/* Q2. Python 리스트 / 슬라이싱 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31101,
  'PRACTICAL', 'SHORT', 'NORMAL',
  '다음 Python 코드를 실행했을 때 출력값을 쓰시오.

```python
def test(lst):
    for i in range(len(lst) // 2):
        lst[i], lst[-i-1] = lst[-i-1], lst[i]

ls = [1, 2, 3, 4, 5, 6]
test(ls)
print(sum(ls[::2]) - sum(ls[1::2]))
```',
  NULL,
  '3',
  '리스트가 [6, 5, 4, 3, 2, 1]로 뒤집힌 뒤, 짝수 인덱스 합(6+4+2=12)에서 홀수 인덱스 합(5+3+1=9)을 빼면 3이 된다.',
  'practical:2024-3:Q2'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


/* Q3. SQL – COUNT(* ) 결과 행 수 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31301,
  'PRACTICAL', 'SHORT', 'NORMAL',
  '다음 SQL을 실행했을 때 결과로 반환되는 행(row)의 개수를 쓰시오.

```sql
SELECT COUNT(*)
FROM employee e
JOIN projects p ON e.project_id = p.project_id
WHERE p.name IN (
    SELECT name
    FROM projects p
    WHERE p.project_id IN (
        SELECT project_id
        FROM employee e
        GROUP BY e.project_id
        HAVING COUNT(*) < 2
    )
);
```',
  NULL,
  '1',
  '서브쿼리 조건에 따라 한 명만 배정된 프로젝트만 남고, 그 결과를 만족하는 조합이 1개뿐이라 COUNT(*) 결과는 1이 된다.',
  'practical:2024-3:Q3'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_SQL_SELECT_JOIN');


/* Q4. LRU 페이지 교체 (프레임 3칸) */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31502,
  'PRACTICAL', 'SHORT', 'NORMAL',
  '프레임이 3개인 시스템에서 LRU 페이지 교체 알고리즘을 사용한다.
다음 페이지 참조열에 대해 발생하는 페이지 부재(page fault)의 총 횟수를 구하시오.

페이지 참조열:
7 0 1 2 0 3 0 4 2 3 0 3 2 1 2 0 1 7 0 1',
  NULL,
  '12',
  '주어진 참조열을 LRU 규칙으로 시뮬레이션하면 총 12번의 페이지 부재가 발생한다.',
  'practical:2024-3:Q4'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');


/* Q5. 네트워크 보안 – 스머프 공격 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31502,
  'PRACTICAL', 'SHORT', 'EASY',
  '다음 설명에 해당하는 공격 기법의 명칭을 쓰시오.

ICMP를 이용하여 송신 주소를 공격 대상의 IP 주소로 위장하고,
수신 주소를 해당 네트워크 라우터의 브로드캐스트 주소로 설정하여
대량의 ICMP Echo를 전송함으로써 네트워크를 과부하시키는 서비스 거부 공격이다.
죽음의 핑과 같이 ICMP 패킷을 활용하는 공격 유형에 속한다.',
  NULL,
  '스머프',
  '공격 대상 IP를 출발지로 위장해 네트워크 전체에 브로드캐스트 ICMP 패킷을 보내는 공격을 스머프(Smurf) 공격이라고 한다.',
  'practical:2024-3:Q5'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');


/* Q6. C – static 지역 변수 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31101,
  'PRACTICAL', 'SHORT', 'NORMAL',
  '다음 C 프로그램을 실행했을 때 출력되는 값을 쓰시오.

```c
#include <stdio.h>

int increase() {
    static int x = 0;
    x += 2;
    return x;
}

int main() {
    int x = 0;
    int sum = 0;
    for (int i = 0; i < 4; i++) {
        x++;
        sum += increase();
    }
    printf("%d", sum);
    return 0;
}
```',
  NULL,
  '20',
  'static 지역변수 x는 최초 한 번만 0으로 초기화되고 호출마다 2씩 증가한다. 반환값은 2, 4, 6, 8이므로 합인 20이 출력된다.',
  'practical:2024-3:Q6'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


/* Q7. VPN */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31502,
  'PRACTICAL', 'SHORT', 'EASY',
  '다음 설명에 해당하는 용어를 약어(영문 3글자)로 쓰시오.

인터넷을 통해 장치 간 사설 네트워크 연결을 생성하는 서비스로,
장치의 실제 IP 주소를 가상 IP 주소로 대체하고, 데이터를 암호화하며,
전 세계에 분산된 보안 네트워크를 통해 트래픽을 라우팅하여 정보를 보호하는 기술이다.',
  NULL,
  'VPN',
  '인터넷 위에 가상 사설망을 구성해 암호화된 통신을 제공하는 기술은 VPN(Virtual Private Network)이다.',
  'practical:2024-3:Q7'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_DB_OPERATION_MONITOR');


/* Q8. GoF 디자인 패턴 – 행위 패턴 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31101,
  'PRACTICAL', 'SHORT', 'EASY',
  '다음 설명에 해당하는 GoF 디자인 패턴 분류명을 쓰시오.

객체 간의 상호 작용 방식과 책임 분배 방법에 초점을 맞추는 디자인 패턴 그룹이다.
Command, Interpreter, Memento, Observer, Visitor 등이 이에 속한다.',
  NULL,
  '행위',
  'Command, Observer 등은 객체의 협력 방식에 초점을 둔 행위(Behavioral) 패턴에 해당한다.',
  'practical:2024-3:Q8'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


/* Q9. 테스트 커버리지 용어 매칭 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31101,
  'PRACTICAL', 'SHORT', 'NORMAL',
  '다음 설명에 해당하는 테스트 커버리지 이름을 보기에서 골라 기호로 쓰시오.

1) 코드 내의 각 분기점이 true와 false로 평가되도록 하여
   모든 분기점이 한 번 이상 수행되었는지를 확인하는 커버리지

2) if, switch 등과 같이 코드 흐름을 여러 갈래로 나누는
   조건문 또는 선택문을 의미한다.

3) 복합 조건식 내부의 각 개별 조건이 true와 false를 모두
   만족하도록 테스트하는 것을 의미한다.

[보기]
㉠ 조건/결정 커버리지
㉡ 결정 커버리지
㉢ 조건 커버리지
㉣ 분기 커버리지
㉤ 변경 조건/결정 커버리지
㉥ 문장 커버리지
㉧ 다중 조건 커버리지',
  NULL,
  '㉥㉣㉢',
  '1)은 모든 명령문을 한 번 이상 실행하는 문장 커버리지(㉥),
2)는 분기 자체를 의미하는 분기 커버리지(㉣),
3)은 개별 조건의 참/거짓을 모두 보장하는 조건 커버리지(㉢)에 해당한다.',
  'practical:2024-3:Q9'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');


/* Q10. C – 문자 배열에 C 삽입 후 정렬 형태 */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, payload_json, answer_key, solution_text, source
) VALUES (
  @cert_id, 31101,
  'PRACTICAL', 'SHORT', 'NORMAL',
  '다음 C 프로그램을 실행했을 때의 출력 결과를 쓰시오.

```c
#include <stdio.h>

char Data[5] = {\'B\', \'A\', \'D\', \'E\'};
char c;

int main() {
    int i, temp, temp2;
    c = \'C\';

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

    for (i = 0; i < 5; i++) {
        printf("%c", Data[i]);
    }

    return 0;
}
```',
  NULL,
  '4 BACDE',
  '문자 \'E\'와 \'A\'의 코드 차이(69-65)는 4가 되고,
\'C\'를 알맞은 위치에 삽입해 배열이 BACDE 순으로 출력되므로 최종 출력은 "4 BACDE"가 된다.',
  'practical:2024-3:Q10'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'P_REQ_SCENARIO');
