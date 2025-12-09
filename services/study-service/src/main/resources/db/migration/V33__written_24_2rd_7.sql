USE certpilot_study;

/* =======================================================
 * Q61. 개발 환경에서 하드웨어와 무관한 것  [정답: A]
 *  - topic_id = 14101 (개발 환경/기본)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14101,
  'WRITTEN',
  'MCQ',
  'EASY',
  '개발에 필요한 환경 구축과 관련하여 하드웨어 환경과 관련이 없는 것은?',
  'A',
  '하드웨어 환경은 CPU, 메모리, 디스크, 네트워크 장비처럼 물리 장비 구성을 말합니다.
Oracle DB와 Eclipse는 모두 소프트웨어(데이터베이스 SW, IDE)로 하드웨어와 직접적인 관련이 없습니다.
반면 웹 서버 장비, WAS 서버 장비, 스토리지는 하드웨어 자원에 해당하므로 정답은 소프트웨어만 나열된 A입니다.',
  'past:2024-2:Q61'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Oracle DB, Eclipse',                               1),
  (@q_id, 'B', '웹 서버(Hardware Server)',                         0),
  (@q_id, 'C', 'WAS 서버(Hardware + OS 구성 장비)',                0),
  (@q_id, 'D', '물리 스토리지 장비(RAID, NAS 등)',                0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');


/* =======================================================
 * Q62. UNIX 설명 – Shell 역할  [정답: C]
 *  - topic_id = 14301 (운영체제/UNIX)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14301,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'UNIX에 대한 설명으로 옳지 않은 것은?',
  'C',
  'UNIX의 핵심 자원 관리(프로세스, 메모리, 입출력)는 커널(Kernel)의 역할입니다.
쉘(Shell)은 커널과 사용자를 이어주는 명령어 해석기(커맨드 인터프리터)일 뿐,
실제 스케줄링이나 메모리 관리를 직접 수행하지 않습니다.
나머지 보기들은 UNIX의 특징(이식성, 멀티태스킹, 다중 사용자)을 바르게 설명하므로 C가 오답입니다.',
  'past:2024-2:Q62'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'UNIX는 상당 부분 C 언어로 작성되어 이식성이 우수하다.', 0),
  (@q_id, 'B', '여러 작업을 백그라운드로 실행해 병행 처리가 가능하다.',   0),
  (@q_id, 'C', '쉘(Shell)은 프로세스 관리, 기억장치 관리, 입출력 관리 등의 주요 자원 관리를 담당한다.', 1),
  (@q_id, 'D', '다수 사용자가 동시에 시스템을 사용하여 자원 공유가 가능하다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OS_PROC_MEM');


/* =======================================================
 * Q63. C/Java에서 탭 문자  [정답: B]
 *  - topic_id = 14201 (언어 기초/문법)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14201,
  'WRITTEN',
  'MCQ',
  'EASY',
  'C, Java 등에서 printf나 System.out.printf 사용 시, 키보드의 [Tab] 키처럼 커서를 일정 간격 앞으로 이동시키는 제어 문자는?',
  'B',
  '제어 문자 중에서 줄 바꿈은 \\n, 캐리지 리턴은 \\r, 백스페이스는 \\b입니다.
탭(Tab)처럼 커서를 일정 간격 앞으로 이동시키는 제어 문자는 \\t 이므로 정답은 B입니다.',
  'past:2024-2:Q63'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '\\n', 0),
  (@q_id, 'B', '\\t', 1),
  (@q_id, 'C', '\\r', 0),
  (@q_id, 'D', '\\b', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q64. 결합도 정의 – 제어 결합도  [정답: B]
 *  - topic_id = 11301 (모듈/결합도)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11301,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 설명에 해당하는 모듈 간 결합도는?
“한 모듈이 다른 모듈의 상세한 처리 절차를 알고 있어 이를 통제하는 경우나, 처리 기능이 두 모듈에 분리되어 설계된 경우에 발생하며 권리 전도 현상이 발생할 수 있다.”',
  'B',
  '설명에서 핵심 키워드는 “다른 모듈의 처리 절차를 알고 이를 통제”, “권리 전도 현상”입니다.
이는 한 모듈이 다른 모듈의 내부 논리를 제어하기 위해 제어 신호나 플래그를 전달하는
제어 결합도(Control Coupling)의 전형적인 정의입니다.
스탬프 결합도는 구조체 전체를 인자로 넘기는 형태, 내용 결합도는 다른 모듈의 내부 데이터를 직접 참조하는 가장 강한 결합입니다.
따라서 정답은 B입니다.',
  'past:2024-2:Q64'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '스탬프 결합도(Stamp Coupling)',   0),
  (@q_id, 'B', '제어 결합도(Control Coupling)',   1),
  (@q_id, 'C', '내용 결합도(Content Coupling)',   0),
  (@q_id, 'D', '외부 결합도(External Coupling)',  0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');


/* =======================================================
 * Q65. OSI 계층별 PDU  [정답: A]
 *  - topic_id = 14302 (네트워크/OSI)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14302,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'OSI 참조 모델에서 계층별 프로토콜 데이터 단위(PDU) 연결이 틀린 것은?',
  'A',
  'OSI 계층의 대표적인 PDU는 다음과 같습니다.
- 물리 계층(Physical): Bit
- 데이터링크 계층(Data Link): Frame
- 네트워크 계층(Network): Packet
- 전송 계층(Transport): Segment
- 세션/표현/응용 계층: 보통 Message로 표현
따라서 물리 계층 PDU를 Byte로 적은 A가 틀린 연결입니다.',
  'past:2024-2:Q65'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Physical Layer - Byte',   1),
  (@q_id, 'B', 'Data Link Layer - Frame', 0),
  (@q_id, 'C', 'Network Layer - Packet',  0),
  (@q_id, 'D', 'Application Layer - Message', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'NET_PROTO_IP');


/* =======================================================
 * Q66. Java Heap에서 사용하지 않는 객체 제거  [정답: B]
 *  - topic_id = 14202 (Java 기본/메모리)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14202,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'Java에서 힙(Heap)에 남아 있으나 참조를 잃어 더 이상 사용되지 않는 객체를 찾아 제거해 주는 모듈은?',
  'B',
  'Java에서는 더 이상 참조되지 않는 객체(Reachable하지 않은 객체)를 자동으로 수거해 메모리를 회수하는 과정이 필요합니다.
이를 담당하는 모듈이 Garbage Collector(가비지 컬렉터)입니다.
Heap Collector / Memory Collector 같은 명칭은 일반적으로 사용하지 않으므로 정답은 B입니다.',
  'past:2024-2:Q66'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Heap Collector',     0),
  (@q_id, 'B', 'Garbage Collector',  1),
  (@q_id, 'C', 'Memory Collector',   0),
  (@q_id, 'D', 'Variable Collector', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q67. 연산자 종류가 다른 하나  [정답: D]
 *  - topic_id = 14201 (언어 기초/연산자)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14201,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 연산자의 종류가 다른 하나는?',
  'D',
  'A, B, C는 모두 “산술/증가·감소” 계열 연산자들입니다.
반면 &&, ||는 참/거짓을 대상으로 하는 논리(Logical) 연산자입니다.
따라서 다른 종류의 연산자를 고르면 논리 연산자 D가 정답입니다.',
  'past:2024-2:Q67'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '+, -   (산술 연산자)',              0),
  (@q_id, 'B', '*, /   (산술 연산자)',              0),
  (@q_id, 'C', '++, -- (증가/감소(단항) 연산자, 산술 계열)', 0),
  (@q_id, 'D', '&&, || (논리 연산자)',              1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q68. 운영체제 기능  [정답: A]
 *  - topic_id = 14301 (운영체제 개요/기능)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14301,
  'WRITTEN',
  'MCQ',
  'EASY',
  '운영체제의 기능으로 틀린 것은?',
  'A',
  '운영체제는 자원 관리, 보호, 스케줄링, 사용자 인터페이스 제공 등의 역할을 담당합니다.
그러나 IDE나 컴파일러 같은 “개발 도구 전체”를 반드시 제공하지는 않습니다.
개발 도구는 별도 애플리케이션(Dev 환경)으로 설치하는 것이 일반적이므로 A가 틀린 설명입니다.',
  'past:2024-2:Q68'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '응용 프로그램 개발에 필요한 모든 도구(IDE, 컴파일러)를 제공한다.', 0),
  (@q_id, 'B', '프로세스, 메모리, 파일 등의 자원 보호 기능을 제공한다.',           0),
  (@q_id, 'C', 'CPU, 메모리 등 자원의 스케줄링 기능을 제공한다.',                    0),
  (@q_id, 'D', '사용자와 시스템 간의 편리한 인터페이스(쉘, GUI)를 제공한다.',        0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OS_PROC_MEM');


/* =======================================================
 * Q69. Java while / continue 동작  [정답: A]
 *  - topic_id = 14202 (Java 제어문)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14202,
  'WRITTEN',
  'MCQ',
  'HARD',
  '다음 Java 프로그램을 실행했을 때의 출력 결과는?

```java
public class Test {
  public static void main(String[] args) {
    int x = 7, y = 0;
    while (x-- > 0) {
      if (x % 3 == 0)
        continue;
      y++;
    }
    System.out.print(y);
  }
}
```',
  'A',
  '루프를 단계별로 추적해 보면 다음과 같습니다. (조건은 x-- > 0, if에서는 감소된 x 사용)
- 초기: x=7, y=0
1회전: 조건 7>0 → true, x는 6이 됨. 6%3==0 → continue, y=0
2회전: 조건 6>0 → true, x=5. 5%3!=0 → y=1
3회전: 조건 5>0 → true, x=4. 4%3!=0 → y=2
4회전: 조건 4>0 → true, x=3. 3%3==0 → continue, y=2
5회전: 조건 3>0 → true, x=2. 2%3!=0 → y=3
6회전: 조건 2>0 → true, x=1. 1%3!=0 → y=4
7회전: 조건 1>0 → true, x=0. 0%3==0 → continue, y=4
8회전: 조건 0>0 → false, 루프 종료
따라서 최종 출력은 4이므로 정답은 A입니다.',
  'past:2024-2:Q69'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '4', 1),
  (@q_id, 'B', '0', 0),
  (@q_id, 'C', '7', 0),
  (@q_id, 'D', '5', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q70. C 포인터/문자열 인덱싱  [정답: A]
 *  - topic_id = 14203 (C 포인터/문자열)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14203,
  'WRITTEN',
  'MCQ',
  'HARD',
  '다음 C 프로그램이 실행되었을 때의 출력 결과는?

```c
#include <stdio.h>
int main() {
  char* s = "Sinagong";
  for (int i = 5; i > 0; i--)
    printf("%c", *(s + i));
}
```',
  'A',
  '문자열 "Sinagong"의 인덱스를 보면 다음과 같습니다.
  s[0]=''S'', s[1]=''i'', s[2]=''n'', s[3]=''a'', s[4]=''g'', s[5]=''o'', s[6]=''n'', s[7]=''g''
for문은 i=5부터 i>0까지 감소하면서 *(s+i)를 출력합니다.
- i=5 → s[5] = ''o''
- i=4 → s[4] = ''g''
- i=3 → s[3] = ''a''
- i=2 → s[2] = ''n''
- i=1 → s[1] = ''i''
출력 문자를 이어 붙이면 “ogani”가 되므로 정답은 A입니다.',
  'past:2024-2:Q70'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'ogani',    1),
  (@q_id, 'B', 'inago',    0),
  (@q_id, 'C', 'gnogan',   0),
  (@q_id, 'D', 'sinagong', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');
