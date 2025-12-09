SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

/* =======================================================
 * 토픽 ID 매핑 (프로그래밍 언어 활용 / OS / 네트워크)
 * ======================================================= */
SET @topic_lang_basic   := 14201; -- 4.2.1 기본문법 활용
SET @topic_lang_feature := 14202; -- 4.2.2 언어특성 활용
SET @topic_lang_lib     := 14203; -- 4.2.3 라이브러리 활용
SET @topic_os_basic     := 14301; -- 4.3.1 운영체제 기초 활용
SET @topic_net_basic    := 14302; -- 4.3.2 네트워크 기초 활용


/* =======================================================
 * Q61. Java 자료형 / 크기 매핑 (LANG_BASIC_SCRIPT, EASY)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  @topic_lang_basic,
  'WRITTEN',
  'MCQ',
  'EASY',
  'Java 언어에서 기본 자료형과 크기의 연결로 옳은 것을 고르시오.',
  'C',
  'Java의 기본 자료형 중 정수형 int는 4바이트(32비트), 실수형 double은 8바이트(64비트)를 사용한다. '
  'byte는 1바이트, short는 2바이트, long은 8바이트, float는 4바이트이다. '
  '문자형 char는 2바이트(UTF-16 코드 유닛)로 표현된다. 따라서 int-4바이트, double-8바이트 조합이 옳다.',
  'past:2024-3:W4:Q61'
);

SET @q61 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q61, 'A', 'char: 1바이트, byte: 1바이트', 0),
  (@q61, 'B', 'int: 2바이트, double: 4바이트', 0),
  (@q61, 'C', 'int: 4바이트, double: 8바이트', 1),
  (@q61, 'D', 'int: 8바이트, double: 4바이트', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q61, 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q62. UNIX 개요 (OS_PROC_MEM, NORMAL)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  @topic_os_basic,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'UNIX 운영체제에 대한 설명으로 옳지 않은 것은 어느 것인가?',
  'C',
  'UNIX는 대부분 C 언어로 구현되어 이식성이 높고, 다중 사용자·다중 작업을 지원하는 범용 OS이다. '
  '여러 사용자가 동시에 로그인하여 작업할 수 있고, 하나의 사용자가 여러 프로세스를 병행 실행할 수 있다. '
  '반면 쉘(Shell)은 명령 해석기 역할을 하며, 프로세스 관리·메모리 관리·입출력 관리 같은 핵심 자원 관리는 커널이 담당한다. '
  '따라서 쉘이 자원 관리를 직접 수행한다는 설명은 옳지 않다.',
  'past:2024-3:W4:Q62'
);

SET @q62 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q62, 'A', 'C 언어로 작성되어 이식성이 우수하다.', 0),
  (@q62, 'B', '여러 사용자가 동시에 시스템을 사용할 수 있다.', 0),
  (@q62, 'C', '쉘이 프로세스·메모리·입출력 관리를 직접 수행한다.', 1),
  (@q62, 'D', '하나의 사용자가 여러 작업을 병행 처리할 수 있다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q62, 'OS_PROC_MEM');


/* =======================================================
 * Q63. IPv6 특징 (NET_PROTO_IP, NORMAL)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  @topic_net_basic,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'IPv6에 대한 설명으로 옳지 않은 것은 어느 것인가?',
  'C',
  'IPv6는 128비트 주소 공간을 제공하여 IPv4의 주소 고갈 문제를 해결한다. '
  '확장 헤더 구조를 이용해 확장성이 좋고, 기본 프로토콜 수준에서 인증·암호화(IPsec)를 지원한다. '
  '패킷 크기(페이로드 길이)는 고정이 아니라 가변이며, 경로 MTU 등에 따라 달라질 수 있다. '
  '따라서 “패킷 크기가 64Kbyte로 고정된다”는 설명은 IPv4의 헤더 필드와 혼동한 잘못된 설명이다.',
  'past:2024-3:W4:Q63'
);

SET @q63 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q63, 'A', '128비트 주소 공간을 제공한다.', 0),
  (@q63, 'B', '확장 헤더를 통해 기능 확장이 용이하다.', 0),
  (@q63, 'C', '패킷 크기가 64Kbyte로 고정되어 있다.', 1),
  (@q63, 'D', '보안을 위한 인증·암호화 기능을 포함할 수 있다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q63, 'NET_PROTO_IP');


/* =======================================================
 * Q64. 스크립트 언어 분류 (LANG_BASIC_SCRIPT, EASY)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  @topic_lang_feature,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 스크립트 언어가 아닌 것은 어느 것인가?',
  'B',
  '스크립트 언어는 보통 인터프리터 방식으로 실행되며, 웹 서버나 애플리케이션의 자동화에 많이 사용된다. '
  'PHP와 Python은 대표적인 스크립트 언어이고, JavaScript 역시 클라이언트/서버에서 널리 사용되는 스크립트 언어이다. '
  '반면 COBOL은 전통적인 절차적 3세대 언어(3GL)로, 대형 전산 시스템의 업무 처리에 사용되며 스크립트 언어로 분류하지 않는다.',
  'past:2024-3:W4:Q64'
);

SET @q64 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q64, 'A', 'PHP', 0),
  (@q64, 'B', 'COBOL', 1),
  (@q64, 'C', 'Python', 0),
  (@q64, 'D', 'JavaScript', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q64, 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q65. OSI 7계층 - 네트워크 계층 (NET_PROTO_IP, NORMAL)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  @topic_net_basic,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'OSI 7계층 중 네트워크 계층(Network Layer)에 대한 설명으로 옳지 않은 것은?',
  'B',
  '네트워크 계층은 발신지에서 목적지까지 패킷을 전달하고, 논리적 주소(IP 주소)를 사용하여 라우팅을 수행한다. '
  '라우터가 이 계층에서 동작하며 최적 경로를 결정한다. 한편 데이터링크 계층이 한 홉(링크) 내에서 프레임을 전송하는 책임을 가진다. '
  '따라서 “한 노드에서 다른 노드로 프레임을 전송하는 책임을 진다”는 설명은 네트워크 계층이 아닌 데이터링크 계층에 대한 설명이다.',
  'past:2024-3:W4:Q65'
);

SET @q65 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q65, 'A', '발신지에서 목적지까지 패킷을 전달하는 책임을 진다.', 0),
  (@q65, 'B', '한 노드에서 다른 노드로 프레임을 전송하는 책임을 진다.', 1),
  (@q65, 'C', '패킷에 발신지와 목적지의 논리적 주소를 추가한다.', 0),
  (@q65, 'D', '라우터가 경로를 지정하여 패킷 전달을 수행한다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q65, 'NET_PROTO_IP');


/* =======================================================
 * Q66. C for 루프 합계 (LANG_BASIC_SCRIPT, NORMAL)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  @topic_lang_basic,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 C 프로그램을 실행했을 때의 출력 결과로 옳은 것은?

```c
#include <stdio.h>
int main() {
    int sum = 0;
    for (int i = 1; i < 20; i *= 2) {
        sum += i;
    }
    printf("%d", sum);
    return 0;
}
```',
  'D',
  'for문에서 i는 1에서 시작하여 매 반복마다 2배가 되며, i < 20 인 동안 반복한다. '
  '따라서 i 값의 순서는 1 → 2 → 4 → 8 → 16 이고, 이후 32가 되면 조건(i < 20)을 만족하지 않아 루프가 종료된다. '
  'sum에는 1 + 2 + 4 + 8 + 16 = 31 이 더해지므로 최종적으로 31이 출력된다.',
  'past:2024-3:W4:Q66'
);

SET @q66 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q66, 'A', '16', 0),
  (@q66, 'B', '20', 0),
  (@q66, 'C', '32', 0),
  (@q66, 'D', '31', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q66, 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q67. HRN 스케줄링 (OS_PROC_MEM, HARD)
 *  - WRITTEN / MCQ / HARD
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  @topic_os_basic,
  'WRITTEN',
  'MCQ',
  'HARD',
  'HRN(Highest Response Ratio Next) 방식으로 스케줄링할 때, 다음과 같은 작업들이 준비 큐에 있다.

| 작업 | 대기 시간 | 서비스 시간 |
|------|-----------|-------------|
| A    | 5         | 20          |
| B    | 40        | 20          |
| C    | 15        | 45          |
| D    | 20        | 2           |

이때 HRN 우선순위(Response Ratio)를 이용하여 처음 선택되는 작업은 어느 것인가?
(단, 우선순위 R = (대기시간 + 서비스시간) / 서비스시간 으로 정의한다.)',
  'B',
  'HRN 방식에서는 응답비율 R = (W + S) / S 로 계산하여 가장 큰 값을 가진 작업을 우선 실행한다.
- A: W=5,  S=20  → R = (5+20)/20  = 25/20  = 1.25
- B: W=40, S=20  → R = (40+20)/20 = 60/20 = 3.0
- C: W=15, S=45  → R = (15+45)/45 = 60/45 ≒ 1.33
- D: W=20, S=2   → R = (20+2)/2   = 22/2 = 11

가장 큰 응답비율은 작업 D(11)이므로, HRN 스케줄러가 처음 선택하는 작업은 D이다.',
  'past:2024-3:W4:Q67'
);

SET @q67 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q67, 'A', '작업 A', 0),
  (@q67, 'B', '작업 D', 1),
  (@q67, 'C', '작업 B', 0),
  (@q67, 'D', '작업 C', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q67, 'OS_PROC_MEM');


/* =======================================================
 * Q68. continue 키워드 (LANG_BASIC_SCRIPT, EASY)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  @topic_lang_basic,
  'WRITTEN',
  'MCQ',
  'EASY',
  'C, Java 등의 언어에서 반복문 내에서 사용되며, 이후의 문장을 실행하지 않고 제어를 반복문의 처음(다음 반복)으로 옮기는 명령어는 무엇인가?',
  'A',
  'continue 문은 현재 반복에서 남아 있는 문장을 건너뛰고, 바로 다음 반복으로 넘어가도록 제어를 이동시킨다.
for문에서는 증감식으로, while/do-while에서는 조건 검사로 점프한 뒤 다음 반복 여부를 판단한다.
break는 반복문이나 switch 문을 완전히 빠져나가는 키워드로 continue와 동작이 다르다.',
  'past:2024-3:W4:Q68'
);

SET @q68 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q68, 'A', 'continue', 1),
  (@q68, 'B', 'break', 0),
  (@q68, 'C', 'return', 0),
  (@q68, 'D', 'next', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q68, 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q69. FIFO 페이지 교체 (OS_PROC_MEM, HARD)
 *  - WRITTEN / MCQ / HARD
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  @topic_os_basic,
  'WRITTEN',
  'MCQ',
  'HARD',
  '3개의 페이지 프레임을 가진 시스템에서 초기 상태는 모두 비어 있다. 페이지 참조 순서가 다음과 같을 때 FIFO 알고리즘을 사용할 경우, 모든 참조를 수행한 후 프레임에 남아 있는 페이지 번호의 조합으로 옳은 것은?
참조 순서: 1, 2, 3, 4, 1, 2',
  'C',
  'FIFO(First-In First-Out)는 가장 먼저 적재된 페이지를 가장 먼저 교체하는 방식이다.
1) 1 → [1, -, -]
2) 2 → [1, 2, -]
3) 3 → [1, 2, 3]
4) 4 → 프레임이 가득 찼으므로 가장 오래된 1을 교체 → [4, 2, 3]
5) 1 → 1은 프레임에 없으므로, 다음으로 오래된 2를 교체 → [4, 1, 3]
6) 2 → 2는 프레임에 없으므로, 다음으로 오래된 3을 교체 → [4, 1, 2]

따라서 최종 프레임 상태는 [4, 1, 2] 이다.',
  'past:2024-3:W4:Q69'
);

SET @q69 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q69, 'A', '[1, 2, 3]', 0),
  (@q69, 'B', '[1, 3, 4]', 0),
  (@q69, 'C', '[4, 1, 2]', 1),
  (@q69, 'D', '[2, 3, 4]', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q69, 'OS_PROC_MEM');


/* =======================================================
 * Q70. ASP 설명 (LANG_BASIC_SCRIPT, EASY)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  @topic_lang_lib,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 설명에 해당하는 기술은 무엇인가?

> - 서버 측에서 동적으로 수행되는 웹 페이지를 만들기 위한 기술이다.
> - Microsoft에서 개발하였으며, 주로 Windows 계열 웹 서버에서 사용된다.',
  'B',
  'ASP(Active Server Pages)는 Microsoft에서 개발한 서버 사이드 스크립트 기술로, '
  'IIS와 같은 Windows 기반 웹 서버에서 동적 웹 페이지를 생성할 때 사용된다. '
  'JavaScript는 클라이언트·서버 양쪽에서 사용 가능한 스크립트 언어이고, '
  'JSP는 Java 기반 서버 사이드 페이지 기술, Python은 범용 프로그래밍 언어이다.',
  'past:2024-3:W4:Q70'
);

SET @q70 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q70, 'A', 'JavaScript', 0),
  (@q70, 'B', 'ASP(Active Server Pages)', 1),
  (@q70, 'C', 'JSP(Java Server Pages)', 0),
  (@q70, 'D', 'Python', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q70, 'LANG_BASIC_SCRIPT');

SET FOREIGN_KEY_CHECKS = 1;
