USE certpilot_study;

------------------------------------------------------------
-- Q71. IEEE 802.3 MAC - CSMA/CD  [정답: B]
-- topic_id = 14302 (네트워크/이더넷·MAC)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14302,
  'WRITTEN',
  'MCQ',
  'EASY',
  'IEEE 802.3 LAN(MAC)에서 사용되는 전송 매체 접속 제어 방식은?',
  'B',
  'IEEE 802.3은 이더넷(Ethernet)을 규정하는 표준으로, 매체 접속 제어 방식으로 CSMA/CD(Carrier Sense Multiple Access with Collision Detection)를 사용합니다. Token Bus, Token Ring, Slotted Ring 등은 다른 MAC 방식에 해당하므로 정답은 CSMA/CD입니다.',
  'past:2024-2:Q71'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Token Bus',   0),
  (@q_id, 'B', 'CSMA/CD',     1),
  (@q_id, 'C', 'Slotted Ring',0),
  (@q_id, 'D', 'Token Ring',  0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'NET_PROTO_IP');

------------------------------------------------------------
-- Q72. 오버레이(Overlay) 기법  [정답: B]
-- topic_id = 14301 (운영체제/메모리 관리)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14301,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '주기억장치보다 큰 사용자 프로그램을 실행하기 위한 기법으로,
보조기억장치에 저장된 하나의 프로그램을 여러 개의 조각으로 분할한 후
필요한 조각을 차례로 주기억장치에 적재하여 실행하는 할당 기법은?',
  'B',
  '오버레이(Overlay)는 프로그램을 여러 조각으로 나누어, 실제 실행 시점에 필요한 부분만 메모리에 적재하고 나머지는 보조기억장치에 둔 채로 교체해 가며 사용하는 기법입니다. 주기억장치보다 큰 프로그램을 실행하기 위한 고전적인 방법으로, 스와핑·페이징·세그먼테이션과는 개념이 다릅니다.',
  'past:2024-2:Q72'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '스와핑(Swapping)',        0),
  (@q_id, 'B', '오버레이(Overlay)',       1),
  (@q_id, 'C', '세그먼테이션(Segmentation)', 0),
  (@q_id, 'D', '페이징(Paging)',         0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OS_PROC_MEM');

------------------------------------------------------------
-- Q73. 배치 유형 – 이벤트성 배치  [정답: A]
-- topic_id = 14301 (운영·배치 개념)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14301,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '배치 프로그램의 자동 수행 주기 중, 사용자가 특정 조건을 설정해 두고
해당 조건이 충족될 때만 수행되도록 하는 것은?',
  'A',
  '배치 유형은 일반적으로 정기 배치(일/주/월 등 주기), 이벤트성 배치(특정 조건·사건 발생 시), On-Demand 배치(사용자 요청 시)로 구분합니다. “특정 조건이 충족될 때만 자동 실행”은 이벤트성 배치의 정의에 해당하므로 정답은 이벤트성 배치입니다.',
  'past:2024-2:Q73'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '이벤트성 배치(Event-driven Batch)', 1),
  (@q_id, 'B', '정기 배치(Periodic Batch)',         0),
  (@q_id, 'C', '사용자 배치(User Batch)',           0),
  (@q_id, 'D', 'On-Demand 배치',                    0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OS_PROC_MEM');

------------------------------------------------------------
-- Q74. Fan-in, Fan-out  [정답: B]
-- topic_id = 11301 (모듈 구조/품질)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source, image_url
) VALUES (
  1,
  11301,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 프로그램 구조에서 모듈 F의 Fan-In과 Fan-Out의 수는 얼마인가?',
  'B',
  'Fan-In은 “해당 모듈을 호출하는 상위 모듈의 수”, Fan-Out은 “해당 모듈이 호출하는 하위 모듈의 수”를 의미합니다. 제시된 구조(시험지 그림 기준)에서 F를 호출하는 모듈이 3개, F가 호출하는 모듈이 2개이므로 Fan-In : 3, Fan-Out : 2가 됩니다.',
  'past:2024-2:Q74',
  'https://api.mycertpilot.com/static/images/questions/q_2024_02_74.png'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Fan-In : 2, Fan-Out : 3', 0),
  (@q_id, 'B', 'Fan-In : 3, Fan-Out : 2', 1),
  (@q_id, 'C', 'Fan-In : 1, Fan-Out : 2', 0),
  (@q_id, 'D', 'Fan-In : 2, Fan-Out : 1', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');

------------------------------------------------------------
-- Q75. Java do-while 결과  [정답: B]
-- topic_id = 14202 (Java 제어문)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14202,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 Java로 만들어진 반복문 코드이다. 실행 후 a와 sum 값은?

```java
int a = 0, sum = 0;
do {
  a++;
  sum += a;
} while (a > 10);
```',
'B',
'do-while문은 조건과 상관없이 최소 1번은 실행됩니다.
초기 a=0, sum=0에서 do 블록 1회 실행 후 a=1, sum=1이 되고,
그 다음 조건 검사에서 a>10은 false이므로 루프가 종료됩니다.
따라서 최종 값은 a=1, sum=1입니다.',
'past:2024-2:Q75'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
(@q_id, 'A', 'a = 0, sum = 0', 0),
(@q_id, 'B', 'a = 1, sum = 1', 1),
(@q_id, 'C', 'a = 9, sum = 45',0),
(@q_id, 'D', 'a = 10, sum = 55',0);

INSERT INTO question_tag (question_id, tag) VALUES
(@q_id, 'LANG_BASIC_SCRIPT');

------------------------------------------------------------
-- Q76. 표준 출력이 아닌 것 (prn)  [정답: B]
-- topic_id = 14202 (Java 기본 I/O)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14202,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 Java에서 표준 출력 시 사용하는 명령어가 아닌 것은?',
  'B',
  'Java에서는 표준 출력을 위해 System.out.print, System.out.println, System.out.printf와 같은 메서드를 사용합니다. prn이라는 이름의 표준 출력 메서드는 제공되지 않으므로, “표준 출력 시 사용하는 명령어가 아닌 것”은 prn입니다.',
  'past:2024-2:Q76'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'print',   0),
  (@q_id, 'B', 'prn',     1),
  (@q_id, 'C', 'println', 0),
  (@q_id, 'D', 'printf',  0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');

------------------------------------------------------------
-- Q77. 스크립트 언어가 아닌 것 – Fortran  [정답: A]
-- topic_id = 14201 (언어 분류/특징)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14201,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 스크립트 언어가 아닌 것은?',
  'A',
  'PHP와 Python은 대표적인 스크립트 언어이며, Basic 역시 인터프리터/스크립트 형태 구현들이 존재합니다. 
반면 Fortran은 전통적인 컴파일 언어로 대규모 수치 계산 등에 사용되는 언어이므로 스크립트 언어라고 보지 않습니다. 따라서 정답은 Fortran입니다.',
  'past:2024-2:Q77'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Fortran',                         1),
  (@q_id, 'B', 'PHP',                             0),
  (@q_id, 'C', 'Python',                          0),
  (@q_id, 'D', 'Basic(스크립트 형태로 사용 가능)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');

------------------------------------------------------------
-- Q78. 페이지 교체 알고리즘이 아닌 것 – SCF  [정답: C]
-- topic_id = 14301 (운영체제/메모리·교체)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14301,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 페이지 교체(Page Replacement) 알고리즘이 아닌 것은?',
  'C',
  'OPT, LRU, NUR은 모두 대표적인 페이지 교체 알고리즘입니다.
반면 SCF(Shortest Completion First)는 CPU 스케줄링(프로세스 스케줄링)에서 사용하는 개념으로, 페이지 교체 알고리즘이 아닙니다. 따라서 정답은 SCF입니다.',
  'past:2024-2:Q78'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'OPT(Optimal Replacement)',        0),
  (@q_id, 'B', 'LRU(Least Recently Used)',        0),
  (@q_id, 'C', 'SCF(Shortest Completion First)',  1),
  (@q_id, 'D', 'NUR(Not Used Recently)',          0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OS_PROC_MEM');

------------------------------------------------------------
-- Q79. Java 문자열에서 문자 개수 세기  [정답: D]
-- topic_id = 14202 (Java 문자열/배열)
------------------------------------------------------------
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
  public static void main(String args[]) {
    String str = "Message Queueing";
    char s[] = str.toCharArray();
    int r = 0;
    for (char c : s)
      if (c == ''e'')
        r++;
    System.out.print(r);
  }
}
```',
  'D',
  '문자열 "Message Queueing"에서 e의 개수를 세면 다음과 같습니다.
- "Message" 부분: M e s s a g e → e가 2개
- 공백 1개
- "Queueing" 부분: Q u e u e i n g → e가 2개

총 e의 개수는 2 + 2 = 4이므로 최종 출력은 4입니다.',
  'past:2024-2:Q79'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '3', 0),
  (@q_id, 'B', '2', 0),
  (@q_id, 'C', '5', 0),
  (@q_id, 'D', '4', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');

------------------------------------------------------------
-- Q80. 서로 다른 프로토콜 구조를 연결 – Gateway  [정답: C]
-- topic_id = 14302 (네트워크/장비)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14302,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '네트워크 계층까지의 프로토콜 구조가 다른 네트워크 간의 연결을 위해
프로토콜 변환 기능을 수행하는 네트워크 장비는?',
  'C',
  '브리지와 스위치는 같은 계층/프로토콜 내에서 세그먼트나 스위칭을 담당하고,
라우터는 같은 3계층(IP 계열) 프로토콜을 사용하는 네트워크 간의 경로 선택을 수행합니다.
프로토콜 스택 구조가 다른 이기종 네트워크를 연결하면서 프로토콜 변환을 수행하는 장비는
게이트웨이(Gateway)이므로 정답은 C입니다.',
  'past:2024-2:Q80'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '브리지(Bridge)',   0),
  (@q_id, 'B', '리피터(Repeater)', 0),
  (@q_id, 'C', '게이트웨이(Gateway)', 1),
  (@q_id, 'D', '라우터(Router)',   0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'NET_PROTO_IP');
