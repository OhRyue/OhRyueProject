USE certpilot_study;

/* =======================================================
 * Q71. Best Fit 내부 단편화
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
  '20KB, 16KB, 8KB, 40KB 크기의 빈 기억공간이 있을 때, Best Fit(최적 적합) 배치 전략을 사용하여 17KB 프로그램을 적재하면 내부 단편화 크기는 얼마인가?',
  'A',
  'Best Fit(최적 적합) 전략은 “프로그램을 수용할 수 있는 블록들 중에서 가장 크기가 근접한(남는 공간이 가장 적은) 블록”을 선택하는 방식입니다.

주어진 가용 블록은 20KB, 16KB, 8KB, 40KB이고, 17KB를 적재하려면 16KB와 8KB는 용량이 부족하므로 선택 대상이 될 수 없습니다. 17KB 이상이면서 가장 근접한 것은 20KB 블록입니다.

- 선택된 블록 크기: 20KB
- 적재할 프로그램 크기: 17KB
- 내부 단편화: 20KB - 17KB = 3KB

따라서 Best Fit 전략으로 적재할 때의 내부 단편화 크기는 3KB입니다.',
  'past:2024-1:Q71'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '3KB', 1),
  (@q_id, 'B', '23KB', 0),
  (@q_id, 'C', '64KB', 0),
  (@q_id, 'D', '67KB', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OS_PROC_MEM');


/* =======================================================
 * Q72. FIFO 페이지 교체(Page Fault 수)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14301,
  'WRITTEN',
  'MCQ',
  'HARD',
  '다음의 페이지 참조 열(Page reference)에 대해 페이지 교체 기법으로
선입선출(FIFO) 알고리즘을 사용할 경우 페이지 부재(Page Fault) 횟수는
얼마인가요?
(단, 할당된 페이지 프레임 수는 3이고 처음에는 모든 프레임이 비어 있다.)

> **페이지 참조열**
> 7, 0, 1, 2, 0, 3, 0, 4, 2, 3, 0, 3, 2, 1, 2, 0, 1, 7, 0',
  'B',
  'FIFO(First-In First-Out) 페이지 교체는 “가장 먼저 들어온 페이지를 가장 먼저 내보내는” 방식입니다.
프레임 수가 3개일 때 참조열을 순서대로 따라가며 페이지 부재(Page Fault)를 계산하면 다음과 같습니다.

초기: [ - , - , - ]  (비어 있음)

1) 7 → Fault 1 → [ 7, -, - ]
2) 0 → Fault 2 → [ 7, 0, - ]
3) 1 → Fault 3 → [ 7, 0, 1 ]
4) 2 → Fault 4 → [ 2, 0, 1 ]
5) 0 → O
6) 3 → Fault 5 → [ 2, 3, 1 ]
7) 0 → Fault 6 → [ 2, 3, 0 ]
8) 4 → Fault 7 → [ 4, 3, 0 ]
9) 2 → Fault 8 → [ 4, 2, 0 ]
10) 3 → Fault 9 → [ 4, 2, 3 ]
11) 0 → Fault 10 → [ 0, 2, 3 ]
12) 3 → O
13) 2 → O
14) 1 → Fault 11 → [ 1, 2, 3 ]
15) 2 → O
16) 0 → Fault 12 → [ 1, 0, 3 ]
17) 1 → O
18) 7 → Fault 13 → [ 1, 0, 7 ]
19) 0 → O

따라서 **총 페이지 부재(Page Fault)는 13회**입니다.',
  'past:2024-1:Q72'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '13회', 0),
  (@q_id, 'B', '14회', 1),
  (@q_id, 'C', '15회', 0),
  (@q_id, 'D', '20회', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OS_PROC_MEM');


/* =======================================================
 * Q73. UNIX에서 새로운 프로세스 생성 명령
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
  'UNIX 계열 시스템에서 새로운 프로세스를 생성하는 시스템 호출(명령)은 무엇인가?',
  'C',
  'UNIX 계열 운영체제에서 새로운 프로세스를 생성하는 대표적인 시스템 호출은 `fork()`입니다. `fork()`가 호출되면 현재 프로세스를 복사한 자식 프로세스가 생성되며, 부모·자식 프로세스는 이후 독립적으로 실행됩니다.

- `cat`, `ls` 등은 단순 명령어(파일 출력, 디렉터리 목록)입니다.
- `chmod`는 파일 접근 권한을 변경하는 명령어입니다.
- `fork()`가 새로운 프로세스를 만드는 시스템 호출이므로 정답입니다.',
  'past:2024-1:Q73'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'cat', 0),
  (@q_id, 'B', 'chmod', 0),
  (@q_id, 'C', 'fork', 1),
  (@q_id, 'D', 'ls', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OS_PROC_MEM');


/* =======================================================
 * Q74. 페이지 크기가 작아질수록 나타나는 현상
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
  '페이징 기법에서 페이지 크기가 작아질수록 발생하는 현상으로 옳지 않은 것은?',
  'D',
  '페이지 크기가 작아지면 다음과 같은 효과들이 나타납니다.

1) 페이지 내부의 사용하지 않는 부분(내부 단편화)이 줄어들기 때문에 **기억장소 이용 효율은 증가**합니다.
2) 같은 프로세스를 표현하기 위해 필요한 페이지 수가 늘어나므로, 페이지를 더 자주 교체해야 하고 **입·출력(I/O) 오버헤드가 증가**합니다.
3) 내부 단편화(한 페이지 안에서 남는 공간)는 일반적으로 **감소**합니다.
4) 다만 페이지 수가 많아지므로, 각 페이지에 대한 정보를 유지하는 **페이지 맵 테이블의 크기와 관리 비용은 오히려 증가**합니다.

따라서 “페이지 크기가 작아질수록 페이지 맵 테이블의 크기가 감소한다”는 설명은 옳지 않습니다.',
  'past:2024-1:Q74'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '기억장소 이용 효율이 증가한다.', 0),
  (@q_id, 'B', '입·출력 시간이 늘어날 수 있다.', 0),
  (@q_id, 'C', '내부 단편화가 감소한다.', 0),
  (@q_id, 'D', '페이지 맵 테이블의 크기가 감소한다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OS_PROC_MEM');


/* =======================================================
 * Q75. C 언어 논리 연산자 (논리합)
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
  'C 언어에서 두 개의 논리 값 중 하나라도 참이면 1을, 둘 다 거짓이면 0을 반환하는 논리 연산자는 무엇인가?',
  'A',
  'C 언어에서 논리 연산자는 다음과 같습니다.

- `&&` : 논리곱(AND) – 두 피연산자가 모두 참일 때만 참이 됩니다.
- `||` : 논리합(OR) – 두 피연산자 중 **하나라도 참이면 참**, 둘 다 거짓일 때만 거짓이 됩니다.
- `!`  : 논리부정(NOT) – 참/거짓을 반대로 뒤집습니다.
- `!=` : 같지 않음을 비교하는 관계 연산자로, 논리 연산자가 아니라 비교 연산자입니다.

따라서 “두 값 중 하나라도 참이면 1을, 모두 거짓이면 0을 반환하는” 연산자는 `||` 입니다.',
  'past:2024-1:Q75'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '||', 1),
  (@q_id, 'B', '&&', 0),
  (@q_id, 'C', '**', 0),
  (@q_id, 'D', '!=', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q76. IPv6 주소체계
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
  'IPv6의 주소체계와 관련된 설명으로 거리가 먼 것은?',
  'C',
  'IPv6에서는 다음과 같은 주소 유형을 사용합니다.

- **유니캐스트(Unicast)** : 하나의 인터페이스(호스트)를 가리키는 단일 수신자 주소
- **애니캐스트(Anycast)** : 여러 인터페이스 중 가장 가까운(라우팅 관점) 하나를 선택해서 보내는 주소
- **멀티캐스트(Multicast)** : 다수의 수신자 그룹을 대상으로 하는 주소

IPv4에서 사용되던 **브로드캐스트(Broadcast)**는 IPv6에서는 더 이상 사용하지 않으며, 멀티캐스트와 애니캐스트로 대체되었습니다. 따라서 “Multicast / Broadcast”와 같이 IPv6에서 브로드캐스트를 사용하는 것처럼 표현하는 것은 IPv6 주소체계와 거리가 멉니다.',
  'past:2024-1:Q76'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Unicast / Anycast', 0),
  (@q_id, 'B', 'Unicast / Multicast', 0),
  (@q_id, 'C', 'Multicast / Broadcast', 1),
  (@q_id, 'D', 'Unicast / Anycast / Multicast', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'NET_PROTO_IP');


/* =======================================================
 * Q77. 결합도(Control Coupling)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302,
  'WRITTEN',
  'MCQ',
  'HARD',
  '어떤 모듈이 다른 모듈의 내부 처리 순서를 제어하기 위해 제어 신호를 인자로 전달하고, 이로 인해 하위 모듈에서 상위 모듈로 제어권이 역전되는 “권리 전도 현상”이 발생하는 결합도는 무엇인가?',
  'C',
  '모듈 간 결합도(Coupling)는 두 모듈이 얼마나 강하게 서로 의존하는지를 나타내며, 일반적으로 약한 결합도가 바람직합니다.

- **데이터 결합(Data Coupling)** : 단순 데이터만 전달하는 비교적 좋은 결합 형태입니다.
- **스탬프 결합(Stamp Coupling)** : 레코드/구조체 전체 등 필요 이상으로 많은 데이터를 전달하는 결합입니다.
- **공통 결합(Common Coupling)** : 전역 변수 등 공통 데이터 영역을 여러 모듈이 공유하는 결합입니다.
- **제어 결합(Control Coupling)** : 한 모듈이 다른 모듈의 내부 처리 논리를 “제어 신호(플래그, 코드 값 등)”로 직접 제어하는 형태입니다.

문제에서처럼 하위 모듈이 상위 모듈의 처리 순서나 분기 흐름까지 제어하게 되면, 모듈 간 독립성이 크게 떨어지고 유지보수가 어려워지는데, 이를 **제어 결합(Control Coupling)**이라고 합니다.',
  'past:2024-1:Q77'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '데이터 결합(Data Coupling)', 0),
  (@q_id, 'B', '스탬프 결합(Stamp Coupling)', 0),
  (@q_id, 'C', '제어 결합(Control Coupling)', 1),
  (@q_id, 'D', '공통 결합(Common Coupling)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');


/* =======================================================
 * Q78. TCP/IP에서 논리 주소 → 물리 주소 변환 프로토콜
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14302,
  'WRITTEN',
  'MCQ',
  'EASY',
  'TCP/IP 네트워크에서 IP와 같은 논리 주소를 실제 MAC과 같은 물리 주소로 변환해 주는 프로토콜은 무엇인가?',
  'B',
  'TCP/IP에서 통신을 하려면, 상위 계층에서 사용하는 IP 주소(논리 주소)를 실제 링크 계층에서 사용하는 MAC 주소(물리 주소)로 변환해야 합니다.

이를 위해 사용하는 프로토콜이 **ARP(Address Resolution Protocol)**입니다.

- **ARP** : IP → MAC 주소 변환
- **RARP(과거)** : MAC → IP 주소 변환 (현재는 거의 사용되지 않음)
- TCP, FTP 등은 각각 전송 계층, 응용 계층의 다른 목적을 가진 프로토콜입니다.

따라서 IP 논리 주소를 물리 주소로 매핑해 주는 것은 ARP가 담당합니다.',
  'past:2024-1:Q78'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'TCP', 0),
  (@q_id, 'B', 'ARP', 1),
  (@q_id, 'C', 'IP', 0),
  (@q_id, 'D', 'FTP', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'NET_PROTO_IP');


/* =======================================================
 * Q79. Working Set과 Thrashing
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
  '가상 기억장치 관리에서 프로세스가 일정 시간 동안 자주 참조하는 페이지들의 집합을 의미하는 용어는 무엇인가?',
  'D',
  '가상 기억장치에서 프로세스는 실제로는 전체 주소 공간을 동시에 사용하는 것이 아니라, “특정 시점에 자주 사용하는 페이지 집합”만 집중적으로 참조하는 경향이 있습니다. 이 성질을 **국소성(Locality)**이라고 하고, 그때 실제로 자주 쓰이는 페이지들의 집합을 **Working Set(작업 집합)**이라고 부릅니다.

- **Working Set** : 일정 시간 창(window) 동안 프로세스가 실제로 참조한 페이지들의 집합.
- **Thrashing** : 페이지 부재가 너무 자주 발생하여 CPU는 거의 쉬고, 대부분의 시간을 페이징에만 소비하는 비정상 상태입니다.
- Locality는 “특정 구간에 접근이 몰린다”는 성질을 말할 뿐, 구체적인 페이지 집합 이름은 아닙니다.

따라서 “특정 시간 동안 자주 참조되는 페이지들의 실제 집합”은 Working Set입니다.',
  'past:2024-1:Q79'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Locality', 0),
  (@q_id, 'B', 'Deadlock', 0),
  (@q_id, 'C', 'Thrashing', 0),
  (@q_id, 'D', 'Working Set', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OS_PROC_MEM');


/* =======================================================
 * Q80. 무선 LAN에서 사용하는 CSMA 방식
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
  '다음 설명에 해당하는 매체 접근 제어 방식은 무엇인가?

> - 무선 LAN에서 데이터 전송 시 매체가 비어 있는지 확인한 뒤,  
>   충돌을 회피하기 위해 임의의 시간을 기다린 후 데이터를 전송한다.  
> - 네트워크에 데이터 전송이 없어도, 동시 전송에 의한 충돌을 막기 위해  
>   확인 신호를 사용한다.',
  'C',
  '유선 이더넷에서는 주로 **CSMA/CD(Carrier Sense Multiple Access with Collision Detection)**를 사용하지만, 무선 LAN에서는 충돌을 “탐지”하기가 어렵기 때문에 **CSMA/CA(Carrier Sense Multiple Access with Collision Avoidance)** 방식을 사용합니다.

CSMA/CA는 다음과 같은 특징을 가집니다.

1) 먼저 채널이 비어 있는지(캐리어 감지) 확인합니다.
2) 비어 있더라도 바로 전송하지 않고, **임의의 대기 시간(Backoff Time)**을 둔 뒤 전송하여 충돌 가능성을 줄입니다.
3) ACK(확인 응답) 프레임 등을 이용해 충돌 여부를 간접적으로 확인합니다.

문제에서 “무선 LAN”, “충돌을 회피하기 위한 임의 시간 대기”를 강조하고 있으므로, 이는 CSMA/CA 방식에 대한 설명입니다.',
  'past:2024-1:Q80'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'STA 방식', 0),
  (@q_id, 'B', 'Collision Domain 방식', 0),
  (@q_id, 'C', 'CSMA/CA', 1),
  (@q_id, 'D', 'CSMA/CD', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'NET_PROTO_IP');
