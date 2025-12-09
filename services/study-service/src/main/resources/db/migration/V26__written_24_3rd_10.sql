USE certpilot_study;

/* =======================================================
 * 91 ~ 100 : 정보시스템 구축 관리
 *  - cert_id = 1 (정보처리기사)
 *  - mode = 'WRITTEN'
 *  - type = 'MCQ'
 * ======================================================= */


/* =======================================================
 * Q91. 구조적 방법론 (SW_PROCESS)
 *  - topic: 5.1.1 소프트웨어 개발방법론 선정
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15101, -- 5.1.1 소프트웨어 개발방법론 선정
  'WRITTEN',
  'MCQ',
  'EASY',
  '구조적 방법론에 대한 설명으로 옳은 것은?',
  'D',
  '구조적 방법론(Structured Methodology)은 분할과 정복(Divide and Conquer) 원리를 적용하여
복잡한 문제를 기능 단위로 단계적으로 분해하고, 각 기능을 모듈화하여 설계·구현하는 방법론입니다.
객체, 클래스, 메시지를 중심으로 하는 것은 객체지향 방법론이며,
확장성·재사용성 측면은 구조적 방법론보다는 객체지향 방법론의 특징에 더 가깝습니다.',
  'seed:2024-3W:91'
);

SET @q91 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q91, 'A', '자료 중심(Data 중심)의 방법론이다.', 0),
  (@q91, 'B', '구성 요소에는 객체, 클래스, 메시지 등이 있다.', 0),
  (@q91, 'C', '새로운 기능을 추가하는 것이 간단하여 확장성이 보장된다.', 0),
  (@q91, 'D', '분할과 정복(Divide and Conquer) 원리를 적용한다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q91, 'SW_PROCESS');


/* =======================================================
 * Q92. SDN (Software Defined Networking) (NW_ARCH)
 *  - topic: 5.2.1 네트워크 구축관리
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15201, -- 5.2.1 네트워크 구축관리
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음에서 설명하는 IT 기술은?

> - 네트워크를 제어부와 데이터 전달부로 분리하여 관리자가 보다 효율적으로 네트워크를 제어·관리할 수 있는 기술
> - 기존의 라우터·스위치 등과 같이 하드웨어에 의존하는 네트워크 체계에서 안정성·속도·보안 등을 소프트웨어로 제어·관리하기 위해 개발됨
> - 네트워크 장비의 펌웨어 업그레이드를 통해 사용자가 데이터 전송 경로를 직접 관리할 수 있고, 특정 서비스의 전송 경로만 수정하여 문제를 처리할 수 있음',
  'A',
  '문제 설명은 제어부(Control Plane)와 데이터 전달부(Data Plane)를 분리하여
중앙에서 네트워크를 소프트웨어로 제어·관리하는 SDN(Software Defined Networking)에 대한 내용입니다.
NFS는 네트워크 파일 시스템, Network Mapper는 포트 스캐너, AOE는 다른 스토리지 관련 용어로 SDN과는 다릅니다.',
  'seed:2024-3W:92'
);

SET @q92 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q92, 'A', 'SDN(Software Defined Networking)', 1),
  (@q92, 'B', 'NFS(Network File System)', 0),
  (@q92, 'C', 'Network Mapper', 0),
  (@q92, 'D', 'AOE Network', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q92, 'NW_ARCH');


/* =======================================================
 * Q93. Secure 코딩 - 입력 데이터 보안 약점 (SEC_INPUT)
 *  - topic: 5.3.2 소프트웨어 개발 보안 구현
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15302, -- 5.3.2 소프트웨어 개발 보안 구현
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'Secure 코딩에서 입력 데이터의 보안 약점과 관련한 설명으로 틀린 것은?',
  'D',
  '입력 데이터 보안 약점은 “외부에서 들어오는 값”이 적절한 검증 없이 사용될 때 발생합니다.
- SQL 삽입: 외부 입력 값이 쿼리에 그대로 삽입되어 SQL 구문이 조작됨
- 크로스 사이트 스크립트(XSS): 검증되지 않은 외부 입력이 브라우저에서 스크립트로 실행됨
- 운영체제 명령어 삽입: 명령어 파라미터에 외부 입력을 검증 없이 사용하여 OS 명령이 조작됨
자원 삽입(Resource Injection)은 외부 입력을 통해 파일/포트/디렉터리와 같은 자원 식별자를 조작하는 취약점이지,
“내부 입력 값으로 사용 불가능한 자원을 반복 입력해 과부하를 발생시키는 것”과는 거리가 있습니다.',
  'seed:2024-3W:93'
);

SET @q93 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q93, 'A', 'SQL 삽입: 사용자의 입력 값 등 외부 입력 값이 쿼리에 삽입되어 공격이 발생한다.', 0),
  (@q93, 'B', '크로스 사이트 스크립트: 검증되지 않은 외부 입력 값에 의해 브라우저에서 악의적인 코드가 실행된다.', 0),
  (@q93, 'C', '운영체제 명령어 삽입: 명령어 파라미터 입력 값이 검증 없이 사용되어 공격자가 OS 명령을 조작한다.', 0),
  (@q93, 'D', '자원 삽입: 사용자가 내부 입력 값을 통해 시스템 내에 사용 불가능한 자원을 지속적으로 입력해 과부하를 발생시킨다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q93, 'SEC_INPUT');


/* =======================================================
 * Q94. DDoS 도구 - Tribe Flood Network (SEC_DOS)
 *  - topic: 5.4.2 시스템 보안 구현
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15402, -- 5.4.2 시스템 보안 구현
  'WRITTEN',
  'MCQ',
  'EASY',
  'DDoS(분산 서비스 거부) 공격과 연관이 있는 공격 도구는?',
  'B',
  'Tribe Flood Network(TFN)는 여러 대의 공격 호스트를 이용해 대량의 트래픽을 발생시키는
전형적인 DDoS 공격 도구입니다.
Secure Shell은 암호화된 원격 접속 프로토콜, Nimda는 웜 바이러스, Deadlock은 프로세스 동기화 문제로
DDoS 도구와는 성격이 다릅니다.',
  'seed:2024-3W:94'
);

SET @q94 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q94, 'A', 'Secure Shell', 0),
  (@q94, 'B', 'Tribe Flood Network', 1),
  (@q94, 'C', 'Nimda', 0),
  (@q94, 'D', 'Deadlock', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q94, 'SEC_DOS');


/* =======================================================
 * Q95. StackGuard / 스택 가드 (SEC_STACK)
 *  - topic: 5.4.2 시스템 보안 구현
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15402, -- 5.4.2 시스템 보안 구현
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '메모리 상에서 프로그램의 복귀 주소와 변수 사이에 특정 값을 저장해 두었다가,
그 값이 변경되었을 경우 오버플로우 상태로 가정하고 프로그램 실행을 중단하는 기술은?',
  'D',
  '스택가드(StackGuard)는 스택 프레임에서 반환 주소와 지역 변수 사이에 canary 값을 두고,
함수 반환 시 이 값이 변조되었는지 검사하여 버퍼 오버플로우 공격을 탐지·차단하는 기법입니다.
값이 변경되면 오버플로우로 간주하고 프로그램을 종료함으로써 쉘코드 실행 등을 막습니다.',
  'seed:2024-3W:95'
);

SET @q95 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q95, 'A', '리커버리(Recovery)', 0),
  (@q95, 'B', '통제 모드 체크(Control Mode Check)', 0),
  (@q95, 'C', '시스로그(Syslog)', 0),
  (@q95, 'D', '스택가드(StackGuard)', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q95, 'SEC_STACK');


/* =======================================================
 * Q96. DPI(Deep Packet Inspection) (NW_PACKET)
 *  - topic: 5.2.1 네트워크 구축관리
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15201, -- 5.2.1 네트워크 구축관리
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'OSI 7계층 전 계층의 프로토콜과 패킷 내부 콘텐츠를 파악하여
침입 시도·해킹 등을 탐지하고 트래픽을 조정하는 패킷 분석 기술은?',
  'D',
  'DPI(Deep Packet Inspection)는 단순히 헤더 정보만 보는 것이 아니라,
애플리케이션 계층까지 포함한 패킷 전체(헤더+페이로드)를 분석하여
침입 시도, 악성 트래픽, 정책 위반 등을 탐지하고 트래픽을 제어하는 기술입니다.',
  'seed:2024-3W:96'
);

SET @q96 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q96, 'A', 'PLCP(Packet Level Control Processor)', 0),
  (@q96, 'B', 'Traffic Distributor', 0),
  (@q96, 'C', 'Packet Tree', 0),
  (@q96, 'D', 'DPI(Deep Packet Inspection)', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q96, 'NW_PACKET');


/* =======================================================
 * Q97. SAN(Storage Area Network) (NW_STORAGE)
 *  - topic: 5.2.3 HW 구축관리
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15203, -- 5.2.3 HW 구축관리
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 내용이 설명하는 것은?

> - 네트워크상에 광채널 스위치의 고속 전송·장거리 연결·멀티 프로토콜 기능을 활용
> - 서로 다른 운영체제를 가진 여러 기종이 네트워크상에서 동일 저장장치의 데이터를 공유
> - 여러 개의 저장장치나 백업 장비를 단일화시키는 저장 시스템',
  'A',
  'SAN(Storage Area Network)은 서버와 스토리지를 전용 고속 네트워크(주로 광채널)로 연결하여
서로 다른 서버·OS가 동일한 스토리지를 공유할 수 있게 하는 구조입니다.
MBR은 디스크 파티션 정보, NIC/NAC는 네트워크 인터페이스 및 접근 제어 관련 용어입니다.',
  'seed:2024-3W:97'
);

SET @q97 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q97, 'A', 'SAN(Storage Area Network)', 1),
  (@q97, 'B', 'MBR(Master Boot Record)', 0),
  (@q97, 'C', 'NIC(Network Interface Card)', 0),
  (@q97, 'D', 'NAC(Network Access Control)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q97, 'NW_STORAGE');


/* =======================================================
 * Q98. 배열 반환과 정보 노출 / 캡슐화 위반 (SEC_API)
 *  - topic: 5.3.2 소프트웨어 개발 보안 구현
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15302, -- 5.3.2 소프트웨어 개발 보안 구현
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'Public/Private 메소드로부터 배열이 그대로 반환될 경우 발생하는 문제점으로 가장 옳은 것은?',
  'B',
  '메소드에서 내부 배열을 그대로 반환하면, 호출 측이 그 배열의 참조(주소)를 그대로 넘겨받게 됩니다.
이 경우 외부 코드가 배열 내용을 직접 수정할 수 있어 캡슐화가 깨지고, 예상치 못한 상태 변경·보안 취약점으로 이어집니다.
그래서 보통은 방어적 복사(defensive copy)를 만들어 반환하는 패턴을 사용합니다.',
  'seed:2024-3W:98'
);

SET @q98 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q98, 'A', '메소드로의 접근이 불가능해진다.', 0),
  (@q98, 'B', '배열의 주소가 외부로 공개되어 외부에서 배열에 직접 접근·수정할 수 있게 된다.', 1),
  (@q98, 'C', '시스템의 모든 내부 정보가 자동으로 노출된다.', 0),
  (@q98, 'D', '동기화 오류가 반드시 발생한다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q98, 'SEC_API');


/* =======================================================
 * Q99. SSH 기본 포트 / 특징 (NW_SEC_PROTOCOL)
 *  - topic: 5.2.1 네트워크 구축관리
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15201, -- 5.2.1 네트워크 구축관리
  'WRITTEN',
  'MCQ',
  'EASY',
  'SSH(Secure Shell)에 대한 설명으로 틀린 것은?',
  'A',
  'SSH는 원격 로그인을 안전하게 수행하기 위한 프로토콜로, 기본 포트는 22번입니다.
따라서 “기본 네트워크 포트로 25번을 사용한다”는 설명은 틀린 설명입니다.
전송 데이터 암호화, 공개키 기반 인증, 원격 명령 실행·쉘 서비스 제공은 SSH의 올바른 특징입니다.',
  'seed:2024-3W:99'
);

SET @q99 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q99, 'A', 'SSH의 기본 네트워크 포트는 25번을 사용한다.', 1),
  (@q99, 'B', '전송되는 데이터는 암호화된다.', 0),
  (@q99, 'C', '키 기반 인증을 위해 클라이언트의 공개키를 서버에 등록해야 한다.', 0),
  (@q99, 'D', '연결된 컴퓨터 간 원격 명령 실행이나 셸 서비스를 수행할 수 있다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q99, 'NW_SEC_PROTOCOL');


/* =======================================================
 * Q100. 간트 차트(Gantt Chart) (PM_SCHEDULE)
 *  - topic: 5.2.2 SW 구축관리
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202, -- 5.2.2 SW 구축관리
  'WRITTEN',
  'MCQ',
  'EASY',
  '프로젝트의 각 작업들이 언제 시작하고 언제 종료되는지에 대한
작업 일정을 막대 도표를 이용하여 표시하는 프로젝트 일정표는?',
  'A',
  '간트 차트(Gantt Chart)는 시간 축에 작업을 막대(bar)로 배치하여
각 작업의 시작·종료 시점, 기간, 겹치는 구간 등을 한눈에 볼 수 있게 하는 일정 관리 도구입니다.
임계 경로 기법(CPM), PERT는 네트워크 다이어그램 기반 공정 관리 기법이고,
WBS는 작업을 구조적으로 분해한 계층 구조입니다.',
  'seed:2024-3W:100'
);

SET @q100 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q100, 'A', '간트 차트(Gantt Chart)', 1),
  (@q100, 'B', '임계 경로 기법(CPM)', 0),
  (@q100, 'C', 'WBS(Work Breakdown Structure)', 0),
  (@q100, 'D', 'PERT(Program Evaluation and Review Technique)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q100, 'PM_SCHEDULE');
