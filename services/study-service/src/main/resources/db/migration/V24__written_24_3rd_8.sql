/* =======================================================
 * 71 ~ 80 : 프로그래밍 언어 / OS / 네트워크
 *  - cert_id = 1 (정보처리기사)
 *  - mode = 'WRITTEN'
 *  - type = 'MCQ'
 * ======================================================= */


/* =======================================================
 * Q71. IP 버전에 대한 설명 (NET_PROTO_IP)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
)
SELECT
  1,
  14302, -- 4.3.2 네트워크 기초 활용
  'WRITTEN', 'MCQ', 'NORMAL',
  'IP 버전에 대한 설명 중 틀린 것은?',
  'D',
  'IPv4는 32비트 주소 공간을 사용하며 8비트씩 4개의 옥텟으로 표현합니다. IPv6는 128비트 주소 공간을 사용하며 16비트씩 8개 묶음을 콜론(:)으로 구분해 표기합니다. 클래스풀 IPv4 주소 체계에서는 A~E 클래스로 나누기도 합니다. 그러나 "IPv6는 IPv4보다 항상 전송 속도가 느리다"는 잘못된 표현입니다. 전송 속도는 프로토콜 버전보다는 회선 품질, 라우팅, 장비 성능 등에 더 큰 영향을 받습니다.',
  'seed:2024-3W:71'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE cert_id = 1
     AND mode = 'WRITTEN'
     AND type = 'MCQ'
     AND stem LIKE 'IP 버전에 대한 설명 중 틀린 것은%');

SET @q71 := (
  SELECT id FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE 'IP 버전에 대한 설명 중 틀린 것은%'
   LIMIT 1
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q71, 'A', 'IPv4는 32비트 주소를 사용하며 8비트씩 4개의 옥텟으로 표현한다.', 0
WHERE @q71 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q71 AND label = 'A');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q71, 'B', 'IPv6 주소는 16비트 블록 8개를 콜론(:)으로 구분해 표기한다.', 0
WHERE @q71 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q71 AND label = 'B');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q71, 'C', '클래스풀 IPv4 주소 체계에서는 A~E 클래스로 네트워크 구간 길이를 구분한다.', 0
WHERE @q71 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q71 AND label = 'C');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q71, 'D', 'IPv6는 IPv4보다 주소 구조가 복잡하므로 항상 전송 속도가 더 느리다.', 1
WHERE @q71 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q71 AND label = 'D');

INSERT INTO question_tag (question_id, tag)
SELECT @q71, 'NET_PROTO_IP'
WHERE @q71 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id = @q71 AND tag = 'NET_PROTO_IP');


/* =======================================================
 * Q72. 서버 스크립트 언어 (PHP) (LANG_BASIC_SCRIPT)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
)
SELECT
  1,
  14202, -- 4.2.2 언어특성 활용
  'WRITTEN', 'MCQ', 'EASY',
  '다음 내용이 설명하는 스크립트 언어는?

  > - 서버용 스크립트 언어로 Linux, Unix, Windows 등 다양한 운영체제에서 사용 가능하다.
  > - C, Java와 문법이 유사해 배우기 쉽고 웹 페이지 제작에 많이 활용된다.',
  'A',
  '서버 측에서 동적으로 HTML을 생성하는 대표적인 스크립트 언어가 PHP입니다. PHP는 C/Java와 문법이 비슷해 진입 장벽이 낮고, 대부분의 웹 서버·운영체제에서 동작합니다. JavaScript는 클라이언트(브라우저) 중심, C#은 .NET 환경 전용인 점에서 차이가 있습니다.',
  'seed:2024-3W:72'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE '다음 내용이 설명하는 스크립트 언어는%');

SET @q72 := (
  SELECT id FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE '다음 내용이 설명하는 스크립트 언어는%'
   LIMIT 1
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q72, 'A', 'PHP', 1
WHERE @q72 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q72 AND label = 'A');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q72, 'B', 'JavaScript', 0
WHERE @q72 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q72 AND label = 'B');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q72, 'C', 'Python', 0
WHERE @q72 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q72 AND label = 'C');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q72, 'D', 'C#', 0
WHERE @q72 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q72 AND label = 'D');

INSERT INTO question_tag (question_id, tag)
SELECT @q72, 'LANG_BASIC_SCRIPT'
WHERE @q72 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id = @q72 AND tag = 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q73. 응집도의 단계 (SW_ARCH_DESIGN)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
)
SELECT
  1,
  11301, -- 1.3.1 공통 모듈 설계
  'WRITTEN', 'MCQ', 'NORMAL',
  'Myers가 구분한 응집도의 단계 중 응집도가 가장 낮은 것은?',
  'D',
  '응집도는 모듈 내부 요소들이 얼마나 밀접하게 연관되어 있는지를 나타내며, 낮은 것에서 높은 순으로 보통 우연적(Coincidental) → 논리적 → 시간적 → 절차적 → 통신적 → 순차적 → 기능적 응집으로 분류합니다. 따라서 응집도가 가장 낮은 단계는 우연적 응집(Coincidental Cohesion)입니다.',
  'seed:2024-3W:73'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE 'Myers가 구분한 응집도의 단계 중 응집도가 가장 낮은 것은%');

SET @q73 := (
  SELECT id FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE 'Myers가 구분한 응집도의 단계 중 응집도가 가장 낮은 것은%'
   LIMIT 1
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q73, 'A', '순차적 응집(Sequential Cohesion)', 0
WHERE @q73 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q73 AND label = 'A');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q73, 'B', '기능적 응집(Functional Cohesion)', 0
WHERE @q73 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q73 AND label = 'B');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q73, 'C', '시간적 응집(Temporal Cohesion)', 0
WHERE @q73 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q73 AND label = 'C');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q73, 'D', '우연적 응집(Coincidental Cohesion)', 1
WHERE @q73 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q73 AND label = 'D');

INSERT INTO question_tag (question_id, tag)
SELECT @q73, 'SW_ARCH_DESIGN'
WHERE @q73 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id = @q73 AND tag = 'SW_ARCH_DESIGN');


/* =======================================================
 * Q74. Python 슬라이싱 (LANG_BASIC_SCRIPT)
 *  - WRITTEN / MCQ / HARD
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
)
SELECT
  1,
  14201, -- 4.2.1 기본문법 활용
  'WRITTEN', 'MCQ', 'HARD',
  '다음 Python 코드 실행 결과로 옳은 것은?

```python
String = "Conceptual Schema"
r = String[-4:6:-2]
print(r)
```',
  'A',
  '문자열 "Conceptual Schema"는 인덱스 0~16까지이고, -4는 뒤에서 네 번째 문자 ''h''(인덱스 13)를 의미합니다. 슬라이스 [-4:6:-2]는 인덱스 13에서 시작해서 6보다 큰 동안 2씩 감소하며 문자를 취합니다. 따라서 13(h) → 11(S) → 9(l) → 7(u)를 읽어 "hSlu"가 됩니다.',
  'seed:2024-3W:74'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE '다음 Python 코드 실행 결과로 옳은 것은%');

SET @q74 := (
  SELECT id FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE '다음 Python 코드 실행 결과로 옳은 것은%'
   LIMIT 1
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q74, 'A', 'hSlu', 1
WHERE @q74 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q74 AND label = 'A');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q74, 'B', 'ShmC', 0
WHERE @q74 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q74 AND label = 'B');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q74, 'C', 'penC', 0
WHERE @q74 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q74 AND label = 'C');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q74, 'D', 'Concept', 0
WHERE @q74 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q74 AND label = 'D');

INSERT INTO question_tag (question_id, tag)
SELECT @q74, 'LANG_BASIC_SCRIPT'
WHERE @q74 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id = @q74 AND tag = 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q75. 교착상태 필요조건 (OS_PROC_MEM)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
)
SELECT
  1,
  14301, -- 4.3.1 운영체제 기초 활용
  'WRITTEN', 'MCQ', 'NORMAL',
  '교착상태 발생의 필요·충분 조건에 해당하지 않는 것은?',
  'D',
  '교착상태가 발생하려면 네 가지 조건이 모두 성립해야 합니다.
  (1) 상호 배제(Mutual Exclusion)
  (2) 점유와 대기(Hold and Wait)
  (3) 비선점(No Preemption)
  (4) 환형 대기(Circular Wait)

  따라서 "선점(Preemption)" 자체는 필요 조건이 아니며, 오히려 비선점이 필요 조건입니다.',
  'seed:2024-3W:75'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE '교착상태 발생의 필요·충분 조건에 해당하지 않는 것은%');

SET @q75 := (
  SELECT id FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE '교착상태 발생의 필요·충분 조건에 해당하지 않는 것은%'
   LIMIT 1
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q75, 'A', '상호 배제(Mutual Exclusion)', 0
WHERE @q75 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q75 AND label = 'A');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q75, 'B', '점유와 대기(Hold and Wait)', 0
WHERE @q75 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q75 AND label = 'B');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q75, 'C', '환형 대기(Circular Wait)', 0
WHERE @q75 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q75 AND label = 'C');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q75, 'D', '선점(Preemption)', 1
WHERE @q75 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q75 AND label = 'D');

INSERT INTO question_tag (question_id, tag)
SELECT @q75, 'OS_PROC_MEM'
WHERE @q75 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id = @q75 AND tag = 'OS_PROC_MEM');


/* =======================================================
 * Q76. 비선점 스케줄링 (OS_PROC_MEM)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
)
SELECT
  1,
  14301, -- 4.3.1 운영체제 기초 활용
  'WRITTEN', 'MCQ', 'NORMAL',
  '다음 중 비선점(Non-preemptive) 스케줄링 알고리즘이 아닌 것은?',
  'D',
  'FCFS, SJF(비선점), HRN 등은 한 번 CPU를 할당받으면 자발적인 종료 또는 I/O 요청 전까지 선점되지 않는 비선점 스케줄링입니다. 반면 SRT(Shortest Remaining Time)는 남은 처리 시간이 더 짧은 작업이 도착하면 현재 작업을 중단하고 CPU를 넘기는 선점형 스케줄링입니다.',
  'seed:2024-3W:76'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE '다음 중 비선점(Non-preemptive) 스케줄링 알고리즘이 아닌 것은%');

SET @q76 := (
  SELECT id FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE '다음 중 비선점(Non-preemptive) 스케줄링 알고리즘이 아닌 것은%'
   LIMIT 1
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q76, 'A', 'FCFS', 0
WHERE @q76 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q76 AND label = 'A');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q76, 'B', 'SJF (비선점)', 0
WHERE @q76 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q76 AND label = 'B');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q76, 'C', 'HRN', 0
WHERE @q76 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q76 AND label = 'C');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q76, 'D', 'SRT', 1
WHERE @q76 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q76 AND label = 'D');

INSERT INTO question_tag (question_id, tag)
SELECT @q76, 'OS_PROC_MEM'
WHERE @q76 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id = @q76 AND tag = 'OS_PROC_MEM');


/* =======================================================
 * Q77. C 변수 이름 (LANG_BASIC_SCRIPT)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
)
SELECT
  1,
  14201, -- 4.2.1 기본문법 활용
  'WRITTEN', 'MCQ', 'EASY',
  'C 언어에서 변수 이름으로 사용할 수 없는 것은?',
  'B',
  'C 언어의 식별자는 영문자, 숫자, 밑줄(_)로 구성되며, 숫자로 시작할 수 없습니다. 따라서 "3num"처럼 숫자로 시작하는 이름은 사용할 수 없습니다. 밑줄로 시작하거나 중간에 숫자가 포함되는 것은 허용됩니다.',
  'seed:2024-3W:77'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE 'C 언어에서 변수 이름으로 사용할 수 없는 것은%');

SET @q77 := (
  SELECT id FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE 'C 언어에서 변수 이름으로 사용할 수 없는 것은%'
   LIMIT 1
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q77, 'A', 'value1', 0
WHERE @q77 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q77 AND label = 'A');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q77, 'B', '3num', 1
WHERE @q77 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q77 AND label = 'B');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q77, 'C', '_total', 0
WHERE @q77 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q77 AND label = 'C');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q77, 'D', 'student_2025', 0
WHERE @q77 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q77 AND label = 'D');

INSERT INTO question_tag (question_id, tag)
SELECT @q77, 'LANG_BASIC_SCRIPT'
WHERE @q77 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id = @q77 AND tag = 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q78. ARP (NET_PROTO_IP)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
)
SELECT
  1,
  14302, -- 4.3.2 네트워크 기초 활용
  'WRITTEN', 'MCQ', 'EASY',
  'TCP/IP에서 논리 주소(IP 주소)를 물리 주소(MAC 주소)로 변환해 주는 프로토콜은?',
  'B',
  'ARP(Address Resolution Protocol)는 IP 주소를 이용해 같은 네트워크 내에서 목적지의 MAC 주소를 알아내는 프로토콜입니다. 반대로 RARP는 MAC에서 IP를 얻기 위한 프로토콜로, 현재는 거의 사용되지 않습니다.',
  'seed:2024-3W:78'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE 'TCP/IP에서 논리 주소(IP 주소)를 물리 주소(MAC 주소)로 변환해 주는 프로토콜은%');

SET @q78 := (
  SELECT id FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE 'TCP/IP에서 논리 주소(IP 주소)를 물리 주소(MAC 주소)로 변환해 주는 프로토콜은%'
   LIMIT 1
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q78, 'A', 'TCP', 0
WHERE @q78 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q78 AND label = 'A');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q78, 'B', 'ARP', 1
WHERE @q78 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q78 AND label = 'B');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q78, 'C', 'IP', 0
WHERE @q78 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q78 AND label = 'C');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q78, 'D', 'FTP', 0
WHERE @q78 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q78 AND label = 'D');

INSERT INTO question_tag (question_id, tag)
SELECT @q78, 'NET_PROTO_IP'
WHERE @q78 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id = @q78 AND tag = 'NET_PROTO_IP');


/* =======================================================
 * Q79. 라이브러리 개념 (LANG_BASIC_SCRIPT)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
)
SELECT
  1,
  14203, -- 4.2.3 라이브러리 활용
  'WRITTEN', 'MCQ', 'NORMAL',
  '라이브러리의 개념과 구성에 대한 설명 중 틀린 것은?',
  'C',
  '라이브러리는 필요할 때 가져다 쓸 수 있도록 모듈화·패키지화된 코드 모음입니다. 언어/플랫폼에 따라 보통 도움말, 설치 파일, 샘플 코드 등을 함께 제공합니다. 표준 라이브러리는 언어가 기본으로 제공하는 라이브러리이고, 외부(서드파티) 라이브러리는 별도의 패키지/설치를 통해 추가하는 라이브러리입니다. 따라서 외부 라이브러리와 표준 라이브러리의 정의를 뒤집어 쓴 설명은 틀린 문장입니다.',
  'seed:2024-3W:79'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE '라이브러리의 개념과 구성에 대한 설명 중 틀린 것은%');

SET @q79 := (
  SELECT id FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE '라이브러리의 개념과 구성에 대한 설명 중 틀린 것은%'
   LIMIT 1
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q79, 'A', '라이브러리는 필요할 때 찾아서 쓸 수 있도록 모듈화되어 제공되는 프로그램/코드 모음이다.', 0
WHERE @q79 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q79 AND label = 'A');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q79, 'B', '언어·플랫폼에 따라 도움말, 설치 파일, 샘플 코드 등을 함께 제공하는 경우가 많다.', 0
WHERE @q79 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q79 AND label = 'B');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q79, 'C', '외부 라이브러리는 언어가 기본으로 포함한 표준 라이브러리를 의미하고, 표준 라이브러리는 별도 설치가 필요한 라이브러리를 의미한다.', 1
WHERE @q79 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q79 AND label = 'C');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q79, 'D', '라이브러리는 모듈과 패키지를 총칭하며, 모듈은 개별 파일, 패키지는 파일들을 모아 둔 폴더(디렉터리)에 해당한다.', 0
WHERE @q79 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q79 AND label = 'D');

INSERT INTO question_tag (question_id, tag)
SELECT @q79, 'LANG_BASIC_SCRIPT'
WHERE @q79 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id = @q79 AND tag = 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q80. C stdlib.h (LANG_BASIC_SCRIPT)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
)
SELECT
  1,
  14203, -- 4.2.3 라이브러리 활용
  'WRITTEN', 'MCQ', 'EASY',
  'C 언어 표준 라이브러리 stdlib.h에 대한 설명으로 옳은 것은?',
  'A',
  'stdlib.h에는 문자열을 수치로 바꾸는 atoi, atol, atof 같은 변환 함수와, 난수 생성(rand), 동적 메모리 관리(malloc/free) 등 프로그램의 일반적인 유틸리티 함수들이 포함됩니다. strlen은 string.h, 수학 함수들은 math.h, 표준 입출력은 stdio.h에 정의됩니다.',
  'seed:2024-3W:80'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE 'C 언어 표준 라이브러리 stdlib.h에 대한 설명으로 옳은 것은%');

SET @q80 := (
  SELECT id FROM question
   WHERE cert_id = 1 AND mode = 'WRITTEN' AND type = 'MCQ'
     AND stem LIKE 'C 언어 표준 라이브러리 stdlib.h에 대한 설명으로 옳은 것은%'
   LIMIT 1
);

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q80, 'A', '문자열을 수치로 변환하는 함수와 난수 생성, 메모리 관리 등 범용 유틸리티 함수가 포함되어 있다.', 1
WHERE @q80 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q80 AND label = 'A');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q80, 'B', '문자열 길이를 구하는 strlen() 등이 포함된 문자열 처리 전용 라이브러리이다.', 0
WHERE @q80 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q80 AND label = 'B');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q80, 'C', '표준 입출력 함수들이 정의된 라이브러리이다.', 0
WHERE @q80 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q80 AND label = 'C');

INSERT INTO question_choice (question_id, label, content, is_correct)
SELECT @q80, 'D', '삼각함수, 제곱근, 지수 함수 등이 정의된 수학 라이브러리이다.', 0
WHERE @q80 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id = @q80 AND label = 'D');

INSERT INTO question_tag (question_id, tag)
SELECT @q80, 'LANG_BASIC_SCRIPT'
WHERE @q80 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id = @q80 AND tag = 'LANG_BASIC_SCRIPT');
