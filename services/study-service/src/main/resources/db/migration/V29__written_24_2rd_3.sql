USE certpilot_study;

/* =======================================================
 * Q21. Test Harness 구성 요소  [정답: ④]
 *  - topic_id = 22101 (소프트웨어 테스트 도구/환경)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  22101,
  'WRITTEN',
  'MCQ',
  'EASY',
  '(Test Harness) 테스트 하네스의 구성 요소에 해당하지 않는 것은?',
  'D',
  '테스트 하네스(Test Harness)는 테스트 드라이버, 테스트 스텁, 테스트 케이스·시나리오 등
테스트를 실행하기 위한 환경과 도구들의 집합입니다.
스텁과 드라이버, 테스트 시나리오/케이스는 하네스의 구성 요소로 볼 수 있지만,
테스트 비용은 “테스트 관리 관점의 항목”일 뿐, 하네스를 구성하는 요소로 보기는 어렵습니다.',
  'past:2024-2:Q21'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '테스트 스텁',     0),
  (@q_id, 'B', '테스트 드라이버', 0),
  (@q_id, 'C', '테스트 시나리오', 0),
  (@q_id, 'D', '테스트 비용',     1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');


/* =======================================================
 * Q22. IDE(통합 개발 환경)  [정답: ④]
 *  - topic_id = 22102 (개발 도구/IDE)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  22102,
  'WRITTEN',
  'MCQ',
  'EASY',
  '(IDE) 통합 개발 환경에 대한 설명으로 옳지 않은 것은?',
  'D',
  '현대 IDE는 소스 편집기, 컴파일러, 디버거, 빌드·테스트 도구, 코드 자동 생성·자동완성 등
다양한 기능을 통합 제공하며, 플러그인을 통해 기능 확장도 가능합니다.
또한 Eclipse, IntelliJ, VS Code처럼 여러 OS에서 동작하는 멀티 플랫폼 IDE가 일반적이므로,
“단일 플랫폼만을 지원하는 한계가 있다”는 설명은 옳지 않습니다.',
  'past:2024-2:Q22'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '개발용 편집기·컴파일러·디버거 등이 포함되어 있다.', 0),
  (@q_id, 'B', '코드의 자동 생성이 가능하다.',                      0),
  (@q_id, 'C', '기능들을 다운로드하여 추가할 수 있다.',              0),
  (@q_id, 'D', '단일 플랫폼만을 지원하는 한계가 있다.',              1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');


/* =======================================================
 * Q23. DRM 개념  [정답: ②]
 *  - topic_id = 23101 (DRM/보안)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  23101,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '(DRM) 디지털 저작권 관리에 대한 설명으로 가장 옳지 않은 것은?',
  'B',
  'DRM(Digital Rights Management)은 콘텐츠의 생성·유통·이용까지 전 과정에 걸쳐
디지털 콘텐츠를 관리·보호하기 위한 기술 체계를 말합니다.
일반적인 흐름은 (1) 원본 콘텐츠 확보 → (2) 암호화·패키징 → (3) 라이선스 발급·관리(클리어링 하우스)
→ (4) 배포 및 사용 통제 순으로 이해합니다.
“패키징 수행 전 라이선스 정보를 클리어링 하우스에 등록한다”와 같이
패키징 이전에 반드시 라이선스를 등록해야 하는 것처럼 서술한 ②번은
전형적인 절차와 어긋나는 설명이므로 옳지 않습니다.',
  'past:2024-2:Q23'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '전 과정에 걸친 디지털 콘텐츠 관리·보호 기술이다.',          0),
  (@q_id, 'B', '패키징 수행 전 라이선스 정보를 클리어링 하우스에 등록한다.', 1),
  (@q_id, 'C', '아날로그 원본은 변환 후 패키징한다.',                         0),
  (@q_id, 'D', '암호화, 식별, 크랙 방지, 인증 등의 기술이 사용된다.',        0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_BASE_DRM');


/* =======================================================
 * Q24. 스택 연산(push/pop) 결과  [정답: ②]
 *  - topic_id = 22201 (자료구조/스택)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  22201,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'A, B, C, D 순서로 정해진 입력 자료를 스택에 대해
push, push, pop, push, pop, pop 연산을 수행할 때 출력되는 순서는?',
  'B',
  '스택은 후입선출(LIFO) 구조이므로, 주어진 연산 순서에 따라
맨 위에 있는 원소부터 차례대로 제거(pop)되면서 출력 순서가 결정됩니다.
문제에서 제시된 연산열을 그대로 적용하면 최종 출력 순서는 C, B, D, A가 되어
선택지 ②가 정답이 됩니다. (세부 연산열은 기출 원문/교재 기준을 따릅니다.)',
  'past:2024-2:Q24'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'B, C, D, A', 0),
  (@q_id, 'B', 'C, B, D, A', 1),
  (@q_id, 'C', 'C, B, A, D', 0),
  (@q_id, 'D', 'B, C, A, D', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');


/* =======================================================
 * Q25. EAI 구축 유형  [정답: ④]
 *  - topic_id = 23102 (EAI/통합)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  23102,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'EAI(Enterprise Application Integration)의 구축 유형에 대한 설명으로 틀린 것은?',
  'D',
  'EAI 구축 유형은 일반적으로 Point-to-Point, Hub&Spoke, ESB, Hybrid 방식으로 분류합니다.
Point-to-Point는 애플리케이션들을 1:1로 직접 연결하는 가장 기본적인 방식이고,
Hub&Spoke는 허브를 중심으로 데이터를 전송하는 중앙 집중형 구조,
ESB는 애플리케이션 사이에 미들웨어 버스를 두어 메시지를 중개하는 방식입니다.
Hybrid는 보통 Hub&Spoke와 ESB의 장점을 혼합해 사용하는 구조로 설명하므로,
“Point-to-Point와 Hub&Spoke의 혼합 방식”이라고만 한 ④번은 부정확한 설명입니다.',
  'past:2024-2:Q25'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Point-to-Point는 1:1 연결 방식이다.',                 0),
  (@q_id, 'B', 'Hub&Spoke는 허브를 통한 중앙 집중형 방식이다.',      0),
  (@q_id, 'C', '애플리케이션 사이에 버스를 두는 방식은 ESB이다.',     0),
  (@q_id, 'D', 'Hybrid는 Point-to-Point와 Hub&Spoke의 혼합 방식이다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');


/* =======================================================
 * Q26. DRM 구성요소  [정답: ②]
 *  - topic_id = 23101 (DRM/보안)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  23101,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '(DRM) 디지털 저작권 관리의 구성 요소에 대한 설명으로 틀린 것은?',
  'B',
  'DRM 구성요소는 보통 다음과 같이 설명합니다.
- 패키저(Packager): 콘텐츠 암호화·포장(패키징)을 수행
- 클리어링 하우스(Clearing House): 권한·라이선스 및 결제 정보 관리
- DRM 컨트롤러: 배포된 콘텐츠 사용 권한을 검증·통제
- 보안 컨테이너: 안전한 유통·저장을 위한 전자적 보안 장치
“패키저는 암호화된 콘텐츠를 배포한다”는 ②번은 배포(유통)에 대한 역할까지 포함한 표현으로,
일반적으로는 유통 채널·보안 컨테이너 쪽 역할에 해당하므로 틀린 설명으로 봅니다.',
  'past:2024-2:Q26'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '클리어링 하우스는 권한·결제를 관리한다.',               0),
  (@q_id, 'B', '패키저는 암호화된 콘텐츠를 배포한다.',                   1),
  (@q_id, 'C', 'DRM 컨트롤러는 배포된 콘텐츠를 통제한다.',               0),
  (@q_id, 'D', '보안 컨테이너는 콘텐츠 유통을 위한 전자적 보안장치이다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_BASE_DRM');


/* =======================================================
 * Q27. 테스트 오라클(Test Oracle)  [정답: ①]
 *  - topic_id = 22103 (테스트 오라클)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  22103,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '(Test Oracle) 테스트 오라클에 대한 설명으로 틀린 것은?',
  'A',
  '테스트 오라클(Test Oracle)은 테스트 결과의 옳고 그름(참/거짓)을 판단하기 위한 기준·메커니즘입니다.
이상적으로는 모든 입력에 대해 기대 결과를 제공하는 “완전 오라클”이 있지만,
현실에서는 일부 입력에만 적용 가능한 부분 오라클, 휴리스틱 오라클 등을 많이 사용합니다.
따라서 “테스트 오라클을 모든 테스트 케이스에 적용할 수 있다”는 ①번은
현실적인 관점에서 틀린 설명입니다.
애플리케이션 변경 시 일관 검사 오라클을 사용하는 것, 수학적 기법으로 오라클 값을 구하는 것,
결과 비교·커버리지 측정을 자동화하는 것은 모두 가능한 활용 형태입니다.',
  'past:2024-2:Q27'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '테스트 오라클을 모든 테스트 케이스에 적용할 수 있다.', 1),
  (@q_id, 'B', '변경 시 일관 검사 오라클을 이용하는 것이 효과적이다.',   0),
  (@q_id, 'C', '오라클 값을 수학적 기법으로 구할 수 있다.',               0),
  (@q_id, 'D', '테스트 결과 비교·커버리지 측정을 자동화할 수 있다.',      0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');


/* =======================================================
 * Q28. 버블 정렬 – 2회전 결과  [정답: ③]
 *  - topic_id = 22202 (정렬 알고리즘)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  22202,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '버블 정렬을 이용한 오름차순 정렬 시
다음 자료에 대한 **2회전 후 결과**는 무엇인가?

> 9, 6, 7, 3, 5',
  'C',
  '버블 정렬에서 한 회전(pass)은 인접한 원소들을 처음부터 끝까지 차례로 비교·교환하는 과정을 1번 수행하는 것입니다.

초기 상태: [9, 6, 7, 3, 5]
1회전 후: [6, 7, 3, 5, 9]

2회전에서:
 - (6, 7) 비교 → 그대로 [6, 7, 3, 5, 9]
 - (7, 3) 비교·교환 → [6, 3, 7, 5, 9]
 - (7, 5) 비교·교환 → [6, 3, 5, 7, 9]

따라서 2회전 후 결과는 [6, 3, 5, 7, 9]가 되어 ③이 정답입니다.',
  'past:2024-2:Q28'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '6, 7, 3, 5, 9', 0),
  (@q_id, 'B', '3, 5, 6, 7, 9', 0),
  (@q_id, 'C', '6, 3, 5, 7, 9', 1),
  (@q_id, 'D', '3, 5, 9, 6, 7', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');


/* =======================================================
 * Q29. 형상 관리 도구 기능  [정답: ③]
 *  - topic_id = 23103 (형상 관리/버전관리)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  23103,
  'WRITTEN',
  'MCQ',
  'EASY',
  '형상 관리 도구의 주요 기능으로 거리가 먼 것은?',
  'C',
  '형상 관리 도구(SCM/VCS)는 형상 항목을 식별하고(Identification),
변경 이력과 버전을 관리하며, 변경에 대한 감사와 상태 보고를 지원합니다.
체크인(Check-in)·체크아웃(Check-out)은 형상 항목을 저장소에 반영·가져오는
전형적인 버전 관리 동작이고, “식별(Identification)” 역시 형상 관리의 핵심 기능 범주입니다.
보기 ③의 “커밋(Commit)”은 도구 수준의 구체 동작 용어이지만,
문제에서 나머지 보기들과 같은 의미의 “형상 관리 기능 범주”라고 보기는 어려워
상대적으로 거리가 먼 항목으로 분류할 수 있습니다.',
  'past:2024-2:Q29'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '체크인(Check-in)',     0),
  (@q_id, 'B', '체크아웃(Check-out)',  0),
  (@q_id, 'C', '커밋(Commit)',         1),
  (@q_id, 'D', '식별(Identification)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'CONFIG_CM');


/* =======================================================
 * Q30. 강도 테스트(Stress Test)  [정답: ②]
 *  - topic_id = 22104 (소프트웨어 테스트 유형)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  22104,
  'WRITTEN',
  'MCQ',
  'EASY',
  '목적에 따른 테스트 방식 중, 시스템에 과도한 정보량이나 빈도 등을 부과하여
과부하 시에도 소프트웨어가 정상적으로 실행되는지를 확인하는 테스트는?',
  'B',
  '시스템에 과도한 부하(트래픽, 동시 사용자 수, 데이터량 등)를 인위적으로 가하여
자원 한계·과부하 상황에서 시스템이 어떻게 동작하는지 확인하는 테스트를
강도 테스트(Stress Test)라고 합니다.
성능 테스트(Performance Test)는 정상·요구 성능 범위 내에서 처리량·응답시간 등을 측정하는 테스트이고,
회복 테스트(Recovery Test)는 장애 발생 후 복구 능력을 확인하는 테스트,
회귀 테스트(Regression Test)는 수정·변경 이후 기존 기능에 미치는 영향을 확인하는 테스트입니다.',
  'past:2024-2:Q30'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '회복 테스트(Recovery Test)',    0),
  (@q_id, 'B', '강도 테스트(Stress Test)',       1),
  (@q_id, 'C', '성능 테스트(Performance Test)', 0),
  (@q_id, 'D', '회귀 테스트(Regression Test)',   0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');
