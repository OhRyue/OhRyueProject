-- =========================================
-- 2024년 1회 – 소프트웨어 개발 (2과목)
-- Q21 ~ Q30
-- =========================================

------------------------------------------------------------
-- Q21. EAI 구축 유형
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12202,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'EAI(Enterprise Application Integration)의 구축 유형으로 옳지 않은 것은?',
  'C',
  'EAI 구축 유형으로는 Point-to-Point, Hub & Spoke, Message Bus, Hybrid 구조 등이 대표적입니다. Tree 구조는 공식적인 EAI 구축 유형으로 분류되지 않으므로 정답은 Tree입니다.',
  'past:2024-1:Q21'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Point-to-Point', 0),
  (@q_id, 'B', 'Hub & Spoke',    0),
  (@q_id, 'C', 'Tree',           1),
  (@q_id, 'D', 'Message Bus',    0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');

------------------------------------------------------------
-- Q22. 알파/베타 테스트
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12401,
  'WRITTEN',
  'MCQ',
  'EASY',
  '검증 검사 기법 중 개발자의 장소에서, 사용자가 개발자 앞에서 통제된 환경에서 함께 수행하는 검사는 무엇인가요?',
  'D',
  '알파 테스트는 개발자의 통제 하에, 개발자 측 환경에서 사용자가 함께 수행하는 시험입니다. 베타 테스트는 실제 사용자 환경에서 사용자가 독립적으로 수행하는 시험이므로, 지문에 해당하는 것은 알파 테스트입니다.',
  'past:2024-1:Q22'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '형상 검사(Configuration Audit)',             0),
  (@q_id, 'B', '동치 분할 검사(Equivalence Partitioning)',    0),
  (@q_id, 'C', '베타 테스트(Beta Test)',                      0),
  (@q_id, 'D', '알파 테스트(Alpha Test)',                     1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');

------------------------------------------------------------
-- Q23. 트리의 차수 정의
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source, image_url
) VALUES (
  1,
  14002,
  'WRITTEN',
  'MCQ',
  'EASY',
  '트리에서 차수(degree)에 대한 설명으로 옳은 것은?',
  'B',
  '트리의 차수는 “하나의 노드가 가질 수 있는 자식 노드의 최대 개수”를 의미합니다. 루트에서 단말까지의 최대 간선 수는 높이(height), 레벨 수는 깊이(depth), 단말 노드 수는 leaf 개수입니다.',
  'past:2024-1:Q23',
  'https://api.mycertpilot.com/static/images/questions/q_2024_01_23.png'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '루트에서 단말 노드까지의 최대 간선 수를 말한다.',           0),
  (@q_id, 'B', '하나의 노드가 가질 수 있는 자식 노드의 최대 개수를 말한다.', 1),
  (@q_id, 'C', '트리가 가질 수 있는 최대 레벨 수를 말한다.',                  0),
  (@q_id, 'D', '단말(leaf) 노드의 개수를 말한다.',                           0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');

------------------------------------------------------------
-- Q24. AJAX
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12502,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 설명에 해당하는 인터페이스 구현 기술은 무엇인가요?

> JavaScript를 사용한 비동기 통신 기술  
> 클라이언트와 서버 간에 XML/JSON 등의 데이터를 주고받으면서  
> 화면 전체를 새로 고치지 않고 일부만 갱신할 수 있다.',
  'C',
  'AJAX는 JavaScript를 이용해 비동기 방식으로 서버와 데이터를 주고받는 기술입니다. 전체 페이지 새로고침 없이 필요한 영역만 갱신할 수 있어, 현대 웹 UI 구현에서 널리 사용됩니다.',
  'past:2024-1:Q24'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Procedure', 0),
  (@q_id, 'B', 'Trigger',   0),
  (@q_id, 'C', 'AJAX',      1),
  (@q_id, 'D', 'Greedy',    0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');


------------------------------------------------------------
-- Q25. 해싱 – 폴딩법
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14002,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '해싱 함수 설계 방법 중, 레코드 키를 여러 부분으로 나눈 뒤 각 부분을 더하거나 XOR 연산을 수행하여 홈 주소를 구하는 방식은 무엇인가요?',
  'B',
  '폴딩법은 키 값을 몇 부분으로 나눈 뒤, 이들을 더하거나 XOR 연산을 해서 해시 주소를 구하는 방식입니다. 제산법은 어떤 수로 나머지를 구하는 방식이고, 숫자 분석법·기수 변환법은 자릿수 특성을 이용한다는 점에서 구분됩니다.',
  'past:2024-1:Q25'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '제산법(Division Method)',                 0),
  (@q_id, 'B', '폴딩법(Folding Method)',                  1),
  (@q_id, 'C', '숫자 분석법(Digit Analysis)',             0),
  (@q_id, 'D', '기수 변환법(Radix Transformation)',       0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');

------------------------------------------------------------
-- Q26. 선택 정렬 3회전 결과
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14002,
  'WRITTEN',
  'MCQ',
  'HARD',
  '다음 자료를 선택 정렬(Selection Sort)로 오름차순 정렬할 때, 3회전 후의 결과로 옳은 것은?

> 초기 자료: 37, 14, 17, 40, 35',
  'D',
  '선택 정렬은 매 회전마다 남은 구간에서 최솟값을 선택해 앞쪽과 교환합니다.
1회전: [37,14,17,40,35] → 최솟값 14 → [14,37,17,40,35]
2회전: [14,37,17,40,35] → 남은 구간 최솟값 17 → [14,17,37,40,35]
3회전: [14,17,37,40,35] → 남은 구간 최솟값 35 → [14,17,35,40,37]
따라서 3회전 후 결과는 14, 17, 35, 40, 37 입니다.',
  'past:2024-1:Q26'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '14, 37, 17, 40, 35', 0),
  (@q_id, 'B', '14, 17, 37, 40, 35', 0),
  (@q_id, 'C', '14, 17, 40, 37, 35', 0),
  (@q_id, 'D', '14, 17, 35, 40, 37', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');


------------------------------------------------------------
-- Q27. 정적 분석 도구
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12401,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 소스 코드 품질 분석 도구 중 정적 분석 도구가 아닌 것은?',
  'D',
  'Checkstyle, PMD, Cppcheck는 코드를 실행하지 않고 소스 구조를 분석하는 정적 분석 도구입니다. Valgrind는 메모리 누수·오류를 실행 중에 검사하는 동적 분석 도구이므로 정적 분석 도구가 아닙니다.',
  'past:2024-1:Q27'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Checkstyle', 0),
  (@q_id, 'B', 'PMD',        0),
  (@q_id, 'C', 'Cppcheck',   0),
  (@q_id, 'D', 'Valgrind',   1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');

------------------------------------------------------------
-- Q28. 중위(Inorder) 순회
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source, image_url
) VALUES (
  1,
  14002,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 이진 트리를 중위(Inorder) 순회(왼쪽 서브트리 → 루트 → 오른쪽 서브트리) 했을 때의 방문 순서로 옳은 것은?,
  'B',
  '중위 순회는 “왼쪽 서브트리 → 루트 → 오른쪽 서브트리” 순서로 방문합니다.
  왼쪽 서브트리(B): D, B, E
  루트: A
  오른쪽 서브트리(C): F, C, G
  따라서 전체 순서는 D, B, E, A, F, C, G 입니다.',
  'past:2024-1:Q28',
  'https://api.mycertpilot.com/static/images/questions/q_2024_01_28.png'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'A, B, D, E, C, F, G', 0),
  (@q_id, 'B', 'D, B, E, A, F, C, G', 1),
  (@q_id, 'C', 'B, D, E, C, F, A, G', 0),
  (@q_id, 'D', 'D, E, B, F, G, C, A', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');

------------------------------------------------------------
-- Q29. 소프트웨어 테스트 설명
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12401,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '소프트웨어 테스트에 대한 설명으로 틀린 것은?',
  'D',
  '기본 경로 테스트에서의 Basis Path는 “사이클을 포함할 수 있는 독립 경로 집합”을 의미하며, 사이클을 전혀 허용하지 않는다는 설명은 잘못입니다. 나머지 선택지는 각 테스트 기법과 테스트 케이스 구성에 대한 올바른 설명입니다.',
  'past:2024-1:Q29'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '화이트박스 테스트는 모듈의 내부 논리 구조를 기준으로 경로를 점검한다.',               0),
  (@q_id, 'B', '블랙박스 테스트는 프로그램의 내부 구조를 고려하지 않고 입·출력만으로 검증한다.',        0),
  (@q_id, 'C', '테스트 케이스에는 시험 조건, 테스트 데이터, 예상 결과가 포함되는 것이 일반적이다.',      0),
  (@q_id, 'D', '기본 경로(Basis Path)는 흐름 그래프에서 사이클을 허용하지 않는 경로만을 의미한다.',      1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');

------------------------------------------------------------
-- Q30. 소프트웨어 형상 관리
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '소프트웨어 형상 관리(Configuration Management)에 대한 설명으로 거리가 먼 것은?',
  'C',
  '형상 관리 도구는 일반적으로 Git, Subversion, CVS와 같은 버전 관리 시스템을 의미합니다. Ant, Maven, Gradle은 빌드·의존성 관리 도구이므로 형상 관리 도구라고 보기 어렵습니다. 따라서 C가 형상 관리 설명과 거리가 먼 선택지입니다.',
  'past:2024-1:Q30'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '소프트웨어에 가해지는 변경을 체계적으로 통제하고 관리하는 활동이다.',                  0),
  (@q_id, 'B', '분석서, 설계서, 소스 코드, 테스트 케이스 등 산출물 전반이 관리 대상이 될 수 있다.',       0),
  (@q_id, 'C', '대표적인 형상 관리 도구로 Ant, Maven, Gradle이 있다.',                                   1),
  (@q_id, 'D', '유지보수 단계뿐만 아니라 개발 전 과정에 적용할 수 있다.',                              0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'CONFIG_CM');
