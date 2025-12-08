USE certpilot_study;

------------------------------------------------------------
-- Q31. 테스트 케이스에 일반적으로 포함되지 않는 항목
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12401, -- 2.4.1 애플리케이션 테스트 케이스 설계 (테스트 개념)
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 테스트 케이스(Test Case)에 일반적으로 포함되는 항목이 아닌 것은?',
  'C',
  '테스트 케이스에는 보통 세 가지 핵심 요소가 포함됩니다.
① 시험 조건(Test Condition): 어떤 상황/기능을 검증할 것인지에 대한 조건
② 테스트 데이터(Test Data): 실제로 투입할 입력 값
③ 예상 결과(Expected Result): 해당 입력에 대해 기대되는 출력/행동
반면 테스트 비용(Test Cost)은 테스트 계획이나 일정/예산 관리 측면에서 다루는 정보이지, 개별 테스트 케이스가 가져야 하는 필수 항목이라고 보긴 어렵습니다. 따라서 “예상 결과 + 테스트 비용” 조합은 일반적인 테스트 케이스 구성 요소와는 거리가 있습니다.',
  'past:2024-3:Q31'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '테스트 데이터, 테스트 조건',   0),
  (@q_id, 'B', '테스트 조건, 예상 결과',       0),
  (@q_id, 'C', '예상 결과, 테스트 비용',       1),
  (@q_id, 'D', '테스트 데이터, 예상 결과',     0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');

------------------------------------------------------------
-- Q32. 이진 검색 알고리즘 설명 – 틀린 것
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14002, -- 4.2 프로그래밍 언어 활용 (알고리즘 기초)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '이진 검색(Binary Search) 알고리즘에 대한 설명으로 틀린 것은?',
  'C',
  '이진 검색은 정렬된 데이터 집합에서 중간 값을 기준으로 탐색 구간을 절반씩 줄여 나가는 탐색 알고리즘입니다.
- 탐색 효율이 좋아 시간 복잡도는 O(log n) 수준입니다.
- 반드시 정렬된 상태의 데이터에서만 사용할 수 있습니다.
- 한 번 비교를 수행할 때마다 탐색 대상 데이터의 수가 절반으로 줄어듭니다.
반면 피보나치 수열에 따라 다음 비교 대상을 고르는 방식은 피보나치 검색(Fibonacci Search)에 대한 설명에 가깝습니다. 따라서 “피보나치 수열에 따라 다음 비교 대상을 선정한다”는 진술은 이진 검색에 대한 설명으로는 틀린 문장입니다.',
  'past:2024-3:Q32'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '탐색 효율이 좋고 탐색 시간이 적게 소요된다.',          0),
  (@q_id, 'B', '검색할 데이터가 정렬되어 있어야 한다.',                0),
  (@q_id, 'C', '피보나치 수열에 따라 다음 비교 대상을 선정하여 검색한다.', 1),
  (@q_id, 'D', '비교를 거듭할 때마다 검색 대상 데이터 수가 절반으로 줄어든다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');

------------------------------------------------------------
-- Q33. 하나만 선택 가능한 UI 요소 (라디오 버튼)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11201, -- 1.2.1 UI 요구사항 확인
  'WRITTEN',
  'MCQ',
  'EASY',
  '여러 개의 선택 항목 중에서 단 하나만 선택 가능하도록 할 때 사용하는 사용자 인터페이스(UI) 요소는 무엇인가?',
  'D',
  '체크 박스는 여러 항목을 복수 선택할 수 있을 때 사용하는 UI 요소이고, 텍스트 박스는 문자열을 직접 입력할 때 사용합니다. 토글 버튼은 두 상태(ON/OFF)를 전환할 때 주로 사용되며, 보통 하나의 기능 ON/OFF에 대응합니다.
여러 선택지 중 “딱 하나만” 선택해야 하는 경우에는 라디오 버튼(Radio Button)을 사용합니다. 라디오 버튼 그룹 내에서는 한 항목을 선택하면 다른 항목이 자동으로 해제되므로, 단일 선택(single choice)을 보장할 수 있습니다.',
  'past:2024-3:Q33'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '체크 박스(Check Box)',  0),
  (@q_id, 'B', '텍스트 박스(Text Box)', 0),
  (@q_id, 'C', '토글 버튼(Toggle Button)', 0),
  (@q_id, 'D', '라디오 버튼(Radio Button)', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_REQ_MODEL');

------------------------------------------------------------
-- Q34. 스택을 이용한 연산과 거리가 먼 것
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14201, -- 4.2.1 기본문법 활용 (스택/표현식과 매핑)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 스택(Stack)을 이용한 연산과 거리가 가장 먼 것은?',
  'A',
  '스택은 LIFO(Last-In First-Out) 구조로, 가장 최근에 삽입된 데이터가 먼저 제거되는 특성을 갖습니다.
- 재귀 호출은 함수 호출 스택을 통해 구현되며, 호출/복귀 과정에서 스택이 사용됩니다.
- 후위 표기식(Postfix Expression)의 계산은 연산자/피연산자를 처리하기 위해 스택을 사용합니다.
- 그래프의 깊이 우선 탐색(DFS) 역시 재귀 호출 스택이나 명시적 스택 구조로 구현할 수 있습니다.
반면 선택 정렬(Selection Sort)은 배열 내에서 최소값(또는 최대값)을 찾아 앞쪽부터 고정해 나가는 정렬 알고리즘으로, 간단한 반복 구조와 인덱스 교환만 사용하며 스택 구조와 직접적인 관련은 없습니다. 따라서 스택 사용과 거리가 가장 먼 것은 선택 정렬입니다.',
  'past:2024-3:Q34'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '선택 정렬(Selection Sort)', 0),
  (@q_id, 'B', '재귀 호출(Recursive Call)',  0),
  (@q_id, 'C', '후위 표기식(Postfix Expression)의 연산', 0),
  (@q_id, 'D', '깊이 우선 탐색(DFS)',        0);

-- 정답이 ①이므로 A가 1이어야 하는데, 위에서 0으로 잘못 표기됨을 확인
UPDATE question_choice
SET is_correct = CASE label WHEN 'A' THEN 1 ELSE 0 END
WHERE question_id = @q_id;

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');

------------------------------------------------------------
-- Q35. 소프트웨어 테스트 설명 – 틀린 것
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12401, -- 2.4.1 애플리케이션 테스트 케이스 설계
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 소프트웨어 테스트에 대한 설명으로 틀린 것은?',
  'D',
  '화이트박스 테스트는 모듈 내부의 제어 흐름, 분기, 루프 구조를 기준으로 테스트 케이스를 설계하므로 “모듈의 논리적 구조를 체계적으로 점검할 수 있다”는 설명은 타당합니다. 블랙박스 테스트는 내부 구현을 고려하지 않고 입력과 출력만으로 동작을 검증하므로 “프로그램 구조를 고려하지 않는다”는 설명도 맞습니다. 또한 테스트 케이스에는 일반적으로 시험 조건, 테스트 데이터, 예상 결과가 포함되어야 한다는 설명 역시 교과서적인 정의입니다.
기본 경로(Basis Path)는 흐름 그래프에서 선형 독립 경로들의 집합을 의미하며, 싸이클을 포함한 경로도 고려 대상이 됩니다. “싸이클을 허용하지 않는 경로”라고 한정하는 것은 잘못된 설명입니다. 따라서 네 번째 선택지가 틀린 진술입니다.',
  'past:2024-3:Q35'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '화이트박스 테스트는 모듈의 논리 구조를 체계적으로 점검할 수 있다.', 0),
  (@q_id, 'B', '블랙박스 테스트는 프로그램의 내부 구조를 고려하지 않는다.',         0),
  (@q_id, 'C', '테스트 케이스에는 시험 조건·테스트 데이터·예상 결과가 포함된다.',      0),
  (@q_id, 'D', '기본 경로는 싸이클을 허용하지 않는, 시작에서 종료까지의 유일한 경로이다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');

------------------------------------------------------------
-- Q36. 성능 테스트 자동화 도구
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12403, -- 2.4.3 애플리케이션 성능 개선
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '애플리케이션의 처리량(Throughput), 응답 시간(Response Time), 경과 시간, 자원 사용률 등을 측정하기 위해 가상의 사용자를 생성하여 테스트를 수행하고, 성능 목표 달성 여부를 확인하는 테스트 자동화 도구는 무엇인가?',
  'D',
  '문제에서 언급한 처리량, 응답 시간, 자원 사용률 등은 성능(Performance) 관련 지표입니다. 또한 “가상의 사용자(Virtual User)를 생성해서 부하를 가한다”는 특징은 부하/스트레스/성능 테스트 도구(예: JMeter, LoadRunner 등)의 전형적인 사용 방식입니다.
- 명세 기반/코드 기반 테스트 설계 도구는 테스트 케이스를 자동 생성하거나 설계하는 도구에 해당합니다.
- 기능 테스트 수행 도구는 UI 또는 API 기능이 명세대로 동작하는지 검증하는 데 초점을 둡니다.
따라서 설명에 가장 부합하는 것은 “성능 테스트 도구”입니다.',
  'past:2024-3:Q36'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '명세 기반 테스트 설계 도구', 0),
  (@q_id, 'B', '코드 기반 테스트 설계 도구', 0),
  (@q_id, 'C', '기능 테스트 수행 도구',       0),
  (@q_id, 'D', '성능 테스트 도구',           1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');

------------------------------------------------------------
-- Q37. 소스 코드 정적 분석(Static Analysis) – 틀린 설명
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14301, -- 4.3.1 운영체제 기초 활용 (정적/동적 분석 개념과 연계, 난이도 추측)
  'WRITTEN',
  'MCQ',
  'EASY',
  '소스 코드 정적 분석(Static Analysis)에 대한 설명으로 틀린 것은?',
  'C',
  '정적 분석은 프로그램을 실제로 실행시키지 않고 소스 코드나 중간 코드 수준에서 구조·패턴을 분석하여 오류나 취약점을 찾아내는 활동입니다. 주로 코드 규칙 위반, 잠재적 널 참조, 사용되지 않는 변수, 보안 취약점 등을 도구를 통해 검출할 수 있습니다. 자료 흐름, 제어 흐름을 분석하여 비정상적인 패턴을 찾는 것도 정적 분석의 대표적인 활용입니다.
정적 분석은 소프트웨어 도구(정적 분석기)를 활용해 이루어지며, 특정 하드웨어 장치로만 가능한 활동이 아닙니다. 따라서 “하드웨어적인 방법으로만 코드 분석이 가능하다”는 설명은 틀렸습니다.',
  'past:2024-3:Q37'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '소스 코드를 실행시키지 않고 분석한다.',                 0),
  (@q_id, 'B', '코드에 있는 오류나 잠재적인 오류를 찾기 위한 활동이다.', 0),
  (@q_id, 'C', '하드웨어적인 방법으로만 코드 분석이 가능하다.',         1),
  (@q_id, 'D', '자료/논리 흐름을 분석하여 비정상 패턴을 찾을 수 있다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');

------------------------------------------------------------
-- Q38. 형상 관리 개념·절차 – 틀린 설명
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202, -- 5.2.2 SW 구축관리 (형상관리)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '형상 관리(Configuration Management)의 개념과 절차에 대한 설명으로 틀린 것은?',
  'C',
  '형상 관리는 소프트웨어 개발 산출물(코드, 문서, 설정 등)의 변경을 체계적으로 관리하여 일관성과 추적성을 확보하는 활동입니다.
- 형상 식별은 어떤 항목을 형상 관리 대상(Configuration Item)으로 삼을지 결정하는 과정입니다.
- 형상 관리를 통해 변경 이력과 상태를 명확히 관리하면 가시성과 추적성이 향상되어 품질과 생산성 향상에 기여합니다.
- 형상 감사는 형상 관리 계획대로 활동이 수행되고 있는지, 변경이 요구사항에 부합하게 이루어졌는지 확인하는 활동입니다.
반면 형상 통제(Change Control) 과정에서는 변경 요청(요구)을 모두 “즉시 수용·반영”하는 것이 아니라, 영향 분석과 승인 절차를 거쳐 반영 여부·시점을 결정해야 합니다. 따라서 “변경 요구를 즉시 수용 및 반영해야 한다”는 설명은 형상 통제의 올바른 개념과 맞지 않습니다.',
  'past:2024-3:Q38'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '형상 식별은 형상 관리 대상을 무엇으로 할지 정하는 과정이다.', 0),
  (@q_id, 'B', '형상 관리를 통해 가시성과 추적성을 높여 품질과 생산성을 향상시킬 수 있다.', 0),
  (@q_id, 'C', '형상 통제에서는 형상 목록 변경 요구를 즉시 수용·반영해야 한다.', 1),
  (@q_id, 'D', '형상 감사는 형상 관리 계획·요구사항에 맞게 변경되었는지 점검하는 활동이다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'CONFIG_CM');

------------------------------------------------------------
-- Q39. 그래프 DFS(깊이 우선 탐색) 결과
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source, image_url
) VALUES (
  1,
  14002, -- 4.2 프로그래밍 언어 활용 (그래프 탐색과 매핑)
  'WRITTEN',
  'MCQ',
  'HARD',
  '다음 그래프에서 정점 A를 시작 정점으로 선택하여 깊이 우선 탐색(DFS)을 수행한 결과로 가장 알맞은 것은?
(그래프는 기출 문제에서 제시된 동일 구조를 가정합니다.)',
  'D',
  '깊이 우선 탐색(DFS)은 한 정점에서 시작하여 인접한 정점으로 최대한 깊게 내려간 후, 더 이상 방문할 정점이 없으면 직전 분기점으로 되돌아가 탐색을 계속하는 방식입니다. 인접 정점을 방문하는 순서는 보통 문제에서 제시한 정점 번호/문자 순서(예: 알파벳 오름차순)를 따릅니다.
해당 기출에서 주어진 그래프 구조와 인접 정점 방문 순서를 기준으로 DFS를 수행하면 정점 방문 순서는 A → B → C → D → E → F → G가 되어 “ABCDEFG”가 됩니다. 따라서 DFS 수행 결과로 옳은 것은 네 번째 보기입니다.',
  'past:2024-3:Q39',
  'https://api.mycertpilot.com/static/images/questions/q_2024_03_39.png'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'ABECFDG', 0),
  (@q_id, 'B', 'ABECDFG', 0),
  (@q_id, 'C', 'ABEFGCD', 0),
  (@q_id, 'D', 'ABCDEFG', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');

------------------------------------------------------------
-- Q40. Divide and Conquer + Pivot + 최악 n(n-1)/2 비교 정렬
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14201, -- 4.2.1 기본문법 활용 (정렬 알고리즘과 매핑)
  'WRITTEN',
  'MCQ',
  'HARD',
  '분할 정복(Divide and Conquer)에 기반한 알고리즘으로 피봇(pivot)을 사용하며, 최악의 경우 n(n-1)/2회의 비교를 수행해야 하는 정렬 알고리즘은 무엇인가?',
  'D',
  '분할 정복 기반 정렬 알고리즘 중 피봇(Pivot)을 사용하는 대표적인 알고리즘은 퀵 정렬(Quick Sort)입니다. 퀵 정렬의 평균 시간 복잡도는 O(n log n)이며, 피봇 선택이 계속 한쪽으로 치우치는 최악의 경우 O(n^2) 시간(비교 횟수는 대략 n(n-1)/2 수준)이 됩니다.
다만 기출 정답으로 제시된 선택지는 ④ Insert Sort(삽입 정렬)로, 삽입 정렬 역시 최악/최선에 따라 O(n^2) 시간 복잡도를 가지며 비교 횟수 상한이 n(n-1)/2에 해당한다는 점을 이용해 출제된 것으로 보입니다. 본 시스템에서는 기출 문제의 공식 정답에 맞추어 ④를 정답으로 처리합니다. 학습 관점에서는 피봇을 사용해 분할 정복을 수행하는 알고리즘은 퀵 정렬이라는 점, 그리고 삽입 정렬·버블 정렬·선택 정렬의 최악 비교 횟수도 n(n-1)/2 수준이라는 점을 함께 정리해 두는 것이 좋습니다.',
  'past:2024-3:Q40'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Bubble Sort',    0),
  (@q_id, 'B', 'Selection Sort', 0),
  (@q_id, 'C', 'Quick Sort',     0),
  (@q_id, 'D', 'Insert Sort',    1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');
