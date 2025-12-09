/* =========================================
 * 2024년 1회 – 소프트웨어 개발 (2과목)
 * Q31 ~ Q40
 * ========================================= */


/* =======================================================
 * 2024-1 Q31. 최악의 경우 검색 효율이 가장 나쁜 트리 구조
 * topic_id = 14002
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14002,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 최악의 경우 검색 효율이 가장 나쁜 트리 구조는?',
  'A',
  '이진 탐색 트리는 삽입 순서에 따라 한쪽으로 줄기가 쏠리면 사실상 선형 리스트와 비슷한 모양으로 변형될 수 있습니다. 이 경우 최악 시간 복잡도는 O(n)까지 나빠집니다. 반면 AVL 트리, 레드-블랙 트리, 2-3 트리와 같은 균형 트리는 삽입/삭제 시 높이를 일정 범위로 유지하도록 재구성되기 때문에, 검색의 최악 시간 복잡도가 O(log n)으로 보장됩니다. 따라서 최악의 경우 검색 효율이 가장 나쁜 구조는 단순 이진 탐색 트리입니다.',
  'past:2024-1:Q31'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '이진 탐색 트리(Binary Search Tree)', 1),
  (@q_id, 'B', 'AVL 트리',                             0),
  (@q_id, 'C', '레드-블랙 트리(Red-Black Tree)',      0),
  (@q_id, 'D', '2-3 트리',                            0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');


/* =======================================================
 * 2024-1 Q32. 선형 구조로만 묶인 것은?
 * topic_id = 14002
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14002,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 선형(linear) 구조로만 묶인 것은?',
  'B',
  '선형 구조는 데이터 원소들이 1:1의 순차적인 관계를 가지는 구조입니다. 대표적인 예로 배열, 리스트, 스택, 큐 등이 있습니다. 트리와 그래프는 하나의 노드가 여러 노드와 연결될 수 있는 비선형 구조이므로, 트리/그래프가 포함된 선택지는 선형 구조 집합이라고 보기 어렵습니다. 리스트·큐·스택으로만 이루어진 ②번이 선형 구조로만 묶인 보기입니다.',
  'past:2024-1:Q32'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '큐, 데크, 스택, 트리', 0),
  (@q_id, 'B', '리스트, 큐, 스택',      1),
  (@q_id, 'C', '리스트, 그래프, 큐',    0),
  (@q_id, 'D', '그래프, 트리, 큐',      0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');


/* =======================================================
 * 2024-1 Q33. 화이트박스 검사 기법만으로 짝지어진 것은?
 * topic_id = 12401
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12401,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 화이트박스(White-box) 검사 기법에 해당하는 것으로만 짝지어진 것은?

> ㉠ 루프 검사(Loop Testing), 데이터 흐름 검사(Data Flow Testing)
> ㉡ 루프 검사, 데이터 흐름 검사
> ㉢ 경계값 분석, 동등 분할 검사
> ㉣ 오류 예측 기법, 원인-결과 그래프 기법',
  'A',
  '화이트박스 테스트는 코드의 내부 구조(문장, 분기, 경로, 루프 등)를 기준으로 테스트 케이스를 설계합니다. 대표적인 기법으로는 루프 검사, 데이터 흐름 검사가 있습니다. 반면 경계값 분석, 동등 분할, 원인-결과 그래프, 오류 예측 기법은 블랙박스 테스트 기법으로, 내부 구조를 보지 않고 입력/출력 조건에 기반해 테스트를 설계합니다. 따라서 화이트박스 기법으로만 묶인 것은 루프 검사와 데이터 흐름 검사가 포함된 집합인 ㉠, ㉡이며, 이에 해당하는 선택지는 ①입니다.',
  'past:2024-1:Q33'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '㉠, ㉡', 1),
  (@q_id, 'B', '㉠, ㉣', 0),
  (@q_id, 'C', '㉡, ㉤', 0),
  (@q_id, 'D', '㉢, ㉥', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');


/* =======================================================
 * 2024-1 Q34. 상향식 테스트에서 사용하는 가상 모듈
 * topic_id = 12401
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12401,
  'WRITTEN',
  'MCQ',
  'EASY',
  '단위 테스트에서 테스트의 대상이 되는 하위 모듈을 호출하고 파라미터를 전달하는 가상의 모듈로, 상향식 테스트에 필요한 것은 무엇인가요?',
  'B',
  '상향식 통합 테스트에서는 먼저 하위 모듈들을 구현·검증한 뒤, 이들을 호출해 주는 상위 모듈이 아직 없기 때문에 “가짜 상위 모듈”이 필요합니다. 이 역할을 하는 것이 테스트 드라이버입니다. 반대로 하향식 통합 테스트에서는 실제 상위 모듈이 먼저 등장하고, 아직 구현되지 않은 하위 모듈을 대신하기 위해 스텁을 사용합니다. 따라서 문제에서 묘사한 “하위 모듈을 호출하는 가상 모듈”은 테스트 드라이버를 의미합니다.',
  'past:2024-1:Q34'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '테스트 스텁(Test Stub)',       0),
  (@q_id, 'B', '테스트 드라이버(Test Driver)', 1),
  (@q_id, 'C', '테스트 슈트(Test Suites)',     0),
  (@q_id, 'D', '테스트 케이스(Test Case)',     0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');


/* =======================================================
 * 2024-1 Q35. Attribute-Value Pairs를 위한 개방형 표준 포맷
 * topic_id = 12502
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12502,
  'WRITTEN',
  'MCQ',
  'EASY',
  '인터페이스 구현 시, 속성-값 쌍(Attribute-Value Pairs)으로 이루어진 데이터 오브젝트를 전달하기 위해 사용하는 개방형 표준 포맷은 무엇인가요?',
  'A',
  'JSON은 속성-값 쌍으로 데이터를 표현하는 경량 데이터 교환 포맷으로, 시스템·언어에 독립적인 개방형 표준입니다. 키-값 구조로 직렬화하기 쉽고, 대부분의 언어에서 라이브러리를 통해 쉽게 파싱·생성할 수 있기 때문에 REST API나 웹 인터페이스에서 사실상 표준처럼 사용됩니다. HTML은 화면 표현을 위한 마크업 언어이고, 나머지 보기의 용어는 표준 데이터 교환 포맷으로 사용되지 않으므로, 정답은 JSON입니다.',
  'past:2024-1:Q35'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'JSON(JavaScript Object Notation)',         1),
  (@q_id, 'B', 'HTML(HyperText Markup Language)',          0),
  (@q_id, 'C', 'DOF(Data Object Format)',                  0),
  (@q_id, 'D', 'AVPN(Attribute-Value Protocol Notation)',  0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');


/* =======================================================
 * 2024-1 Q36. DRM 설명 중 틀린 것
 * topic_id = 15301
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15301,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'DRM(Digital Rights Management)에 대한 설명으로 틀린 것은?',
  'D',
  'DRM은 디지털 콘텐츠의 불법 복제·유통을 막기 위해 암호화, 키 관리, 라이선스 발급, 권한 제어 등 전자적 보안 기술을 적극적으로 활용하는 체계입니다. 따라서 “전자적 보안을 고려하지 않는다”, “불법 유통과 복제 방지는 불가능하다”는 설명은 DRM의 목적과 정반대입니다. 반면 A, B, C는 DRM이 다루는 주체(제조업자·저작권자 등), 생명주기 관리(권한·과금·유통), 클리어링 하우스의 역할을 적절히 설명하고 있으므로 옳은 설명입니다.',
  'past:2024-1:Q36'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '디지털 콘텐츠와 디바이스의 사용을 제한하기 위해 하드웨어 제조업자, 저작권자, 출판업자 등이 사용할 수 있는 접근 제어 기술이다.', 0),
  (@q_id, 'B', '디지털 미디어의 생명 주기 동안 발생하는 사용 권한, 과금, 유통 단계를 관리하는 기술로 볼 수 있다.', 0),
  (@q_id, 'C', '클리어링 하우스(Clearing House)는 사용자에게 콘텐츠 라이선스를 발급하고 권한을 부여하는 시스템이다.', 0),
  (@q_id, 'D', '원본을 안전하게 유통하기 위한 전자적 보안은 고려하지 않기 때문에, 불법 유통과 복제의 방지는 사실상 불가능하다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_BASE_DRM');


/* =======================================================
 * 2024-1 Q37. 테스트 오라클 설명 중 옳지 않은 것
 * topic_id = 12401
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12401,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 테스트 오라클(Test Oracle)에 대한 설명으로 옳지 않은 것은?',
  'B',
  '테스트 오라클은 “테스트 결과가 올바른지 판별하는 기준”입니다. 샘플링 오라클은 대표적인 일부 테스트 케이스에 대해서만 기대 결과를 알고 있는 형태입니다. 휴리스틱 오라클은 명확한 정답을 알기 어려운 경우 규칙(휴리스틱)에 따라 합리적인지 여부를 추정하는 형태입니다. 일관성 검사 오라클은 변경 전·후 결과가 일정한지, 혹은 시스템 불변 조건이 유지되는지를 확인합니다. 토탈 오라클이라는 용어는 이론적으로 “모든 입력에 대해 기대 결과를 줄 수 있는 이상적인 오라클”을 뜻하지만, 일반적인 소프트웨어 시스템에서는 사실상 구현·사용이 불가능합니다. 시험에서는 현실적인 관점에서 이 정의를 그대로 적용하는 것을 부정적으로 보아 오답으로 취급합니다.',
  'past:2024-1:Q37'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '샘플링 오라클은 특정한 일부 테스트 케이스의 입력 값에 대해서만 기대 결과를 제공한다.', 1),
  (@q_id, 'B', '토탈 오라클은 모든 테스트 케이스의 입력 값에 대해 기대 결과를 제공하는 이상적인 오라클이다.', 0),
  (@q_id, 'C', '휴리스틱 오라클은 일부 입력 값에 대해서만 기대 결과를 정확히 알고, 나머지에 대해서는 추정 규칙을 적용한다.', 1),
  (@q_id, 'D', '일관성 검사 오라클은 변경 전후 결과를 비교해 일관성이 유지되는지를 확인한다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');


/* =======================================================
 * 2024-1 Q38. 인터페이스 구현 검증 도구가 아닌 것은
 * topic_id = 12502
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12502,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 인터페이스 구현을 검증하기 위한 테스트/자동화 도구로 보기 어려운 것은?',
  'A',
  'xUnit 계열 도구는 단위 테스트 프레임워크로서 인터페이스 검증에도 활용할 수 있지만, ESB(Enterprise Service Bus)는 여러 시스템과 서비스를 통합하기 위한 미들웨어 아키텍처로, “테스트 도구”라기보다는 연동 인프라에 가깝습니다. 문제의 보기들은 “인터페이스 구현 검증 도구”라는 공통 분류를 가정하고 있는데, xUnit과 ESB를 한 묶음으로 놓고 이를 통째로 검증 도구라고 보기에는 ESB의 성격이 맞지 않기 때문에 ①이 정답이 됩니다.',
  'past:2024-1:Q38'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'xUnit, ESB',                   1),
  (@q_id, 'B', 'NTAF',                         0),
  (@q_id, 'C', 'STAF',                         0),
  (@q_id, 'D', 'Selenium 기반 테스트 도구',    0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');


/* =======================================================
 * 2024-1 Q39. 방향 그래프 최대 간선 수
 * topic_id = 14002
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14002,
  'WRITTEN',
  'MCQ',
  'EASY',
  '정점이 5개인 방향 그래프가 가질 수 있는 최대 간선 수는 얼마인가요?
  (단, 자기 간선과 중복 간선은 배제합니다.)',
  'B',
  '정점이 n개인 방향 그래프에서, 자기 간선과 중복 간선을 허용하지 않는 경우 한 정점에서 나갈 수 있는 간선은 나머지 (n-1)개의 정점으로 향하는 간선뿐입니다. 따라서 가능한 전체 간선 수는 n×(n-1)이 됩니다. 이 문제에서는 n=5이므로 최대 간선 수는 5×4 = 20개입니다.',
  'past:2024-1:Q39'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '7개',   0),
  (@q_id, 'B', '20개',  1),
  (@q_id, 'C', '10개',  0),
  (@q_id, 'D', '27개',  0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');


/* =======================================================
 * 2024-1 Q40. 파티션 유형이 아닌 것
 * topic_id = 13402
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13402,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '물리 데이터 저장소의 파티션 설계에서 사용하는 파티션 유형으로 옳지 않은 것은?',
  'D',
  '범위 분할은 값의 범위(예: 날짜 구간)에 따라 파티션을 나누는 방식이고, 해시 분할은 해시 함수를 이용해 데이터를 균등하게 분산시키는 방식입니다. 조합 분할(Composite Partitioning)은 범위+해시와 같이 둘 이상의 방식을 섞어 사용하는 구조입니다. “유닛 분할(Unit Partitioning)”이라는 용어는 일반적인 DB 파티션 유형으로 사용되지 않으므로, 파티션 유형으로 옳지 않은 것은 ④입니다.',
  'past:2024-1:Q40'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '범위 분할(Range Partitioning)',      0),
  (@q_id, 'B', '해시 분할(Hash Partitioning)',       0),
  (@q_id, 'C', '조합 분할(Composite Partitioning)',  0),
  (@q_id, 'D', '유닛 분할(Unit Partitioning)',       1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');
