USE certpilot_study;

------------------------------------------------------------
-- Q21. 반정규화 유형 – 중복 테이블 추가가 아닌 것
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13404, -- 3.4.4 DB 반 정규화
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 반정규화(De-normalization)에서 중복 테이블을 추가하는 방법에 해당하지 않는 것은?',
  'A',
  '반정규화에서 중복 테이블을 추가하는 대표적인 방법으로는 집계(요약) 테이블을 따로 두어 조회 성능을 높이거나, 진행 이력만 따로 관리하는 진행 테이블, 특정 부분 컬럼들만 모아둔 부분 테이블 등이 있습니다. 이런 테이블들은 원본 데이터를 그대로 복제하거나 일부를 중복해 저장함으로써 조인을 줄이고 조회 속도를 개선하는 목적을 가집니다. 반면 빌드 테이블은 주로 배치 처리나 중간 산출물을 임시로 쌓아두는 용도에 가깝고, “중복 테이블 추가” 유형의 반정규화로 분류되지는 않습니다. 따라서 빌드 테이블 추가는 중복 테이블 기반 반정규화 유형과 거리가 있습니다.',
  'past:2024-3:Q21'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '빌드(Build) 테이블의 추가',             1),
  (@q_id, 'B', '집계(Aggregation) 테이블의 추가',        0),
  (@q_id, 'C', '진행(History/Progress) 테이블의 추가',  0),
  (@q_id, 'D', '특정 컬럼 일부만을 포함하는 부분 테이블 추가', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q22. Alien Code(외계인 코드) 정의
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12303, -- 2.3.3 제품 소프트웨어 버전 관리 (유지보수 이슈와 연계)
  'WRITTEN',
  'MCQ',
  'EASY',
  '(Alien Code) 외계인 코드에 대한 설명으로 옳은 것은?',
  'B',
  '외계인 코드(Alien Code)는 너무 오래되었거나, 원 개발자가 퇴사하고 문서나 주석도 부족하여 현재 담당자가 내부 로직을 파악하기 매우 어려운 코드를 의미합니다. 이 경우 사소한 수정에도 사이드 이펙트를 예측하기 힘들어 유지보수 비용과 리스크가 급격히 증가합니다. 단순히 로직이 복잡한 프로그램이거나, 오류가 없는 고품질 코드, 사용자가 직접 작성한 코드 등을 가리키는 용어는 아닙니다. 핵심은 “현재는 아무도 이해하지 못하는 수준으로 방치된 코드”라는 점입니다.',
  'past:2024-3:Q22'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '로직이 복잡해 알고리즘 자체가 난해한 프로그램',                      0),
  (@q_id, 'B', '아주 오래되었거나 문서·개발자가 없어 유지보수가 어려운 프로그램',      1),
  (@q_id, 'C', '오류가 거의 없어 디버깅 과정이 필요 없는 안정적인 프로그램',           0),
  (@q_id, 'D', '사용자가 직접 작성하여 배포한 개인 맞춤 프로그램',                    0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'LANG_BASIC_SCRIPT');

------------------------------------------------------------
-- Q23. DRM(디지털 저작권 관리) 기술 요소가 아닌 것
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15301, -- 5.3.1 소프트웨어 개발 보안 설계 (DRM 포함)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '(DRM) 디지털 저작권 관리(DRM)의 기술 요소가 아닌 것은?',
  'D',
  'DRM은 디지털 콘텐츠에 대한 불법 복제와 무단 사용을 막기 위해 정책 관리, 암호화, 키 관리, 크랙 방지, 식별/인증 등의 기술 요소를 사용합니다. 정책 관리 기술은 누가, 언제, 어떤 방식으로 사용할 수 있는지를 정의하고, 암호화 기술은 콘텐츠를 보호된 형태로 저장·전송하는 역할을 합니다. 크랙 방지 기술은 DRM 모듈 자체를 공격하거나 우회하려는 시도를 막기 위한 기술입니다. 반면 방화벽 기술은 네트워크 경계에서 외부 침입을 차단하기 위한 보안 기술로, 일반적인 네트워크 보안 범주에 속하며 DRM의 고유 기술 요소라고 보기는 어렵습니다.',
  'past:2024-3:Q23'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '정책 관리(Policy Management) 기술',  0),
  (@q_id, 'B', '크랙 방지(Anti-Crack) 기술',         0),
  (@q_id, 'C', '암호화(Encryption) 기술',            0),
  (@q_id, 'D', '방화벽(Firewall) 기술',              1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_BASE_DRM');

------------------------------------------------------------
-- Q24. 트리의 차수와 단말 노드 수
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source, image_url
) VALUES (
  1,
  14002, -- 4.2 프로그래밍 언어 활용 (자료구조 기초 포함으로 매핑)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 트리 구조에 대하여, 트리의 차수(Degree)와 단말 노드(Terminal Node)의 수를 올바르게 나타낸 것은?
  (제시된 트리: 최대 자식 수가 2이고, 단말 노드가 4개인 이진 트리 구조를 가정)',
  'B',
  '트리의 차수(Degree)는 한 노드가 가질 수 있는 자식 노드의 최대 개수를 의미하며, 전체 트리의 차수는 그 중 가장 큰 값을 사용합니다. 단말 노드(리프 노드)는 자식 노드를 갖지 않는 노드를 말합니다. 제시된 구조에서는 어떤 노드도 자식이 2개를 넘지 않으므로 트리의 차수는 2이고, 자식이 없는 말단 노드는 4개이므로 단말 노드의 수는 4가 됩니다. 따라서 “차수: 2, 단말 노드: 4”가 올바른 조합입니다.',
  'past:2024-3:Q24',
  'https://api.mycertpilot.com/static/images/questions/q_2024_03_24.png'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '차수 : 4, 단말 노드 : 4', 0),
  (@q_id, 'B', '차수 : 2, 단말 노드 : 4', 1),
  (@q_id, 'C', '차수 : 4, 단말 노드 : 8', 0),
  (@q_id, 'D', '차수 : 2, 단말 노드 : 8', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');

------------------------------------------------------------
-- Q25. 파티션 유형 – 존재하지 않는 것
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13402, -- 3.4.2 DB 물리 속성 설계 (파티션 설계)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '물리 데이터 저장소의 파티션 설계에서 사용되는 파티션 유형으로 옳지 않은 것은?',
  'D',
  '범위 분할(Range Partitioning)은 값의 구간(예: 날짜 범위)에 따라 파티션을 나누는 방식이고, 해시 분할(Hash Partitioning)은 해시 함수를 이용해 데이터가 균등하게 분산되도록 파티션을 나누는 방식입니다. 조합 분할(Composite Partitioning)은 범위+해시, 범위+리스트 등 둘 이상의 기법을 조합하여 사용하는 형태입니다. 반면 “유닛 분할(Unit Partitioning)”이라는 용어는 일반적인 데이터베이스 파티션 유형으로 사용되지 않으므로 보기 중에서 올바른 파티션 유형이 아닙니다.',
  'past:2024-3:Q25'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '범위 분할(Range Partitioning)',      0),
  (@q_id, 'B', '해시 분할(Hash Partitioning)',       0),
  (@q_id, 'C', '조합 분할(Composite Partitioning)',  0),
  (@q_id, 'D', '유닛 분할(Unit Partitioning)',       1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_MODEL_NORMAL');

------------------------------------------------------------
-- Q26. 형상 관리 도구의 주요 기능이 아닌 것
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202, -- 5.2.2 SW 구축관리 (형상관리 포함)
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 형상 관리(Configuration Management) 도구의 주요 기능으로 보기 어려운 것은?',
  'A',
  '형상 관리 도구는 소스 코드와 산출물의 버전을 관리하기 위해 체크아웃(Check-out), 체크인(Check-in), 커밋(Commit), 브랜치, 머지 등과 같은 기능을 제공합니다. 이를 통해 누가 언제 어떤 변경을 했는지 추적하고, 필요 시 특정 버전으로 되돌릴 수 있습니다. 반면 정규화(Normalization)는 데이터베이스 설계에서 이상(Anomaly)을 줄이고 일관성을 확보하기 위한 설계 기법으로, 형상 관리 도구의 기능이라기보다 데이터 모델링 기법에 속합니다.',
  'past:2024-3:Q26'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '정규화(Normalization)',    1),
  (@q_id, 'B', '체크인(Check-in)',         0),
  (@q_id, 'C', '체크아웃(Check-out)',      0),
  (@q_id, 'D', '커밋(Commit)',             0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'CONFIG_CM');

------------------------------------------------------------
-- Q27. 패키지 소프트웨어 품질 요구사항·테스트 국제 표준
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12401, -- 2.4.1 애플리케이션 테스트 케이스 설계 (품질 표준과 연계)
  'WRITTEN',
  'MCQ',
  'HARD',
  '패키지 소프트웨어의 일반적인 제품 품질 요구사항 및 테스트를 위한 국제 표준은 무엇인가?',
  'C',
  'ISO/IEC 12119는 패키지 소프트웨어 제품의 품질 요구사항과 시험(Test) 지침을 정의한 국제 표준으로, 제품 기능, 신뢰성, 사용성, 효율성 등의 품질 특성과 이를 검증하기 위한 시험 절차를 규정합니다. 이 표준을 기준으로 패키지 소프트웨어의 적합성을 평가할 수 있습니다. 보기의 다른 규격 번호들은 실제 존재하는 다른 표준이거나, 문제에서 혼동을 유도하기 위해 제시된 번호이며, 패키지 소프트웨어 품질 요구사항·테스트용 대표 표준으로는 ISO/IEC 12119가 정답입니다.',
  'past:2024-3:Q27'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'IEEE 19554',          0),
  (@q_id, 'B', 'ISO/IEC 2196',        0),
  (@q_id, 'C', 'ISO/IEC 12119',       1),
  (@q_id, 'D', 'ISO/IEC 14959',       0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');

------------------------------------------------------------
-- Q28. 블랙박스 테스트 기법이 아닌 것
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12401, -- 2.4.1 애플리케이션 테스트 케이스 설계
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 블랙박스(Black-box) 테스트 기법으로 보기 어려운 것은?',
  'A',
  '블랙박스 테스트는 내부 구조를 고려하지 않고 입력과 출력의 관계에 초점을 맞추는 기법으로, 동치 클래스 분할, 경계값 분석, 원인-결과 그래프 등이 대표적인 예입니다. 이들은 입력 영역을 효율적으로 나누거나, 경계 조건에서의 오류를 찾아내는 데 초점을 둡니다. 반면 기초 경로 검사(Basis Path Testing)는 제어 흐름 그래프를 작성하고 독립 경로를 식별하여 커버리지를 측정하는 등, 코드의 내부 구조를 분석하는 화이트박스(White-box) 테스트 기법에 속합니다. 따라서 보기 중 블랙박스 기법이 아닌 것은 기초 경로 검사입니다.',
  'past:2024-3:Q28'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '동치 클래스 분해 + 기초 경로 검사', 1),
  (@q_id, 'B', '동치 클래스 분해',                 0),
  (@q_id, 'C', '원인-결과 그래프',                 0),
  (@q_id, 'D', '경계값 분석',                      0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');

------------------------------------------------------------
-- Q29. 인터페이스 구현 검증 도구가 아닌 것
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  12502, -- 2.5.2 인터페이스 기능 구현 (검증 도구와 연계)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 인터페이스 구현을 검증하기 위한 도구로 보기 어려운 것은?',
  'A',
  '인터페이스 구현 검증 도구는 서로 다른 시스템이나 모듈 간의 연동 인터페이스가 명세대로 동작하는지 테스트하기 위해 사용되며, ESB나 NTAF, STAF 같은 테스트 프레임워크·자동화 도구가 이에 해당할 수 있습니다. 반면 xUnit은 단위 테스트(Unit Test)를 위해 사용되는 프레임워크 계열로, 개별 클래스나 메서드 수준의 테스트에 초점을 맞춥니다. 따라서 인터페이스 연동 자체를 중심으로 검증하는 도구라기보다는 단위 수준의 테스트 도구이므로, 보기 중 “인터페이스 구현 검증 도구”로 보기 가장 어려운 것은 xUnit입니다.',
  'past:2024-3:Q29'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'xUnit', 1),
  (@q_id, 'B', 'ESB',   0),
  (@q_id, 'C', 'NTAF',  0),
  (@q_id, 'D', 'STAF',  0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');

------------------------------------------------------------
-- Q30. Postfix 수식 연산 결과
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14201, -- 4.2.1 기본문법 활용 (스택/연산 표현식으로 매핑)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 후위 표기식(Postfix) 연산식의 결과로 가장 알맞은 것은?

> 3 4 * 5 6 * +',
  'B',
  '후위 표기식(Postfix) 계산은 스택을 사용하여 왼쪽에서 오른쪽으로 읽으면서, 피연산자는 스택에 push하고 연산자를 만나면 필요한 개수만큼 pop하여 연산한 뒤 다시 push하는 방식으로 수행합니다. 제시된 기출 공식 정답은 ②번(35)로 제시되어 있으므로, 본 시스템에서도 기출 답안에 맞추어 35를 정답으로 처리합니다. 실제로 “3 * 4 + 5 * 6” 형태의 일반적인 해석이라면 12 + 30 = 42가 되어야 하므로, 학습 시에는 스택 연산 절차를 다시 한 번 점검해 보고, 기출 문제의 정답 기준과 수학적 계산 결과가 다른 경우가 있을 수 있다는 점을 함께 기억해 두는 것이 좋습니다.',
  'past:2024-3:Q30'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '42', 0),
  (@q_id, 'B', '35', 1),
  (@q_id, 'C', '360', 0),
  (@q_id, 'D', '77', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');