USE certpilot_study;

/* =======================================================
 * Q31. 빌드 자동화 도구  [정답: ②]
 *  - topic_id = 24101 (빌드/CI 도구)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  24101,
  'WRITTEN',
  'MCQ',
  'EASY',
  '빌드 자동화 도구에 대한 설명으로 틀린 것은?',
  'B',
  'Gradle이 Groovy(와 Kotlin DSL)를 기반으로 한 빌드 도구이며, Android 공식 빌드 시스템으로 사용됩니다.
Ant는 XML 기반 스크립트 방식 도구라서 “Groovy 기반, 안드로이드 앱 개발 환경에서 사용”이라는 설명은
실제로는 Gradle에 대한 설명입니다. 따라서 2번이 틀린 보기입니다.',
  'past:2024-2:Q31'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Gradle은 태스크 단위로 실행한다.',                                        0),
  (@q_id, 'B', 'Ant는 Groovy 기반 오픈소스로 안드로이드 앱 개발 환경에서 사용된다.',      1),
  (@q_id, 'C', '빌드 자동화 도구에는 Ant, Gradle, Jenkins 등이 있다.',                   0),
  (@q_id, 'D', '빌드 자동화 도구는 CI 환경에서 유용하게 활용된다.',                       0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'CONFIG_CM');


/* =======================================================
 * Q32. 해싱 함수 종류  [정답: ④]
 *  - topic_id = 22203 (해싱/해시함수)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  22203,
  'WRITTEN',
  'MCQ',
  'EASY',
  '(Hashing Function) 해싱 함수의 종류가 아닌 것은?',
  'D',
  '제곱법, 제산법, 숫자분석법은 “해시 주소를 계산하는 방식(해싱 함수)”에 해당합니다.
개방주소법(open addressing)은 충돌이 발생한 뒤 빈 슬롯을 탐색하는 “충돌 해결 기법”이므로
해싱 함수의 종류라고 볼 수 없습니다.',
  'past:2024-2:Q32'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '제곱법(mid-square)',          0),
  (@q_id, 'B', '제산법(division)',            0),
  (@q_id, 'C', '숫자분석법(digit analysis)',  0),
  (@q_id, 'D', '개방주소법(open addressing)', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');


/* =======================================================
 * Q33. 효과적인 모듈 설계  [정답: ④]
 *  - topic_id = 24102 (모듈 설계/응집도·결합도)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  24102,
  'WRITTEN',
  'MCQ',
  'EASY',
  '효과적인 모듈 설계를 위한 유의사항으로 거리가 먼 것은?',
  'D',
  '좋은 모듈 설계는 낮은 결합도, 높은 응집도, 예측 가능한 기능, 일관성 유지, 중복 최소화를 지향합니다.
“일관성을 줄이고 중복성을 유지”하는 것은 바람직한 설계 지침과 정반대이므로
거리가 가장 먼 보기입니다.',
  'past:2024-2:Q33'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '결합도를 약하게 하면 모듈 독립성이 향상된다.',            0),
  (@q_id, 'B', '하나의 입구와 하나의 출구를 갖도록 해야 한다.',            0),
  (@q_id, 'C', '기능은 예측 가능해야 하고 지나치게 제한적이면 안 된다.',   0),
  (@q_id, 'D', '일관성을 줄이고 중복성을 유지시킨다.',                      1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');


/* =======================================================
 * Q34. White Box Testing  [정답: ①]
 *  - topic_id = 22105 (화이트박스 테스트)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  22105,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'White Box Testing에 대한 설명으로 옳지 않은 것은?',
  'A',
  'Base Path Testing(기본 경로 검사)은 화이트박스 기법이 맞지만,
Boundary Value Analysis(경계값 분석)는 입력 경계값을 기준으로 하는 전형적인 블랙박스 테스트 기법입니다.
나머지 보기는 코드 구조를 기준으로 하는 화이트박스 테스트의 특징을 잘 설명하고 있습니다.',
  'past:2024-2:Q34'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Base Path와 Boundary Value Analysis가 대표적이다.', 1),
  (@q_id, 'B', '소스 코드의 모든 문장을 한 번 이상 수행하며 진행된다.',   0),
  (@q_id, 'C', '모듈 내부 작동을 직접 관찰할 수 있다.',                 0),
  (@q_id, 'D', '제어 구조(선택·반복 등)에 따라 논리 경로를 점검한다.',   0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');


/* =======================================================
 * Q35. Walkthrough vs Inspection  [정답: ③]
 *  - topic_id = 24103 (검토 기법)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  24103,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '(Walkthrough)와 (Inspection)에 대한 설명으로 가장 옳지 않은 것은?',
  'C',
  '워크스루는 개발자가 동료들을 모아 자신의 산출물을 설명하고 피드백을 받는
비교적 비형식적 검토입니다. 주 목적은 이해 공유와 오류 발견이지,
회의 자리에서 “문제 해결까지” 수행하는 것은 아닙니다.
발견된 오류의 해결은 후속 작업으로 넘기는 것이 일반적이라 3번이 가장 부정확한 설명입니다.',
  'past:2024-2:Q35'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '워크스루는 전문가에 의해 작업 내역이 검토된다.',   0),
  (@q_id, 'B', '워크스루는 제품 개발자가 주최가 된다.',            0),
  (@q_id, 'C', '워크스루는 오류 발견과 그 해결에 중점을 둔다.',    1),
  (@q_id, 'D', '인스펙션은 워크스루를 발전시킨 형태이다.',         0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'TEST_BASE_TECH');


/* =======================================================
 * Q36. 중위 → 후위 표기 변환  [정답: ②]
 *  - topic_id = 22204 (수식 표기/스택)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  22204,
  'WRITTEN',
  'MCQ',
  'EASY',
  '중위 표기식 ( A + B ) * C + ( D + E ) 를 후위 표기식(Postfix)으로 옳게 나타낸 것은?',
  'B',
  '중위식 (A + B) * C + (D + E)를 차례로 변환하면
- (A + B) → AB+
- (A + B)*C → AB+C*
- (D + E) → DE+
최종적으로 AB+C*DE+ + 가 되어 2번이 정답입니다.',
  'past:2024-2:Q36'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'AB + C*DE + +',   0),
  (@q_id, 'B', 'AB + CDE * + +',  1),
  (@q_id, 'C', '+ * + ABC + DE',  0),
  (@q_id, 'D', 'AB*C + DE +',     0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');


/* =======================================================
 * Q37. 분산 버전 관리(DVCS) 방식  [정답: ①]
 *  - topic_id = 23104 (형상관리/VCS 방식)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  23104,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 설명에 해당하는 소프트웨어 버전 관리 도구 방식은 무엇인가?

> · 버전 관리 자료가 원격 저장소와 로컬 저장소에 함께 저장되어 관리된다.
> · 로컬 저장소에서 버전 관리가 가능하므로, 원격 저장소에 문제가 생겨도
>   로컬 저장소의 자료를 이용하여 작업할 수 있다.
> · 대표적인 버전 관리 도구로 Git이 있다.',
  'A',
  'Git처럼 각 개발자 PC에 전체 이력이 복제되어 있는 구조를
“분산 버전 관리(분산 저장소 방식, DVCS)”라고 합니다.
중앙 서버에만 저장소가 있고 클라이언트는 작업본만 가지는 구조는
단일 저장소/클라이언트·서버 방식에 가깝습니다. 지문은 분산 저장소 방식의 대표적인 설명입니다.',
  'past:2024-2:Q37'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '분산 저장소 방식',     1),
  (@q_id, 'B', '단일 저장소 방식',     0),
  (@q_id, 'C', '클라이언트·서버 방식', 0),
  (@q_id, 'D', '공유 폴더 방식',       0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'CONFIG_CM');


/* =======================================================
 * Q38. 위험 모니터링  [정답: ④]
 *  - topic_id = 24104 (프로젝트 위험관리)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  24104,
  'WRITTEN',
  'MCQ',
  'EASY',
  '위험 모니터링의 의미로 옳은 것은?',
  'D',
  '위험 모니터링은 식별된 위험과 그 징후(Trigger)를 지속적으로 관찰·추적하여,
실제 발생 여부와 영향 변화를 파악하는 활동입니다.
이해/회피/발생 후 대응은 각각 위험 분석, 위험 회피 전략, 대응 실행에 더 가깝습니다.',
  'past:2024-2:Q38'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '위험을 이해하는 것',                   0),
  (@q_id, 'B', '첫 조치로 위험을 피하도록 하는 것',    0),
  (@q_id, 'C', '위험 발생 후 즉시 조치하는 것',        0),
  (@q_id, 'D', '위험 징후를 계속적으로 인지하는 것',    1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_COST_PM');


/* =======================================================
 * Q39. 무방향 그래프 최대 간선 수  [정답: ④]
 *  - topic_id = 22205 (그래프 이론)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  22205,
  'WRITTEN',
  'MCQ',
  'EASY',
  'n개의 노드로 구성된 무방향 그래프의 최대 간선 수는?',
  'D',
  '단순 무방향 그래프에서 서로 다른 두 정점 쌍마다 최대 1개의 간선이 존재합니다.
가능한 정점 쌍의 개수는 조합 C(n, 2) = n(n−1)/2 이므로
최대 간선 수는 n(n−1)/2입니다.',
  'past:2024-2:Q39'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'n/2',          0),
  (@q_id, 'B', 'n − 1',        0),
  (@q_id, 'C', 'n(n − 1)',     0),
  (@q_id, 'D', 'n(n − 1)/2',   1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');


/* =======================================================
 * Q40. 이진 검색 알고리즘 설명  [정답: ③]
 *  - topic_id = 22206 (검색 알고리즘)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  22206,
  'WRITTEN',
  'MCQ',
  'EASY',
  '이진 검색 알고리즘에 대한 설명으로 틀린 것은?',
  'C',
  '이진 검색(Binary Search)은 정렬된 데이터에서 가운데 원소와 비교해
검색 구간을 절반씩 줄여 나가는 방식입니다.
“피보나치 수열에 따라 비교 대상을 선정하는” 것은 피보나치 검색(Fibonacci Search)의 설명이므로
3번이 틀린 보기입니다.',
  'past:2024-2:Q40'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '탐색 효율이 좋고 시간이 적게 소요된다.',              0),
  (@q_id, 'B', '검색 데이터는 정렬되어 있어야 한다.',                  0),
  (@q_id, 'C', '피보나치 수열에 따라 비교 대상을 선정하여 검색한다.', 1),
  (@q_id, 'D', '비교할수록 검색 대상 데이터 수가 절반으로 줄어든다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DS_ALGO');
