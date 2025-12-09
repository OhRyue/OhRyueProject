USE certpilot_study;

/* =======================================================
 * Q11. 코드 설계 – 표의 숫자 코드  [정답: ④]
 *  - topic_id = 11201 (1.2.1 UI 요구사항/코드·화면 설계 인근 영역)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11201,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '코드 설계에서 코드화 대상 항목의 성질, 즉 길이·넓이·부피·지름·높이 등의 물리적 수치를 그대로 코드에 적용시키는 방식의 코드는 무엇인가?',
  'D',
  '코드화 대상 항목의 물리적 수치(길이·넓이·부피·지름·높이 등)를 그대로 코드에 반영하는 방식은 표의 숫자 코드(유효 숫자 코드, Significant Digit Code)입니다. 각 자릿수나 숫자가 실제 속성 값과 직접 対응하도록 설계한다는 점이 핵심입니다.
순차 코드는 단순 일련번호 부여, 블록 코드는 구간별 번호 블록 사용, 연상 코드는 의미를 떠올리기 쉬운 문자·숫자를 사용하는 방식이므로 문제에서 제시한 “물리적 수치 그대로 사용”과는 다릅니다.',
  'past:2024-2:Q11'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '연상 코드',       0),
  (@q_id, 'B', '블록 코드',       0),
  (@q_id, 'C', '순차 코드',       0),
  (@q_id, 'D', '표의 숫자 코드',  1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');


/* =======================================================
 * Q12. 애자일(Agile) 가치  [정답: ②]
 *  - topic_id = 15101 (5.1.1 소프트웨어 개발방법론/애자일)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15101,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 애자일(Agile) 소프트웨어 개발 기법의 가치가 아닌 것은?',
  'B',
  '애자일 선언(Agile Manifesto)의 네 가지 가치는 다음과 같습니다.
1) 프로세스와 도구보다 개인과 상호작용
2) 포괄적인 문서보다 작동하는 소프트웨어
3) 계약 협상보다 고객과의 협업
4) 계획을 따르기보다 변화에의 대응
따라서 “실제 작동하는 소프트웨어보다는 이해하기 좋은 문서에 더 가치를 둔다”는 ②번은 애자일 가치와 정반대에 해당하므로, 애자일의 가치가 아닌 보기입니다.',
  'past:2024-2:Q12'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '계획을 따르기보다는 변화에 대응하는 것에 더 가치를 둔다.',        0),
  (@q_id, 'B', '실제 작동하는 소프트웨어보다는 이해하기 좋은 문서에 더 가치를 둔다.', 1),
  (@q_id, 'C', '계약 협상보다는 고객과의 협업에 더 가치를 둔다.',                   0),
  (@q_id, 'D', '프로세스와 도구보다는 개인과 상호작용에 더 가치를 둔다.',            0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'PROCESS_WATERFALL_AGILE');


/* =======================================================
 * Q13. 비용 산정 – Putnam 모형  [정답: ①]
 *  - topic_id = 15202 (프로젝트/관리·도구 쪽으로 정리)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'Rayleigh-Norden 곡선의 노력 분포도를 이용한 프로젝트 비용 산정 기법은?',
  'A',
  '시간에 따른 투입 노력 분포를 Rayleigh-Norden 곡선으로 모델링하여 프로젝트 규모와 개발 노력, 개발 기간 사이의 관계를 추정하는 비용 산정 기법은 Putnam 모형입니다(SLIM 모형의 기반).
델파이 모형은 전문가 집단의 설문·합의를 이용한 직관적 추정 기법, 기능 점수(Function Point) 모형은 기능 수를 중심으로 점수화하여 규모를 추정하는 방법, COCOMO는 LOC(코드 라인 수)를 기반으로 한 알고리즘적 비용 산정 모형입니다.',
  'past:2024-2:Q13'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Putnam 모형',                    1),
  (@q_id, 'B', '델파이(Delphi) 모형',            0),
  (@q_id, 'C', '기능 점수(Function Point) 모형', 0),
  (@q_id, 'D', 'COCOMO 모형',                    0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_COST_PM');


/* =======================================================
 * Q14. 객체지향 기본 개념 – 캡슐화 정의  [정답: ③]
 *  - topic_id = 11302 (OOP/UML 쪽이 더 자연스러움)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302,
  'WRITTEN',
  'MCQ',
  'EASY',
  '객체지향의 주요 개념에 대한 설명으로 틀린 것은?',
  'C',
  '캡슐화(encapsulation)는 데이터(속성)와 이를 처리하는 연산(메소드)을 하나의 단위(클래스)에 묶고, 외부에는 필요한 인터페이스만 공개하며 내부 구현을 숨기는 개념입니다. 이를 통해 정보 은닉, 변경 용이성, 모듈성 향상 효과를 얻습니다.
보기 ③처럼 “두 개 이상의 객체(클래스)들이 상호 참조하는 관계”는 단순히 상호(순환) 참조 관계를 말하는 것이지 캡슐화의 정의가 아닙니다. 나머지 보기들은 상속, 객체, 다형성의 일반적인 정의와 잘 일치합니다.',
  'past:2024-2:Q14'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '상속은 상위 클래스에서 속성이나 연산을 전달받아 새로운 형태의 클래스로 확장하여 사용하는 것을 의미한다.', 0),
  (@q_id, 'B', '객체는 실세계에 존재하거나 생각할 수 있는 것을 말한다.',                                 0),
  (@q_id, 'C', '캡슐화는 두 개 이상의 객체(클래스)들이 상호 참조하는 관계이다.',                        1),
  (@q_id, 'D', '다형성은 상속받은 여러 하위 객체들이 다른 형태의 특성을 갖는 객체로 이용될 수 있는 성질이다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * Q15. 웹 애플리케이션 서버(WAS)  [정답: ①]
 *  - topic_id = 15201 (네트워크/시스템·미들웨어 인프라 영역)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15201,
  'WRITTEN',
  'MCQ',
  'EASY',
  '웹 애플리케이션 서버(WAS; Web Application Server)에 대한 설명으로 틀린 것은?',
  'A',
  'WAS(Web Application Server)는 동적인 비즈니스 로직을 실행하고, DB 연동 및 트랜잭션 관리 등 서버 측 애플리케이션 처리를 담당하는 미들웨어입니다. 정적인 콘텐츠(HTML, 이미지, CSS 등)는 일반적으로 웹 서버(Apache, Nginx 등)가 담당하며, WAS는 주로 동적 페이지 처리에 초점을 둡니다.
따라서 “정적인 콘텐츠를 처리하기 위해 사용되는 미들웨어”라는 ①번 설명은 잘못된 설명입니다. 나머지 보기들은 웹 환경 중심의 미들웨어라는 점, Java/EJB 기반 미션 크리티컬 업무 구현 가능, WebLogic/WebSphere 등이 대표 WAS라는 점을 올바르게 설명하고 있습니다.',
  'past:2024-2:Q15'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '정적인 콘텐츠를 처리하기 위해 사용되는 미들웨어이다.',         1),
  (@q_id, 'B', '클라이언트/서버 환경보다는 웹 환경을 구현하기 위한 미들웨어이다.', 0),
  (@q_id, 'C', '미션 크리티컬한 기업 업무도 Java, EJB 컴포넌트 기반으로 구현이 가능하다.', 0),
  (@q_id, 'D', '대표적인 WAS의 종류에는 오라클 WebLogic, IBM WebSphere 등이 있다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');


/* =======================================================
 * Q16. 유스케이스 다이어그램 구성 요소  [정답: ③]
 *  - topic_id = 11302 (UML 기초/Use Case)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 유스케이스 다이어그램(Use Case Diagram)의 구성 요소가 아닌 것은?',
  'C',
  '유스케이스 다이어그램의 기본 구성요소는 Actor(행위자), Use Case(유스케이스), System Boundary(시스템 경계)와 이들 사이의 관계(연관, include/extend, 일반화)입니다.
Operation은 클래스 다이어그램에서 클래스가 가지는 오퍼레이션(메서드)을 표현할 때 사용하는 개념으로, 유스케이스 다이어그램의 구성 요소라고 보기는 어렵습니다. 따라서 ③ Operation이 정답입니다.',
  'past:2024-2:Q16'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'System',    0),
  (@q_id, 'B', 'Actor',     0),
  (@q_id, 'C', 'Operation', 1),
  (@q_id, 'D', 'UseCase',   0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * Q17. 폭포수(Waterfall) 모형 특징  [정답: ②]
 *  - topic_id = 15101 (생명주기 모형/방법론)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15101,
  'WRITTEN',
  'MCQ',
  'EASY',
  '폭포수(Waterfall) 모형의 특징으로 거리가 먼 것은?',
  'B',
  '폭포수 모형은 요구분석 → 설계 → 구현 → 테스트 → 유지보수의 단계를 위에서 아래로 순차적으로 진행하는 전통적인 선형 생명주기 모형입니다. 각 단계의 산출물이 비교적 명확하고, 적용 경험과 사례가 많다는 장점이 있습니다.
반대로 나선형(Spiral) 모형은 폭포수 모형의 단점(변경 대응 어려움, 위험 관리 부족 등)을 보완하기 위해 제안된 점진적·반복적 모형입니다. 따라서 “나선형 모형의 단점을 보완하기 위한 모형”이라는 ②번 설명은 폭포수가 아니라 나선형 모형에 해당하므로 폭포수 모형의 특징과 가장 거리가 먼 보기입니다.',
  'past:2024-2:Q17'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '순차적인 접근방법을 이용한다.',                 0),
  (@q_id, 'B', '나선형 모형의 단점을 보완하기 위한 모형이다.', 1),
  (@q_id, 'C', '단계적 정의와 산출물이 명확하다.',             0),
  (@q_id, 'D', '모형의 적용 경험과 성공 사례가 많다.',         0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'PROCESS_WATERFALL_AGILE');


/* =======================================================
 * Q18. 송수신 데이터 처리 방식 – 배치  [정답: ④]
 *  - topic_id = 15201 (시스템/데이터 처리 방식 인근)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15201,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '송수신 데이터의 처리 방식 중 대량의 데이터를 처리할 때 사용하는 방식은?',
  'D',
  '대량의 데이터를 일정 기간 동안 모았다가 한 번에 일괄 처리하는 방식은 배치 처리(batch processing)입니다. 정산, 통계 집계, 대규모 로그 처리 등에서 자주 사용됩니다.
실시간 처리 방식은 입력과 동시에 즉시 처리·반영하는 방식이고, 분산 처리는 여러 노드에 작업을 분산하는 구조를, “지연 처리”는 트리거나 큐 등에 의해 늦춰서 처리하는 개념으로 쓰입니다. 문제에서 묻는 “대량 데이터 처리의 전형적인 방식”은 배치 처리입니다.',
  'past:2024-2:Q18'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '분산 처리 방식',   0),
  (@q_id, 'B', '실시간 처리 방식', 0),
  (@q_id, 'C', '지연 처리 방식',   0),
  (@q_id, 'D', '배치 처리 방식',   1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');


/* =======================================================
 * Q19. 결합도 정의 – 데이터 결합도 설명 오류  [정답: ①]
 *  - topic_id = 11303 (모듈화/결합도·응집도 영역)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11303,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '결합도(Coupling)에 대한 설명으로 틀린 것은?',
  'A',
  '결합도(Coupling)는 두 모듈 간 상호의존성의 정도를 나타내며, 결합도가 낮을수록 변경 영향이 줄어들어 좋은 설계라고 봅니다.
데이터 결합도(Data Coupling)는 두 모듈이 “단순 데이터 값”을 매개변수로 주고받는 가장 낮은 수준의 결합도입니다. 보기 ①처럼 “자료 구조 형태로 전달될 때”라는 설명은 오히려 스탬프 결합도(Stamp Coupling)에 해당합니다.
내용 결합도(Content Coupling)는 한 모듈이 다른 모듈의 내부 구현(코드나 데이터 영역)을 직접 참조할 때 발생하는 가장 나쁜 결합도이며, 공통 결합도(Common Coupling)는 전역 데이터를 여러 모듈이 함께 사용할 때의 결합도입니다. 나머지 보기들은 이 정의와 일치합니다.',
  'past:2024-2:Q19'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '데이터 결합도는 자료 구조 형태로 전달되어 이용될 때 발생한다.', 1),
  (@q_id, 'B', '내용 결합도는 한 모듈이 직접 다른 모듈의 내용을 참조할 때 발생한다.', 0),
  (@q_id, 'C', '공통 결합도는 두 모듈이 동일한 전역 데이터를 접근할 때 발생한다.',      0),
  (@q_id, 'D', '결합도는 두 모듈 간 상호작용 또는 의존도의 정도를 나타낸다.',            0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');


/* =======================================================
 * Q20. CASE 도구 주요 기능  [정답: ④]
 *  - topic_id = 15202 (CASE/도구)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'CASE(Computer Aided Software Engineering)의 주요 기능으로 옳지 않은 것은?',
  'D',
  'CASE 도구는 요구분석부터 설계·구현·테스트·유지보수까지 소프트웨어 생명주기 전 단계를 지원하기 위한 도구로, 그래픽 기반 모델링, 산출물 추적, 다양한 개발 모형 지원 등 생산성과 일관성을 높이는 기능을 제공합니다.
“언어 번역” 자체는 CASE 도구의 핵심 목적이 아니며, 단순한 언어 번역 기능은 별도 도구 영역에 가깝습니다. 따라서 ④ 언어 번역은 CASE 주요 기능이라고 보기 어려워 옳지 않은 보기입니다.',
  'past:2024-2:Q20'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '소프트웨어 생명주기 전 단계의 연결 지원', 0),
  (@q_id, 'B', '그래픽 지원',                           0),
  (@q_id, 'C', '다양한 소프트웨어 개발 모형 지원',       0),
  (@q_id, 'D', '언어 번역',                             1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'PROCESS_WATERFALL_AGILE');
