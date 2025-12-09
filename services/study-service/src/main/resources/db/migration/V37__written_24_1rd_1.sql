SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id    := 1;

/* =======================================================
 * 2024-1 Q1. Coad-Yourdon 객체지향 분석 방법  [정답: ①]
 * topic_id = 11302 (1.3.2 객체 지향 설계)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '객체지향 분석 방법론 중 E-R 다이어그램을 사용하여 객체의 행위를 모델링하고,
객체 식별, 구조 식별, 주체 정의, 속성과 관계 정의, 서비스 정의 등의 과정으로 구성되는 것은 무엇인가?',
  'A',
  '문제에서 제시한 특징은 Coad-Yourdon 객체지향 분석 방법론의 대표적인 설명입니다.

Coad-Yourdon 방법은 다음과 같은 특징을 가집니다.
- E-R 다이어그램을 활용하여 객체와 그 관계를 모델링합니다.
- 시스템에서 요구되는 객체를 식별하고, 객체의 속성과 연산(서비스)을 정의합니다.
- 객체들 간의 관계(연관, 집합, 분해 등)를 구조적으로 표현합니다.
- 이런 과정을 통해 분석 단계에서부터 객체 지향적인 관점을 유지하면서 설계로 자연스럽게 이어질 수 있도록 도와줍니다.

반면 Booch, Jacobson, Wirfs-Brock 등도 객체지향 분석/설계 방법론이지만,
E-R 모델링을 중심으로 한 위와 같은 기술적인 설명과 가장 직접적으로 연결되는 것은 Coad-Yourdon 방법입니다.
따라서 정답은 ① Coad-Yourdon 방법입니다.',
  'past:2024-1:Q01'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Coad-Yourdon 방법', 1),
  (@q_id, 'B', 'Booch 방법', 0),
  (@q_id, 'C', 'Jacobson 방법', 0),
  (@q_id, 'D', 'Wirfs-Brock 방법', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * 2024-1 Q2. 트랜잭션 감시·제어 미들웨어  [정답: ③]
 * topic_id = 15202 (5.2.2 SW 구축관리)
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
  '트랜잭션이 올바르게 처리되고 있는지 데이터를 감시하고 제어하는 역할을 하는 미들웨어는 무엇인가?',
  'C',
  '여기서 핵심 키워드는 "트랜잭션이 올바르게 처리되고 있는지 데이터를 감시하고 제어하는 미들웨어"입니다.
이 설명은 트랜잭션 처리와 관련된 미들웨어인 TP monitor(Transaction Processing monitor)를 가리킵니다.

TP 모니터는 다음과 같은 역할을 합니다.
- 여러 사용자로부터 들어오는 트랜잭션 요청을 관리하고 스케줄링합니다.
- 트랜잭션의 시작, 커밋, 롤백 등을 제어하여 데이터의 일관성을 유지합니다.
- 분산 환경에서 여러 서버에 걸친 트랜잭션 처리도 조정할 수 있습니다.

선지별로 보면,
- RPC(Remote Procedure Call): 원격 프로시저 호출 메커니즘으로, 단순히 원격 함수를 호출하는 통신 방법입니다.
- ORB(Object Request Broker): CORBA 환경에서 객체 간 메시지 전달을 중개하는 미들웨어입니다.
- HUB: 네트워크 장비의 하나로 단순한 신호 분배 기능을 합니다.
이들은 트랜잭션을 감시·제어하는 미들웨어라고 보기는 어렵습니다.

반면 TP monitor는 바로 이러한 트랜잭션 처리 흐름을 관리하는 미들웨어이므로,
정답은 ③ TP monitor입니다.',
  'past:2024-1:Q02'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'RPC(Remote Procedure Call)', 0),
  (@q_id, 'B', 'ORB(Object Request Broker)', 0),
  (@q_id, 'C', 'TP monitor', 1),
  (@q_id, 'D', 'HUB', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');


/* =======================================================
 * 2024-1 Q3. 자료 흐름도(DFD)의 구성 요소  [정답: ②]
 * topic_id = 11103 (1.1.3 분석 모델 확인)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11103,
  'WRITTEN',
  'MCQ',
  'EASY',
  '자료 흐름도(Data Flow Diagram)의 기본 구성 요소 조합으로 옳은 것은 어느 것인가?',
  'B',
  '자료 흐름도(DFD)는 시스템을 데이터의 흐름 관점에서 표현하는 도구로,
일반적으로 다음 네 가지 기본 요소로 구성됩니다.
- 프로세스(Process)
- 데이터 흐름(Data Flow)
- 데이터 저장소(Data Store)
- 단말/외부 엔티티(Terminator)

선지별로 보면,
① process, data flow, data store, comment
  → comment는 DFD의 기본 구성 요소가 아닙니다.
② process, data flow, data store, terminator
  → 프로세스, 데이터 흐름, 데이터 저장소, 단말의 조합으로 정석적인 구성입니다.
③ data flow, data store, terminator, data dictionary
  → data dictionary는 설계/분석을 보조하는 도구이지만 DFD의 그림 요소는 아닙니다.
④ process, data store, terminator, mini-spec
  → mini-spec(소단위 명세서) 역시 보조 문서이지 DFD 도형 요소는 아닙니다.

따라서 DFD의 기본 구성 요소를 올바르게 나열한 것은 ②이므로,
정답은 ② process, data flow, data store, terminator입니다.',
  'past:2024-1:Q03'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'process, data flow, data store, comment', 0),
  (@q_id, 'B', 'process, data flow, data store, terminator', 1),
  (@q_id, 'C', 'data flow, data store, terminator, data dictionary', 0),
  (@q_id, 'D', 'process, data store, terminator, mini-spec', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_REQ_MODEL');


/* =======================================================
 * 2024-1 Q4. 정보 은닉과 가장 밀접한 객체지향 개념  [정답: ①]
 * topic_id = 11302 (1.3.2 객체 지향 설계)
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
  '객체지향에서 정보 은닉(Information Hiding)과 가장 밀접한 관계가 있는 개념은 무엇인가?',
  'A',
  '정보 은닉은 객체 내부의 구현 세부 사항을 외부에서 직접 접근하지 못하도록 숨기고,
공개된 인터페이스만을 통해서만 접근하도록 만드는 개념입니다.
이 개념과 가장 밀접하게 연결되는 것이 바로 캡슐화(Encapsulation)입니다.

각 용어를 정리해 보면,
- Encapsulation: 데이터와 이를 조작하는 메소드를 하나의 단위(클래스)로 묶고,
  외부에는 필요한 부분만 공개하여 내부 구현을 숨기는 개념입니다.
- Class: 객체 생성을 위한 설계도(틀)이며, 캡슐화를 구현하는 단위입니다.
- Method: 객체가 수행하는 연산(동작)을 의미합니다.
- Instance: 클래스로부터 실제로 생성된 구체적인 객체입니다.

정보 은닉의 직접적인 개념 자체는 캡슐화라고 볼 수 있기 때문에,
정답은 ① Encapsulation입니다.',
  'past:2024-1:Q04'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Encapsulation', 1),
  (@q_id, 'B', 'Class', 0),
  (@q_id, 'C', 'Method', 0),
  (@q_id, 'D', 'Instance', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * 2024-1 Q5. 자료 사전에서 선택(Selection)을 의미하는 기호  [정답: ①]
 * topic_id = 13302 (3.3.2 데이터 모델링 및 설계)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  13302,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '자료 사전(Data Dictionary)에서 "선택(Selection)"의 의미를 나타내는 기호로 옳은 것은 무엇인가?',
  'A',
  '자료 사전에서는 데이터의 구조를 간단한 표기법으로 표현하기 위해 여러 기호를 사용합니다.
대표적인 의미는 다음과 같습니다.
- "+" : 순차(Sequence) 또는 AND 구조, 즉 둘 다 포함되는 경우
- "|" 또는 "[ ]" : 선택(Selection), 여러 후보 중 하나를 선택하는 경우
- "{ }" : 반복(Iteration), 0회 이상 또는 여러 번 반복되는 구조
- "=" : 정의(Definition), 해당 항목이 어떻게 구성되는지 정의할 때 사용

문제에서 묻는 "선택"은 여러 후보 중 하나를 고르는 의미이므로,
선택을 나타내는 기호인 대괄호 표기 "[ ]"가 해당합니다.

따라서 정답은 ① [ ] 입니다.',
  'past:2024-1:Q05'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '[ ]', 1),
  (@q_id, 'B', '{ }', 0),
  (@q_id, 'C', '+', 0),
  (@q_id, 'D', '=', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_META_CRUD');


/* =======================================================
 * 2024-1 Q6. 요구 분석 과정 설명 중 거리가 먼 것  [정답: ②]
 * topic_id = 11102 (1.1.2 요구사항 확인)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11102,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '소프트웨어 개발 단계에서 요구 분석 과정에 대한 설명으로 가장 거리가 먼 것은 어느 것인가?',
  'B',
  '요구 분석 과정은 사용자의 요구를 체계적으로 수집·정리·분석하여
"무엇을 할 것인가"를 명확히 정의하는 단계입니다. 이 단계에서 요구사항 명세를 잘 정리해 두면
설계·구현·테스트 등 이후 단계의 품질과 생산성에 큰 영향을 줍니다.

선지별로 살펴보면,
① 분석 결과를 문서화하여 향후 유지보수에 활용한다.
   → 요구사항 명세서, 분석 모델 등의 문서는 유지보수와 변경 관리에 매우 중요하므로 적절한 설명입니다.
② 개발 비용이 가장 많이 소요되는 단계이다.
   → 일반적으로 개발 비용은 설계/구현/테스트 단계 등에서 더 많이 소요되며,
     요구 분석이 프로젝트 전체 비용에서 "가장 많이" 차지한다고 보기는 어렵습니다.
③ 자료 흐름도, 자료 사전 등을 효과적으로 활용할 수 있다.
   → 요구 분석 단계에서 DFD, 자료 사전, UML 등 분석 도구가 많이 사용되므로 타당한 설명입니다.
④ 보다 구체적인 명세를 위해 소단위 명세서(Mini-Spec)를 활용할 수 있다.
   → 프로세스 단위의 상세 동작을 명세하는 데 Mini-Spec을 사용하므로 적절한 설명입니다.

따라서 다른 선택지들은 요구 분석의 특징을 잘 설명하고 있지만,
②처럼 요구 분석이 "개발 비용이 가장 많이 소요되는 단계"라는 표현은 일반적인 관점과 거리가 멉니다.
정답은 ②입니다.',
  'past:2024-1:Q06'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '분석 결과를 문서화하여 향후 유지보수에 활용될 수 있다.', 0),
  (@q_id, 'B', '개발 비용이 가장 많이 소요되는 단계이다.', 1),
  (@q_id, 'C', '자료 흐름도와 자료 사전 등이 효과적으로 이용될 수 있다.', 0),
  (@q_id, 'D', '보다 구체적인 명세를 위해 소단위 명세서(Mini-Spec)를 활용할 수 있다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_REQ_MODEL');


/* =======================================================
 * 2024-1 Q7. Rumbaugh 분석 기법 - 정보 모델링(Object 모델)  [정답: ①]
 * topic_id = 11302 (1.3.2 객체 지향 설계)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '럼바우(Rumbaugh) 객체지향 분석 기법에서 "정보 모델링"이라고도 하며,
시스템에서 요구되는 객체를 찾아내고 속성과 연산을 식별하며 객체들 간 관계를 규정하여
다이어그램으로 표현하는 모델링은 무엇인가?',
  'A',
  'Rumbaugh의 객체지향 분석 기법에서는 세 가지 주요 모델을 사용합니다.
- 객체 모델(Object Model): 객체, 클래스, 속성, 연관관계 등을 표현하는 정적 구조 모델
- 동적 모델(Dynamic Model): 상태 변화, 이벤트, 상태도 등을 표현하는 동적 행위 모델
- 기능 모델(Functional Model): 데이터 흐름, 처리 기능 등을 표현하는 모델

문제에서 "정보 모델링이라고도 하며, 시스템에서 요구되는 객체를 찾아내어 속성과 연산을 식별하고
객체들 간 관계를 규정하여 다이어그램으로 표시하는 모델링"이라고 했는데,
이는 바로 객체 모델(Object Model)에 대한 설명입니다.

따라서 정답은 ① Object 모델입니다.',
  'past:2024-1:Q07'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Object 모델', 1),
  (@q_id, 'B', 'Dynamic 모델', 0),
  (@q_id, 'C', 'Function 모델', 0),
  (@q_id, 'D', 'Static 모델', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * 2024-1 Q8. UML 다이어그램 설명 중 틀린 것  [정답: ④]
 * topic_id = 11302 (1.3.2 객체 지향 설계)
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
  'UML(Unified Modeling Language)에 대한 설명 중 틀린 것은 어느 것인가?',
  'D',
  '각 문장은 UML의 주요 다이어그램과 모델을 설명하고 있습니다. 하나씩 확인해 보겠습니다.

① 기능적 모델은 사용자 측면에서 본 시스템 기능을 나타내며, UML에서는 유스케이스 다이어그램(Use Case Diagram)을 사용한다.
   → 사용자의 목표와 시스템이 제공해야 할 기능을 표현하므로 올바른 설명입니다.

② 정적 모델은 객체, 속성, 연관관계, 오퍼레이션 등 시스템의 구조를 나타내며, UML에서는 클래스 다이어그램(Class Diagram)을 사용한다.
   → 시스템의 정적인 구조를 표현하는 것이 클래스 다이어그램의 역할이므로 적절한 설명입니다.

③ 동적 모델은 시스템의 내부 동작과 시간에 따른 상태 변화를 나타내며,
   UML에서는 시퀀스 다이어그램(Sequence Diagram), 상태 다이어그램(State Diagram), 활동 다이어그램(Activity Diagram) 등을 사용한다.
   → 객체 사이의 메시지 흐름, 상태 변화, 활동 흐름 등을 표현한다는 점에서 맞는 설명입니다.

④ State Diagram은 객체들 사이의 메시지 교환을 나타내며,
   Sequence Diagram은 하나의 객체가 가진 상태와 그 상태 변화에 따른 동작 순서를 나타낸다.
   → 이 부분이 서로 뒤바뀐 설명입니다.
   - 상태 다이어그램(State Diagram)은 한 객체의 "상태 변화"와 그에 따른 이벤트·전이(Transition)를 표현합니다.
   - 시퀀스 다이어그램(Sequence Diagram)은 여러 객체들 사이에서 "메시지가 시간 순서대로 어떻게 오가는지"를 표현합니다.
   따라서 ④는 두 다이어그램의 역할을 서로 뒤바꾼 틀린 설명입니다.

따라서 정답은 ④입니다.',
  'past:2024-1:Q08'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '기능적 모델은 사용자 측면의 시스템 기능을 표현하며 유스케이스 다이어그램을 사용한다.', 0),
  (@q_id, 'B', '정적 모델은 객체, 속성, 연관관계, 오퍼레이션 등 시스템 구조를 표현하며 클래스 다이어그램을 사용한다.', 0),
  (@q_id, 'C', '동적 모델은 시스템의 내부 동작을 표현하며 시퀀스, 상태, 활동 다이어그램 등을 사용한다.', 0),
  (@q_id, 'D', '상태 다이어그램은 객체 간 메시지 교환을, 시퀀스 다이어그램은 한 객체의 상태 변화에 따른 동작 순서를 나타낸다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * 2024-1 Q9. UI 특성 중 틀린 것  [정답: ②]
 * topic_id = 11201 (1.2.1 UI 요구사항 확인)
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
  '사용자 인터페이스(UI)의 일반적인 특징으로 틀린 것은 어느 것인가?',
  'B',
  '좋은 사용자 인터페이스(UI)는 사용자가 시스템을 더 쉽고 빠르게 사용할 수 있도록 돕는 것이 목적입니다.
따라서 오류를 줄이고, 작업 효율을 높이며, 사용자 중심의 상호작용을 제공하는 방향으로 설계됩니다.

선지별로 보면,
① 구현하고자 하는 결과의 오류를 최소화한다.
   → 사용자가 입력·조작 과정에서 실수할 가능성을 줄이고 오류를 예방하는 것은 좋은 UI의 특징입니다.

② 사용자의 편의성을 높임으로써 작업시간을 증가시킨다.
   → 사용자의 편의성이 높아지면 일반적으로 작업 시간은 "감소"해야 합니다.
     편리한 UI라면 사용자가 더 빨리, 더 적은 단계로 작업을 끝낼 수 있어야 하기 때문입니다.
     따라서 "편의성 증가 → 작업시간 증가"라는 표현은 일반적인 UI 목표와 반대입니다.

③ 막연한 작업 기능에 대해 구체적인 방법을 제시하여 준다.
   → UI를 통해 사용자가 어떤 순서로 무엇을 해야 하는지 안내해 주는 것은 바람직한 특성입니다.

④ 사용자 중심의 상호 작용이 되도록 한다.
   → 시스템 관점이 아니라 사용자 관점에서 상호작용을 설계하는 것이 중요합니다.

따라서 틀린 설명은 ②이며, 정답은 ②입니다.',
  'past:2024-1:Q09'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '구현하고자 하는 결과의 오류를 최소화한다.', 0),
  (@q_id, 'B', '사용자의 편의성을 높임으로써 작업시간을 증가시킨다.', 1),
  (@q_id, 'C', '막연한 작업 기능에 대해 구체적인 방법을 제시한다.', 0),
  (@q_id, 'D', '사용자 중심의 상호 작용이 되도록 한다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_REQ_MODEL');


/* =======================================================
 * 2024-1 Q10. GoF 디자인 패턴 설명 중 틀린 것  [정답: ③]
 * topic_id = 11302 (1.3.2 객체 지향 설계)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 GoF(Gangs of Four) 디자인 패턴에 대한 설명으로 틀린 것은 어느 것인가?',
  'C',
  '각 선택지는 GoF에서 다루는 대표적인 디자인 패턴들에 대한 설명입니다. 하나씩 살펴보겠습니다.

① Factory Method 패턴은 상위 클래스에서 객체를 생성하는 인터페이스를 정의하고,
   실제 어떤 구체 클래스를 생성할지는 하위 클래스에서 결정하도록 하는 생성(Creational) 패턴입니다.
   → 올바른 설명입니다.

② Prototype 패턴은 원형이 되는 Prototype 객체를 먼저 하나 만들어 두고,
   이를 복제(Clone)하여 새로운 인스턴스를 생성하는 구조입니다.
   → 역시 전형적인 Prototype 패턴 설명입니다.

③ Bridge 패턴은 기존에 구현되어 있는 클래스에 기능이 추가될 때,
   기존 클래스를 그대로 두고 중간에서 맞춰주는 형태라고 설명하고 있으나,
   일반적으로 Bridge 패턴의 핵심은 "추상(abstraction)과 구현(implementation)의 분리"입니다.
   즉, 기능 계층과 구현 계층을 분리하여 서로 독립적으로 확장할 수 있도록 하는 구조이며,
   단순히 기존 클래스를 감싸서 재사용하는 정도라면 어댑터(Adapter) 패턴에 더 가깝습니다.
   따라서 이 설명은 Bridge 패턴의 의도를 부정확하게 표현하고 있어 틀린 설명입니다.

④ Mediator 패턴은 여러 객체가 서로 직접 통신하지 않고 중재자(Mediator)를 통해 상호작용하도록 하여
   객체 간 결합도를 낮추는 패턴입니다. "객체 간의 통제와 지시 역할을 하는 중재자를 둔다"는 설명은 맞는 표현입니다.

따라서 틀린 설명은 ③ Bridge 패턴에 대한 설명이며, 정답은 ③입니다.',
  'past:2024-1:Q10'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Factory Method 패턴은 상위 클래스에서 객체 생성 인터페이스를 정의하고 하위 클래스에서 인스턴스를 생성하도록 한다.', 0),
  (@q_id, 'B', 'Prototype 패턴은 원형 객체(Prototype)를 먼저 만든 뒤 이를 복제하여 인스턴스를 생성한다.', 0),
  (@q_id, 'C', 'Bridge 패턴은 기존 구현 클래스에 기능이 생길 때 기존 클래스를 재사용하도록 중간에서 맞춰주는 역할을 한다.', 1),
  (@q_id, 'D', 'Mediator 패턴은 객체 간 통제와 지시 역할을 하는 중재자를 두어 객체지향의 목표를 달성하게 한다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');
