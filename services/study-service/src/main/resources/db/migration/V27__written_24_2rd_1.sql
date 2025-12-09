USE certpilot_study;

/* =======================================================
 * Q1. Requirements Engineering (요구공학)  [정답: ③]
 *  - topic_id = 11102 (1.1.2 요구사항 확인)
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
  '요구공학(Requirements Engineering)에 대한 설명으로 옳지 않은 것은?',
  'C',
  '요구공학은 “무엇을 개발해야 하는지”를 정의·분석·검증·관리하는 전 과정을 다루는 학문입니다. 요구사항 개발/분석/검증/관리 등의 활동을 포괄하며, 복잡해지는 요구와 잦은 변경 속에서 품질을 높이고 실패 가능성을 줄이기 위한 체계적인 접근을 제공합니다.
①, ②, ④는 요구공학의 목적과 등장 배경을 잘 설명하지만, ③처럼 “요구사항 개발 활동의 한 요소”라고 보는 것은 요구공학의 범위를 지나치게 축소한 설명입니다. 오히려 요구사항 개발이 요구공학의 하위 활동에 해당하므로 ③이 옳지 않습니다.',
  'past:2024-2:Q01'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '무엇을 개발해야 하는지 요구사항을 정의·분석·관리하는 프로세스를 연구하는 학문이다.', 0),
  (@q_id, 'B', '사용자 요구가 복잡해지고 잦은 변경이 발생하자 이를 적절히 관리하기 위해 등장하였다.', 0),
  (@q_id, 'C', '요구공학은 요구사항 개발 활동의 한 요소이다.', 1),
  (@q_id, 'D', '품질 개선과 프로젝트 실패 최소화를 목적으로 한다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_REQ_MODEL');


/* =======================================================
 * Q2. XP (eXtreme Programming)  [정답: ④]
 *  - topic_id = 15101 (5.1.1 소프트웨어 개발방법론 선정)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15101,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'XP(eXtreme Programming)에 대한 설명으로 옳지 않은 것은?',
  'D',
  'XP는 짧은 반복(iteration), 지속적인 통합(Continuous Integration), 자동화된 테스트, 고객 상주와 적극적인 참여를 강조하는 대표적인 애자일 방법론입니다. 작은 단위로 빠르게 개발·배포하면서도 테스트 중심으로 품질을 확보하는 것이 핵심입니다.
① 릴리즈 주기를 짧게 가져가 요구 변화에 빠르게 대응하고, ② 작업이 끝날 때마다 코드를 지속적으로 통합하며, ③ 자동화된 단위 테스트를 반복해서 수행한다는 설명은 모두 XP의 특징에 부합합니다.
반면 ④처럼 “개발 책임자가 모든 책임을 지고 팀원들은 책임 없이 자유롭게 개발한다”는 내용은 XP의 집단 코드 소유, 팀 차원의 책임 공유 문화와 정반대이므로 옳지 않은 설명입니다.',
  'past:2024-2:Q02'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '릴리즈 기간을 짧게 반복하여 고객 요구 변화에 빠르게 대응한다.', 0),
  (@q_id, 'B', '코드들은 하나의 작업이 마무리될 때마다 지속적으로 통합한다.', 0),
  (@q_id, 'C', '테스트 자동화 도구를 사용하여 테스트가 지속적으로 수행되도록 한다.', 0),
  (@q_id, 'D', '개발 책임자가 모든 책임을 지고 팀원들은 책임 없이 자유롭게 개발한다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'PROCESS_WATERFALL_AGILE');


/* =======================================================
 * Q3. UML 다이어그램 매칭  [정답: ①]
 *  - topic_id = 11302 (1.3.2 객체 지향 설계)
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
  'UML에서 다이어그램 이름과 설명의 연결이 올바르지 않은 것은?',
  'A',
  '클래스 다이어그램은 클래스, 속성, 연관관계, 상속 구조 등 시스템의 “정적 구조”를 표현하는 다이어그램입니다. 시퀀스 다이어그램처럼 시간 흐름에 따라 객체들이 주고받는 메시지와 상호작용을 표현하는 것은 동적 다이어그램(시퀀스/커뮤니케이션 다이어그램)의 역할입니다.
배치 다이어그램은 실행 환경의 노드와 컴포넌트 배치를, 유스케이스 다이어그램은 사용자 관점의 기능 요구를, 활동 다이어그램은 처리 절차·분기·병행 흐름을 나타내므로 각각의 설명은 일반적인 정의와 부합합니다. 따라서 클래스 다이어그램을 동적 상호작용 표현으로 설명한 ①이 잘못된 연결입니다.',
  'past:2024-2:Q03'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '클래스 다이어그램: 시퀀스 다이어그램처럼 객체들이 주고받는 메시지와 상호작용을 표현한다.', 1),
  (@q_id, 'B', '배치 다이어그램: 결과물·프로세스·컴포넌트 등 물리적 요소들의 배치를 표현한다.', 0),
  (@q_id, 'C', '유스케이스 다이어그램: 사용자의 요구를 분석하는 기능 모델링에 사용된다.', 0),
  (@q_id, 'D', '활동 다이어그램: 처리 로직이나 조건에 따른 처리 흐름을 순서대로 표현한다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * Q4. HIPO 관련 도표 설명  [정답: ④]
 *  - topic_id = 15202 (5.2.2 SW 구축관리)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 설명에 해당하는 도표는 무엇인가?

> "시스템 기능을 여러 고유 모듈로 분할하고,
> 이들 간 인터페이스를 계층 구조로 표현하는 문서화 도구로,
> 가시적 도표(Visual Table of Contents), 총체적 도표(Overview Diagram),
> 세부적 도표(Detail Diagram)로 구성된다."',
  'D',
  '이 설명은 HIPO(Hierarchy Input Process Output) 차트의 특징을 말합니다. HIPO는 상위 기능에서 하위 기능으로 점진적으로 세분화하면서, 각 모듈의 입력(Input)·처리(Process)·출력(Output)을 계층 구조로 문서화하는 도구입니다.
번다운 차트는 스프린트 잔여 작업량 추이를, 순서도는 제어 흐름을, 단순 시각 도표는 특정 구조 없이 그림 위주로만 정보를 정리하는 경우가 많습니다. 계층적 기능 분해와 “가시적/총체적/세부적 도표” 구성을 모두 언급하는 것은 HIPO 차트이므로 ④가 정답입니다.',
  'past:2024-2:Q04'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '번다운 차트(Burn-down Chart)', 0),
  (@q_id, 'B', '순서도(Flow Chart)', 0),
  (@q_id, 'C', '시각 도표(Visual Diagram)', 0),
  (@q_id, 'D', 'HIPO 차트(Hierarchy Input Process Output)', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_REQ_MODEL');


/* =======================================================
 * Q5. 싱글톤 패턴  [정답: ①]
 *  - topic_id = 11302 (1.3.2 객체 지향 설계)
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
  '불필요한 메모리 낭비를 줄이기 위해 “여러 프로세스가 동시에 참조할 수는 없지만,
  어디서든 하나의 동일한 객체를 참조”하도록 만드는 디자인 패턴은?',
  'A',
  '싱글톤(Singleton) 패턴은 애플리케이션 전체에서 단 하나의 인스턴스만 생성되도록 제한하고, 전역에서 이 인스턴스에 접근할 수 있는 통로를 제공하는 생성(Creational) 패턴입니다. 전역 상태를 하나로 통합해야 하는 설정 객체, 커넥션 풀 등에서 자주 사용됩니다.
옵서버 패턴은 상태 변경을 여러 관찰자에게 통보하는 행위(Behavioral) 패턴이고, 프로토타입 패턴은 복제를 통해 객체를 생성하는 패턴, 상태 패턴은 상태 객체를 캡슐화하여 상태에 따라 행위를 바꾸는 패턴입니다. 따라서 설명에 가장 잘 부합하는 것은 싱글톤 패턴입니다.',
  'past:2024-2:Q05'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '싱글톤 패턴(Singleton)', 1),
  (@q_id, 'B', '옵서버 패턴(Observer)', 0),
  (@q_id, 'C', '프로토타입 패턴(Prototype)', 0),
  (@q_id, 'D', '상태 패턴(State)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * Q6. 객체가 수행할 구체적 연산 (메소드)  [정답: ②]
 *  - topic_id = 11302 (1.3.2 객체 지향 설계)
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
  '객체지향 기법에서 “객체가 메시지를 받았을 때 수행해야 할 구체적인 연산”을 정의한 것은?',
  'B',
  '객체지향에서 객체는 속성(필드)과 연산(메소드)으로 구성됩니다. 메시지는 “무엇을 해 달라”는 요청이고, 메소드는 그 메시지를 받았을 때 실제로 실행되는 구체적인 처리 로직(함수 몸체)에 해당합니다.
엔터티(Entity)는 주로 데이터 집합을 의미하고, 클래스(Class)는 객체 생성을 위한 설계도, 인스턴스(Instance)는 클래스로부터 생성된 실제 객체이므로 “메시지에 대한 연산”이라는 설명에는 메소드(Method)가 가장 알맞습니다.',
  'past:2024-2:Q06'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '엔터티(Entity)', 0),
  (@q_id, 'B', '메소드(Method)', 1),
  (@q_id, 'C', '클래스(Class)', 0),
  (@q_id, 'D', '인스턴스(Instance)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * Q7. 바람직한 설계 지침이 아닌 것  [정답: ④]
 *  - topic_id = 11301 (1.3.1 공통 모듈 설계)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11301,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '바람직한 소프트웨어 설계 지침이 아닌 것은?',
  'D',
  '좋은 설계 지침은 일반적으로 낮은 결합도·높은 응집도, 복잡도 감소, 중복 최소화, 일관성 유지, 명확한 제어 구조 등을 강조합니다. 이는 변경에 강하고 재사용이 쉬운 구조를 만들기 위한 공통된 원칙입니다.
반면 ④처럼 “모듈의 크기를 가능한 작게만 구성하여 병행성 수준을 높인다”는 극단적인 세분화는 모듈 수만 불필요하게 증가시키고, 인터페이스 복잡도를 높여 전체 유지보수를 어렵게 만들 수 있습니다. 따라서 바람직한 설계 지침이라고 보기 어렵기 때문에 ④가 오답입니다.',
  'past:2024-2:Q07'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '결합도를 최소화하고 응집도를 최대화한다.', 0),
  (@q_id, 'B', '복잡도와 중복성을 줄이고 일관성을 유지한다.', 0),
  (@q_id, 'C', '하나의 입구와 하나의 출구를 갖도록 제어구조를 설계한다.', 0),
  (@q_id, 'D', '모듈의 크기를 가능한 작게만 구성하여 병행성 수준을 높인다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');


/* =======================================================
 * Q8. 객체지향 설계 원칙 (SOLID)  [정답: ④]
 *  - topic_id = 11302 (1.3.2 객체 지향 설계)
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
  '객체지향 설계 원칙(SOLID)에 대한 설명 중 틀린 것은?',
  'D',
  '각 선택지는 SOLID 원칙 중 일부를 요약한 문장입니다.
- OCP(Open-Closed Principle): 기존 코드를 수정하지 않고 기능을 확장할 수 있도록 설계해야 한다.
- LSP(Liskov Substitution Principle): 자식 클래스는 부모 타입으로 치환되어도 정상적으로 동작해야 한다.
- DIP(Dependency Inversion Principle): 구체 클래스가 아니라 인터페이스·추상 클래스와 같은 고수준 추상에 의존해야 한다.
ISP(Interface Segregation Principle)는 “인터페이스를 작고 역할별로 잘게 나누어, 클라이언트가 자신에게 필요 없는 메소드에 의존하지 않도록 한다”는 원칙입니다. “객체는 단 하나의 책임만 가져야 한다”는 설명은 SRP(Single Responsibility Principle)에 해당하므로 ISP 설명으로는 틀렸고, 따라서 ④가 오답입니다.',
  'past:2024-2:Q08'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'OCP: 기존 코드를 변경하지 않고 기능을 추가할 수 있도록 설계해야 한다.', 0),
  (@q_id, 'B', 'LSP: 자식 클래스는 최소한 부모 클래스에서 가능한 행위는 수행할 수 있어야 한다.', 0),
  (@q_id, 'C', 'DIP: 추상성이 낮은 클래스보다 추상성이 높은 클래스에 의존하도록 설계해야 한다.', 0),
  (@q_id, 'D', 'ISP: 객체는 단 하나의 책임만 가져야 한다는 원칙이다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * Q9. Coad-Yourdon 객체지향 분석 방법  [정답: ②]
 *  - topic_id = 11302 (1.3.2 객체 지향 설계 / 분석 방법론)
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
  '다음 설명에 해당하는 객체지향 분석 방법론은?

  “미시적(Micro) 개발 프로세스와 거시적(Macro) 개발 프로세스를 모두 사용하며,
  클래스와 객체를 분석·식별하고 클래스의 속성과 연산을 정의하는 방법이다.”',
  'B',
  'Coad-Yourdon 객체지향 분석 방법론은 객체 모델(클래스·객체·관계), 동적 모델, 기능 모델을 구분하고, 미시적(Micro)·거시적(Macro) 개발 프로세스를 모두 사용하는 것이 특징입니다. 특히 클래스와 객체를 식별하고, 각 클래스의 속성과 연산(메소드)을 정의해 나가는 절차가 강조됩니다.
Booch, Jacobson, Wirfs-Brock 등의 방법론도 객체지향 분석/설계 방법이지만, “Micro/Macro 프로세스를 모두 사용한다”는 문장과 가장 직접적으로 연결되는 것은 Coad-Yourdon 방법이므로 ②가 정답입니다.',
  'past:2024-2:Q09'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Booch 방법', 0),
  (@q_id, 'B', 'Coad-Yourdon 방법', 1),
  (@q_id, 'C', 'Jacobson 방법', 0),
  (@q_id, 'D', 'Wirfs-Brock 방법', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * Q10. HIPO Chart 설명 중 틀린 것  [정답: ②]
 *  - topic_id = 15202 (5.2.2 SW 구축관리)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202,
  'WRITTEN',
  'MCQ',
  'EASY',
  'HIPO 차트(HIPO Chart)에 대한 설명으로 틀린 것은?',
  'B',
  'HIPO는 하향식 소프트웨어 개발을 지원하는 문서화 도구로, 가시적 도표(Visual), 총체적 도표(Overview), 세부적 도표(Detail)로 구성되며 기능과 자료의 의존 관계를 동시에 표현할 수 있고 구조가 직관적이라는 장점이 있습니다. 즉, 시스템 구조와 처리 흐름을 한눈에 파악하기 쉽게 해 주는 것이 목적입니다.
따라서 “충분한 사전 지식과 학습 없이는 이해하기 어렵다”는 설명은 HIPO의 장점과 정반대되는 표현으로, 틀린 설명에 해당합니다.',
  'past:2024-2:Q10'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '가시적 도표·총체적 도표·세부적 도표로 구성된다.', 0),
  (@q_id, 'B', '충분한 사전 지식과 학습이 없으면 이해하기 어렵다.', 1),
  (@q_id, 'C', '기능과 자료의 의존 관계를 동시에 표현할 수 있다.', 0),
  (@q_id, 'D', '하향식 소프트웨어 개발을 위한 문서화 도구이다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_REQ_MODEL');
