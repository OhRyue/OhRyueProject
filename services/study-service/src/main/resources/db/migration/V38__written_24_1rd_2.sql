USE certpilot_study;

/* =======================================================
 * 2024-1 Q11. XP(eXtreme Programming) 설명 중 틀린 것  [정답: ①]
 * topic_id = 15101 (5.1.1 소프트웨어 개발방법론 선정)
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
  'XP(eXtreme Programming)에 대한 설명으로 틀린 것은 무엇인가?',
  'A',
  'XP(익스트림 프로그래밍)는 고객과의 긴밀한 협업, 짧은 반복 주기, 지속적인 통합, 테스트 중심 개발 등을 강조하는 대표적인 애자일(Agile) 방법론입니다.
각 선택지를 하나씩 살펴보면 다음과 같습니다.

① 빠른 개발을 위해 테스트를 수행하지 않는다.
  → XP의 핵심 실천 항목 중 하나가 바로 “테스트 주도 개발(TDD)”과 “지속적인 테스트”입니다.
    빠르게 개발하되, 자동화된 테스트를 꾸준히 실행해서 품질을 확보하는 것이 XP의 중요한 특징입니다.
    따라서 “빠른 개발을 위해 테스트를 수행하지 않는다”는 설명은 XP와 정반대이므로 틀린 문장입니다.

② 사용자의 요구사항은 언제든지 변할 수 있다.
  → XP는 요구사항 변경을 자연스러운 것으로 보고, 짧은 반복(iteration)과 지속적인 피드백으로 변화에 유연하게 대응하는 것을 목표로 합니다. 올바른 설명입니다.

③ 고객과 직접 대면하며 요구사항을 이야기하기 위해 사용자 스토리(User Story)를 활용할 수 있다.
  → XP에서는 고객과 개발자가 가까운 거리에 있고, 고객이 요구사항을 “사용자 스토리” 형태로 제시하는 방식을 많이 사용합니다. 역시 타당한 설명입니다.

④ 기존 방법론들에 비해 실용주의(Pragmatism)를 강조한 것으로 볼 수 있다.
  → XP는 복잡한 문서보다 실제 작동하는 소프트웨어, 지속적인 커뮤니케이션, 피드백을 통해 실용적인 개발을 지향합니다.
    형식보다 실질적인 가치를 중시한다는 면에서 실용주의를 강조한다고 볼 수 있습니다.

따라서 XP에 대한 설명으로 틀린 것은 ①입니다.',
  'past:2024-1:Q11'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '빠른 개발을 위해 테스트를 수행하지 않는다.', 1),
  (@q_id, 'B', '사용자의 요구사항은 언제든지 변할 수 있다.', 0),
  (@q_id, 'C', '고객과 직접 대면하며 요구사항을 이야기하기 위해 사용자 스토리를 활용할 수 있다.', 0),
  (@q_id, 'D', '기존의 방법론에 비해 실용성(Pragmatism)을 강조한 것이라고 볼 수 있다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'PROCESS_WATERFALL_AGILE');


/* =======================================================
 * 2024-1 Q12. CLI 사용자 인터페이스  [정답: ②]
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
  'EASY',
  'DOS, UNIX 등 운영체제에서 대표적으로 사용되던 것으로,
정해진 명령 문자열을 입력하여 시스템을 조작하는 사용자 인터페이스(User Interface)는 무엇인가?',
  'B',
  '문제에서 말하는 것은 “명령 문자열을 입력해서 시스템을 조작하는 방식”입니다.
이는 대표적인 명령줄 기반 인터페이스로, CLI(Command Line Interface)를 의미합니다.

각 선택지를 보시면,
① GUI(Graphical User Interface): 아이콘, 버튼, 창 등의 그래픽 요소를 사용해 마우스나 터치로 조작하는 방식입니다.
② CLI(Command Line Interface): 키보드로 명령어를 직접 입력하여 시스템을 제어하는 방식으로, DOS나 UNIX 셸 등이 대표적인 예입니다.
③ CUI(Cell User Interface): 일반적으로 사용되지 않는 용어로, UI 분류로 보기는 어렵습니다.
④ MUI(Mobile User Interface): 모바일 기기(스마트폰, 태블릿 등)를 위한 UI를 의미합니다.

따라서 설명에 해당하는 것은 ② CLI(Command Line Interface)입니다.',
  'past:2024-1:Q12'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'GUI(Graphical User Interface)', 0),
  (@q_id, 'B', 'CLI(Command Line Interface)', 1),
  (@q_id, 'C', 'CUI(Cell User Interface)', 0),
  (@q_id, 'D', 'MUI(Mobile User Interface)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'UI_UX');


/* =======================================================
 * 2024-1 Q13. UML 정적 다이어그램이 아닌 것  [정답: ③]
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
  '다음 UML 다이어그램 중 “정적(Structural) 다이어그램”이 아닌 것은 어느 것인가?',
  'C',
  'UML 다이어그램은 크게 정적(구조) 다이어그램과 동적(행위) 다이어그램으로 나눌 수 있습니다.

대표적인 정적 다이어그램에는 다음이 포함됩니다.
- 클래스 다이어그램(Class Diagram)
- 객체 다이어그램(Object Diagram)
- 컴포넌트 다이어그램(Component Diagram)
- 배치(배포) 다이어그램(Deployment Diagram)
- 패키지 다이어그램(Package Diagram) 등

반면 동적 다이어그램은 시스템의 동작, 상호작용, 시간에 따른 변화 등을 표현하며,
그 예로는 다음이 있습니다.
- 시퀀스 다이어그램(Sequence Diagram)
- 통신(커뮤니케이션) 다이어그램
- 상태 다이어그램(State Diagram)
- 활동 다이어그램(Activity Diagram) 등

각 선택지를 보면,
A. 배치 다이어그램: 시스템의 물리적 배치 구조를 나타내는 정적 다이어그램입니다.
B. 컴포넌트 다이어그램: 컴포넌트 간의 구조를 나타내는 정적 다이어그램입니다.
C. 순차(시퀀스) 다이어그램: 객체들 사이의 메시지 교환을 시간 순서대로 표현하는 동적 다이어그램입니다.
D. 패키지 다이어그램: 패키지 간의 의존 관계를 표현하는 정적 다이어그램입니다.

따라서 정적 다이어그램이 아닌 것은 ③ 순차(시퀀스) 다이어그램이며, 정답은 ③입니다.',
  'past:2024-1:Q13'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '배치(Deployment) 다이어그램', 0),
  (@q_id, 'B', '컴포넌트(Component) 다이어그램', 0),
  (@q_id, 'C', '순차(Sequence) 다이어그램', 1),
  (@q_id, 'D', '패키지(Package) 다이어그램', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * 2024-1 Q14. UI 설계 도구 - 목업  [정답: ①]
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
  '다음 내용이 설명하는 사용자 인터페이스(UI) 설계 도구는 무엇인가?

> 디자인, 사용 방법 설명, 평가 등을 위해 실제 화면과 유사하게 만든 정적인 형태의 모형
> 시각적으로만 구성 요소를 배치하는 것으로, 일반적으로 실제로 구현되지는 않음',
  'A',
  '지문에서 강조하는 특징은
- 실제 화면과 유사하게 만든 정적인 형태의 모형
- 시각적으로만 구성 요소를 배치하며, 일반적으로 실제로 구현되지는 않음
입니다.

이는 UI 설계 도구 중 “목업(Mockup)”의 전형적인 정의입니다.
- 목업(Mockup): 실제 화면과 유사하게 만든 정적인 형태의 시안으로,
  디자인 방향성 확인, 사용 방법 설명, 평가 등에 활용되지만 동적인 동작은 하지 않습니다.
- 스토리보드(Storyboard): 여러 화면(목업/와이어프레임)을 시나리오 흐름에 따라 배열하고,
  각 화면의 설명·이동 경로·처리 로직 등을 함께 기술한 설계 문서입니다.
- 유스케이스(Use Case): 사용자가 목표를 달성하기 위해 수행하는 절차를 시나리오로 표현한 요구 분석 도구입니다.
- 프로토타입(Prototype): 실제와 유사하게 인터랙션이 가능한 동적인 시제품으로, 사용성 테스트에 활용됩니다.

따라서 문제에서 설명하는 UI 설계 도구는 ① 목업(Mockup)입니다.',
  'past:2024-1:Q14'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '목업(Mockup)',         1),
  (@q_id, 'B', '스토리보드(Storyboard)', 0),
  (@q_id, 'C', '유스케이스(Use Case)',   0),
  (@q_id, 'D', '프로토타입(Prototype)',  0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'UI_UX');


/* =======================================================
 * 2024-1 Q15. 비기능적 요구에 대한 설명  [정답: ②]
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
  '요구사항 분석에서 비기능적 요구(Nonfunctional Requirement)에 대한 설명으로 옳은 것은 어느 것인가?',
  'B',
  '요구사항은 일반적으로 “기능적 요구(무엇을 할 것인가)”와 “비기능적 요구(어떤 품질과 제약을 가질 것인가)”로 나눕니다.

비기능적 요구에는 다음과 같은 것들이 포함됩니다.
- 성능(처리량, 응답 시간 등)
- 보안, 안전성
- 신뢰성, 가용성, 유지보수성
- 사용성, 확장성, 제약 조건(플랫폼, 규격 등)

각 선택지를 살펴보면,
① 시스템의 처리량, 반응 시간 등의 성능 요구나 품질 요구는 비기능적 요구에 해당하지 않는다.
  → 처리량, 응답 시간, 품질 요구는 전형적인 비기능적 요구에 해당하므로, 이 진술은 틀렸습니다.

② “차량 대여 시스템이 제공하는 모든 화면이 3초 이내에 사용자에게 보여야 한다”는 비기능적 요구이다.
  → 시간 제약(응답 시간)에 대한 요구로, 성능/사용성 관련 비기능 요구의 전형적인 예입니다. 올바른 설명입니다.

③ 시스템 구축과 관련된 안전, 보안에 대한 요구사항들은 비기능적 요구에 해당하지 않는다.
  → 안전, 보안은 대표적인 비기능적 요구이므로 이 설명은 틀렸습니다.

④ “금융 시스템은 조회, 인출, 입금, 송금의 기능이 있어야 한다”는 비기능적 요구이다.
  → 이는 시스템이 제공해야 할 구체적인 “기능” 목록이므로 기능적 요구에 해당합니다.

따라서 비기능적 요구에 대한 올바른 설명은 ②이고, 정답은 ②입니다.',
  'past:2024-1:Q15'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '시스템의 처리량, 반응 시간 등의 성능 요구나 품질 요구는 비기능적 요구에 해당하지 않는다.', 0),
  (@q_id, 'B', '“차량 대여 시스템이 제공하는 모든 화면이 3초 이내에 사용자에게 보여야 한다”는 비기능적 요구이다.', 1),
  (@q_id, 'C', '시스템 구축과 관련된 안전, 보안에 대한 요구사항들은 비기능적 요구에 해당하지 않는다.', 0),
  (@q_id, 'D', '“금융 시스템은 조회, 인출, 입금, 송금의 기능이 있어야 한다”는 비기능적 요구이다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_REQ_MODEL');


/* =======================================================
 * 2024-1 Q16. 컴포넌트의 정의  [정답: ③]
 * topic_id = 11301 (1.3.1 공통 모듈 설계)
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
  '명백한 역할을 가지고 독립적으로 존재할 수 있는 시스템의 부분으로,
넓은 의미에서는 재사용되는 모든 단위라고 볼 수 있으며
인터페이스를 통해서만 접근할 수 있는 것은 무엇인가?',
  'C',
  '문제에서 설명하는 것은 “명확한 역할을 가진 독립적인 부분”이고,
“재사용되는 단위”이며, “인터페이스를 통해 접근”한다는 특징을 가지고 있습니다.
이 설명은 소프트웨어 공학에서 말하는 컴포넌트(Component)의 정의와 일치합니다.

컴포넌트는 다음과 같은 특성을 가집니다.
- 명확한 기능과 책임을 가진 모듈 단위입니다.
- 외부에 공개된 인터페이스(입·출력, 서비스)를 통해서만 접근할 수 있습니다.
- 내부 구현은 숨기고, 재사용이 가능하도록 설계됩니다.

반면,
- Sheet, Cell 등은 일반적으로 스프레드시트의 구성 요소를 떠올리게 하는 용어로, 소프트웨어 컴포넌트의 정의와는 거리가 있습니다.
- Model은 MVC 패턴 등에서 데이터·비즈니스 로직을 의미할 수 있지만,
  “인터페이스로만 접근하는 재사용 단위”라는 좁은 의미의 컴포넌트를 직접 가리키지는 않습니다.

따라서 정답은 ③ Component입니다.',
  'past:2024-1:Q16'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Sheet', 0),
  (@q_id, 'B', 'Model', 0),
  (@q_id, 'C', 'Component', 1),
  (@q_id, 'D', 'Cell', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');


/* =======================================================
 * 2024-1 Q17. SOLID 원칙에 속하지 않는 것  [정답: ④]
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
  '다음 중 SOLID라고 불리는 객체지향 설계 원칙에 속하지 않는 것은 어느 것인가?',
  'D',
  'SOLID는 객체지향 설계의 다섯 가지 핵심 원칙의 머리글자를 딴 약어입니다.
각 원칙은 다음과 같습니다.
- S: SRP (Single Responsibility Principle) – 단일 책임 원칙
- O: OCP (Open-Closed Principle) – 개방-폐쇄 원칙
- L: LSP (Liskov Substitution Principle) – 리스코프 치환 원칙
- I: ISP (Interface Segregation Principle) – 인터페이스 분리 원칙
- D: DIP (Dependency Inversion Principle) – 의존성 역전 원칙

각 선택지를 살펴보면,
① ISP(Interface Segregation Principle): 클라이언트가 자신에게 필요 없는 메소드에 의존하지 않도록 인터페이스를 분리하자는 원칙으로, SOLID의 I에 해당합니다.
② DIP(Dependency Inversion Principle): 고수준 모듈이 저수준 구현이 아닌 추상(인터페이스, 추상 클래스 등)에 의존하도록 설계하자는 원칙으로, SOLID의 D에 해당합니다.
③ LSP(Liskov Substitution Principle): 자식 타입이 부모 타입으로 치환되어도 정상적으로 동작해야 한다는 원칙으로, SOLID의 L에 해당합니다.
④ SSO(Single Sign On): 한 번 인증으로 여러 시스템에 로그인할 수 있게 하는 인증/보안 관련 개념으로, 설계 원칙 SOLID와는 무관한 용어입니다.

따라서 SOLID 원칙에 속하지 않는 것은 ④ SSO입니다.',
  'past:2024-1:Q17'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'ISP(Interface Segregation Principle)', 0),
  (@q_id, 'B', 'DIP(Dependency Inversion Principle)', 0),
  (@q_id, 'C', 'LSP(Liskov Substitution Principle)', 0),
  (@q_id, 'D', 'SSO(Single Sign On)', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * 2024-1 Q18. UML 스테레오타입 표기 기호  [정답: ①]
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
  'UML 확장 모델에서 스테레오타입(stereotype) 객체를 표현할 때 사용하는 기호로 알맞은 것은?',
  'A',
  'UML에서는 메타 모델을 확장하기 위해 “스테레오타입(stereotype)”이라는 개념을 사용합니다.
스테레오타입은 기존 요소에 의미를 확장하거나 특별한 역할을 부여할 때 사용하며,
표기법으로는 이름을 꺽쇠 괄호(<< >>)로 감싸는 형식을 사용합니다.

예를 들어,
- <<interface>>
- <<controller>>
- <<entity>>
와 같이 표현합니다.

따라서 스테레오타입을 나타내는 기호는 << >> 이므로 정답은 ①입니다.',
  'past:2024-1:Q18'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '<< >>', 1),
  (@q_id, 'B', '(( ))', 0),
  (@q_id, 'C', '{{ }}', 0),
  (@q_id, 'D', '[[ ]]', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');


/* =======================================================
 * 2024-1 Q19. CASE의 원천 기술이 아닌 것  [정답: ④]
 * topic_id = 15101 (5.1.1 소프트웨어 개발방법론 선정)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15101,
  'WRITTEN',
  'MCQ',
  'HARD',
  '다음 중 CASE(Computer-Aided Software Engineering)의 원천 기술이 아닌 것은 어느 것인가?',
  'D',
  'CASE 도구는 소프트웨어 개발 전 과정을 자동화·반자동화하여 생산성과 품질을 높이기 위한 도구입니다.
이를 가능하게 하는 여러 “원천 기술”들이 있는데, 대표적으로 다음이 포함됩니다.
- 프로토타이핑 기술: 빠르게 시제품을 만들어 요구사항을 명확히 하고 검증하는 기술
- 구조적 기법(Structured Techniques): 구조적 분석/설계 기법, DFD, 구조적 차트 등
- 정보 저장소(Repository) 기술: 분석/설계/구현 산출물들을 공통 저장소에 통합 관리하는 기술

선지별로 보면,
① 프로토타이핑 기술: CASE의 핵심 원천 기술 중 하나입니다.
② 구조적 기법: 초기 CASE 도구들은 구조적 분석/설계 기법을 기반으로 한 것이 많습니다.
③ 정보 저장소 기술: CASE 도구의 중심이 되는 것이 공통 저장소(Repository)이므로 역시 핵심 기술입니다.
④ 일괄처리 기술(Batch Processing): 오래된 처리 방식 중 하나이지만, CASE 도구의 고유한 원천 기술로 보기는 어렵습니다.
  CASE는 개발 공정의 자동화·통합을 지향하는데, 단순한 일괄처리 기술은 이를 뒷받침하는 핵심 기술이라고 보기 어렵습니다.

따라서 CASE의 원천 기술이 아닌 것은 ④ 일괄처리 기술입니다.',
  'past:2024-1:Q19'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '프로토타이핑 기술', 0),
  (@q_id, 'B', '구조적 기법', 0),
  (@q_id, 'C', '정보 저장소(Repository) 기술', 0),
  (@q_id, 'D', '일괄처리(Batch) 기술', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'PROCESS_WATERFALL_AGILE');


/* =======================================================
 * 2024-1 Q20. 상태 다이어그램에서 전이의 요인이 되는 요소  [정답: ①]
 * topic_id = 11302 (1.3.2 객체 지 향 설계)
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
  '상태(State) 다이어그램에서 객체가 한 상태에서 다른 상태로 전이(Transition)하게 되는 요인이 되는 요소는 무엇인가?',
  'A',
  'UML 상태(State) 다이어그램은 “객체가 어떤 상태들을 가지며, 어떤 사건에 의해 다른 상태로 전이하는지”를 표현하는 다이어그램입니다.
이때 전이(Transition)를 일으키는 직접적인 원인은 “이벤트(Event)”입니다.

구성 요소를 정리하면 다음과 같습니다.
- State(상태): 객체가 일정 시간 동안 만족하는 조건이나 모드
- Event(이벤트): 상태 전이를 유발하는 사건 (신호, 호출, 조건 충족 등)
- Transition(전이): 한 상태에서 다른 상태로의 이동을 나타내는 화살표
- Message(메시지): 객체 간 상호작용에서 전달되는 호출/신호 등으로, 시퀀스 다이어그램에서 주로 강조됩니다.

따라서 상태 다이어그램에서 객체 전이의 요인이 되는 요소는 Event(이벤트)이며,
정답은 ①입니다.',
  'past:2024-1:Q20'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Event(이벤트)', 1),
  (@q_id, 'B', 'State(상태)', 0),
  (@q_id, 'C', 'Transition(전이)', 0),
  (@q_id, 'D', 'Message(메시지)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');
