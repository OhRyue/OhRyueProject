------------------------------------------------------------
-- Q1. 텍스트 기반 UI (CLI)
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
  '사용자 인터페이스(UI) 중에서 명령 입력과 출력 결과가 모두 텍스트 형태로만 이루어지는 인터페이스를 무엇이라고 하는가?',
  'C',
  'CLI(Command Line Interface)는 키보드로 명령어를 텍스트 형태로 입력하고, 결과도 텍스트로 출력하는 방식의 사용자 인터페이스입니다. GUI는 아이콘·윈도우·마우스를 사용하는 그래픽 인터페이스이고, NUI는 제스처·음성·터치 등 자연스러운 동작을 이용하는 인터페이스입니다. OUI는 일반적으로 객체(Object) 기반 UI 등을 가리키는 용어로, 본 보기의 정의와는 거리가 있습니다.',
  'past:2024-3:Q01'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'GUI (Graphical User Interface)', 0),
  (@q_id, 'B', 'NUI (Natural User Interface)',   0),
  (@q_id, 'C', 'CLI (Command Line Interface)',   1),
  (@q_id, 'D', 'OUI (Object-based User Interface 등)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_REQ_MODEL');

------------------------------------------------------------
-- Q2. XP(eXtreme Programming) 설명 (틀린 것)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15101, -- 5.1.1 소프트웨어 개발방법론 선정
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 XP(eXtreme Programming)에 대한 설명으로 옳지 않은 것은?',
  'D',
  'XP는 고객의 요구 변화를 빠르게 반영하기 위해 짧은 반복(iteration)과 잦은 릴리즈를 사용하는 애자일 방법론입니다. 또한 작은 단위의 기능을 구현할 때마다 지속적으로 통합(Continuous Integration)을 수행하고, 테스트 주도 개발과 자동화된 테스트 도구를 활용하여 품질을 확보합니다. 따라서 개발 책임자 한 명이 모든 책임을 지고 팀원들은 책임 없이 자유롭게 개발한다는 설명은 XP의 협업·책임 공유 문화와 정반대이므로 오답입니다.',
  'past:2024-3:Q02'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '짧은 릴리즈 주기를 반복하여 요구 변화에 빠르게 대응한다.', 0),
  (@q_id, 'B', '코드는 하나의 작업이 마무리될 때마다 지속적으로 통합한다.', 0),
  (@q_id, 'C', '테스트 자동화 도구를 활용하여 지속적으로 테스트를 수행한다.', 0),
  (@q_id, 'D', '개발 책임자가 모든 책임을 지며 팀원들은 책임 없이 자유롭게 개발한다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'PROCESS_WATERFALL_AGILE');

------------------------------------------------------------
-- Q3. 자료 흐름도(DFD) 구성요소
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11103, -- 1.1.3 분석 모델 확인
  'WRITTEN',
  'MCQ',
  'EASY',
  '자료 흐름도(DFD, Data Flow Diagram)를 구성하는 기본 요소로 옳은 것은?',
  'B',
  '자료 흐름도(DFD)는 시스템을 데이터 관점에서 표현하기 위한 도구로, 프로세스(Process), 자료 흐름(Data Flow), 자료 저장소(Data Store), 단말(Terminator)을 기본 구성 요소로 사용합니다. comment나 mini-spec, data dictionary 등은 설계 문서이긴 하지만 DFD의 기본 기호 요소는 아닙니다. 따라서 process, data flow, data store, terminator 조합이 정답입니다.',
  'past:2024-3:Q03'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'process, data flow, data store, comment', 0),
  (@q_id, 'B', 'process, data flow, data store, terminator', 1),
  (@q_id, 'C', 'data flow, data store, terminator, data dictionary', 0),
  (@q_id, 'D', 'process, data store, terminator, mini-spec', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_REQ_MODEL');

------------------------------------------------------------
-- Q4. 미들웨어(Middleware) 설명 (틀린 것)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  14102, -- 4.1.2 서버 프로그램 구현 (플랫폼/미들웨어 맥락)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '분산 시스템 환경에서 사용되는 미들웨어(Middleware)에 대한 설명으로 틀린 것은?',
  'D',
  '미들웨어는 분산 환경에서 서로 다른 컴포넌트, 서버, 애플리케이션 간의 통신과 데이터 교환을 중개하는 소프트웨어 계층입니다. 위치 투명성(Location Transparency)을 제공하여 클라이언트가 서버의 실제 위치를 의식하지 않고 서비스에 접근할 수 있게 하고, 여러 컴포넌트에서 공통으로 사용하는 재사용 가능한 서비스를 제공합니다. 따라서 “애플리케이션과 사용자 사이에서만 분산 서비스를 제공한다”는 설명은 범위를 지나치게 제한한 표현으로 틀린 설명입니다. 미들웨어는 주로 애플리케이션과 애플리케이션, 애플리케이션과 데이터베이스/서버 사이를 중개합니다.',
  'past:2024-3:Q04'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '분산 시스템에서 다양한 부분을 관리·통신·데이터 교환하게 해주는 소프트웨어이다.', 0),
  (@q_id, 'B', '위치 투명성(Location Transparency)을 제공한다.', 0),
  (@q_id, 'C', '여러 컴포넌트가 요구하는 재사용 가능한 서비스 구현을 제공한다.', 0),
  (@q_id, 'D', '애플리케이션과 사용자 사이에서만 분산 서비스를 제공한다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');

------------------------------------------------------------
-- Q5. 디자인 패턴 장·단점 (거리가 먼 것)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302, -- 1.3.2 객체 지향 설계
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 디자인 패턴 사용의 장단점에 대한 설명으로 가장 거리가 먼 것은?',
  'D',
  '디자인 패턴은 검증된 설계 구조를 재사용함으로써 소프트웨어 구조를 이해하기 쉽게 만들고, 객체지향 설계와 구현의 생산성을 높여주며, 재사용 가능한 설계를 제공함으로써 개발 시간을 단축하는 데 도움을 줍니다. 반면, 절차형 언어와 함께 사용할 때 효율이 극대화된다는 설명은 디자인 패턴이 주로 객체지향 언어를 전제로 정립되었다는 점과 맞지 않습니다. 따라서 절차형 언어와의 결합을 강조한 선택지가 본문에서 가장 거리가 먼 설명입니다.',
  'past:2024-3:Q05'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '소프트웨어 구조를 이해하기 쉽게 만들어 구조 파악이 용이하다.', 0),
  (@q_id, 'B', '객체지향 설계 및 구현의 생산성을 높이는 데 적합하다.', 0),
  (@q_id, 'C', '재사용 가능한 설계를 통해 개발 시간을 단축하는 데 도움이 된다.', 0),
  (@q_id, 'D', '절차형 언어와 함께 이용될 때 효율이 극대화된다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');

------------------------------------------------------------
-- Q6. UML 스테레오타입 표기 기호
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302, -- 1.3.2 객체 지향 설계 (UML)
  'WRITTEN',
  'MCQ',
  'EASY',
  'UML 확장 메커니즘에서 스테레오타입(stereotype) 객체를 표현할 때 사용하는 기호로 알맞은 것은?',
  'A',
  'UML에서 스테레오타입은 기존 메타모델 요소를 확장하기 위한 메커니즘으로, «control», «entity»와 같이 이름 앞뒤에 홑화살괄호(<< >>)를 붙여 표기합니다. (( )), [[ ]], {{ }} 등의 기호는 UML 표준 스테레오타입 표기와는 무관합니다. 따라서 << >> 형태의 표기가 정답입니다.',
  'past:2024-3:Q06'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '<< >>', 1),
  (@q_id, 'B', '(( ))', 0),
  (@q_id, 'C', '[[ ]]', 0),
  (@q_id, 'D', '{{ }}', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');

------------------------------------------------------------
-- Q7. 플랫폼 성능 특성 분석 측정 항목이 아닌 것
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  10001, -- 1. 소프트웨어 설계 (상위 토픽에 매핑)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '소프트웨어 설계 시 구축된 플랫폼의 성능 특성을 분석할 때 사용하는 측정 항목이 아닌 것은?',
  'B',
  '플랫폼 성능 특성을 분석할 때는 일반적으로 응답 시간(Response Time), 사용률(Utilization), 가용성(Availability) 등과 같이 수치로 측정 가능한 지표를 사용합니다. 서버 튜닝(Server Tuning)은 성능을 개선하기 위한 행위나 작업 유형에 해당하며, 그 자체가 측정 지표는 아닙니다. 따라서 “서버 튜닝”은 성능 특성을 나타내는 측정 항목이라고 보기 어려워 오답입니다.',
  'past:2024-3:Q07'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '응답 시간(Response Time)', 0),
  (@q_id, 'B', '서버 튜닝(Server Tuning)', 1),
  (@q_id, 'C', '가용성(Availability)',     0),
  (@q_id, 'D', '사용률(Utilization)',       0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');

------------------------------------------------------------
-- Q8. 프로토타입 모형
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15101, -- 5.1.1 소프트웨어 개발방법론 선정
  'WRITTEN',
  'MCQ',
  'EASY',
  '사용자의 요구사항을 정확히 파악하기 위해 실제 개발될 소프트웨어의 견본품을 먼저 만들어 최종 결과물을 예측하는 개발 모형은 무엇인가?',
  'D',
  '프로토타입 모형은 사용자의 요구를 명확히 파악하기 위해 초기 견본품(Prototype)을 신속하게 제작한 뒤, 이를 반복적으로 수정·보완하면서 최종 시스템을 완성해 가는 개발 모형입니다. 애자일이나 나선형 모형도 반복·점진적 개발이라는 공통점이 있지만, “견본품을 실제로 만들어 보면서 요구를 구체화한다”는 설명과 가장 직접적으로 연결되는 것은 프로토타입 모형입니다.',
  'past:2024-3:Q08'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '애자일(Agile) 모형',   0),
  (@q_id, 'B', '나선형(Spiral) 모형', 0),
  (@q_id, 'C', '폭포수(Waterfall) 모형', 0),
  (@q_id, 'D', '프로토타입(Prototype) 모형', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'PROCESS_WATERFALL_AGILE');

------------------------------------------------------------
-- Q9. Coad-Yourdon 객체지향 분석 방법론
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302, -- 1.3.2 객체 지향 설계 / 분석 방법론 연계
  'WRITTEN',
  'MCQ',
  'HARD',
  '다음 중 Coad-Yourdon 객체지향 분석 방법론에 대한 설명으로 옳은 것은?',
  'A',
  'Coad-Yourdon 객체지향 분석 방법론은 객체, 클래스, 속성, 구조 등을 중심으로 모델링하며, ERD 개념을 응용해 객체의 행위와 데이터를 함께 모델링하는 데 초점을 둔 방법론으로 자주 소개됩니다. 객체의 동적·기능 모델로만 나누어 수행한다거나, 미시/거시 개발 프로세스의 구분을 핵심으로 삼는 방법, 또는 Use Case를 가장 핵심으로 강조하는 방법론은 다른 분석·설계 방법론의 특징과 섞인 설명입니다. 따라서 ER 다이어그램을 활용하여 객체의 행위까지 포함해 데이터 모델링에 중점을 둔다는 설명이 Coad-Yourdon 방법론에 가장 가깝습니다.',
  'past:2024-3:Q09'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'E-R 다이어그램을 활용해 객체의 행위까지 포함하여 데이터 모델링에 초점을 둔 방법이다.', 1),
  (@q_id, 'B', '객체를 동적 모델과 기능 모델로만 나누어 분석하는 방법이다.', 0),
  (@q_id, 'C', '미시적·거시적 개발 프로세스를 모두 사용하는 하이브리드 공정에 초점을 둔 방법이다.', 0),
  (@q_id, 'D', 'Use Case만을 핵심으로 강조하는 분석 방법에 해당한다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');

------------------------------------------------------------
-- Q10. 객체지향 분류/일반화 관계
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302, -- 1.3.2 객체 지향 설계
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '객체지향 기법에서 동일한 형의 특성을 갖는 객체들을 모아 구성한 것을 클래스로 보고, 이들 사이의 관계를 ‘‘is a kind of’’ 관계로 설명할 때 사용하는 용어는 무엇인가?',
  'B',
  '객체지향에서 일반화(Generalization)는 상위 클래스와 하위 클래스 사이의 ‘‘is-a’’ 관계를 의미합니다. 여러 하위 클래스들이 공통 속성과 연산을 상위 클래스로부터 일반화하여 상속받는 구조를 표현할 때 사용합니다. 분류화(Classification)는 객체와 클래스 간의 ‘‘is-instance-of’’ 관계를 설명하는 용어이고, 연관화(Association)는 서로 협력하는 객체들 간의 링크를, 집단화(Aggregation)는 전체-부분 관계를 표현할 때 사용합니다. 따라서 보기에서 서술한 관계에 가장 부합하는 용어는 일반화입니다.',
  'past:2024-3:Q10'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '분류화(Classification)', 0),
  (@q_id, 'B', '일반화(Generalization)', 1),
  (@q_id, 'C', '연관화(Association)',    0),
  (@q_id, 'D', '집단화(Aggregation)',    0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');