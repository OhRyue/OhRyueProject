USE certpilot_study;

------------------------------------------------------------
-- Q11. UML 의존 관계 (Dependency)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302, -- 1.3.2 객체 지향 설계 (UML 관계)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'UML 모델에서 한 사물의 명세가 바뀌면 다른 사물에 영향을 주며, 일반적으로 한 클래스가 다른 클래스를 오퍼레이션의 매개 변수나 지역 변수로 사용할 때 나타나는 관계는 무엇인가?',
  'B',
  'UML에서 의존 관계(Dependency)는 한 요소의 정의가 변경되면 그 요소에 의존하는 다른 요소에도 영향을 줄 수 있는 느슨한 관계를 의미합니다. 일반적으로 한 클래스가 다른 클래스를 메서드의 매개변수나 지역 변수 타입으로 사용할 때 의존 관계로 표현합니다. 연관(Association)은 두 객체 사이에 지속적인 링크가 있는 구조적인 관계이고, 일반화(Generalization)는 상위/하위 클래스 사이의 상속 관계, 실체화(Realization)는 인터페이스와 이를 구현하는 클래스 사이의 관계를 나타냅니다.',
  'past:2024-3:Q11'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Dependency (의존)',     0),
  (@q_id, 'B', 'Association (연관)',    1),
  (@q_id, 'C', 'Generalization (일반화)', 0),
  (@q_id, 'D', 'Realization (실체화)',  0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');

------------------------------------------------------------
-- Q12. Rayleigh-Norden 곡선 / Putnam 모형
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15002, -- 5.2 IT 프로젝트 정보 시스템 구축관리 (비용·자원 계획과 연계)
  'WRITTEN',
  'MCQ',
  'HARD',
  'Rayleigh-Norden 곡선의 노력 분포도를 이용하여 소프트웨어 프로젝트의 작업량과 개발 기간 등을 추정하는 프로젝트 비용 산정 기법은 무엇인가?',
  'A',
  'Putnam 모형은 Rayleigh-Norden 곡선 형태로 인력 투입량이 시간에 따라 변화한다고 가정하고, 이를 기반으로 총 개발 노력과 개발 기간, 평균 인력 등을 추정하는 비용 산정 기법입니다. 델파이 모형은 전문가의 의견 수렴을 이용하는 정성적 추정 기법이고, 기능점수 모형은 기능점수(FP)를 이용해 규모를 추정한 뒤 비용을 환산하는 정량적 기법이며, COCOMO 모형은 LOC(라인 수) 기반의 경험적 회귀식을 이용해 노력과 비용을 계산하는 모형입니다.',
  'past:2024-3:Q12'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Putnam 모형',      1),
  (@q_id, 'B', '델파이(Delphi) 모형', 0),
  (@q_id, 'C', '기능점수(Function Point) 모형', 0),
  (@q_id, 'D', 'COCOMO 모형',      0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_COST_PM');

------------------------------------------------------------
-- Q13. 애자일 프로세스 모델 특징 (틀린 것)
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
  '애자일(Agile) 프로세스 모델에 대한 설명으로 가장 틀린 것은?',
  'A',
  '애자일 프로세스는 변화에 유연하게 대응하는 것을 목표로 하며, 사전에 모든 것을 상세하게 계획하기보다는 짧은 반복과 주기적인 피드백을 통해 요구를 반영합니다. 또한 프로세스와 도구보다는 개인과 상호작용을, 계약 협상보다는 고객과의 협력을, 방대한 문서보다는 동작하는 소프트웨어를 더 중시합니다. 따라서 “변화에 대한 대응보다는 자세한 계획을 중심으로 소프트웨어를 개발한다”는 설명은 전통적인 계획 기반(Plan-driven) 프로세스에 가까운 표현으로 애자일의 철학과 거리가 멉니다.',
  'past:2024-3:Q13'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '변화 대응보다 상세한 계획 수립을 중심으로 개발한다.', 1),
  (@q_id, 'B', '프로세스와 도구보다 개개인과 상호 소통을 중시한다.', 0),
  (@q_id, 'C', '계약 협상보다 고객과의 협력을 중시한다.',           0),
  (@q_id, 'D', '문서보다 실행 가능한 소프트웨어를 더 중시한다.',      0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'PROCESS_WATERFALL_AGILE');

------------------------------------------------------------
-- Q14. 디자인 패턴 구성 요소 (가장 거리가 먼 것)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302, -- 1.3.2 객체 지향 설계 (디자인 패턴)
  'WRITTEN',
  'MCQ',
  'EASY',
  '객체지향 소프트웨어 설계 시 사용하는 디자인 패턴을 구성하는 요소로서 가장 거리가 먼 것은?',
  'C',
  '디자인 패턴은 반복해서 등장하는 설계 문제와 그에 대한 해결 구조를 정리한 것으로, 일반적으로 패턴이 적용되는 문제와 배경, 해결 구조, 참여하는 클래스와 객체의 역할, 실제 적용 사례, 그리고 재사용 가능한 샘플 코드 등의 요소로 기술됩니다. 반면 개발자의 성명은 패턴의 이해나 재사용과 직접적인 관련이 없으므로 디자인 패턴의 구성 요소라고 보기는 어렵습니다.',
  'past:2024-3:Q14'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '패턴이 적용되는 문제와 배경',       0),
  (@q_id, 'B', '실제 적용된 사례와 활용 방식',       0),
  (@q_id, 'C', '패턴을 제안한 개발자의 성명',        1),
  (@q_id, 'D', '재사용 가능한 샘플 코드와 구조',     0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');

------------------------------------------------------------
-- Q15. 아키텍처 설계 과정 순서
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  10001, -- 1. 소프트웨어 설계 (상위: 아키텍처 설계 포함)
  'WRITTEN',
  'MCQ',
  'HARD',
  '다음은 소프트웨어 아키텍처 설계 과정의 주요 활동들이다. 올바른 수행 순서를 나열한 것은?

> ㉮ 설계 목표 설정  
> ㉯ 시스템 타입 결정  
> ㉰ 스타일 적용 및 커스터마이즈  
> ㉱ 서브시스템의 기능·인터페이스·동작 작성  
> ㉲ 아키텍처 설계 검토',
  'A',
  '아키텍처 설계에서는 먼저 품질 속성, 제약사항 등을 반영하여 설계 목표를 설정하고(㉮), 그 목표에 적합한 시스템 타입이나 구조 스타일을 결정합니다(㉯). 이후 선택한 스타일을 구체적인 시스템에 맞게 적용·커스터마이즈하고(㉰), 서브시스템별로 기능, 인터페이스, 동작을 설계합니다(㉱). 마지막으로 이해관계자와 함께 설계를 검토하여 품질 속성과 제약을 만족하는지 확인합니다(㉲). 따라서 ㉮ → ㉯ → ㉰ → ㉱ → ㉲ 순서가 올바릅니다.',
  'past:2024-3:Q15'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '㉮ → ㉯ → ㉰ → ㉱ → ㉲', 1),
  (@q_id, 'B', '㉲ → ㉮ → ㉯ → ㉱ → ㉰', 0),
  (@q_id, 'C', '㉮ → ㉲ → ㉯ → ㉱ → ㉰', 0),
  (@q_id, 'D', '㉮ → ㉯ → ㉰ → ㉲ → ㉱', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');

------------------------------------------------------------
-- Q16. 클래스 구성 요소: 속성 / 오퍼레이션
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  11302, -- 1.3.2 객체 지향 설계 (클래스 정의)
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음은 클래스에 대한 설명이다. 괄호 ㉠, ㉡에 들어갈 알맞은 용어의 조합은?

> 클래스는 각각의 객체들이 갖는 ( ㉠ )과(와) ( ㉡ )을 표현한다.  
> ( ㉠ )은(는) 클래스의 상태나 정보를 표현한다.  
> ( ㉡ )은(는) 클래스가 수행할 수 있는 동작으로, 함수이며 메소드라고도 한다.',
  'B',
  '객체지향에서 클래스는 객체들이 공통으로 가지는 속성과 오퍼레이션(연산)을 정의한 설계 틀입니다. 속성(Attribute)은 상태나 데이터를 표현하고, 오퍼레이션(Operation)은 그 클래스가 수행할 수 있는 동작이나 기능을 의미하며 메소드(Method)라고도 부릅니다. 제약조건은 값 범위를 제한하는 규칙이고, 관계는 클래스 간 연결 구조를 설명할 때 사용하는 용어입니다. 따라서 ㉠ 속성, ㉡ 오퍼레이션 조합이 정답입니다.',
  'past:2024-3:Q16'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '㉠ 제약조건, ㉡ 속성',     0),
  (@q_id, 'B', '㉠ 속성,     ㉡ 오퍼레이션', 1),
  (@q_id, 'C', '㉠ 오퍼레이션, ㉡ 제약조건', 0),
  (@q_id, 'D', '㉠ 속성,     ㉡ 관계',       0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'OOP_UML_PATTERN');

------------------------------------------------------------
-- Q17. 폭포수 모형 특징 (거리가 먼 것)
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
  '폭포수(Waterfall) 모형의 특징으로 가장 거리가 먼 것은?',
  'B',
  '폭포수 모형은 요구분석에서 유지보수까지의 단계를 위에서 아래로 흐르는 폭포처럼 순차적으로 진행하는 전통적인 개발 모형입니다. 각 단계의 산출물이 비교적 명확하고, 오랫동안 사용되어 온 만큼 적용 경험과 성공 사례가 많은 편입니다. 반면 나선형 모형의 단점을 보완하기 위해 등장한 것이 아니라, 오히려 폭포수 모형의 한계를 보완하고 위험 관리를 강화하기 위해 나선형 모형이 제안된 것으로 보는 것이 일반적입니다. 따라서 “나선형 모형의 단점을 보완하기 위한 모형이다”라는 설명이 폭포수 모형의 특징과 가장 거리가 멉니다.',
  'past:2024-3:Q17'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '단계를 순차적으로 진행하는 접근 방법을 사용한다.', 0),
  (@q_id, 'B', '나선형 모형의 단점을 보완하기 위해 제안된 모형이다.', 1),
  (@q_id, 'C', '각 단계의 정의와 산출물이 비교적 명확하다.',       0),
  (@q_id, 'D', '적용 경험과 성공 사례가 많이 축적되어 있다.',        0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'PROCESS_WATERFALL_AGILE');

------------------------------------------------------------
-- Q18. CASE 원천 기술 (아닌 것)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  10001, -- 1. 소프트웨어 설계 (CASE 도구 활용)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 중 CASE(Computer-Aided Software Engineering)를 가능하게 한 원천 기술이 아닌 것은?',
  'D',
  'CASE 도구는 프로토타이핑을 통해 요구를 검증하고, 구조적 기법으로 분석·설계를 형식화하며, 다양한 개발 산출물을 일관되게 관리하기 위한 정보 저장소(Repository) 기술 등 여러 기반 기술을 조합하여 구현됩니다. 반면 단순한 일괄처리 기술은 배치 작업 수행을 위한 운영 방식으로, CASE 도구의 핵심 원천 기술이라고 보기는 어렵습니다. 그래서 보기 중에서 “일괄처리 기술”이 CASE의 원천 기술과 가장 거리가 있습니다.',
  'past:2024-3:Q18'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '프로토타이핑 기술',   0),
  (@q_id, 'B', '구조적 기법',        0),
  (@q_id, 'C', '정보 저장소 기술',    0),
  (@q_id, 'D', '일괄처리 기술',      1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_REQ_MODEL');

------------------------------------------------------------
-- Q19. fan-in / fan-out 개념 (정답값: 3, 2)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source, image_url
) VALUES (
  1,
  12201, -- 2.2.1 모듈 구현 (모듈 구조, fan-in/out)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 프로그램 구조에서 모듈 F의 fan-in과 fan-out의 수를 올바르게 나타낸 것은?
(도식 예: 여러 상위 모듈이 F를 호출하고, F가 몇 개의 하위 모듈을 호출하는 구조를 가정)',
  'B',
  'fan-in은 특정 모듈을 호출하거나 참조하는 상위 모듈의 개수를 의미하고, fan-out은 특정 모듈이 직접 호출하는 하위 모듈의 개수를 의미합니다. 제시된 구조에서는 세 개의 상위 모듈이 F를 호출하고, F가 두 개의 하위 모듈을 호출하므로 fan-in은 3, fan-out은 2가 됩니다. fan-in과 fan-out은 모듈의 응집도, 결합도 분석이나 유지보수성 평가에 활용되는 대표적인 구조적 설계 지표입니다.',
  'past:2024-3:Q19',
  'https://api.mycertpilot.com/static/images/questions/q_2024_03_19.png'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'fan-in : 2, fan-out : 3', 0),
  (@q_id, 'B', 'fan-in : 3, fan-out : 2', 1),
  (@q_id, 'C', 'fan-in : 1, fan-out : 2', 0),
  (@q_id, 'D', 'fan-in : 2, fan-out : 1', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');

------------------------------------------------------------
-- Q20. HIPO(Hierarchy Input Process Output) 특징 (거리가 먼 것)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  10001, -- 1. 소프트웨어 설계 (구조적 설계 기법)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'HIPO(Hierarchy Input Process Output)에 대한 설명으로 가장 거리가 먼 것은?',
  'A',
  'HIPO는 시스템의 기능과 입력·처리·출력 관계를 계층적으로 표현하는 구조적 설계 및 문서화 도구입니다. 보통 상위 기능에서 하위 기능으로 내려가는 하향식 설계와 잘 어울리며, 가시적 도표, 총체적 도표, 세부적 도표 등의 차트로 구성됩니다. 또한 기능과 자료의 의존 관계를 함께 표현할 수 있어 이해하기 쉽다는 장점이 있습니다. 반면 HIPO를 “상향식 소프트웨어 개발을 위한 문서화 도구”라고 하는 설명은 실제 특징과 반대되는 서술이므로 가장 거리가 먼 설명입니다.',
  'past:2024-3:Q20'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '상향식 소프트웨어 개발을 위한 문서화 도구이다.', 1),
  (@q_id, 'B', '가시적·총체적·세부적 도표로 구성된 차트 표현을 사용한다.', 0),
  (@q_id, 'C', '기능과 자료의 의존 관계를 동시에 표현할 수 있다.',       0),
  (@q_id, 'D', '보기 쉽고 이해하기 쉬운 문서화 도구이다.',                0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_REQ_MODEL');
