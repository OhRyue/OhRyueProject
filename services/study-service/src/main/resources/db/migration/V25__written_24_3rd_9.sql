USE certpilot_study;

/* =======================================================
 * 81 ~ 90 : 정보시스템 구축 관리
 *  - cert_id = 1 (정보처리기사)
 *  - mode = 'WRITTEN'
 *  - type = 'MCQ'
 * ======================================================= */


/* =======================================================
 * Q81. 정보보안 3요소(CIA) (SEC_POLICY)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15301, -- 5.3.1 소프트웨어 개발 보안 설계
  'WRITTEN',
  'MCQ',
  'EASY',
  '소프트웨어 개발에서 정보보안 요소에 해당하지 않는 설명은?',
  'D',
  '정보보안의 기본 3요소는 기밀성(Confidentiality), 무결성(Integrity), 가용성(Availability)입니다.
- 기밀성: 인가된 사용자만 정보에 접근할 수 있어야 합니다.
- 무결성: 인가된 주체만 데이터를 수정할 수 있고, 전송 중 정보가 임의로 변경되지 않아야 합니다.
- 가용성: 인가된 사용자가 필요한 시점에 자원에 접근할 수 있어야 합니다.
“휘발성”은 메모리 특성을 설명하는 용어로 보안 3요소에는 포함되지 않습니다.',
  'seed:2024-3W:81'
);

SET @q81 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q81, 'A', '기밀성: 인가된 사용자에 대해서만 자원 접근이 가능하다.', 0),
  (@q81, 'B', '무결성: 인가된 사용자에 의해서만 자원이 수정되며, 전송 중인 정보는 임의로 변경되지 않는다.', 0),
  (@q81, 'C', '가용성: 인가된 사용자는 권한 범위 내에서 언제든 자원에 접근할 수 있어야 한다.', 0),
  (@q81, 'D', '휘발성: 인가된 사용자가 수행한 데이터는 처리 완료 즉시 폐기되어야 한다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q81, 'SEC_POLICY');


/* =======================================================
 * Q82. COCOMO - Organic 유형 (PM_ESTIMATION)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202, -- 5.2.2 SW 구축관리
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'COCOMO 모델에서, 기관 내부에서 개발된 중소 규모의 소프트웨어로
일괄 자료 처리나 비즈니스 자료 처리·과학기술 계산용 등 50K 라인 이하
소프트웨어를 개발하는 유형은 무엇인가?',
  'B',
  'COCOMO(Constructive Cost Model)에서는 프로젝트 특성에 따라 크게 세 가지 유형으로 분류합니다.
- Organic: 기관 내부, 중소 규모, 업무·과학기술용, 50K LOC 이하
- Semi-Detached: 중간 규모, 중간 난이도 시스템
- Embedded: 하드웨어와 밀접히 결합된 고난이도 시스템
문제에서 설명한 것은 전형적인 Organic 유형입니다.',
  'seed:2024-3W:82'
);

SET @q82 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q82, 'A', 'Embedded형', 0),
  (@q82, 'B', 'Organic형', 1),
  (@q82, 'C', 'Semi-Embedded형', 0),
  (@q82, 'D', 'Semi-Detached형', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q82, 'PM_ESTIMATION');


/* =======================================================
 * Q83. 비용 산정 설명 (PM_ESTIMATION)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202, -- 5.2.2 SW 구축관리
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '소프트웨어 비용 산정에 대한 설명으로 옳지 않은 것은?',
  'B',
  '소프트웨어 비용 산정은 규모, 인력, 생산성, 프로젝트 특성 등을 기반으로 개발·유지보수에 필요한 비용을 예측하는 활동입니다.
일반적으로 상향식, 하향식, 전문가 판단, 수학적 모델 등 다양한 기법이 존재합니다.
문제의 보기 2번은 출제 의도 상 “기법 분류 표현이 부정확하다”는 의미에서 오답으로 제시됩니다.',
  'seed:2024-3W:83'
);

SET @q83 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q83, 'A', '소프트웨어의 규모, 인력 등의 요소를 기반으로 개발에 필요한 비용을 예측하는 것이다.', 0),
  (@q83, 'B', '소프트웨어 비용 산정 기법에는 오직 상향식·하향식 두 가지만 존재한다.', 1),
  (@q83, 'C', '비용을 과도하게 높게 산정하면 예산 낭비와 일의 효율성 저하를 초래할 수 있다.', 0),
  (@q83, 'D', '비용 결정 요소에는 프로젝트 요소, 자원 요소, 생산성 요소 등이 있다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q83, 'PM_ESTIMATION');


/* =======================================================
 * Q84. 취약점 관리 - 응용 프로그램 보안 설정 (SEC_OPERATION)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15401, -- 5.4.1 시스템 보안 설계
  'WRITTEN',
  'MCQ',
  'EASY',
  '취약점 관리를 위한 응용 프로그램의 보안 설정과 가장 거리가 먼 것은?',
  'A',
  '응용 프로그램 보안 설정은 주로 실행 프로세스 권한, OS 접근 통제, 불필요한 서비스 차단, 로깅 및 감사 설정 등 소프트웨어와 OS 레벨에서 이뤄집니다.
서버 관리실 출입 통제는 매우 중요한 보안 활동이지만, 이는 물리적 보안 영역에 해당하며 애플리케이션 설정과는 거리가 있습니다.',
  'seed:2024-3W:84'
);

SET @q84 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q84, 'A', '서버 관리실 출입 통제', 1),
  (@q84, 'B', '실행 프로세스 권한 최소화 설정', 0),
  (@q84, 'C', '운영체제 계정·파일 접근 제한', 0),
  (@q84, 'D', '운영체제 수준의 정보 수집·로깅 제한 설정', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q84, 'SEC_OPERATION');


/* =======================================================
 * Q85. Django 프레임워크 (LANG_FRAMEWORK)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202, -- 5.2.2 SW 구축관리 (프레임워크 선택/구축 관점)
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'Python을 기반으로 컴포넌트 재사용과 플러그인화를 강조하여
신속한 개발을 지원하는 웹 프레임워크는?',
  'C',
  'Django는 Python 기반의 대표적인 웹 프레임워크로, MTV 패턴을 기반으로 컴포넌트 재사용과 생산성 향상에 초점을 둡니다. ORM, 관리자 페이지, 인증 등 많은 기능을 내장하고 있어 신속한 개발에 적합합니다.
CodeIgniter는 PHP, Spring은 Java 기반 프레임워크입니다.',
  'seed:2024-3W:85'
);

SET @q85 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q85, 'A', 'Node.js', 0),
  (@q85, 'B', 'Spring', 0),
  (@q85, 'C', 'Django', 1),
  (@q85, 'D', 'CodeIgniter', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q85, 'LANG_FRAMEWORK');


/* =======================================================
 * Q86. LOC 기반 기간 산정 (PM_ESTIMATION)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202, -- 5.2.2 SW 구축관리
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'LOC 기법으로 총 36,000 라인으로 예측된 프로젝트가 있다.
개발에 참여할 프로그래머는 6명이고, 1인당 평균 생산성이
월 300라인일 때, 개발에 소요되는 기간(개월 수)은?',
  'D',
  '총 LOC = 36,000라인
개발자 수 = 6명, 1인당 생산성 = 300 LOC/월
팀 전체 월간 생산성 = 6 × 300 = 1,800 LOC/월
필요 개월 수 = 36,000 ÷ 1,800 = 20개월이므로 정답은 20개월입니다.',
  'seed:2024-3W:86'
);

SET @q86 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q86, 'A', '5개월', 0),
  (@q86, 'B', '10개월', 0),
  (@q86, 'C', '15개월', 0),
  (@q86, 'D', '20개월', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q86, 'PM_ESTIMATION');


/* =======================================================
 * Q87. 무선 LAN - CSMA/CA (NW_WIRELESS)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15201, -- 5.2.1 네트워크 구축관리
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '무선 LAN에서 데이터 전송 시 매체가 비어 있음을 확인한 뒤,
충돌을 피하기 위해 일정 시간을 기다렸다가 전송하는 방법은?',
  'D',
  '무선 LAN(IEEE 802.11)에서는 충돌 검출(CSMA/CD)이 어렵기 때문에
충돌 회피 방식인 CSMA/CA(Carrier Sense Multiple Access with Collision Avoidance)를 사용합니다.
전송 전 채널을 감시하고, 비어 있으면 무작위 대기 후 전송함으로써 충돌 가능성을 줄입니다.',
  'seed:2024-3W:87'
);

SET @q87 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q87, 'A', 'STP(Spanning Tree Protocol)', 0),
  (@q87, 'B', 'VLAN(Virtual LAN)', 0),
  (@q87, 'C', '단순 LAN 스위칭 방식', 0),
  (@q87, 'D', 'CSMA/CA', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q87, 'NW_WIRELESS');


/* =======================================================
 * Q88. 물리적 위협 (SEC_OPERATION)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15401, -- 5.4.1 시스템 보안 설계
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 물리적 위협으로 인한 문제에 해당하지 않는 것은?',
  'D',
  '물리적 위협은 화재·홍수 같은 자연재해, 테러·방화, 하드웨어 파손·도난처럼
장비 자체에 직접적인 피해를 주는 사건을 의미합니다.
반면, 방화벽 설정 오류는 네트워크·시스템 구성 상의 논리적(기술적) 보안 위협에 해당합니다.',
  'seed:2024-3W:88'
);

SET @q88 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q88, 'A', '화재·홍수 등 천재지변으로 인한 위협', 0),
  (@q88, 'B', '하드웨어 파손·고장으로 인한 장애', 0),
  (@q88, 'C', '방화 테러로 하드웨어와 기록 장치를 물리적으로 파괴하는 행위', 0),
  (@q88, 'D', '방화벽 설정 오류로 인한 네트워크·서버 보안 위협', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q88, 'SEC_OPERATION');


/* =======================================================
 * Q89. 나선형(Spiral) 모델 4단계 (SW_PROCESS)
 *  - WRITTEN / MCQ / NORMAL
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15101, -- 5.1.1 개발방법론 선정
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '소프트웨어 개발 모델 중 나선형(Spiral) 모델의 네 가지 주요 활동을
올바른 순서로 나열한 것은?

> Ⓐ 계획 수립  Ⓑ 고객 평가
> Ⓒ 개발 및 검증  Ⓓ 위험 분석',
  'B',
  '나선형(Spiral) 모델의 한 사이클은 일반적으로 다음 순서로 진행됩니다.
1) 계획 수립(Planning)
2) 위험 분석(Risk Analysis)
3) 개발 및 검증(Engineering)
4) 고객 평가(Evaluation)
따라서 Ⓐ → Ⓓ → Ⓒ → Ⓑ 순서가 정답입니다.',
  'seed:2024-3W:89'
);

SET @q89 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q89, 'A', 'Ⓐ → Ⓑ → Ⓓ → Ⓒ', 0),
  (@q89, 'B', 'Ⓐ → Ⓓ → Ⓒ → Ⓑ', 1),
  (@q89, 'C', 'Ⓐ → Ⓑ → Ⓒ → Ⓓ', 0),
  (@q89, 'D', 'Ⓐ → Ⓒ → Ⓑ → Ⓓ', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q89, 'SW_PROCESS');


/* =======================================================
 * Q90. Docker 개념 (OPS_DEVOPS)
 *  - WRITTEN / MCQ / EASY
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15202, -- 5.2.2 SW 구축관리 (DevOps/컨테이너)
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음이 설명하는 기술은?

> - 컨테이너 응용프로그램의 배포를 자동화하는 오픈 소스 엔진이다.  
> - 소프트웨어 컨테이너 안에 응용 프로그램들을 배치·실행하는 일을 자동화해 주는 오픈 소스 프로젝트이자 소프트웨어이다.',
  'B',
  'Docker는 컨테이너 기반 가상화 기술로, 애플리케이션과 그 종속성을
이미지로 패키징하고, 이를 컨테이너로 실행·배포·이동하기 쉽게 해 줍니다.
DevOps 환경에서 CI/CD, 마이크로서비스 배포에 널리 사용됩니다.',
  'seed:2024-3W:90'
);

SET @q90 := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q90, 'A', 'Stack Guard', 0),
  (@q90, 'B', 'Docker', 1),
  (@q90, 'C', 'Scytale', 0),
  (@q90, 'D', 'Cipher Container', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q90, 'OPS_DEVOPS');
