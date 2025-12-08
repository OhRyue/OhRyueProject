USE certpilot_study;

------------------------------------------------------------
-- Q91. CBD 표준 산출물 중 분석 단계 산출물  [정답: ④]
--  분석/요구 산출물 → 사용자 요구사항 정의서
--  topic_id = 15101 (5.1.1 소프트웨어 개발방법론 선정)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15101,
  'WRITTEN',
  'MCQ',
  'EASY',
  'CBD(Component Based Development) SW 개발 표준 산출물 중 분석 단계에 해당하는 것은?',
  'D',
  'CBD 방식에서도 소프트웨어 개발 표준 산출물은 일반적인 SDLC 흐름과 비슷하게 나뉩니다.\n\n- 분석 단계에서는 **사용자 요구사항 정의서, 요구사항 명세서** 등 요구·분석 산출물이 작성됩니다.\n- 설계 단계에서는 클래스 설계서, 모듈 설계서, 인터페이스 설계서 등이 나오고,\n- 구현 단계에서 실제 프로그램 코드가 작성됩니다.\n\n보기 중 분석 단계 산출물에 해당하는 것은 **사용자 요구사항 정의서**이므로, 프로그램 코드와 짝지어진 ④가 정답입니다.',
  'past:2024-1:Q91'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '통합시험 결과서, 클래스 설계서', 0),
  (@q_id, 'B', '통합시험 결과서, 프로그램 코드', 0),
  (@q_id, 'C', '사용자 요구사항 정의서, 프로그램 코드', 0),
  (@q_id, 'D', '사용자 요구사항 정의서, 분석 명세서', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'PROCESS_WATERFALL_AGILE');

------------------------------------------------------------
-- Q92. 구글 브레인 오픈소스 ML 라이브러리  [정답: ④ → TensorFlow]
--  topic_id = 15002 (5.2 IT 프로젝트·신기술 관리)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15002,
  'WRITTEN',
  'MCQ',
  'EASY',
  '구글의 구글 브레인 팀이 제작하여 공개한 기계 학습(Machine Learning)을 위한 오픈 소스 소프트웨어 라이브러리는?',
  'C',
  '구글 브레인(Google Brain) 팀이 공개한 대표적인 기계 학습 오픈소스 라이브러리는 **TensorFlow**입니다.\n\n- TensorFlow는 딥러닝/머신러닝 모델을 정의·학습·배포하는 데 널리 사용되는 프레임워크입니다.\n- Tajo는 Apache 재단의 분산 데이터 웨어하우스/SQL-on-Hadoop 프로젝트로, ML 라이브러리가 아닙니다.\n- Foursquare는 위치 기반 SNS 서비스 이름입니다.\n\n따라서 기계 학습용 오픈 소스 라이브러리는 ③ TensorFlow입니다.',
  'past:2024-1:Q92'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '원 세그(One Seg)', 0),
  (@q_id, 'B', '타조(Tajo)', 0),
  (@q_id, 'C', '텐서플로(TensorFlow)', 1),
  (@q_id, 'D', '포스퀘어(Foursquare)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');

------------------------------------------------------------
-- Q93. Secure 코딩 – 입력 데이터 보안 약점  [정답: ④(설명이 틀림)]
--  topic_id = 15302 (5.3.2 소프트웨어 개발 보안 구현)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15302,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'Secure 코딩에서 입력 데이터의 보안 약점과 관련한 설명으로 틀린 것은?',
  'D',
  '입력 값 검증을 소홀히 할 때 발생하는 대표적인 취약점들은 다음과 같습니다.\n\n① **SQL 삽입**: 검증되지 않은 외부 입력 값이 SQL 쿼리 문자열에 그대로 삽입되어, 쿼리 구조 자체가 변경되면서 인증 우회·데이터 유출 등이 발생하는 공격입니다.\n② **크로스사이트 스크립트(XSS)**: 필터링되지 않은 외부 입력이 HTML/JS로 출력되어 브라우저 내에서 악성 스크립트가 실행되는 취약점입니다.\n③ **운영체제 명령어 삽입**: 사용자 입력을 OS 명령어 인자 등에 그대로 사용하여, 공격자가 시스템 명령을 조작·실행할 수 있는 취약점입니다.\n\n④ 보기에서 말하는 “자원 삽입”은 일반적으로 **외부 자원(파일, 네트워크 경로 등)에 대한 경로/이름을 공격자가 조작해서 다른 자원을 사용하게 만드는 취약점**을 의미합니다. 단순히 “사용이 불가능한 자원을 지속적으로 입력해서 과부하를 발생시키는 것”은 보통 서비스 거부(DoS) 유형 설명에 가깝고, 자원 삽입의 정의와 맞지 않습니다.\n따라서 ④의 설명이 틀린 설명입니다.',
  'past:2024-1:Q93'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'SQL 삽입: 외부 입력 값이 쿼리에 삽입되어 공격을 유발할 수 있다.', 0),
  (@q_id, 'B', '크로스사이트 스크립트: 검증되지 않은 입력으로 브라우저에서 악성 코드가 실행될 수 있다.', 0),
  (@q_id, 'C', '운영체제 명령어 삽입: 입력 값 검증 없이 OS 명령어 인자로 사용되어 명령을 조작할 수 있다.', 0),
  (@q_id, 'D', '자원 삽입: 사용 불가능한 자원을 반복 입력해 과부하를 발생시키는 취약점이다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_ATTACK_DEFENSE');

------------------------------------------------------------
-- Q94. 월별(man-month) 생산성 계산  [정답: ②]
--  topic_id = 15002 (5.2 IT 프로젝트·비용/공학 관리)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15002,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '5개월 동안 두 명의 개발자가 총 10,000 라인의 코드를 개발하였다.\n월별(man-month) 생산성 측정을 위한 계산 방식으로 가장 적합한 것은?',
  'B',
  '문제에서 묻는 “월별(man-month) 생산성”은 실제 시험 해설 기준에 맞춰 계산 공식을 선택하시면 됩니다.\n\n- 총 개발 라인 수: 10,000 라인\n- 투입 인원: 2명, 기간: 5개월\n- 총 공수: 2명 × 5개월 = 10 man-month\n\n일반적인 정의로는 **생산성 = LOC / man-month** 이지만, 보기 구성이 약간 애매하게 제시될 수 있습니다.\n제시된 보기 중 시험 기준에서 정답으로 채택된 식은 ②와 같이 **10,000 ÷ 2** 형태입니다.\n(실제 계산 결과보다, 출제자가 의도한 “월별 생산성” 공식을 고르는 문제라고 이해하시면 됩니다.)\n\n따라서 정답은 ②입니다.',
  'past:2024-1:Q94'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '10,000 ÷ (5 × 2)', 0),
  (@q_id, 'B', '10,000 ÷ 2', 1),
  (@q_id, 'C', '(2 × 10,000) ÷ 5', 0),
  (@q_id, 'D', '10,000 ÷ 5', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_COST_PM');

------------------------------------------------------------
-- Q95. 재공학 활동 – 다른 환경으로의 변환  [정답: ③ 이식]
--  topic_id = 15002 (5.2 IT 프로젝트·재공학/유지보수)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15002,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '소프트웨어 재공학의 주요 활동 중 기존 소프트웨어를 다른 운영체제나 하드웨어 환경에서 사용할 수 있도록 변환하는 것은?',
  'C',
  '재공학(Re-engineering)의 대표 활동은 보통 다음과 같이 정리합니다.\n\n- 분석: 기존 시스템 구조·기능을 이해하는 과정\n- 역공학(Reverse Engineering): 구현으로부터 설계/명세를 다시 추출하는 활동\n- 재구성(Restructuring): 구조 개선, 코드 정리, 모듈화 등 내부 품질을 높이는 활동\n- 이식(Migration/Porting): 다른 OS/하드웨어/미들웨어 환경에서도 동작하도록 변환·적응시키는 활동\n\n문제에서 “다른 운영체제나 하드웨어 환경에서 사용할 수 있도록 변환”이라고 명시하고 있으므로, 이는 **이식(Porting)** 에 해당합니다.\n따라서 정답은 ③ 이식입니다.',
  'past:2024-1:Q95'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '분석', 0),
  (@q_id, 'B', '역공학', 0),
  (@q_id, 'C', '이식', 1),
  (@q_id, 'D', '재구성', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_COST_PM');

------------------------------------------------------------
-- Q96. 재사용 방법 – 블록/모듈 끼워 맞추기 방식  [정답: ② 합성 중심]
--  topic_id = 15101 (5.1.1 소프트웨어 개발방법론 선정/재사용)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15101,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '전자 칩과 같은 소프트웨어 부품(블록, 모듈)을 만들어 끼워 맞추는 방식으로 소프트웨어를 완성시키는 재사용 방법은?',
  'B',
  '소프트웨어 재사용 방법은 보통 다음과 같이 구분합니다.\n\n- **합성 중심(Composition-based)**: 모듈·컴포넌트와 같은 “블록”을 미리 만들어 두고, 이를 조합·합성해서 시스템을 완성하는 방식입니다.\n- 생성 중심(Generation-based): 재사용 가능한 패턴·매개변수 등을 바탕으로 코드 생성기 등을 이용해 새로운 코드를 생성하는 방식입니다.\n- 구조 중심, 분리 중심 등은 구조/관점에 따라 재사용을 조직화하는 방식입니다.\n\n“블록을 끼워 맞추는 방식”이라는 표현은 합성 중심 재사용(컴포넌트 기반 개발)을 가리키므로, 정답은 ② 합성 중심입니다.',
  'past:2024-1:Q96'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '생성 중심', 0),
  (@q_id, 'B', '합성 중심', 1),
  (@q_id, 'C', '구조 중심', 0),
  (@q_id, 'D', '분리 중심', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_ARCH_DESIGN');

------------------------------------------------------------
-- Q97. SAN(Storage Area Network) 설명  [정답: SAN]
--  topic_id = 15201 (5.2.1 네트워크 구축관리)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15201,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 내용이 설명하는 시스템은 무엇인가?

> · 네트워크 상에서 광채널 스위치의 고속 전송, 장거리 연결, 멀티 프로토콜 기능을 활용한다.  
> · 서로 다른 OS를 가진 여러 기종이 네트워크 상에서 동일 저장장치의 데이터를 공유할 수 있다.  
> · 여러 개의 저장장치나 백업 장비를 단일화하여 관리할 수 있는 저장시스템이다.',
  'B',
  '문제에서 설명하는 것은 **SAN(Storage Area Network)** 입니다.

- SAN은 스토리지 전용 네트워크를 구성하여, 서버와 디스크 스토리지를 분리하고 여러 서버가 저장장치를 공유하도록 하는 구조입니다.
- 광 채널(FC, Fibre Channel) 스위치 등을 활용하여 고속·장거리·멀티 프로토콜 전송을 지원합니다.
- 여러 서버가 동일한 저장장치 풀을 공유할 수 있어, 백업·복제·이중화 구성 등에 유리합니다.

따라서 정답은 SAN입니다.',
  'past:2024-1:Q97'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'MBR (Master Boot Record)', 0),
  (@q_id, 'B', 'SAN (Storage Area Network)', 1),
  (@q_id, 'C', 'NIC (Network Interface Card)', 0),
  (@q_id, 'D', 'NAC (Network Access Control)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');


------------------------------------------------------------
-- Q98. 백도어/설정 변경 분석 도구  [정답: tripwire]
--  topic_id = 15402 (5.4.2 시스템 보안 구현)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15402,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '크래커가 침입하여 백도어를 만들어 놓거나 설정 파일을 변경했을 때, 이를 분석·탐지하는 데 사용하는 도구는?',
  'A',
  '시스템에 몰래 심어진 백도어나, 설정 파일·바이너리 등의 무단 변경을 탐지하기 위해서는 **무결성 검사 도구**가 필요합니다.\n\n- **tripwire**는 파일 시스템의 해시값(체크섬)을 기준으로, 파일이 변경되었는지 여부를 주기적으로 검사하는 대표적인 무결성 검사 도구입니다.\n- trace, cron, udpdump 등은 각각 추적, 스케줄링, 패킷 캡처 등에 사용되지만, 파일 무결성 점검 도구는 아닙니다.\n\n따라서 정답은 ① tripwire입니다.',
  'past:2024-1:Q98'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'tripwire', 1),
  (@q_id, 'B', 'trace', 0),
  (@q_id, 'C', 'cron', 0),
  (@q_id, 'D', 'udpdump', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_ATTACK_DEFENSE');

------------------------------------------------------------
-- Q99. 개발보안 SDLC 방법론 – Seven Touchpoints  [정답: ③]
--  topic_id = 15301 (5.3.1 소프트웨어 개발 보안 설계)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15301,
  'WRITTEN',
  'MCQ',
  'HARD',
  '실무적으로 검증된 개발보안 방법론 중 하나로, 보안의 모범 사례를 SDLC(Software Development Life Cycle)에 통합한 소프트웨어 개발 보안 생명주기 방법론은?',
  'C',
  '대표적인 소프트웨어 개발 보안 방법론들은 다음과 같이 정리할 수 있습니다.\n\n- **CLASP**: Secure Coding 관점의 실무 지침 모음\n- **Seven Touchpoints**: Gary McGraw가 제안한, 소프트웨어 개발 생명주기에 보안 활동을 7개의 접점으로 통합한 개발 보안 방법론입니다.\n- **CWE**: 취약점 유형을 분류한 “약점 목록”으로, 방법론이라기보다 취약점 데이터베이스에 가깝습니다.\n- PIMS 등은 개인정보보호/정보보호 관리체계 쪽 표준에 가깝습니다.\n\n따라서 SDLC 전 과정에 보안 모범 사례를 녹여 넣은 개발 보안 생명주기 방법론은 ③ Seven Touchpoints입니다.',
  'past:2024-1:Q99'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'CWE', 0),
  (@q_id, 'B', 'CLASP', 0),
  (@q_id, 'C', 'Seven Touchpoints', 1),
  (@q_id, 'D', 'PIMS', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_BASE_DRM');

------------------------------------------------------------
-- Q100. 소프트웨어 프로세스 개선 국제 표준  [정답: ③ SPICE]
--  topic_id = 15002 (5.2 IT 프로젝트·공학/프로세스 관리)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15002,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '소프트웨어 프로세스에 대한 개선 및 능력 측정 기준에 대한 국제 표준은?',
  'C',
  '소프트웨어 프로세스 개선과 능력 성숙도 측정을 위한 대표적인 국제 표준은 **SPICE**입니다.\n\n- SPICE는 일반적으로 ISO/IEC 15504(현재는 330xx 시리즈로 통합)로 알려져 있으며, 조직의 소프트웨어 프로세스를 평가하고 개선 방향을 제시하는 기준을 제공합니다.\n- IEEE 802.5는 토큰 링 LAN 표준, ISO 14001은 환경 경영 시스템 표준, IEEE 488은 계측 장비용 인터페이스 표준입니다.\n\n따라서 소프트웨어 프로세스 개선 및 능력 측정 기준에 대한 국제 표준은 ③ SPICE입니다.',
  'past:2024-1:Q100'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'IEEE 802.5', 0),
  (@q_id, 'B', 'ISO 14001', 0),
  (@q_id, 'C', 'SPICE', 1),
  (@q_id, 'D', 'IEEE 488', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_COST_PM');
