USE certpilot_study;

------------------------------------------------------------
-- Q81. IDS(침입탐지시스템) 설명으로 틀린 것  [정답: ①]
--  topic_id = 15401 (5.4.1 시스템 보안 설계)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15401,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '(IDS: Intrusion Detection System) 침입탐지 시스템과 관련한 설명으로 틀린 것은?',
  'A',
  '침입탐지 기법은 크게 서명 기반(Signature-based)과 이상 징후 기반(Anomaly-based)으로 나뉩니다.

① 보기에서는 “Signature Base 이상 탐지 기법”이라고 섞어서 표현하고, 또 Knowledge Base라고도 불리며 이미 발견된 공격 패턴을 이용한다고 설명하고 있습니다.  
- **서명 기반(Signature-based)**: 이미 알려진 공격 패턴(시그니처)을 DB에 저장해 두었다가 일치 여부를 보고 탐지합니다.  
- **이상 징후 기반(Anomaly-based)**: 정상 행위의 기준선을 만들어 두고, 그와 다른 비정상 행위를 탐지합니다.  

따라서 서명 기반과 이상 탐지 기법을 혼용해 표현한 ①의 설명은 틀린 문장입니다.

② HIDS는 호스트(서버) 내부에서 계정·로그·파일 변경 등을 감시하는 에이전트를 두고, 어떤 사용자가 어떤 작업을 했는지 추적하는 방식이므로 옳습니다.  
③ NIDS의 대표적인 오픈소스 구현으로 Snort가 널리 사용되므로 옳습니다.  
④ DMZ 구간에 외부 서비스용 서버를 두고, 이 구간의 트래픽을 감시하기 위해 IDS를 설치할 수 있으므로 옳은 설명입니다.',
  'past:2024-1:Q81'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '이상 탐지 기법을 Signature Base·Knowledge Base라고 부르며, 알려진 공격 패턴으로 탐지한다.', 1),
  (@q_id, 'B', 'HIDS는 호스트에서 사용자 계정별 접근·행위를 기록·추적한다.', 0),
  (@q_id, 'C', 'NIDS의 대표적인 예로 Snort가 있다.', 0),
  (@q_id, 'D', 'DMZ 구간에 IDS를 설치해 외부 서비스 구간을 감시할 수 있다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_ATTACK_DEFENSE');

------------------------------------------------------------
-- Q82. 고가용성(HA) 클러스터링 설명  [정답: ①]
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
  '다음 설명에 해당하는 정보시스템 구성 방식은 무엇인가?

> · 공유 디스크를 중심으로 클러스터링하여 다수의 시스템을 동시에 연결할 수 있다.  
> · 조직·기업의 기간 업무 서버 안정성을 높이기 위해 사용될 수 있다.  
> · 여러 방식 중 2개의 서버를 서로 연결하여 각각 업무를 수행하도록 구성하는 2노드 방식이 널리 사용된다.',
  'A',
  '문제에서 설명하는 것은 “고가용성(High Availability) 클러스터링”에 대한 내용입니다.

- 공유 디스크를 두고 여러 서버를 클러스터로 묶어, 한 노드에 장애가 발생하면 다른 노드가 서비스를 이어받는 구조를 흔히 HA 클러스터라고 부릅니다.
- 기간 업무 서버의 다운타임을 최소화하기 위해 사용되며, 2노드 클러스터(Active-Standby, Active-Active)가 가장 일반적인 형태입니다.
- HACMP(High Availability Cluster Multi-Processing)와 같이 상용 HA 솔루션들이 대표적인 예입니다.

따라서 ① “고가용성 솔루션(HACMP 등)”이 올바른 선택입니다.',
  'past:2024-1:Q82'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '고가용성 솔루션(HACMP 등) 기반 클러스터', 1),
  (@q_id, 'B', '점대점 연결(Point-to-Point Mode)', 0),
  (@q_id, 'C', '스턱스넷(Stuxnet)', 0),
  (@q_id, 'D', '루팅(Rooting)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');


------------------------------------------------------------
-- Q83. 초고속 근접무선통신(NFC) 기반 Zing  [정답: ②]
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
  '기기를 키오스크에 갖다 대면 원하는 데이터를 바로 가져올 수 있고,
  10cm 이내 근접 거리에서 기가급 속도로 데이터 전송이 가능한
  초고속 근접무선통신(NFC) 기술은 무엇인가?',
  'B',
  '문제에서 말하는 “10cm 이내 근거리에서 기가급 속도 전송이 가능한 NFC 기반 기술”은 Zing 기술에 대한 설명입니다.

- 사용자가 스마트폰 등을 키오스크에 가까이 대면, 별도의 복잡한 접속 절차 없이 필요한 데이터를 빠르게 가져올 수 있는 근접 통신 방식입니다.
- 초고속 근접무선통신이라는 키워드와 “기가급”이라는 표현은 Zing의 특징을 그대로 반영한 표현입니다.

따라서 정답은 ② Zing 입니다.',
  'past:2024-1:Q83'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'BcN(Broadband Convergence Network)', 0),
  (@q_id, 'B', 'Zing', 1),
  (@q_id, 'C', 'Marine Navi', 0),
  (@q_id, 'D', 'C-V2X(Cellular Vehicle To Everything)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');

------------------------------------------------------------
-- Q84. 세션 하이재킹 탐지 방법  [정답: ①이 아닌 것, 즉 ①이 “거리가 먼 것”]
--  topic_id = 15401 (5.4.1 시스템 보안 설계)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15401,
  'WRITTEN',
  'MCQ',
  'HARD',
  '다음 중 세션 하이재킹(Session Hijacking)을 탐지하는 방법으로 거리가 먼 것은?',
  'A',
  '세션 하이재킹은 TCP 세션의 제어 정보를 빼앗거나 위조하여, 공격자가 합법적인 사용자인 것처럼 세션을 가로채는 공격입니다. 이를 탐지할 때는 주로 다음과 같은 현상들을 관찰합니다.

- 비동기화 상태(Sequence/Ack 번호가 비정상적으로 어긋나는 상황)  
- ACK STORM(양쪽이 서로 ACK를 계속 주고받는 비정상 상태)  
- 패킷 유실 및 재전송 급증(세션이 불안정해질 때 자주 발생)  

이들은 모두 세션 하이재킹과 연관된 이상 징후로 간주할 수 있습니다.

반면, “FTP SYN Segment 탐지”는 특정 프로토콜(FTP)의 접속 시도 패턴에 초점을 맞춘 것으로, 세션 하이재킹 일반 탐지 기법이라 보기 어렵습니다.  
따라서 세션 하이재킹 탐지 방법으로 거리가 먼 것은 ①입니다.',
  'past:2024-1:Q84'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'FTP SYN 세그먼트 탐지', 1),
  (@q_id, 'B', '비동기화 상태 탐지', 0),
  (@q_id, 'C', 'ACK STORM(ACK 폭주) 탐지', 0),
  (@q_id, 'D', '패킷 유실 및 재전송 증가 탐지', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_ATTACK_DEFENSE');

------------------------------------------------------------
-- Q85. Spiral Model 설명으로 틀린 것  [정답: ④]
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
  'NORMAL',
  '소프트웨어 생명주기 모형 중 Spiral Model에 대한 설명으로 틀린 것은?',
  'D',
  'Spiral 모델은 보헴(Bohem)이 제안한 위험 중심(Risk-Driven) 반복 개발 모델입니다.

- ① 대규모·고위험 프로젝트에 적합한 것으로 알려져 있으므로 옳은 설명입니다.  
- ② 계획 수립 → 위험 분석 → 공학적 개발 → 고객 평가 순으로 한 바퀴를 돌며, 이 “스파이럴(나선)”을 반복하면서 점진적으로 시스템을 완성해 나가므로 옳은 설명입니다.  
- ③ 핵심 목적은 각 단계에서 발생 가능한 **위험(Risk)을 식별·분석·완화**하여 전체 프로젝트 실패 가능성을 줄이는 것이므로 옳습니다.  

④ “계획·설계·개발·평가의 개발 주기가 한 번만 수행된다”는 설명은 **반복·점진적 특성**을 전혀 반영하지 못한 문장으로, Spiral 모델의 가장 큰 특징과 정반대입니다.  
따라서 ④가 틀린 설명입니다.',
  'past:2024-1:Q85'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '비교적 대규모·고위험 시스템에 적합하다.', 0),
  (@q_id, 'B', '계획 → 위험 분석 → 공학적 개발 → 고객 평가 순으로 진행된다.', 0),
  (@q_id, 'C', '위험을 관리·최소화하는 것을 주요 목적으로 한다.', 0),
  (@q_id, 'D', '계획·설계·개발·평가 주기는 한 번만 수행된다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'PROCESS_WATERFALL_AGILE');

------------------------------------------------------------
-- Q86. Hadoop 설명  [정답: ①]
--  topic_id = 15201 (5.2.1 네트워크/인프라·빅데이터 구축관리)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15201,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 설명에 해당하는 용어는 무엇인가?

> · 오픈 소스를 기반으로 한 분산 컴퓨팅 플랫폼이다.  
> · 일반 PC급 컴퓨터들로 가상화된 대형 스토리지를 형성한다.  
> · 다양한 소스로부터 생성되는 빅데이터를 효율적으로 저장하고 처리한다.',
  'A',
  '문제에서 설명하는 것은 **하둡(Hadoop)**입니다.

- 하둡은 오픈소스 기반 분산 처리 프레임워크로, 여러 대의 저가 PC(Commodity Hardware)를 묶어 하나의 대규모 스토리지·연산 자원처럼 사용하는 것이 특징입니다.
- HDFS(분산 파일 시스템)와 MapReduce(분산 처리 프레임워크)를 핵심 구성 요소로 하며, 빅데이터 저장·분석에 널리 사용됩니다.

따라서 정답은 ① 하둡(Hadoop)입니다.',
  'past:2024-1:Q86'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '하둡(Hadoop)', 1),
  (@q_id, 'B', '비컨(Beacon)', 0),
  (@q_id, 'C', '맴리스터(Memristor)', 0),
  (@q_id, 'D', '포스퀘어(Foursquare)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');


------------------------------------------------------------
-- Q87. 키보드 입력을 훔치는 공격  [정답: Key Logger Attack]
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
  'EASY',
  '컴퓨터 사용자의 키보드 움직임을 몰래 기록하여 패스워드 등 중요한 개인정보를 탈취하는 해킹 공격은 무엇인가?',
  'B',
  '사용자의 키보드 입력을 가로채어, 타이핑한 내용(아이디, 비밀번호, 카드번호 등)을 그대로 공격자에게 보내는 악성 프로그램·공격 기법을 **키로거(Key Logger) 공격**이라고 합니다.

- Worm, Zombie Worm 등은 자기 복제·봇넷과 관련된 악성코드·공격입니다.
- Rollback은 일반적으로 데이터·트랜잭션을 이전 상태로 되돌리는 동작을 의미합니다.

따라서 키보드 움직임을 탐지해 정보를 훔치는 공격은 Key Logger Attack입니다.',
  'past:2024-1:Q87'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Worm', 0),
  (@q_id, 'B', 'Key Logger Attack', 1),
  (@q_id, 'C', 'Zombie Worm', 0),
  (@q_id, 'D', 'Rollback', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_ATTACK_DEFENSE');

------------------------------------------------------------
-- Q88. COCOMO 프로젝트 유형이 아닌 것  [정답: ④]
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
  'COCOMO 모형에서 사용하는 프로젝트 유형으로 거리가 먼 것은?',
  'D',
  'COCOMO(Constructive Cost Model)에서는 프로젝트 특성에 따라 세 가지 유형을 정의합니다.

1) **Organic** : 비교적 작은 규모, 익숙한 환경, 유사 경험이 많은 소규모 팀  
2) **Semi-detached** : 중간 규모, 일부는 익숙하고 일부는 새로운 요소가 섞인 프로젝트  
3) **Embedded** : 하드웨어·소프트웨어가 강하게 결합된, 제약 조건이 많은 내장형 시스템

보기 중 “Organic”, “Semi-detached”, “Embedded”는 모두 COCOMO의 공식 프로젝트 유형입니다.  
반면 “Sequential”은 COCOMO 프로젝트 유형이 아니므로, ④가 정답입니다.',
  'past:2024-1:Q88'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Organic', 0),
  (@q_id, 'B', 'Semi-detached', 0),
  (@q_id, 'C', 'Embedded', 0),
  (@q_id, 'D', 'Sequential', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_COST_PM');

------------------------------------------------------------
-- Q89. 접근통제 정책 유형 (MAC/DAC/RBAC 비교)  [정답: MAC]
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
  '다음은 정보의 접근통제 정책에 대한 설명입니다. 괄호 ( )에 들어갈 정책으로 알맞은 것은 무엇인가요?

> | 구분       | ( )                 | DAC                        | RBAC                       |
> |------------|---------------------|----------------------------|----------------------------|
> | 권한 부여  | 시스템(보안 정책)   | 데이터 소유자              | 중앙 관리자                 |
> | 접근 결정  | 보안 등급(Label)    | 신분(Identity)             | 역할(Role)                 |
> | 정책 변경  | 고정적(변경 어려움) | 변경 용이                  | 변경 용이                  |
> | 장점       | 안정적·중앙집중적   | 구현 용이                  | 유연함·관리 용이           |',
  'A',
  '접근통제 정책의 대표적인 세 가지 유형은 다음과 같습니다.

1) **MAC(강제적 접근통제, Mandatory Access Control)**  
   - 정책 기준: 보안 등급(Label, 기밀/2급/3급 등)  
   - 권한 부여: 시스템이 정책에 따라 강제로 결정  
   - 사용자는 임의로 권한을 변경할 수 없고, 정책 변경도 매우 제한적입니다.  
   → 중앙집중적·안정적이지만 유연성이 낮습니다.

2) **DAC(임의적 접근통제, Discretionary Access Control)**  
   - 정책 기준: 신분(아이디), 소유자  
   - 객체(파일 등)의 소유자가 다른 사용자에게 권한을 부여/회수할 수 있어 유연합니다.

3) **RBAC(역할기반 접근통제, Role-Based Access Control)**  
   - 정책 기준: 역할(Role)  
   - 사용자는 역할을 부여받고, 역할 단위로 권한을 관리하므로 관리성이 뛰어납니다.

문제의 표에서  
- “보안등급(Label)”을 기준으로 하고  
- “권한 부여 주체가 시스템(중앙)”이며  
- “정책 변경이 고정적·변경 어려움”인 정책은 **MAC**에 해당합니다.

따라서 ( )에 들어갈 정책은 MAC입니다.',
  'past:2024-1:Q89'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'MAC (Mandatory Access Control)', 1),
  (@q_id, 'B', 'DAC (Discretionary Access Control)', 0),
  (@q_id, 'C', 'RBAC (Role-Based Access Control)', 0),
  (@q_id, 'D', 'NAC 등 기타 정책', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_BASE_DRM');

------------------------------------------------------------
-- Q90. 정보보안 3요소가 아닌 것  [정답: “휘발성” 포함 선택지]
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
  'EASY',
  '다음 중 정보보안의 3대 핵심 요소(CIA Triad)에 해당하지 않는 것은?',
  'C',
  '정보보안의 가장 기본적인 3요소는 흔히 **CIA Triad**라고 부릅니다.

- **Confidentiality(기밀성)** : 허가되지 않은 자에게 정보가 노출되지 않도록 보호  
- **Integrity(무결성)** : 정보가 인가되지 않은 방법으로 임의 변경·훼손되지 않도록 보호  
- **Availability(가용성)** : 필요할 때 적절한 사용자가 정보와 시스템에 접근할 수 있도록 보장  

“휘발성(Volatility)”은 메모리 특성과 관련된 용어일 뿐, 정보보안의 3대 요소에는 포함되지 않습니다.  
따라서 “휘발성”이 포함된 선택지가 보안 3요소에 해당하지 않는 답입니다.',
  'past:2024-1:Q90'
);

SET @q_id := LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '무결성(Integrity)', 0),
  (@q_id, 'B', '기밀성(Confidentiality)', 0),
  (@q_id, 'C', '휘발성(Volatility)', 1),
  (@q_id, 'D', '가용성(Availability)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_BASE_DRM');
