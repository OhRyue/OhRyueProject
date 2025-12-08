USE certpilot_study;

------------------------------------------------------------
-- Q81. 정보보안 3요소 – 휘발성은 해당 아님  [정답: D]
-- topic_id = 15101 (정보보안 기초)
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
  '소프트웨어 개발에서 정보보안 3요소에 해당하지 않는 설명은?',
  'D',
  '정보보안의 기본 3요소는 기밀성(Confidentiality), 무결성(Integrity), 가용성(Availability)이며, 휘발성은 여기에 포함되지 않습니다. 보기 D의 내용은 정의 자체도 부정확하므로 “보안 요소가 아닌 것”에 해당합니다.',
  'past:2024-2:Q81'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '기밀성: 인가된 사용자에 대해서만 자원 접근이 가능하다.', 0),
  (@q_id, 'B', '무결성: 인가된 사용자에 대해서만 자원 수정이 가능하며 전송 중인 정보는 수정되지 않는다.', 0),
  (@q_id, 'C', '가용성: 인가된 사용자는 권한 범위 내에서 언제든 자원 접근이 가능하다.', 0),
  (@q_id, 'D', '휘발성: 인가된 사용자가 수행한 데이터는 처리 완료 즉시 폐기되어야 한다.', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_BASE_DRM');

------------------------------------------------------------
-- Q82. SSO (Single Sign-On)  [정답: D]
-- topic_id = 15102 (인증/접근 통제)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15102,
  'WRITTEN',
  'MCQ',
  'EASY',
  '시스템이 여러 대여도 하나의 시스템에서 인증에 성공하면
다른 시스템에 대한 접근 권한도 얻는 인증 시스템을 의미하는 것은?',
  'D',
  'Single Sign-On(SSO)는 한 번 로그인으로 여러 시스템에 대해 연속적으로 인증된 상태를 유지하게 해 주는 통합 인증 방식입니다. SBO, SOS, SOA는 다른 개념이므로 정답은 SSO입니다.',
  'past:2024-2:Q82'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'SBO', 0),
  (@q_id, 'B', 'SOS', 0),
  (@q_id, 'C', 'SOA', 0),
  (@q_id, 'D', 'SSO', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_BASE_DRM');

------------------------------------------------------------
-- Q83. COCOMO 모형 설명 – 틀린 것은 C  [정답: C]
-- topic_id = 15103 (소프트웨어 비용 산정)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15103,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'COCOMO 모형에 대한 설명으로 옳지 않은 것은?',
  'C',
  '기본형(Basic), 중간형(Intermediate), 세부형(Detailed)으로 갈수록 산정 인자와 복잡도가 증가합니다. 보기 C처럼 “중간형이 개발 공정별 노력까지 자세히 산출한다”는 표현은 세부형(Detailed)에 더 가까운 설명으로, 시험 기준에서 틀린 보기로 취급됩니다.',
  'past:2024-2:Q83'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '원시 프로그램의 규모인 LOC에 의한 비용 산정 기법이다.', 0),
  (@q_id, 'B', '30만 라인 이상의 소프트웨어에서는 내장형(Embedded) 방식으로 비용을 산정한다.', 0),
  (@q_id, 'C', '중간형(Intermediate)은 개발 공정별로 보다 자세하고 정확하게 노력을 산출하여 비용을 산정하는 모형이다.', 1),
  (@q_id, 'D', '사무 처리용·업무용 등의 소규모 소프트웨어의 비용 산정에는 조직형(Organic) 모델이 적합하다.', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_COST_PM');

------------------------------------------------------------
-- Q84. 종료되지 않는 재귀 – 보안 약점  [정답: A]
-- topic_id = 15104 (SW 개발 보안/취약점)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15104,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 Java 코드에서 밑줄로 표시된 부분에는 어떤 보안 약점이 존재하는가?

```java
static int func(int a) {
  return a <= 5 ? func(a) : 3;
}
```',
  'A',
  '조건식이 a <= 5일 때 다시 func(a)를 호출하기 때문에 a 값이 바뀌지 않고 재귀 호출이 무한히 반복되는 구조입니다. 이는 자원 고갈과 서비스 거부(DoS)를 유발할 수 있는 “종료되지 않는 재귀/반복문” 취약점에 해당합니다.',
  'past:2024-2:Q84'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '종료되지 않는 반복문 또는 재귀함수', 1),
  (@q_id, 'B', '널 포인터 역참조',                0),
  (@q_id, 'C', '하드코드된 암호화 키 사용',        0),
  (@q_id, 'D', '초기화되지 않은 변수 사용',        0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_ATTACK_DEFENSE');

------------------------------------------------------------
-- Q85. SAN – 비용 관련 설명이 틀림  [정답: D]
-- topic_id = 15105 (인프라/스토리지)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15105,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  'SAN(Storage Area Network)에 대한 설명으로 틀린 것은?',
  'D',
  'SAN은 고성능·고가용성 스토리지 네트워크를 구축하는 방식으로, 전용 네트워크와 FC 스위치, 스토리지 장비가 필요하기 때문에 초기 구축 비용이 크다는 단점이 있습니다. “초기 설치 비용을 절약할 수 있다”는 설명은 SAN의 특징과 반대이므로 틀렸습니다.',
  'past:2024-2:Q85'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '서버와 저장 장치를 연결하는 전용 네트워크를 별도로 구성하는 방식이다.', 0),
  (@q_id, 'B', '파이버 채널(FC) 스위치를 이용하여 네트워크를 구성한다.',                0),
  (@q_id, 'C', '서버들이 저장장치 및 파일을 공유할 수 있다.',                             0),
  (@q_id, 'D', '초기 설치 시 소요되는 비용을 절약할 수 있다.',                           1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');

------------------------------------------------------------
-- Q86. 해시 설명 – 틀린 것은 B  [정답: B]
-- topic_id = 15106 (암호/해시/전자서명)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15106,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '해시(Hash)에 대한 설명으로 옳지 않은 것은?',
  'B',
  '암호학적 해시 함수는 “임의 길이의 입력”을 받아 “고정 길이의 해시값”으로 매핑하는 함수입니다. 보기 B는 입력과 출력의 성격을 반대로 서술하고 있어 옳지 않습니다.',
  'past:2024-2:Q86'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '해시 알고리즘으로 변환된 값이나 키를 해시값 또는 해시키라고 부른다.', 0),
  (@q_id, 'B', '고정된 길이의 입력 데이터를 임의 길이의 값이나 키로 변환한다.',       1),
  (@q_id, 'C', '데이터 암호화, 무결성 검증 등 정보보호의 다양한 분야에서 활용된다.',   0),
  (@q_id, 'D', '대표적인 해시 알고리즘에는 SHA 시리즈, MD 시리즈 등이 있다.',          0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_ATTACK_DEFENSE');

------------------------------------------------------------
-- Q87. 소유기반 인증이 아닌 것 – i-PIN  [정답: A]
-- topic_id = 15102 (인증 수단/요소)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15102,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 소유 기반(Something you have) 인증 방법에 해당하지 않는 것은?',
  'A',
  '소유 기반 인증은 신분증, 스마트카드, OTP 토큰처럼 “사용자가 물리적으로 소지하는 것”을 기반으로 합니다. i-PIN은 인터넷 상의 본인확인용 “지식 기반(Something you know)”/계정 기반 요소에 더 가깝기 때문에, 보기 A(OTP와 함께 묶은 표현)가 소유 기반만으로 묶기에는 부적절하여 “해당하지 않는 것”으로 출제되었습니다.',
  'past:2024-2:Q87'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'OTP, i-PIN', 1),
  (@q_id, 'B', '신분증',      0),
  (@q_id, 'C', '스마트 카드', 0),
  (@q_id, 'D', '보안 토큰',   0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_BASE_DRM');

------------------------------------------------------------
-- Q88. Mesh Network  [정답: A]
-- topic_id = 15107 (신기술/네트워크 인프라)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15107,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '기존 무선 LAN의 한계를 극복하고, 대규모 디바이스 네트워크 생성에 최적화되어
차세대 이동통신·홈네트워킹·공공 안전 등 특수 목적에 사용되는 새로운 네트워크 기술은?',
  'A',
  '여러 노드가 서로 메시 형태로 연결되어, 특정 노드 장애에도 우회 경로로 통신이 가능한 구조를 갖는 것이 Mesh Network입니다. 대규모 무선 센서망, 공공 안전, 스마트시티 등에서 활용되는 차세대 무선 네트워크 구조로, 문제에서 설명하는 특성에 정확히 부합합니다.',
  'past:2024-2:Q88'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Mesh Network',                 1),
  (@q_id, 'B', 'Virtual Private Network',      0),
  (@q_id, 'C', 'Local Area Network',           0),
  (@q_id, 'D', 'Software Defined Perimeter',   0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');

------------------------------------------------------------
-- Q89. 개인키(대칭키) 암호가 아닌 것 – RSA  [정답: D]
-- topic_id = 15106 (암호/키체계)
------------------------------------------------------------
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15106,
  'WRITTEN',
  'MCQ',
  'EASY',
  '개인키(대칭키) 암호화 기법으로 옳지 않은 것은?',
  'D',
  'DES, SEED, ARIA는 모두 동일한 비밀키를 사용하는 대칭키(개인키) 암호 알고리즘입니다. 반면 RSA는 공개키(비대칭키) 암호 알고리즘이므로 “개인키 암호화 기법이 아닌 것”에 해당합니다.',
  'past:2024-2:Q89'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'ARIA', 0),
  (@q_id, 'B', 'DES',  0),
  (@q_id, 'C', 'SEED', 0),
  (@q_id, 'D', 'RSA',  1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_ATTACK_DEFENSE');

------------------------------------------------------------
-- Q90. 스니핑(Sniffing)  [정답: B]
-- topic_id = 15101 (보안 위협 유형)
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
  '수동적인 네트워크 침해 공격의 하나로, 네트워크의 중간에서
남의 패킷 정보를 도청하는 해킹 유형은?',
  'B',
  '스니핑(Sniffing)은 네트워크 상을 흐르는 패킷을 가로채어 내용(계정, 비밀번호 등)을 도청하는 수동적 공격 기법입니다. 스미싱·피싱은 사회공학 기반 공격이고, 백도어는 우회 접속 통로에 해당하므로 문제에서 묘사한 “패킷 도청”은 스니핑입니다.',
  'past:2024-2:Q90'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '스미싱(Smishing)',  0),
  (@q_id, 'B', '스니핑(Sniffing)',  1),
  (@q_id, 'C', '백도어(Back Door)', 0),
  (@q_id, 'D', '피싱(Phishing)',    0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_ATTACK_DEFENSE');
