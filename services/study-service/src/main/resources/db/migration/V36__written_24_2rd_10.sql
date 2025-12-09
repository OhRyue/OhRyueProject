USE certpilot_study;

/* =======================================================
 * Q91. 메타데이터  [정답: D]
 *  - topic_id = 15108 (메타데이터/데이터 관리)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15108,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 괄호에 공통으로 들어갈 용어는 무엇인가?

> 일련의 데이터를 정의·설명하는 데이터로, 데이터사전·스키마 등을 의미하며
> 빠른 검색과 체계적인 정리를 위해 사용된다.',
  'D',
  '데이터를 설명하는 “데이터에 대한 데이터”를 메타데이터(Meta Data)라고 합니다. 데이터사전, 스키마 정의 등이 대표적인 예입니다.',
  'past:2024-2:Q91'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'Broad Data', 0),
  (@q_id, 'B', 'View',       0),
  (@q_id, 'C', 'Big Data',   0),
  (@q_id, 'D', 'Meta Data',  1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_META_CRUD');


/* =======================================================
 * Q92. SDN  [정답: A]
 *  - topic_id = 15109 (SDN/네트워크 가상화)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15109,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '다음 설명에 해당하는 네트워크 기술은 무엇인가?

> - 제어부와 데이터 전달부를 분리하여 네트워크를 소프트웨어로 제어
> - 기존 라우터/스위치의 한계를 보완하고, 경로를 동적으로 제어 가능
> - 펌웨어 변경 없이 특정 서비스의 전송 경로를 유연하게 조정',
  'A',
  '네트워크 제어 영역과 데이터 전달 영역을 분리해 중앙에서 프로그래밍 가능한 구조로 만드는 기술은 SDN입니다.',
  'past:2024-2:Q92'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'SDN(Software Defined Networking)', 1),
  (@q_id, 'B', 'NFS(Network File System)',         0),
  (@q_id, 'C', 'Network Mapper',                   0),
  (@q_id, 'D', 'AOE Network',                      0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');


/* =======================================================
 * Q93. 접근통제 정책이 아닌 것  [정답: B]
 *  - topic_id = 15110 (접근통제 정책)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15110,
  'WRITTEN',
  'MCQ',
  'EASY',
  '정보보안을 위한 접근통제 정책 종류에 해당하지 않는 것은?',
  'B',
  '대표적인 접근통제 정책은 임의적(DAC), 강제적(MAC), 역할 기반(RBAC)입니다. “데이터 전환 접근통제”라는 용어는 표준 정책 분류에 해당하지 않습니다.',
  'past:2024-2:Q93'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '임의적 접근통제(DAC)',  0),
  (@q_id, 'B', '데이터 전환 접근통제',   1),
  (@q_id, 'C', '강제적 접근통제(MAC)',  0),
  (@q_id, 'D', '역할 기반 접근통제(RBAC)', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_BASE_DRM');


/* =======================================================
 * Q94. 병행수행 문제점이 아닌 것  [정답: B]
 *  - topic_id = 15111 (트랜잭션/병행제어)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15111,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '트랜잭션 병행수행으로 발생할 수 있는 문제점이 아닌 것은?',
  'B',
  '동시성 제어 실패 시 대표 문제는 갱신 분실, 모순성, 연쇄 복귀 등입니다. “완료 의존성”은 일반적으로 병행수행 이상 현상으로 분류되지 않습니다.',
  'past:2024-2:Q94'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '갱신 분실(Lost Update)',          0),
  (@q_id, 'B', '완료 의존성(Committed Dependency)', 1),
  (@q_id, 'C', '모순성(Inconsistency)',            0),
  (@q_id, 'D', '연쇄 복귀(Cascading Rollback)',    0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'DB_SQL_TX');


/* =======================================================
 * Q95. USN / RFID  [정답: C]
 *  - topic_id = 15112 (USN/RFID/유비쿼터스)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15112,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 괄호에 들어갈 기술은 무엇인가?

> (    ) / RFID 는 모든 것에 태그를 부착하고, 태그에서 수집한 정보를
>  무선으로 수집·전송할 수 있도록 구성한 네트워크를 말한다.',
  'C',
  '각종 센서와 RFID 태그로부터 정보를 수집·전송하는 유비쿼터스 센서 네트워크는 USN(Ubiquitous Sensor Network)입니다.',
  'past:2024-2:Q95'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'SON', 0),
  (@q_id, 'B', 'GIS', 0),
  (@q_id, 'C', 'USN', 1),
  (@q_id, 'D', 'UWB', 0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');


/* =======================================================
 * Q96. 소프트웨어 재공학 – 틀린 설명  [정답: D]
 *  - topic_id = 15113 (SW 재공학/유지보수)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15113,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '소프트웨어 재공학(Reengineering)에 대한 설명으로 틀린 것은?',
  'D',
  '재공학은 품질과 유지보수성을 높여 수명 연장과 함께 총 소유 비용을 줄이려는 활동입니다. “비용이 증가하는 것이 일반적”이라는 설명은 재공학의 목적과 맞지 않습니다.',
  'past:2024-2:Q96'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '기존 시스템을 활용해 보다 나은 시스템을 다시 구축하는 활동이다.', 0),
  (@q_id, 'B', '유지보수 비용 절감을 위해 유지보수성과 품질을 높이는 것이 목적이다.', 0),
  (@q_id, 'C', '소프트웨어 위기 해결 방안 중 하나로 사용된다.',                       0),
  (@q_id, 'D', '수명은 연장되지만 개발 비용이 증가하는 것이 일반적이다.',             1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_COST_PM');


/* =======================================================
 * Q97. 양방향 암호가 아닌 것 (해시)  [정답: D]
 *  - topic_id = 15106 (암호/해시)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15106,
  'WRITTEN',
  'MCQ',
  'EASY',
  '다음 중 양방향(복호화 가능한) 암호화 방식이 아닌 것은?',
  'D',
  'SEED, DES, RSA는 복호화가 가능한 양방향 암호(대칭/비대칭)입니다. 해시 함수는 원래 평문을 복구할 수 없는 단방향 함수이므로 양방향 암호에 해당하지 않습니다.',
  'past:2024-2:Q97'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'SEED',      0),
  (@q_id, 'B', 'DES',       0),
  (@q_id, 'C', 'RSA',       0),
  (@q_id, 'D', 'Hash 함수', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_ATTACK_DEFENSE');


/* =======================================================
 * Q98. 데이터 다이어트  [정답: B]
 *  - topic_id = 15114 (데이터 관리/최적화)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15114,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '데이터 다이어트(Data Diet)에 대한 설명으로 옳은 것은?',
  'B',
  '데이터 다이어트는 데이터 압축, 중복 제거, 재분류 등을 통해 저장·관리 효율을 높이고 불필요한 데이터를 줄이는 활동을 의미합니다.',
  'past:2024-2:Q98'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '대용량 분산 처리를 위한 프로그래밍 모델',                 0),
  (@q_id, 'B', '압축·중복 제거·재분류를 통해 데이터를 정리하는 작업',     1),
  (@q_id, 'C', '데이터 집합에서 유용한 정보를 찾는 마이닝 기법',           0),
  (@q_id, 'D', '초대용량 정형·비정형 데이터 집합',                         0);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'IT_NEW_DOCKER_SDN');


/* =======================================================
 * Q99. 테일러링 내부 기준이 아닌 것  [정답: D]
 *  - topic_id = 15115 (테일러링/방법론)
 * ======================================================= */
INSERT INTO question (
  cert_id, topic_id, mode, type, difficulty,
  stem, answer_key, solution_text, source
) VALUES (
  1,
  15115,
  'WRITTEN',
  'MCQ',
  'NORMAL',
  '테일러링(Tailoring) 시 고려하는 내부 기준에 해당하지 않는 것은?',
  'D',
  '테일러링 기준은 보통 프로젝트 특성, 기술 환경, 일정·비용, 조직 표준/품질 기준 등입니다. 구성원 개개인의 능력은 인력 배치 요소이지 방법론 내부 기준으로 보지는 않습니다.',
  'past:2024-2:Q99'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', '기술 환경',              0),
  (@q_id, 'B', '납기/비용',              0),
  (@q_id, 'C', '국제 표준·품질 기준',    0),
  (@q_id, 'D', '구성원 개인 능력',       1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SW_COST_PM');


/* =======================================================
 * Q100. OWASP  [정답: D]
 *  - topic_id = 15101 (보안 단체/가이드라인)
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
  'OWASP(the Open Web Application Security Project)에 대한 설명으로 옳은 것은?',
  'D',
  'OWASP는 웹 애플리케이션 보안 취약점(예: Top 10)을 연구·정리하고 가이드라인을 제공하는 국제 비영리 단체입니다.',
  'past:2024-2:Q100'
);

SET @q_id := LAST_INSERT_ID();

INSERT INTO question_choice (question_id, label, content, is_correct) VALUES
  (@q_id, 'A', 'P2P로 금융 거래 정보를 분산 저장하는 기술',                0),
  (@q_id, 'B', '대표적인 사례로 블록체인이 있다.',                          0),
  (@q_id, 'C', '양자 통신용 비밀키 분배·관리 기술',                         0),
  (@q_id, 'D', '웹 취약점·악성 스크립트 등 애플리케이션 보안을 연구하는 비영리 단체', 1);

INSERT INTO question_tag (question_id, tag) VALUES
  (@q_id, 'SEC_ATTACK_DEFENSE');
