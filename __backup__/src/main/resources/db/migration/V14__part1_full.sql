-- V14__part1_full_pack.sql
-- 목적:
-- 1) 세세항목(1.1.1.1 ~ 1.1.1.6) 각각에 5문항씩 데모 문제 INSERT
-- 2) 각 문항을 해당 세세항목 topic에 question_topic 매핑
-- 3) 각 세세항목(topic_id)에 concept 1건 연결(없으면 생성)
-- 4) 각 concept에 OX 미니체크 4문항 시드(중복 방지)
--    (메인 학습은 Topic 기반, 보조학습(Quick)은 재활용 가능)
-- ※ 톱레벨 SQL에서 실패를 바로 드러내기 위해 1/0 guard 사용

SET @CERT_ID := 1;

-- ===== 세세항목 id 캐시 =====
SELECT id INTO @T_1111 FROM topic WHERE cert_id=@CERT_ID AND code='1.1.1.1' LIMIT 1; -- 플랫폼 기능 분석
SELECT id INTO @T_1112 FROM topic WHERE cert_id=@CERT_ID AND code='1.1.1.2' LIMIT 1; -- 플랫폼 성능 특성 분석
SELECT id INTO @T_1113 FROM topic WHERE cert_id=@CERT_ID AND code='1.1.1.3' LIMIT 1; -- 운영체제 분석
SELECT id INTO @T_1114 FROM topic WHERE cert_id=@CERT_ID AND code='1.1.1.4' LIMIT 1; -- 네트워크 분석
SELECT id INTO @T_1115 FROM topic WHERE cert_id=@CERT_ID AND code='1.1.1.5' LIMIT 1; -- DBMS 분석
SELECT id INTO @T_1116 FROM topic WHERE cert_id=@CERT_ID AND code='1.1.1.6' LIMIT 1; -- 비즈니스융합분석

-- ===== 존재 가드(없으면 의도적으로 1/0로 실패시킴) =====
SELECT IF(@T_1111 IS NULL, (SELECT 1/0 FROM DUAL), 0) AS assert_topic_1111;
SELECT IF(@T_1112 IS NULL, (SELECT 1/0 FROM DUAL), 0) AS assert_topic_1112;
SELECT IF(@T_1113 IS NULL, (SELECT 1/0 FROM DUAL), 0) AS assert_topic_1113;
SELECT IF(@T_1114 IS NULL, (SELECT 1/0 FROM DUAL), 0) AS assert_topic_1114;
SELECT IF(@T_1115 IS NULL, (SELECT 1/0 FROM DUAL), 0) AS assert_topic_1115;
SELECT IF(@T_1116 IS NULL, (SELECT 1/0 FROM DUAL), 0) AS assert_topic_1116;

-- =========================================================
-- 공통 헬퍼: topic_code JSON 비교는 JSON_UNQUOTE로 안전하게
-- =========================================================
-- 주석: 아래 모든 WHERE JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='x' 형태 사용

-- =========================================================
-- 각 세세항목당 5문항 (idempotent: WHERE NOT EXISTS로 중복 방지)
-- =========================================================

-- ---------- 1.1.1.1 플랫폼 기능 분석 ----------
INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '플랫폼 기능 분석의 목적은 무엇인가?',
       JSON_ARRAY('플랫폼의 비기능 요구를 검증','플랫폼이 제공하는 핵심 기능을 파악','운영체제 커널 코드 최적화','DBMS의 내부 구조 변경'),
       1,
       '업무 요구와의 적합성 확인을 위해 플랫폼 핵심 기능을 파악한다.',
       JSON_OBJECT('tags', JSON_ARRAY('플랫폼','기능분석'), 'topic_code','1.1.1.1')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='플랫폼 기능 분석의 목적은 무엇인가?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.1'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '플랫폼 기능을 점검할 때 우선 확인해야 할 것은?',
       JSON_ARRAY('UI 색상 규칙','서비스 제공 API/SDK 스펙','네트워크 케이블 규격','프린터 드라이버 버전'),
       1,
       'API/SDK 스펙이 제공하는 기능/제약을 먼저 확인한다.',
       JSON_OBJECT('tags', JSON_ARRAY('플랫폼','API'), 'topic_code','1.1.1.1')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='플랫폼 기능을 점검할 때 우선 확인해야 할 것은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.1'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '플랫폼 적합성 판단 시 덜 중요한 항목은?',
       JSON_ARRAY('핵심 요구에 대한 지원 여부','확장 포인트(Plugin) 유무','테마 색상 커스터마이징 난이도','권한/보안 모델'),
       2,
       '테마 색상은 본질 기능 적합성보다 우선순위가 낮다.',
       JSON_OBJECT('tags', JSON_ARRAY('플랫폼'), 'topic_code','1.1.1.1')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='플랫폼 적합성 판단 시 덜 중요한 항목은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.1'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '플랫폼 기능 갭을 메우는 일반적 방법은?',
       JSON_ARRAY('핵심 요구 변경','플랫폼 교체만 고려','Extension/Plugin으로 보완','DB 재설계'),
       2,
       '확장 포인트로 기능 갭을 보완하는 전략을 검토한다.',
       JSON_OBJECT('tags', JSON_ARRAY('플랫폼','확장성'), 'topic_code','1.1.1.1')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='플랫폼 기능 갭을 메우는 일반적 방법은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.1'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '기능 분석 산출물로 적절한 것은?',
       JSON_ARRAY('성능 테스트 스크립트','기능 매트릭스(요구대비지원)','네트워크 패킷 캡처','OS 스케줄러 설정'),
       1,
       '요구 대비 플랫폼 지원 여부를 정리한 기능 매트릭스가 핵심 산출물.',
       JSON_OBJECT('tags', JSON_ARRAY('플랫폼','분석산출물'), 'topic_code','1.1.1.1')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='기능 분석 산출물로 적절한 것은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.1'
);

INSERT IGNORE INTO question_topic(question_id, topic_id)
SELECT q.id, @T_1111
  FROM question q
 WHERE JSON_UNQUOTE(JSON_EXTRACT(q.meta_json,'$.topic_code')) = '1.1.1.1';

-- ---------- 1.1.1.2 플랫폼 성능 특성 분석 ----------
INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '플랫폼 성능 특성 분석의 핵심은?',
       JSON_ARRAY('디자인 시스템 도입','CPU/메모리/IO 병목 파악','사내 규정 검토','개발자 수 파악'),
       1,
       '리소스 사용 특성과 병목 구간을 파악한다.',
       JSON_OBJECT('tags', JSON_ARRAY('플랫폼','성능'), 'topic_code','1.1.1.2')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='플랫폼 성능 특성 분석의 핵심은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.2'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '성능 분석에서 먼저 볼 지표는?',
       JSON_ARRAY('브랜드 인지도','평균 응답시간/백분위','회의 참석율','디자인 가이드 준수'),
       1,
       '평균/백분위 응답시간 등 사용자 체감 지표가 중요하다.',
       JSON_OBJECT('tags', JSON_ARRAY('성능','지표'), 'topic_code','1.1.1.2')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='성능 분석에서 먼저 볼 지표는?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.2'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '부하테스트 전 필수 준비는?',
       JSON_ARRAY('색상 팔레트 정의','테스트 데이터/시나리오 준비','신규 기능 기획','로고 변경'),
       1,
       '현실적인 테스트 데이터/시나리오가 필수다.',
       JSON_OBJECT('tags', JSON_ARRAY('성능','테스트'), 'topic_code','1.1.1.2')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='부하테스트 전 필수 준비는?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.2'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '성능 개선의 일반적 순서는?',
       JSON_ARRAY('최적화→측정→목표 정의','측정→목표 정의→최적화','배포→측정→최적화','기획→디자인→개발'),
       1,
       '우선 측정으로 병목을 찾고 목표를 정한 뒤 최적화한다.',
       JSON_OBJECT('tags', JSON_ARRAY('성능','튜닝'), 'topic_code','1.1.1.2')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='성능 개선의 일반적 순서는?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.2'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '캐시 전략 수립 시 고려 요소는?',
       JSON_ARRAY('브랜딩 문구','무결성/TTL/적중률','사내 교육 횟수','회의록 수'),
       1,
       '무결성, TTL, 적중률을 균형있게 설계한다.',
       JSON_OBJECT('tags', JSON_ARRAY('캐시'), 'topic_code','1.1.1.2')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='캐시 전략 수립 시 고려 요소는?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.2'
);

INSERT IGNORE INTO question_topic(question_id, topic_id)
SELECT q.id, @T_1112
  FROM question q
 WHERE JSON_UNQUOTE(JSON_EXTRACT(q.meta_json,'$.topic_code')) = '1.1.1.2';

-- ---------- 1.1.1.3 운영체제 분석 ----------
INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '운영체제 선택 시 우선 고려할 요소는?',
       JSON_ARRAY('아이콘 세트','스케줄러/메모리 관리','폰트 종류','테마 색상'),
       1,
       '스케줄링과 메모리 관리 등 핵심 특성이 중요하다.',
       JSON_OBJECT('tags', JSON_ARRAY('OS'), 'topic_code','1.1.1.3')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='운영체제 선택 시 우선 고려할 요소는?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.3'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       'OS I/O 성능 진단 도구로 적절한 것은?',
       JSON_ARRAY('ping','iostat/vmstat','dig','traceroute'),
       1,
       'iostat/vmstat 등 시스템 계측 도구를 사용한다.',
       JSON_OBJECT('tags', JSON_ARRAY('OS','I/O'), 'topic_code','1.1.1.3')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='OS I/O 성능 진단 도구로 적절한 것은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.3'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '컨테이너 환경에서 커널 의존 동작 문제 해결은?',
       JSON_ARRAY('컨테이너 내부 커널 교체','호스트 커널 튜닝/권한 설정','로고 리디자인','테마 교체'),
       1,
       '호스트 커널 설정/권한 조정이 핵심이다.',
       JSON_OBJECT('tags', JSON_ARRAY('컨테이너'), 'topic_code','1.1.1.3')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='컨테이너 환경에서 커널 의존 동작 문제 해결은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.3'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '메모리 누수 의심 시 1차 대응은?',
       JSON_ARRAY('재부팅 반복','프로파일링/덤프 분석','테마 변경','폰트 변경'),
       1,
       '프로파일링과 덤프 분석으로 원인을 특정한다.',
       JSON_OBJECT('tags', JSON_ARRAY('메모리'), 'topic_code','1.1.1.3')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='메모리 누수 의심 시 1차 대응은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.3'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '파일 시스템 선택 시 고려 요소는?',
       JSON_ARRAY('아이콘 크기','저널링/일관성/성능','배경화면','마우스 감도'),
       1,
       '저널링/일관성/성능 특성이 중요하다.',
       JSON_OBJECT('tags', JSON_ARRAY('FS'), 'topic_code','1.1.1.3')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='파일 시스템 선택 시 고려 요소는?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.3'
);

INSERT IGNORE INTO question_topic(question_id, topic_id)
SELECT q.id, @T_1113
  FROM question q
 WHERE JSON_UNQUOTE(JSON_EXTRACT(q.meta_json,'$.topic_code')) = '1.1.1.3';

-- ---------- 1.1.1.4 네트워크 분석 ----------
INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '네트워크 병목 1차 진단은 무엇으로 하는가?',
       JSON_ARRAY('HTTP 헤더 꾸미기','ping/traceroute/mtu 점검','폰트 캐싱','로고 스프라이트'),
       1,
       '기본 진단 도구로 연결/경로/MTU를 확인한다.',
       JSON_OBJECT('tags', JSON_ARRAY('네트워크'), 'topic_code','1.1.1.4')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='네트워크 병목 1차 진단은 무엇으로 하는가?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.4'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       'TLS 핸드셰이크 최적화 방안은?',
       JSON_ARRAY('서버 인증서 길이 늘리기','세션 재사용/ALPN','테마 변경','폰트 압축'),
       1,
       '세션 재사용과 ALPN 사용으로 지연을 줄인다.',
       JSON_OBJECT('tags', JSON_ARRAY('TLS'), 'topic_code','1.1.1.4')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='TLS 핸드셰이크 최적화 방안은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.4'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '대역폭 포화 시 우선 조치는?',
       JSON_ARRAY('색상 최적화','QoS/트래픽 셰이핑','폰트 최적화','배경화면 변경'),
       1,
       'QoS/셰이핑으로 우선순위를 제어한다.',
       JSON_OBJECT('tags', JSON_ARRAY('QoS'), 'topic_code','1.1.1.4')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='대역폭 포화 시 우선 조치는?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.4'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       'DNS 지연 문제 진단 도구는?',
       JSON_ARRAY('dig','sed','awk','less'),
       0,
       'DNS는 dig로 질의/응답/지연을 확인한다.',
       JSON_OBJECT('tags', JSON_ARRAY('DNS'), 'topic_code','1.1.1.4')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='DNS 지연 문제 진단 도구는?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.4'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       'L4/L7 구분 설명으로 맞는 것은?',
       JSON_ARRAY('L4는 패킷/포트 기반, L7은 컨텐츠/프로토콜 기반','둘 다 패킷만 본다','둘 다 컨텐츠만 본다','구분 없음'),
       0,
       'L4는 전송계층, L7은 응용계층 특성이다.',
       JSON_OBJECT('tags', JSON_ARRAY('OSI'), 'topic_code','1.1.1.4')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='L4/L7 구분 설명으로 맞는 것은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.4'
);

INSERT IGNORE INTO question_topic(question_id, topic_id)
SELECT q.id, @T_1114
  FROM question q
 WHERE JSON_UNQUOTE(JSON_EXTRACT(q.meta_json,'$.topic_code')) = '1.1.1.4';

-- ---------- 1.1.1.5 DBMS 분석 ----------
INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       'DBMS 인덱스 설계 기본 원칙은?',
       JSON_ARRAY('모든 컬럼에 인덱스','선택도 높은 컬럼 우선','색상 테마 우선','로고 크기 우선'),
       1,
       '선택도 높은 컬럼을 우선으로 설계한다.',
       JSON_OBJECT('tags', JSON_ARRAY('DBMS','인덱스'), 'topic_code','1.1.1.5')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='DBMS 인덱스 설계 기본 원칙은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.5'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '트랜잭션 격리수준 RC의 특징은?',
       JSON_ARRAY('Dirty Read 허용','Non-Repeatable Read 허용','항상 직렬화','스냅샷 강제'),
       1,
       'RC는 NR 허용(일반 정의).',
       JSON_OBJECT('tags', JSON_ARRAY('DBMS','트랜잭션'), 'topic_code','1.1.1.5')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='트랜잭션 격리수준 RC의 특징은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.5'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '실행계획에서 Nested Loop가 유리한 경우?',
       JSON_ARRAY('둘 다 대량 테이블','드라이빙 테이블 소량 + 인덱스 적중','둘 다 인덱스 없음','항상 Hash Join'),
       1,
       '소량 드라이빙 + 인덱스 적중 시 유리하다.',
       JSON_OBJECT('tags', JSON_ARRAY('실행계획'), 'topic_code','1.1.1.5')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='실행계획에서 Nested Loop가 유리한 경우?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.5'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '파티셔닝의 주 목적은?',
       JSON_ARRAY('디자인 통일','대용량 데이터 관리/성능','폰트 관리','테마 관리'),
       1,
       '관리성과 성능을 위한 분할이다.',
       JSON_OBJECT('tags', JSON_ARRAY('파티셔닝'), 'topic_code','1.1.1.5')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='파티셔닝의 주 목적은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.5'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       'Slow Query 대응 1차 조치는?',
       JSON_ARRAY('재부팅','쿼리/인덱스 점검','테마 변경','로고 교체'),
       1,
       '쿼리/인덱스 점검이 우선이다.',
       JSON_OBJECT('tags', JSON_ARRAY('튜닝'), 'topic_code','1.1.1.5')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='Slow Query 대응 1차 조치는?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.5'
);

INSERT IGNORE INTO question_topic(question_id, topic_id)
SELECT q.id, @T_1115
  FROM question q
 WHERE JSON_UNQUOTE(JSON_EXTRACT(q.meta_json,'$.topic_code')) = '1.1.1.5';

-- ---------- 1.1.1.6 비즈니스융합분석 ----------
INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '비즈니스 융합 분석의 1차 목표는?',
       JSON_ARRAY('남는 예산 소진','신규 시너지 식별','테마 일관성','로고 통일'),
       1,
       '이질 도메인 결합으로 시너지를 식별한다.',
       JSON_OBJECT('tags', JSON_ARRAY('비즈니스','융합'), 'topic_code','1.1.1.6')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='비즈니스 융합 분석의 1차 목표는?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.6'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '융합 기회 탐색 시 고려할 요소는?',
       JSON_ARRAY('조직 로고 크기','고객 여정/데이터 흐름','폰트 두께','아이콘 스타일'),
       1,
       '고객 여정과 데이터 흐름을 본다.',
       JSON_OBJECT('tags', JSON_ARRAY('고객여정'), 'topic_code','1.1.1.6')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='융합 기회 탐색 시 고려할 요소는?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.6'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '융합 PoC 성공 지표는?',
       JSON_ARRAY('컬러 팔레트 다양성','성과지표(전환/리텐션/매출)','폰트 수','슬로건 길이'),
       1,
       '전환/리텐션/매출 등 성과지표로 판단한다.',
       JSON_OBJECT('tags', JSON_ARRAY('지표'), 'topic_code','1.1.1.6')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='융합 PoC 성공 지표는?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.6'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '데이터 융합 시 주의할 점은?',
       JSON_ARRAY('스키마/품질/정합성','로고 위치','폰트 정렬','색상 대비'),
       0,
       '스키마/품질/정합성이 핵심이다.',
       JSON_OBJECT('tags', JSON_ARRAY('데이터품질'), 'topic_code','1.1.1.6')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='데이터 융합 시 주의할 점은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.6'
);

INSERT INTO question(source, year, round, difficulty, stem, choices_json, answer_idx, exp, meta_json)
SELECT 'custom', 2025, 1, 2,
       '파트너십 검토 시 핵심은?',
       JSON_ARRAY('브랜드 컬러','계약/데이터 공유 규정','폰트','배경 이미지'),
       1,
       '계약/데이터 공유 규정을 명확히 한다.',
       JSON_OBJECT('tags', JSON_ARRAY('거버넌스'), 'topic_code','1.1.1.6')
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE stem='파트너십 검토 시 핵심은?'
     AND JSON_UNQUOTE(JSON_EXTRACT(meta_json,'$.topic_code'))='1.1.1.6'
);

INSERT IGNORE INTO question_topic(question_id, topic_id)
SELECT q.id, @T_1116
  FROM question q
 WHERE JSON_UNQUOTE(JSON_EXTRACT(q.meta_json,'$.topic_code')) = '1.1.1.6';

-- =========================================================
-- (추가) 각 세세항목에 concept 1건 연결 + OX 미니체크 4문항 시드
-- =========================================================

-- 공통: 개념 생성 (없을 때만), 그리고 concept_id 캐시
-- 1.1.1.1
INSERT INTO concept (cert_id, category, title, summary, pitfalls, examples_json, tags_json, topic_id, created_at)
SELECT @CERT_ID, '커리큘럼', '플랫폼 기능 분석 - 개념(데모)',
       '플랫폼이 제공하는 핵심 기능과 확장 포인트를 파악하는 것이 목적입니다.',
       '핵심 요구 적합성보다 UI/테마를 우선 고려하는 실수 주의',
       JSON_ARRAY('핵심 기능 매트릭스 작성','API/SDK 스펙 리뷰'),
       JSON_ARRAY('플랫폼','기능분석','1.1.1.1'),
       @T_1111, NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id=@T_1111);
SET @C_1111 := (SELECT id FROM concept WHERE topic_id=@T_1111 ORDER BY id DESC LIMIT 1);

-- 1.1.1.2
INSERT INTO concept (cert_id, category, title, summary, pitfalls, examples_json, tags_json, topic_id, created_at)
SELECT @CERT_ID, '커리큘럼', '플랫폼 성능 특성 분석 - 개념(데모)',
       'CPU/메모리/IO 특성과 병목을 파악하고 개선 목표를 수립합니다.',
       '측정 없이 최적화부터 시도하는 실수 주의',
       JSON_ARRAY('APM지표 읽기','부하 시나리오 설계'),
       JSON_ARRAY('플랫폼','성능','1.1.1.2'),
       @T_1112, NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id=@T_1112);
SET @C_1112 := (SELECT id FROM concept WHERE topic_id=@T_1112 ORDER BY id DESC LIMIT 1);

-- 1.1.1.3
INSERT INTO concept (cert_id, category, title, summary, pitfalls, examples_json, tags_json, topic_id, created_at)
SELECT @CERT_ID, '커리큘럼', '운영체제 분석 - 개념(데모)',
       '스케줄러/메모리 관리/파일시스템 등 핵심 특성을 이해합니다.',
       '아이콘/테마 등 비핵심에 치우치는 실수 주의',
       JSON_ARRAY('프로세스 모니터링','덤프분석'),
       JSON_ARRAY('OS','1.1.1.3'),
       @T_1113, NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id=@T_1113);
SET @C_1113 := (SELECT id FROM concept WHERE topic_id=@T_1113 ORDER BY id DESC LIMIT 1);

-- 1.1.1.4
INSERT INTO concept (cert_id, category, title, summary, pitfalls, examples_json, tags_json, topic_id, created_at)
SELECT @CERT_ID, '커리큘럼', '네트워크 분석 - 개념(데모)',
       '기본 진단(ping/traceroute/MTU)과 TLS 최적화, QoS/셰이핑 개념을 학습합니다.',
       '툴 사용없이 감으로 판단하는 실수 주의',
       JSON_ARRAY('traceroute 사용법','QoS 기본'),
       JSON_ARRAY('네트워크','1.1.1.4'),
       @T_1114, NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id=@T_1114);
SET @C_1114 := (SELECT id FROM concept WHERE topic_id=@T_1114 ORDER BY id DESC LIMIT 1);

-- 1.1.1.5
INSERT INTO concept (cert_id, category, title, summary, pitfalls, examples_json, tags_json, topic_id, created_at)
SELECT @CERT_ID, '커리큘럼', 'DBMS 분석 - 개념(데모)',
       '인덱스/실행계획/파티셔닝 등의 기본 원리를 이해합니다.',
       '무조건 모든 컬럼 인덱싱 같은 극단적 선택 주의',
       JSON_ARRAY('실행계획 읽기','인덱스 설계'),
       JSON_ARRAY('DBMS','1.1.1.5'),
       @T_1115, NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id=@T_1115);
SET @C_1115 := (SELECT id FROM concept WHERE topic_id=@T_1115 ORDER BY id DESC LIMIT 1);

-- 1.1.1.6
INSERT INTO concept (cert_id, category, title, summary, pitfalls, examples_json, tags_json, topic_id, created_at)
SELECT @CERT_ID, '커리큘럼', '비즈니스 융합 분석 - 개념(데모)',
       '고객 여정/데이터 흐름을 기준으로 융합 시너지를 식별합니다.',
       '브랜딩 요소 위주로만 판단하는 실수 주의',
       JSON_ARRAY('융합 PoC 설계','성과지표 정의'),
       JSON_ARRAY('비즈니스','융합','1.1.1.6'),
       @T_1116, NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id=@T_1116);
SET @C_1116 := (SELECT id FROM concept WHERE topic_id=@T_1116 ORDER BY id DESC LIMIT 1);

-- ===== 각 concept에 OX 미니체크 4문항(중복 방지) =====
-- 컬럼명: explanation (현재 스키마 기준). 다른 이름이면 엔티티/DDL과 맞춰주세요.

-- 1.1.1.1
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1111, '플랫폼 기능 분석의 1차 목적은 핵심 기능 파악이다.', JSON_ARRAY('O','X'), 0, '핵심 기능 적합성 확인이 우선입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1111 AND stem='플랫폼 기능 분석의 1차 목적은 핵심 기능 파악이다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1111, 'API/SDK 스펙 리뷰는 기능 분석에 중요하지 않다.', JSON_ARRAY('O','X'), 1, 'API/SDK 스펙은 플랫폼 기능/제약 파악의 핵심입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1111 AND stem='API/SDK 스펙 리뷰는 기능 분석에 중요하지 않다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1111, '확장 포인트(Plugin)로 기능 갭을 보완할 수 있다.', JSON_ARRAY('O','X'), 0, '확장 포인트로 기능 갭을 메우는 전략이 일반적입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1111 AND stem='확장 포인트(Plugin)로 기능 갭을 보완할 수 있다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1111, '기능 분석 산출물로 기능 매트릭스(요구 대비 지원)가 적절하다.', JSON_ARRAY('O','X'), 0, '요구 대비 지원 여부 표가 핵심 산출물입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1111 AND stem='기능 분석 산출물로 기능 매트릭스(요구 대비 지원)가 적절하다.');

-- 1.1.1.2
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1112, '성능 분석의 핵심은 병목 파악이다.', JSON_ARRAY('O','X'), 0, 'CPU/메모리/IO 병목 파악이 핵심입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1112 AND stem='성능 분석의 핵심은 병목 파악이다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1112, '사용자 체감 지표(응답시간/백분위)는 중요하지 않다.', JSON_ARRAY('O','X'), 1, '체감 지표는 매우 중요합니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1112 AND stem='사용자 체감 지표(응답시간/백분위)는 중요하지 않다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1112, '부하테스트는 데이터/시나리오 준비 없이도 의미 있다.', JSON_ARRAY('O','X'), 1, '현실적인 데이터/시나리오가 필수입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1112 AND stem='부하테스트는 데이터/시나리오 준비 없이도 의미 있다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1112, '측정 없이 최적화부터 하는 것이 효율적이다.', JSON_ARRAY('O','X'), 1, '항상 측정→목표→최적화 순서가 바람직합니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1112 AND stem='측정 없이 최적화부터 하는 것이 효율적이다.');

-- 1.1.1.3
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1113, 'OS 선택 시 스케줄러/메모리 관리가 핵심 고려다.', JSON_ARRAY('O','X'), 0, '핵심 특성입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1113 AND stem='OS 선택 시 스케줄러/메모리 관리가 핵심 고려다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1113, '컨테이너 문제는 컨테이너 내부 커널 교체로 해결된다.', JSON_ARRAY('O','X'), 1, '호스트 커널 설정/권한 조정이 핵심입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1113 AND stem='컨테이너 문제는 컨테이너 내부 커널 교체로 해결된다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1113, '메모리 누수 의심 시 프로파일링/덤프 분석이 1차 대응이다.', JSON_ARRAY('O','X'), 0, '맞습니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1113 AND stem='메모리 누수 의심 시 프로파일링/덤프 분석이 1차 대응이다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1113, '파일시스템 선택 시 저널링/일관성/성능이 중요하다.', JSON_ARRAY('O','X'), 0, '핵심 고려 요소입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1113 AND stem='파일시스템 선택 시 저널링/일관성/성능이 중요하다.');

-- 1.1.1.4
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1114, '네트워크 병목 1차 진단은 ping/traceroute/MTU 점검이다.', JSON_ARRAY('O','X'), 0, '기본 진단 도구입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1114 AND stem='네트워크 병목 1차 진단은 ping/traceroute/MTU 점검이다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1114, 'TLS 최적화는 세션 재사용/ALPN으로 지연을 줄인다.', JSON_ARRAY('O','X'), 0, '맞습니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1114 AND stem='TLS 최적화는 세션 재사용/ALPN으로 지연을 줄인다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1114, '대역폭 포화 시 QoS/셰이핑이 우선이다.', JSON_ARRAY('O','X'), 0, '맞습니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1114 AND stem='대역폭 포화 시 QoS/셰이핑이 우선이다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1114, 'DNS 지연 진단 도구는 dig가 대표적이다.', JSON_ARRAY('O','X'), 0, '대표 도구입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1114 AND stem='DNS 지연 진단 도구는 dig가 대표적이다.');

-- 1.1.1.5
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1115, '인덱스 설계는 선택도 높은 컬럼을 우선한다.', JSON_ARRAY('O','X'), 0, '맞습니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1115 AND stem='인덱스 설계는 선택도 높은 컬럼을 우선한다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1115, 'RC는 Non-Repeatable Read를 허용한다.', JSON_ARRAY('O','X'), 0, '일반적 정의상 허용합니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1115 AND stem='RC는 Non-Repeatable Read를 허용한다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1115, 'Nested Loop는 소량 드라이빙+인덱스 적중 시 유리하다.', JSON_ARRAY('O','X'), 0, '맞습니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1115 AND stem='Nested Loop는 소량 드라이빙+인덱스 적중 시 유리하다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1115, 'Slow Query 1차 대응은 쿼리/인덱스 점검이다.', JSON_ARRAY('O','X'), 0, '맞습니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1115 AND stem='Slow Query 1차 대응은 쿼리/인덱스 점검이다.');

-- 1.1.1.6
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1116, '융합 분석의 1차 목표는 신규 시너지 식별이다.', JSON_ARRAY('O','X'), 0, '맞습니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1116 AND stem='융합 분석의 1차 목표는 신규 시너지 식별이다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1116, '융합 기회 탐색은 고객 여정/데이터 흐름이 핵심이 아니다.', JSON_ARRAY('O','X'), 1, '핵심입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1116 AND stem='융합 기회 탐색은 고객 여정/데이터 흐름이 핵심이 아니다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1116, '데이터 융합 시 스키마/품질/정합성에 유의해야 한다.', JSON_ARRAY('O','X'), 0, '맞습니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1116 AND stem='데이터 융합 시 스키마/품질/정합성에 유의해야 한다.');
INSERT INTO concept_check (concept_id, stem, choices_json, answer_idx, explanation, created_at)
SELECT @C_1116, '파트너십 검토 시 계약/데이터 공유 규정은 부차적이다.', JSON_ARRAY('O','X'), 1, '핵심입니다.', NOW()
WHERE NOT EXISTS (SELECT 1 FROM concept_check WHERE concept_id=@C_1116 AND stem='파트너십 검토 시 계약/데이터 공유 규정은 부차적이다.');

-- (세세항목 6개 x 문제 5 = 30문항 기본 시드 + 각 세세항목 개념 1건 + OX 미니체크 4문항)
