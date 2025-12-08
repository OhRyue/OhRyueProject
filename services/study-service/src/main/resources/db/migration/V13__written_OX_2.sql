SET @cert_id := 1;

-- =========================================
-- Topic ID 상수 (2과목 소프트웨어 개발 – 2.x.x)
-- =========================================
SET @tp_21101 := 12101; -- 2.1.1 논리 데이터 저장소 확인
SET @tp_22101 := 12201; -- 2.2.1 연계 데이터 구성
SET @tp_22102 := 12202; -- 2.2.2 연계 메커니즘 구성
SET @tp_23101 := 12301; -- 2.3.1 개발환경 구축
SET @tp_23102 := 12302; -- 2.3.2 공통 모듈 구현
SET @tp_23103 := 12303; -- 2.3.3 서버 프로그램 구현
SET @tp_24101 := 12401; -- 2.4.1 배포 환경 구성
SET @tp_24102 := 12402; -- 2.4.2 설치 패키지 구성
SET @tp_24103 := 12403; -- 2.4.3 릴리즈 노트 작성
SET @tp_25101 := 12501; -- 2.5.1 인터페이스 설계 확인
SET @tp_25102 := 12502; -- 2.5.2 인터페이스 기능 구현

/* ========================================================
 * 2.1.1 논리 데이터 저장소 확인  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_21101, 'WRITTEN', 'OX', 'EASY',
       '논리 데이터 모델에서는 엔터티와 속성, 관계를 이용해 업무 데이터를 표현한다.',
       'O',
       '논리 데이터 모델은 업무 데이터를 엔터티·속성·관계로 추상화해 표현하는 기법입니다.',
       'seed:2.1.1:logical-model:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_21101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '논리 데이터 모델에서는 엔터티와 속성, 관계를 이용해 업무 데이터를 표현한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_21101, 'WRITTEN', 'OX', 'NORMAL',
       '논리 데이터 저장소 확인 단계에서는 테이블의 파티션 구성과 인덱스 종류 같은 물리 구조를 상세히 설계한다.',
       'X',
       '파티션·인덱스와 같은 물리 구조는 물리 설계 단계의 주요 대상이며, 논리 단계에서는 주로 엔터티·속성·관계 등을 검토합니다.',
       'seed:2.1.1:logical-vs-physical:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_21101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '논리 데이터 저장소 확인 단계에서는 테이블의 파티션 구성과 인덱스 종류 같은 물리 구조를 상세히 설계한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_21101, 'WRITTEN', 'OX', 'NORMAL',
       '논리 데이터 저장소를 점검할 때는 각 엔터티의 기본키와 관계 차수(1:1, 1:N, N:M) 등이 올바르게 정의되었는지 확인해야 한다.',
       'O',
       '논리 모델 검토 시에는 기본키, 외래키 후보, 관계 차수 및 선택성 등이 제대로 정의됐는지 확인하는 것이 중요합니다.',
       'seed:2.1.1:key-relationship:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_21101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '논리 데이터 저장소를 점검할 때는 각 엔터티의 기본키와 관계 차수(1:1, 1:N, N:M) 등이 올바르게 정의되었는지 확인해야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_21101, 'WRITTEN', 'OX', 'EASY',
       '정규화는 논리 데이터 모델에서 이상 현상을 줄이고 데이터의 일관성을 높이기 위한 대표적인 기법이다.',
       'O',
       '정규화는 삽입·삭제·갱신 이상을 줄여 데이터 무결성과 일관성을 높이는 논리 설계 기법입니다.',
       'seed:2.1.1:normalization:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_21101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정규화는 논리 데이터 모델에서 이상 현상을 줄이고 데이터의 일관성을 높이기 위한 대표적인 기법이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_21101, 'WRITTEN', 'OX', 'NORMAL',
       '논리 데이터 저장소 확인 단계에서는 향후 CRUD 매트릭스를 사용해 업무 기능과 데이터 조작의 누락 여부를 점검할 수 있다.',
       'O',
       'CRUD 매트릭스는 프로세스와 엔터티 간 생성·조회·갱신·삭제 관계를 확인해 누락된 기능이나 데이터 조작을 점검하는 도구입니다.',
       'seed:2.1.1:crud-matrix:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_21101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '논리 데이터 저장소 확인 단계에서는 향후 CRUD 매트릭스를 사용해 업무 기능과 데이터 조작의 누락 여부를 점검할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_21101, 'WRITTEN', 'OX', 'NORMAL',
       '논리 데이터 모델 검토 시에는 속성의 도메인(자료형·허용 값 범위 등)은 물리 설계 단계에서만 확인하면 되므로 굳이 신경 쓰지 않아도 된다.',
       'X',
       '속성의 도메인 정의는 논리 설계 단계에서부터 고려해야 하며, 이후 물리 설계에서 구체적인 타입과 제약으로 구체화됩니다.',
       'seed:2.1.1:domain:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_21101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '논리 데이터 모델 검토 시에는 속성의 도메인(자료형·허용 값 범위 등)은 물리 설계 단계에서만 확인하면 되므로 굳이 신경 쓰지 않아도 된다.%'
);

/* ========================================================
 * 2.2.1 연계 데이터 구성  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_22101, 'WRITTEN', 'OX', 'EASY',
       '연계 데이터 구성 단계에서는 송신 시스템과 수신 시스템 간에 교환할 데이터 항목과 형식을 정의한다.',
       'O',
       '연계 데이터 구성의 핵심은 인터페이스에서 주고받을 데이터 항목, 형식, 길이, 단위 등을 합의하는 것입니다.',
       'seed:2.2.1:data-items:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_22101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '연계 데이터 구성 단계에서는 송신 시스템과 수신 시스템 간에 교환할 데이터 항목과 형식을 정의한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_22101, 'WRITTEN', 'OX', 'NORMAL',
       '연계 데이터 구성 시에는 코드 값, 단위, 인코딩 방식 등이 시스템마다 달라도 실제 연계에는 영향을 주지 않는다.',
       'X',
       '코드 값·단위·인코딩이 다르면 변환 로직이 필요하고 오류가 발생하기 쉬우므로, 가능하면 표준을 맞추거나 명확한 매핑을 정의해야 합니다.',
       'seed:2.2.1:code-mapping:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_22101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '연계 데이터 구성 시에는 코드 값, 단위, 인코딩 방식 등이 시스템마다 달라도 실제 연계에는 영향을 주지 않는다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_22101, 'WRITTEN', 'OX', 'NORMAL',
       '연계 데이터 정의 시에는 필수/선택 여부와 허용 길이, 허용 값 범위 등을 명시해 검증 기준을 마련하는 것이 바람직하다.',
       'O',
       '필수 여부, 길이, 값 범위 등을 정의해야 인터페이스 구현 시 입력 검증 및 오류 처리가 명확해집니다.',
       'seed:2.2.1:validation-rules:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_22101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '연계 데이터 정의 시에는 필수/선택 여부와 허용 길이, 허용 값 범위 등을 명시해 검증 기준을 마련하는 것이 바람직하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_22101, 'WRITTEN', 'OX', 'EASY',
       '연계 데이터 구성 단계에서는 데이터 민감도(개인정보, 기밀 데이터 등)를 고려해 마스킹이나 암호화 필요성을 검토해야 한다.',
       'O',
       '민감한 데이터는 전송 시 암호화, 로그 마스킹 등의 요구가 발생하므로 연계 데이터 정의 단계에서부터 보안 요구를 함께 검토해야 합니다.',
       'seed:2.2.1:security:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_22101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '연계 데이터 구성 단계에서는 데이터 민감도(개인정보, 기밀 데이터 등)를 고려해 마스킹이나 암호화 필요성을 검토해야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_22101, 'WRITTEN', 'OX', 'NORMAL',
       '연계 데이터는 비즈니스 키와 상관없이 단순 일련번호만 포함하면 되며, 송신·수신 시스템의 동일 레코드를 식별할 필요는 없다.',
       'X',
       '연계 데이터에는 두 시스템에서 동일 객체를 식별할 수 있는 식별자(비즈니스 키나 매핑 키)가 포함되어야 동기화 및 추적이 가능합니다.',
       'seed:2.2.1:business-key:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_22101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '연계 데이터는 비즈니스 키와 상관없이 단순 일련번호만 포함하면 되며, 송신·수신 시스템의 동일 레코드를 식별할 필요는 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_22101, 'WRITTEN', 'OX', 'NORMAL',
       '일괄(batch) 연계에서는 변경분만 전송하는 증분 방식보다는 항상 전체 데이터를 전송하는 전량 방식만 사용할 수 있다.',
       'X',
       '일괄 연계에서도 업무 특성에 따라 전체 또는 변경분(증분) 전송을 선택할 수 있으며, 변경 이력과 기준 키 설계가 중요합니다.',
       'seed:2.2.1:batch-incremental:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_22101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '일괄(batch) 연계에서는 변경분만 전송하는 증분 방식보다는 항상 전체 데이터를 전송하는 전량 방식만 사용할 수 있다.%'
);

/* ========================================================
 * 2.2.2 연계 메커니즘 구성  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_22102, 'WRITTEN', 'OX', 'EASY',
       '연계 메커니즘 구성에서는 동기·비동기, 실시간·배치 등 연계 패턴을 결정한다.',
       'O',
       '요구되는 응답 시간, 트래픽 특성, 장애 영향도 등을 고려해 동기/비동기, 실시간/일괄 연계 방식을 결정해야 합니다.',
       'seed:2.2.2:pattern:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_22102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '연계 메커니즘 구성에서는 동기·비동기, 실시간·배치 등 연계 패턴을 결정한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_22102, 'WRITTEN', 'OX', 'NORMAL',
       '메시지 큐를 사용하는 비동기 연계에서는 송신 시스템이 메시지를 전송한 즉시 수신 시스템의 처리 결과를 반드시 받아야 한다.',
       'X',
       '비동기 큐 기반 연계는 전송과 처리를 분리해 송신 측이 즉시 결과를 기다리지 않는 구조가 일반적입니다.',
       'seed:2.2.2:async-queue:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_22102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '메시지 큐를 사용하는 비동기 연계에서는 송신 시스템이 메시지를 전송한 즉시 수신 시스템의 처리 결과를 반드시 받아야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_22102, 'WRITTEN', 'OX', 'NORMAL',
       '연계 메커니즘 설계 시에는 예외 처리 및 재전송(재시도) 전략을 함께 정의하는 것이 중요하다.',
       'O',
       '타임아웃, 통신 오류 등 예외 상황에서 재시도 간격, 최대 재시도 횟수, 보정 절차 등을 함께 정의해야 안정적인 연계가 가능합니다.',
       'seed:2.2.2:retry:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_22102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '연계 메커니즘 설계 시에는 예외 처리 및 재전송(재시도) 전략을 함께 정의하는 것이 중요하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_22102, 'WRITTEN', 'OX', 'EASY',
       '파일 기반 일괄 연계에서도 전송 성공 여부를 확인하기 위한 수신 확인(ACK) 또는 검증 절차가 필요하다.',
       'O',
       '파일 연계에서는 체크섬, 레코드 건수, 완료 플래그 파일 등을 이용해 송수신 성공 여부를 확인하는 절차가 필요합니다.',
       'seed:2.2.2:file-ack:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_22102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '파일 기반 일괄 연계에서도 전송 성공 여부를 확인하기 위한 수신 확인(ACK) 또는 검증 절차가 필요하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_22102, 'WRITTEN', 'OX', 'NORMAL',
       '연계 메커니즘 구성 시 트랜잭션 경계와 롤백 범위는 데이터베이스 설계 단계에서만 고려하면 되고 인터페이스 설계와는 무관하다.',
       'X',
       '연계 호출이 여러 시스템의 트랜잭션을 걸치는 경우 보상 트랜잭션, 부분 실패 처리 등 인터페이스 수준의 트랜잭션 정책이 중요합니다.',
       'seed:2.2.2:transaction:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_22102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '연계 메커니즘 구성 시 트랜잭션 경계와 롤백 범위는 데이터베이스 설계 단계에서만 고려하면 되고 인터페이스 설계와는 무관하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_22102, 'WRITTEN', 'OX', 'NORMAL',
       '동기 호출 방식은 사용자 경험 측면에서 항상 더 유리하므로 인터페이스는 가능하면 모두 동기 방식으로 설계한다.',
       'X',
       '긴 시간이 걸리는 처리나 대량 배치 작업은 동기 호출 시 응답 지연과 타임아웃 위험이 크므로 비동기 패턴을 검토해야 합니다.',
       'seed:2.2.2:synchronous:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_22102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '동기 호출 방식은 사용자 경험 측면에서 항상 더 유리하므로 인터페이스는 가능하면 모두 동기 방식으로 설계한다.%'
);

/* ========================================================
 * 2.3.1 개발환경 구축  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23101, 'WRITTEN', 'OX', 'EASY',
       '통합 개발 환경(IDE)은 편집기, 빌드 도구, 디버거 등을 통합 제공해 개발 생산성을 높이는 도구이다.',
       'O',
       'IDE는 코드 편집, 컴파일/빌드, 디버깅, 버전 관리 연동 등을 한 화면에서 제공해 개발 효율을 높여 줍니다.',
       'seed:2.3.1:ide:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '통합 개발 환경(IDE)은 편집기, 빌드 도구, 디버거 등을 통합 제공해 개발 생산성을 높이는 도구이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23101, 'WRITTEN', 'OX', 'NORMAL',
       '형상 관리 도구는 소스 코드 버전만 관리하며, 요구사항 문서나 설계 산출물은 관리 대상이 아니다.',
       'X',
       '형상 관리는 소스 코드뿐 아니라 요구사항, 설계서, 테스트 케이스 등 주요 산출물을 버전과 함께 관리하는 활동입니다.',
       'seed:2.3.1:scm:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '형상 관리 도구는 소스 코드 버전만 관리하며, 요구사항 문서나 설계 산출물은 관리 대상이 아니다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23101, 'WRITTEN', 'OX', 'EASY',
       '빌드 스크립트와 의존성 관리는 수동으로 수행하는 것보다 Maven, Gradle과 같은 도구를 사용하는 것이 일반적이다.',
       'O',
       '빌드 도구를 사용하면 의존성 관리, 컴파일, 패키징, 테스트 실행을 자동화해 일관성을 유지할 수 있습니다.',
       'seed:2.3.1:build-tool:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '빌드 스크립트와 의존성 관리는 수동으로 수행하는 것보다 Maven, Gradle과 같은 도구를 사용하는 것이 일반적이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23101, 'WRITTEN', 'OX', 'NORMAL',
       '개발환경 구축 시 로컬 개발 환경, 테스트 환경, 운영 환경의 설정 값이 서로 달라도 되므로 별도의 환경 분리 전략은 필요 없다.',
       'X',
       '환경별 설정은 분리하되, 가능한 한 구조를 통일하고 프로파일, 설정 파일, 환경 변수를 통해 관리하는 것이 바람직합니다.',
       'seed:2.3.1:env-profile:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '개발환경 구축 시 로컬 개발 환경, 테스트 환경, 운영 환경의 설정 값이 서로 달라도 되므로 별도의 환경 분리 전략은 필요 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23101, 'WRITTEN', 'OX', 'NORMAL',
       '개발 브랜치 전략을 수립하지 않고 모든 개발자가 동일한 기본 브랜치만 사용하는 것이 충돌 관리에 더 유리하다.',
       'X',
       '기본 브랜치(예: main)와 기능 브랜치, 릴리즈 브랜치 등을 구분해 사용하는 것이 변경 관리와 충돌 해결에 더 효율적입니다.',
       'seed:2.3.1:git-branch:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '개발 브랜치 전략을 수립하지 않고 모든 개발자가 동일한 기본 브랜치만 사용하는 것이 충돌 관리에 더 유리하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23101, 'WRITTEN', 'OX', 'EASY',
       'CI 서버를 구축하면 소스 코드 변경 시 자동 빌드와 단위 테스트 실행을 통해 품질 저하를 조기에 발견할 수 있다.',
       'O',
       'CI(지속적 통합)는 변경이 발생할 때마다 자동으로 빌드·테스트를 수행해 통합 시점의 문제를 빠르게 발견하도록 돕습니다.',
       'seed:2.3.1:ci:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'CI 서버를 구축하면 소스 코드 변경 시 자동 빌드와 단위 테스트 실행을 통해 품질 저하를 조기에 발견할 수 있다.%'
);

/* ========================================================
 * 2.3.2 공통 모듈 구현  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23102, 'WRITTEN', 'OX', 'EASY',
       '공통 모듈은 여러 기능에서 반복적으로 사용하는 로직을 재사용하기 위해 분리한 모듈이다.',
       'O',
       '로그, 인증, 권한, 유틸리티, 공통 DAO 등은 여러 기능에서 재사용되는 전형적인 공통 모듈의 예입니다.',
       'seed:2.3.2:common-module:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '공통 모듈은 여러 기능에서 반복적으로 사용하는 로직을 재사용하기 위해 분리한 모듈이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23102, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈은 재사용성이 높기 때문에 코딩 표준과 예외 처리 규칙을 통일할 필요가 없다.',
       'X',
       '공통 모듈은 시스템 전반에 영향을 주므로 코딩 규칙과 예외 처리 정책을 일관되게 적용하는 것이 특히 중요합니다.',
       'seed:2.3.2:standard:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '공통 모듈은 재사용성이 높기 때문에 코딩 표준과 예외 처리 규칙을 통일할 필요가 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23102, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈은 가능한 한 특정 업무 도메인에 종속되도록 설계해 재사용 시 커스터마이징 비용을 줄인다.',
       'X',
       '공통 모듈은 특정 도메인에 종속되기보다 범용적으로 설계해 여러 도메인에서 재사용 가능하도록 하는 것이 목표입니다.',
       'seed:2.3.2:domain-independent:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '공통 모듈은 가능한 한 특정 업무 도메인에 종속되도록 설계해 재사용 시 커스터마이징 비용을 줄인다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23102, 'WRITTEN', 'OX', 'EASY',
       '공통 모듈 변경 시 다른 모듈에 미치는 영향을 줄이기 위해 인터페이스(추상 타입)를 통해 의존성을 관리하는 것이 바람직하다.',
       'O',
       '인터페이스 기반 의존성 주입(DI)을 사용하면 공통 모듈 구현을 교체하더라도 사용하는 쪽의 영향을 최소화할 수 있습니다.',
       'seed:2.3.2:interface:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '공통 모듈 변경 시 다른 모듈에 미치는 영향을 줄이기 위해 인터페이스(추상 타입)를 통해 의존성을 관리하는 것이 바람직하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23102, 'WRITTEN', 'OX', 'NORMAL',
       '공통 모듈은 단위 테스트 대상에서 제외해도 전체 시스템 품질에는 영향을 주지 않는다.',
       'X',
       '공통 모듈은 여러 기능에서 사용되므로, 결함 발생 시 영향 범위가 넓어 단위 테스트를 특히 철저히 수행해야 합니다.',
       'seed:2.3.2:unit-test:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '공통 모듈은 단위 테스트 대상에서 제외해도 전체 시스템 품질에는 영향을 주지 않는다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23102, 'WRITTEN', 'OX', 'EASY',
       '공통 모듈의 인터페이스와 사용 방법은 별도의 개발자 문서나 예제 코드를 통해 공유하는 것이 좋다.',
       'O',
       '공통 모듈은 다양한 팀에서 사용하므로 API 문서와 샘플 코드를 제공해 사용법을 통일하는 것이 효과적입니다.',
       'seed:2.3.2:doc:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '공통 모듈의 인터페이스와 사용 방법은 별도의 개발자 문서나 예제 코드를 통해 공유하는 것이 좋다.%'
);

/* ========================================================
 * 2.3.3 서버 프로그램 구현  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23103, 'WRITTEN', 'OX', 'EASY',
       '서버 프로그램 구현 시에는 동시 접속을 고려해 세션 관리와 스레드 안전성을 검토해야 한다.',
       'O',
       '멀티스레드 환경에서는 공유 자원 접근, 세션 관리, 상태 관리 방식 등을 신중히 설계해야 합니다.',
       'seed:2.3.3:concurrency:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '서버 프로그램 구현 시에는 동시 접속을 고려해 세션 관리와 스레드 안전성을 검토해야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23103, 'WRITTEN', 'OX', 'NORMAL',
       '서버 프로그램은 예외가 발생하면 즉시 프로세스를 종료해 문제를 빠르게 인지하는 것이 가장 좋다.',
       'X',
       '일반적으로 예외는 로깅 후 복구 가능한 경우에는 복구하거나, 요청 단위에서만 실패하도록 처리해 서버 전체가 중단되지 않도록 해야 합니다.',
       'seed:2.3.3:exception:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '서버 프로그램은 예외가 발생하면 즉시 프로세스를 종료해 문제를 빠르게 인지하는 것이 가장 좋다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23103, 'WRITTEN', 'OX', 'EASY',
       '서버 로그에는 최소한 요청 식별자, 시간, 주요 파라미터, 처리 결과, 오류 메시지 등을 남기는 것이 좋다.',
       'O',
       '적절한 로그는 장애 분석과 모니터링에 필수이며, 과도한 개인정보 노출만 주의하면 됩니다.',
       'seed:2.3.3:logging:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '서버 로그에는 최소한 요청 식별자, 시간, 주요 파라미터, 처리 결과, 오류 메시지 등을 남기는 것이 좋다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23103, 'WRITTEN', 'OX', 'NORMAL',
       '서버 구현에서 입력 값 검증은 프론트엔드에서 이미 수행하므로 백엔드에서는 생략해도 된다.',
       'X',
       '프론트 검증은 보조 수단일 뿐이며, 보안을 위해 서버 측에서도 필수 입력 여부, 범위, 길이 등을 반드시 검증해야 합니다.',
       'seed:2.3.3:validation:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '서버 구현에서 입력 값 검증은 프론트엔드에서 이미 수행하므로 백엔드에서는 생략해도 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23103, 'WRITTEN', 'OX', 'NORMAL',
       '커넥션 풀을 사용하면 데이터베이스 접속을 매번 생성·종료해야 하므로 성능이 오히려 저하된다.',
       'X',
       '커넥션 풀은 미리 생성된 연결을 재사용해 매번 접속을 생성·해제하는 오버헤드를 줄이는 성능 최적화 기법입니다.',
       'seed:2.3.3:connection-pool:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '커넥션 풀을 사용하면 데이터베이스 접속을 매번 생성·종료해야 하므로 성능이 오히려 저하된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_23103, 'WRITTEN', 'OX', 'EASY',
       '서버 프로그램의 설정 값(포트, 외부 서비스 URL 등)은 코드에 하드코딩하기보다 설정 파일이나 환경 변수로 분리하는 것이 좋다.',
       'O',
       '설정과 코드를 분리하면 환경별 설정 변경이 쉬워지고, 재배포 없이도 일부 설정을 조정할 수 있습니다.',
       'seed:2.3.3:config:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_23103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '서버 프로그램의 설정 값(포트, 외부 서비스 URL 등)은 코드에 하드코딩하기보다 설정 파일이나 환경 변수로 분리하는 것이 좋다.%'
);

/* ========================================================
 * 2.4.1 배포 환경 구성  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24101, 'WRITTEN', 'OX', 'EASY',
       '배포 환경 구성 단계에서는 운영 서버의 OS, 미들웨어, DBMS 버전 등을 표준화하는 것이 중요하다.',
       'O',
       '환경 표준화를 통해 호환성 문제를 줄이고 장애 분석과 운영을 단순화할 수 있습니다.',
       'seed:2.4.1:env-standard:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '배포 환경 구성 단계에서는 운영 서버의 OS, 미들웨어, DBMS 버전 등을 표준화하는 것이 중요하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24101, 'WRITTEN', 'OX', 'NORMAL',
       '운영 환경 구성 시에는 장애 복구를 위해 이중화나 백업 전략을 고려할 필요가 없다.',
       'X',
       '고가용성과 장애 복구를 위해 이중화(Active-Standby, Active-Active) 및 백업 정책 수립은 필수적으로 검토해야 합니다.',
       'seed:2.4.1:ha:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '운영 환경 구성 시에는 장애 복구를 위해 이중화나 백업 전략을 고려할 필요가 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24101, 'WRITTEN', 'OX', 'EASY',
       '배포 환경 구성에서는 포트, 방화벽, 로드밸런서 설정 등 네트워크 구성이 애플리케이션 접근성에 직접적인 영향을 준다.',
       'O',
       '네트워크 경로, 포트 개방, 로드밸런싱 정책은 서비스 접근성과 성능을 좌우하는 핵심 요소입니다.',
       'seed:2.4.1:network:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '배포 환경 구성에서는 포트, 방화벽, 로드밸런서 설정 등 네트워크 구성이 애플리케이션 접근성에 직접적인 영향을 준다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24101, 'WRITTEN', 'OX', 'NORMAL',
       '배포 환경 구성 단계에서 모니터링 도구나 로깅 경로는 이후 운영 중에 필요해지면 추가해도 된다.',
       'X',
       '장애 분석과 성능 관리를 위해 배포 이전에 모니터링/로깅 수집 경로와 대시보드를 함께 설계하는 것이 바람직합니다.',
       'seed:2.4.1:monitoring:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '배포 환경 구성 단계에서 모니터링 도구나 로깅 경로는 이후 운영 중에 필요해지면 추가해도 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24101, 'WRITTEN', 'OX', 'NORMAL',
       '테스트 환경과 운영 환경의 구성 차이는 클수록 실제 운영 이슈를 사전에 발견하는 데 도움이 된다.',
       'X',
       '테스트·스테이징 환경은 운영 환경과 최대한 유사하게 구성해야 운영 시 발생할 수 있는 이슈를 미리 발견할 수 있습니다.',
       'seed:2.4.1:staging:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '테스트 환경과 운영 환경의 구성 차이는 클수록 실제 운영 이슈를 사전에 발견하는 데 도움이 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24101, 'WRITTEN', 'OX', 'EASY',
       '배포 환경 구성 문서는 서버 인프라 담당자뿐 아니라 개발자와 운영 인력 모두가 참고할 수 있도록 공유하는 것이 좋다.',
       'O',
       '환경 구성 문서를 공유하면 문제 발생 시 역할별 협업과 원인 분석이 훨씬 수월해집니다.',
       'seed:2.4.1:doc:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '배포 환경 구성 문서는 서버 인프라 담당자뿐 아니라 개발자와 운영 인력 모두가 참고할 수 있도록 공유하는 것이 좋다.%'
);

/* ========================================================
 * 2.4.2 설치 패키지 구성  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24102, 'WRITTEN', 'OX', 'EASY',
       '애플리케이션 패키징은 사용자가 설치·실행할 수 있도록 실행 파일과 설정, 매뉴얼 등을 묶어 제공하는 활동이다.',
       'O',
       '패키징은 배포 단위를 정의하고, 설치/제거 절차와 함께 사용자에게 전달 가능한 형태로 만드는 과정입니다.',
       'seed:2.4.2:packaging-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '애플리케이션 패키징은 사용자가 설치·실행할 수 있도록 실행 파일과 설정, 매뉴얼 등을 묶어 제공하는 활동이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24102, 'WRITTEN', 'OX', 'NORMAL',
       '설치 패키지에는 반드시 모든 개발용 도구와 디버깅 심벌을 포함해야 운영 환경 디버깅이 쉬워진다.',
       'X',
       '운영 패키지에는 불필요한 개발 도구나 디버깅 심벌을 포함하지 않는 것이 보안과 성능 측면에서 더 안전합니다.',
       'seed:2.4.2:dev-tools:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '설치 패키지에는 반드시 모든 개발용 도구와 디버깅 심벌을 포함해야 운영 환경 디버깅이 쉬워진다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24102, 'WRITTEN', 'OX', 'NORMAL',
       '설치 프로그램은 롤백 기능 없이 설치만 성공하면 되므로, 실패 시 이전 상태로 복구할 필요는 없다.',
       'X',
       '부분 설치 상태는 시스템을 불안정하게 만들 수 있으므로, 설치 실패 시 이전 상태로 복구하는 롤백 기능을 고려해야 합니다.',
       'seed:2.4.2:rollback:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '설치 프로그램은 롤백 기능 없이 설치만 성공하면 되므로, 실패 시 이전 상태로 복구할 필요는 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24102, 'WRITTEN', 'OX', 'EASY',
       '패키징 시 라이선스 정책과 오픈소스 사용 조건을 준수하는 것은 매우 중요하다.',
       'O',
       '라이선스 위반은 법적 문제로 이어질 수 있으므로, 패키징 단계에서 사용하는 라이브러리의 라이선스 조건을 반드시 확인해야 합니다.',
       'seed:2.4.2:license:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '패키징 시 라이선스 정책과 오픈소스 사용 조건을 준수하는 것은 매우 중요하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24102, 'WRITTEN', 'OX', 'NORMAL',
       '애플리케이션 설치 매뉴얼에는 설치 절차만 포함하면 되고, 장애 발생 시 복구 절차는 별도로 문서화하지 않아도 된다.',
       'X',
       '설치·구성, 장애 발생 시 복구 절차, 로그 위치 등 운영에 필요한 정보까지 함께 문서화하는 것이 좋습니다.',
       'seed:2.4.2:manual:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '애플리케이션 설치 매뉴얼에는 설치 절차만 포함하면 되고, 장애 발생 시 복구 절차는 별도로 문서화하지 않아도 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24102, 'WRITTEN', 'OX', 'EASY',
       '패키징 결과물은 해시 값이나 서명을 통해 배포 과정에서 위·변조 여부를 검증할 수 있도록 제공하는 것이 바람직하다.',
       'O',
       '해시나 코드 서명을 제공하면 전송 중 파일 손상이나 위·변조 여부를 검증할 수 있습니다.',
       'seed:2.4.2:hash-sign:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '패키징 결과물은 해시 값이나 서명을 통해 배포 과정에서 위·변조 여부를 검증할 수 있도록 제공하는 것이 바람직하다.%'
);

/* ========================================================
 * 2.4.3 릴리즈 노트 작성  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24103, 'WRITTEN', 'OX', 'EASY',
       '릴리즈 노트에는 버전, 배포 일자, 주요 변경 사항 등이 포함되어야 한다.',
       'O',
       '릴리즈 노트는 어떤 버전이 언제, 무엇이 바뀌었는지를 사용자와 운영자에게 전달하는 문서입니다.',
       'seed:2.4.3:basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '릴리즈 노트에는 버전, 배포 일자, 주요 변경 사항 등이 포함되어야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24103, 'WRITTEN', 'OX', 'NORMAL',
       '릴리즈 노트에는 수정된 버그나 신규 기능만 기록하고, 알려진 제한 사항이나 주의 사항은 포함하지 않는 것이 일반적이다.',
       'X',
       '알려진 제한 사항, 호환성 이슈, 주의 사항 등을 함께 기록해야 사용자와 운영자가 적절히 대응할 수 있습니다.',
       'seed:2.4.3:known-issues:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '릴리즈 노트에는 수정된 버그나 신규 기능만 기록하고, 알려진 제한 사항이나 주의 사항은 포함하지 않는 것이 일반적이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24103, 'WRITTEN', 'OX', 'EASY',
       '릴리즈 노트의 변경 이력에는 버전 간 차이를 쉽게 파악할 수 있도록 항목별로 정리하는 것이 좋다.',
       'O',
       '기능 추가, 개선, 버그 수정 등으로 구분해 기록하면 이해하기 쉬운 변경 이력이 됩니다.',
       'seed:2.4.3:changelog:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '릴리즈 노트의 변경 이력에는 버전 간 차이를 쉽게 파악할 수 있도록 항목별로 정리하는 것이 좋다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24103, 'WRITTEN', 'OX', 'NORMAL',
       '릴리즈 노트는 개발자만 참고하는 내부 문서이므로, 최종 사용자나 운영 인력에게는 제공하지 않는다.',
       'X',
       '릴리즈 노트는 개발자뿐 아니라 운영, 헬프데스크, 최종 사용자에게도 중요한 정보이므로 공유하는 것이 일반적입니다.',
       'seed:2.4.3:audience:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '릴리즈 노트는 개발자만 참고하는 내부 문서이므로, 최종 사용자나 운영 인력에게는 제공하지 않는다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24103, 'WRITTEN', 'OX', 'NORMAL',
       '릴리즈 노트에는 배포 전·후에 필요한 사전/사후 작업이 있다면 함께 안내하는 것이 바람직하다.',
       'O',
       'DB 마이그레이션, 캐시 초기화, 구성 변경 등 사전/사후 작업이 있다면 릴리즈 노트에 명시해야 합니다.',
       'seed:2.4.3:pre-post:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '릴리즈 노트에는 배포 전·후에 필요한 사전/사후 작업이 있다면 함께 안내하는 것이 바람직하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_24103, 'WRITTEN', 'OX', 'EASY',
       '릴리즈 노트는 버전 관리 시스템이나 포털에 누락 없이 축적해 두어야 과거 변경 이력을 추적할 수 있다.',
       'O',
       '릴리즈 노트를 지속적으로 축적하면 변경 추적, 회귀 분석, 문제 재현 등에 큰 도움이 됩니다.',
       'seed:2.4.3:archive:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_24103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '릴리즈 노트는 버전 관리 시스템이나 포털에 누락 없이 축적해 두어야 과거 변경 이력을 추적할 수 있다.%'
);

/* ========================================================
 * 2.5.1 인터페이스 설계 확인  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_25101, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 설계서에는 송수신 시스템, 인터페이스 ID, 데이터 항목, 형식, 길이 등이 포함되어야 한다.',
       'O',
       '인터페이스 설계서는 두 시스템 간 연계 방식을 명확히 하기 위해 ID, 시스템 정보, 데이터 정의 등을 포함합니다.',
       'seed:2.5.1:spec:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_25101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 설계서에는 송수신 시스템, 인터페이스 ID, 데이터 항목, 형식, 길이 등이 포함되어야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_25101, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 설계 확인 단계에서는 장애 처리나 타임아웃과 같은 예외 시나리오는 고려할 필요가 없다.',
       'X',
       '장애 시 재시도, 대체 경로, 오류 코드 정의 등 예외 시나리오도 인터페이스 설계 단계에서 함께 검토해야 합니다.',
       'seed:2.5.1:exception:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_25101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 설계 확인 단계에서는 장애 처리나 타임아웃과 같은 예외 시나리오는 고려할 필요가 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_25101, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 설계 검토 시에는 호출 빈도와 최대 처리량 등 성능 요구사항도 함께 확인하는 것이 바람직하다.',
       'O',
       '피크 트래픽, 허용 지연 시간 등을 설계 단계에서 합의해야 적절한 연계 방식과 용량을 산정할 수 있습니다.',
       'seed:2.5.1:performance:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_25101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 설계 검토 시에는 호출 빈도와 최대 처리량 등 성능 요구사항도 함께 확인하는 것이 바람직하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_25101, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 보안 요구사항에는 인증, 권한, 암호화, 로그 마스킹 등의 요소가 포함될 수 있다.',
       'O',
       '민감 데이터 보호와 접근 통제를 위해 인증/인가, 암호화, 로깅 정책 등을 함께 정의해야 합니다.',
       'seed:2.5.1:security:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_25101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 보안 요구사항에는 인증, 권한, 암호화, 로그 마스킹 등의 요소가 포함될 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_25101, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 설계 확인은 개발자만 참여하며, 업무 담당자나 운영 인력은 참석하지 않는 것이 일반적이다.',
       'X',
       '업무 요구와 운영 관점까지 반영하기 위해 분석가, 개발자, 운영 담당자 등이 함께 설계를 검토하는 것이 좋습니다.',
       'seed:2.5.1:stakeholder:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_25101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 설계 확인은 개발자만 참여하며, 업무 담당자나 운영 인력은 참석하지 않는 것이 일반적이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_25101, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 설계서는 추후 변경 관리와 영향도 분석을 위해 형상 관리 대상에 포함하는 것이 바람직하다.',
       'O',
       '설계 변경 이력을 남기기 위해 인터페이스 설계서도 형상 관리 도구에서 버전 관리하는 것이 일반적입니다.',
       'seed:2.5.1:scm:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_25101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 설계서는 추후 변경 관리와 영향도 분석을 위해 형상 관리 대상에 포함하는 것이 바람직하다.%'
);

/* ========================================================
 * 2.5.2 인터페이스 기능 구현  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_25102, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 기능 구현 시에는 설계서에 정의된 데이터 항목과 검증 규칙대로 송수신 로직을 구현해야 한다.',
       'O',
       '데이터 형식, 길이, 필수 여부, 코드 값 등 설계서 요구를 구현에 충실히 반영해야 합니다.',
       'seed:2.5.2:impl-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_25102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 기능 구현 시에는 설계서에 정의된 데이터 항목과 검증 규칙대로 송수신 로직을 구현해야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_25102, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 구현에서는 오류 코드와 예외 메시지를 통일할 필요가 없으며, 각 개발자가 자유롭게 정의해도 된다.',
       'X',
       '오류 코드와 메시지를 표준화해야 문제 원인 파악과 모니터링, 운영 대응이 쉬워집니다.',
       'seed:2.5.2:error-code:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_25102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 구현에서는 오류 코드와 예외 메시지를 통일할 필요가 없으며, 각 개발자가 자유롭게 정의해도 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_25102, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 구현 시 로깅은 성능에 영향을 주므로 오류가 발생했을 때만 기록하는 것이 바람직하다.',
       'X',
       '중요 인터페이스는 정상 호출도 최소한의 요약 로그를 남겨야 트랜잭션 추적과 장애 분석이 가능합니다.',
       'seed:2.5.2:logging:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_25102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 구현 시 로깅은 성능에 영향을 주므로 오류가 발생했을 때만 기록하는 것이 바람직하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_25102, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 구현 후에는 모의(stub) 시스템이나 테스트 더블을 이용해 단위·통합 테스트를 수행하는 것이 좋다.',
       'O',
       '실제 연계 시스템이 준비되지 않았더라도 스텁, 목, 가짜 서버 등을 활용해 인터페이스 기능을 사전에 검증할 수 있습니다.',
       'seed:2.5.2:stub-mock:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_25102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 구현 후에는 모의(stub) 시스템이나 테스트 더블을 이용해 단위·통합 테스트를 수행하는 것이 좋다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_25102, 'WRITTEN', 'OX', 'NORMAL',
       '인터페이스 구현 시 재시도 로직은 항상 무한 반복으로 구현해 일시적인 오류가 해결될 때까지 계속 시도하도록 한다.',
       'X',
       '재시도 횟수와 간격을 제한하고, 실패 시 보정 프로세스나 운영 알림으로 전환하는 전략이 필요합니다.',
       'seed:2.5.2:retry:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_25102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 구현 시 재시도 로직은 항상 무한 반복으로 구현해 일시적인 오류가 해결될 때까지 계속 시도하도록 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text, source)
SELECT @cert_id, @tp_25102, 'WRITTEN', 'OX', 'EASY',
       '인터페이스 구현 결과는 운영 전 사전 연계 테스트(시나리오 테스트)를 통해 양쪽 시스템이 동일한 해석을 하는지 확인해야 한다.',
       'O',
       '시나리오 기반 연계 테스트를 수행해 데이터 해석, 코드 매핑, 오류 처리 등이 양쪽 시스템에서 일관되게 동작하는지 검증해야 합니다.',
       'seed:2.5.2:scenario-test:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id = @tp_25102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '인터페이스 구현 결과는 운영 전 사전 연계 테스트(시나리오 테스트)를 통해 양쪽 시스템이 동일한 해석을 하는지 확인해야 한다.%'
);
