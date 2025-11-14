SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;
SET @cert_id := 1;

/* =========================================================
 * 기존 토픽 ID 재바인딩
 * ========================================================= */
SET @topic_analysis := (
  SELECT id FROM topic WHERE cert_id=@cert_id AND code='INF-W-ANALYSIS' LIMIT 1
);
SET @topic_oop := (
  SELECT id FROM topic WHERE cert_id=@cert_id AND code='INF-W-OOP' LIMIT 1
);
SET @topic_interface := (
  SELECT id FROM topic WHERE cert_id=@cert_id AND code='INF-W-INTERFACE' LIMIT 1
);
SET @topic_practical := (
  SELECT id FROM topic WHERE cert_id=@cert_id AND code='INF-P-DESIGN' LIMIT 1
);

/* =========================================================
 * [추가] 필기 OX 문제 (현행 분석 / OOP / 인터페이스)
 * ========================================================= */

/* 현행 시스템 분석 - 비기능 요구 수집 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_analysis, 'WRITTEN', 'OX', 'EASY',
       '현행 시스템 분석에서는 기능 요구사항만 수집하고, 보안·성능·가용성과 같은 비기능 요구는 이후 설계 단계에서만 고려하면 된다. (O/X)',
       'X',
       '비기능 요구는 아키텍처 제약과 용량 산정에 직접 영향을 주기 때문에, 현행 분석 단계에서부터 함께 수집해야 합니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석에서는 기능 요구사항만 수집하고%'
);
SET @q_ox_analysis_nfr := (
  SELECT id FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석에서는 기능 요구사항만 수집하고%' LIMIT 1
);
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_analysis_nfr, '비기능요구'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_analysis_nfr AND tag='비기능요구');

/* 현행 시스템 분석 - 인터페이스 목록 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_analysis, 'WRITTEN', 'OX', 'NORMAL',
       '현행 시스템 분석 시에는 외부 시스템과의 인터페이스 목록, 프로토콜, 전송 주기 등을 함께 정리하는 것이 좋다. (O/X)',
       'O',
       '후속 인터페이스 설계에서 재사용되므로, 현행 분석 단계에서 인터페이스 목록/프로토콜/전송 주기를 정리해 두는 것이 좋습니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석 시에는 외부 시스템과의 인터페이스 목록%'
);
SET @q_ox_analysis_if := (
  SELECT id FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석 시에는 외부 시스템과의 인터페이스 목록%' LIMIT 1
);
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_analysis_if, '인터페이스현황'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_analysis_if AND tag='인터페이스현황');

/* OOP - 단일 책임 원칙 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_oop, 'WRITTEN', 'OX', 'EASY',
       '단일 책임 원칙(SRP)은 한 클래스가 오직 한 종류의 변경 이유만을 가져야 한다는 원칙이다. (O/X)',
       'O',
       'SRP는 클래스가 하나의 책임에만 집중하도록 하여 변경 영향을 최소화하고 응집도를 높이는 원칙입니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_oop AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '단일 책임 원칙(SRP)은 한 클래스가 오직 한 종류의 변경 이유만을%'
);
SET @q_ox_srp := (
  SELECT id FROM question
   WHERE topic_id=@topic_oop AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '단일 책임 원칙(SRP)은 한 클래스가 오직 한 종류의 변경 이유만을%' LIMIT 1
);
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_srp, 'SRP'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_srp AND tag='SRP');

/* 인터페이스 설계 - 동기/비동기 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_interface, 'WRITTEN', 'OX', 'NORMAL',
       '요청 후 결과를 곧바로 받아야 하는 업무는 비동기 인터페이스로만 설계하는 것이 바람직하다. (O/X)',
       'X',
       '결과를 즉시 확인해야 하는 업무는 보통 동기 인터페이스가 적합하며, 비동기는 결과 지연을 허용할 수 있는 업무에 적합합니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_interface AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '요청 후 결과를 곧바로 받아야 하는 업무는 비동기 인터페이스로만%'
);
SET @q_ox_sync := (
  SELECT id FROM question
   WHERE topic_id=@topic_interface AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '요청 후 결과를 곧바로 받아야 하는 업무는 비동기 인터페이스로만%' LIMIT 1
);
INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_sync, '동기비동기'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_sync AND tag='동기비동기');

/* =========================================================
 * [추가] 필기 MCQ 문제 - OOP 심화
 * ========================================================= */

/* DIP */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_oop, 'WRITTEN', 'MCQ', 'NORMAL',
       '의존성 역전 원칙(DIP)에 대한 설명으로 가장 적절한 것은?',
       'C',
       'DIP는 고수준 모듈과 저수준 모듈이 모두 추상화에 의존하도록 하여, 구체 구현 변경에 덜 민감한 구조를 만드는 원칙입니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_oop AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '의존성 역전 원칙(DIP)에 대한 설명으로 가장 적절한 것은?%'
);
SET @q_mcq_dip := (
  SELECT id FROM question
   WHERE topic_id=@topic_oop AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '의존성 역전 원칙(DIP)에 대한 설명으로 가장 적절한 것은?%' LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_dip,'A','상속 계층을 가능한 얕게 유지하는 원칙이다.',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_dip AND label='A');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_dip,'B','모든 모듈이 구체 클래스를 직접 참조하도록 하는 원칙이다.',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_dip AND label='B');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_dip,'C','고수준·저수준 모듈 모두 추상화에 의존하도록 설계하는 원칙이다.',1
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_dip AND label='C');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_dip,'D','데이터베이스 스키마를 캡슐화하는 원칙이다.',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_dip AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_dip, 'DIP'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_dip AND tag='DIP');

/* LSP */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_oop, 'WRITTEN', 'MCQ', 'HARD',
       '리스코프 치환 원칙(LSP)을 가장 잘 위배한 사례는 어느 것인가?',
       'B',
       'LSP는 자식 타입이 부모 타입을 완전히 대체할 수 있어야 한다는 원칙으로, 부모가 보장하던 행위를 자식이 깨뜨리면 위배됩니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_oop AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '리스코프 치환 원칙(LSP)을 가장 잘 위배한 사례는 어느 것인가?%'
);
SET @q_mcq_lsp := (
  SELECT id FROM question
   WHERE topic_id=@topic_oop AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '리스코프 치환 원칙(LSP)을 가장 잘 위배한 사례는 어느 것인가?%' LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_lsp,'A','부모 클래스의 메서드를 자식에서 오버라이드하여 성능을 개선하였다.',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_lsp AND label='A');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_lsp,'B','부모가 항상 양수를 반환하던 메서드를, 자식이 경우에 따라 음수를 반환하도록 변경하였다.',1
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_lsp AND label='B');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_lsp,'C','부모 클래스에 없던 편의 메서드를 자식 클래스에 추가하였다.',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_lsp AND label='C');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_lsp,'D','자식 생성자에서 부모 생성자를 호출하였다.',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_lsp AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_lsp, 'LSP'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_lsp AND tag='LSP');

/* =========================================================
 * [추가] 필기 MCQ 문제 - 인터페이스 설계 / REST
 * ========================================================= */

/* REST 리소스 설계 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_interface, 'WRITTEN', 'MCQ', 'NORMAL',
       'RESTful 인터페이스 설계에 대한 설명으로 가장 적절한 것은?',
       'A',
       'REST에서는 리소스를 URI로 표현하고, 행위는 HTTP 메서드(GET/POST/PUT/DELETE 등)로 구분하며, 무상태성(stateless)을 유지하는 것이 중요합니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_interface AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE 'RESTful 인터페이스 설계에 대한 설명으로 가장 적절한 것은?%'
);
SET @q_mcq_rest := (
  SELECT id FROM question
   WHERE topic_id=@topic_interface AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE 'RESTful 인터페이스 설계에 대한 설명으로 가장 적절한 것은?%' LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_rest,'A','리소스를 URI로 표현하고 HTTP 메서드로 행위를 구분한다.',1
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_rest AND label='A');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_rest,'B','모든 기능을 단일 POST 엔드포인트로 제공한다.',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_rest AND label='B');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_rest,'C','클라이언트 상태를 서버 세션에 항상 저장한다.',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_rest AND label='C');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_rest,'D','HTTP 상태 코드는 항상 200만 사용한다.',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_rest AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_rest, 'REST'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_rest AND tag='REST');

/* 타임아웃/재시도 정책 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_interface, 'WRITTEN', 'MCQ', 'HARD',
       '고부하 환경에서 외부 API 인터페이스를 설계할 때, 타임아웃과 재시도 정책을 함께 고려하는 주된 이유는?',
       'C',
       '너무 긴 타임아웃이나 무제한 재시도는 실패를 확산시켜 장애를 키우므로, 적절한 타임아웃과 재시도(백오프)를 함께 설계해야 자원 고갈을 막을 수 있습니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_interface AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '고부하 환경에서 외부 API 인터페이스를 설계할 때, 타임아웃과 재시도 정책을%'
);
SET @q_mcq_timeout := (
  SELECT id FROM question
   WHERE topic_id=@topic_interface AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '고부하 환경에서 외부 API 인터페이스를 설계할 때, 타임아웃과 재시도 정책을%' LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_timeout,'A','네트워크 로그를 보기 좋게 만들기 위해서이다.',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_timeout AND label='A');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_timeout,'B','응답 속도를 느리게 만들어 부하를 분산하기 위해서이다.',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_timeout AND label='B');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_timeout,'C','실패 전파와 자원 고갈을 막기 위해 적절한 차단과 백오프를 적용하기 위해서이다.',1
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_timeout AND label='C');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_timeout,'D','API 버전 관리를 단순화하기 위해서이다.',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_timeout AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_timeout, '타임아웃'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_timeout AND tag='타임아웃');

/* =========================================================
 * [추가] 실기 SHORT/LONG 문제 - DB/트랜잭션/동시성
 * ========================================================= */

/* SHORT - 인덱스 설계 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_practical, 'PRACTICAL', 'SHORT', 'NORMAL',
       '온라인 조회 트랜잭션에서 지나치게 많은 풀스캔이 발생할 때, 인덱스 설계 관점에서 적용할 수 있는 개선 방안을 한 가지 쓰세요.',
       '조회 조건/정렬 기준에 맞는 복합 인덱스 설계',
       'WHERE 절과 ORDER BY 절에 자주 사용되는 컬럼을 기준으로 복합 인덱스를 설계해 풀스캔을 줄이고 랜덤 I/O를 최소화하도록 조정합니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='SHORT'
     AND stem LIKE '온라인 조회 트랜잭션에서 지나치게 많은 풀스캔이 발생할 때%'
);
SET @q_short_index := (
  SELECT id FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='SHORT'
     AND stem LIKE '온라인 조회 트랜잭션에서 지나치게 많은 풀스캔이 발생할 때%' LIMIT 1
);
INSERT INTO question_tag (question_id, tag)
SELECT @q_short_index, '인덱스설계'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_short_index AND tag='인덱스설계');

/* SHORT - 트랜잭션 격리수준 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_practical, 'PRACTICAL', 'SHORT', 'HARD',
       '동일 레코드에 대한 동시 갱신 충돌을 줄이기 위해, 트랜잭션 격리 수준 또는 잠금 전략 측면에서 적용할 수 있는 방법 한 가지를 쓰세요.',
       '비관적 잠금(SELECT ... FOR UPDATE) 또는 격리 수준 상향',
       '충돌 가능성이 높은 레코드는 비관적 잠금(SELECT ... FOR UPDATE)을 사용하거나, REPEATABLE READ 이상으로 격리 수준을 높여 동시 갱신 충돌과 Lost Update를 줄입니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='SHORT'
     AND stem LIKE '동일 레코드에 대한 동시 갱신 충돌을 줄이기 위해%'
);
SET @q_short_tx := (
  SELECT id FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='SHORT'
     AND stem LIKE '동일 레코드에 대한 동시 갱신 충돌을 줄이기 위해%' LIMIT 1
);
INSERT INTO question_tag (question_id, tag)
SELECT @q_short_tx, '트랜잭션'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_short_tx AND tag='트랜잭션');

/* LONG - 정규화/성능 균형 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_practical, 'PRACTICAL', 'LONG', 'NORMAL',
       '정규화가 잘 되어 있지만 조인이 너무 많이 발생해 응답 지연이 커진 테이블 구조가 있다. 정규화 이점을 해치지 않으면서 조회 성능을 개선할 수 있는 설계 방안을 서술하세요.',
       '부분 비정규화, 조회 전용 집계 테이블, 인덱스 튜닝',
       '쓰기 경로는 정규형을 유지하되, 조회 패턴에 맞춘 부분 비정규화 테이블이나 Materialized View/집계 테이블을 별도로 두고, 적절한 인덱스 튜닝으로 조인 수와 I/O를 줄이는 방식으로 설계합니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='LONG'
     AND stem LIKE '정규화가 잘 되어 있지만 조인이 너무 많이 발생해 응답 지연이 커진 테이블 구조가 있다%'
);
SET @q_long_norm := (
  SELECT id FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='LONG'
     AND stem LIKE '정규화가 잘 되어 있지만 조인이 너무 많이 발생해 응답 지연이 커진 테이블 구조가 있다%' LIMIT 1
);
INSERT INTO question_tag (question_id, tag)
SELECT @q_long_norm, '정규화'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_long_norm AND tag='정규화');

/* LONG - 트랜잭션/동시성/락 경합 */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_practical, 'PRACTICAL', 'LONG', 'HARD',
       '대규모 동시 접속 환경에서 주문/결제 테이블에 대한 락 경합으로 응답 지연이 심각하다. 트랜잭션 경계, 잠금 범위, 큐 기반 비동기 처리 등을 고려하여 개선 방안을 설계하세요.',
       '트랜잭션 경계 최소화, 행 단위 잠금, 비동기 큐 분리',
       '결제 승인 등 외부 연동은 별도 큐/비동기 프로세스로 분리하고, 트랜잭션 범위를 최소화하며, 테이블 잠금이 아닌 행 단위 잠금과 인덱스 설계를 통해 락 경합을 줄이는 방향으로 설계합니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='LONG'
     AND stem LIKE '대규모 동시 접속 환경에서 주문/결제 테이블에 대한 락 경합으로 응답 지연이 심각하다%'
);
SET @q_long_lock := (
  SELECT id FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='LONG'
     AND stem LIKE '대규모 동시 접속 환경에서 주문/결제 테이블에 대한 락 경합으로 응답 지연이 심각하다%' LIMIT 1
);
INSERT INTO question_tag (question_id, tag)
SELECT @q_long_lock, '동시성제어'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_long_lock AND tag='동시성제어');

SET FOREIGN_KEY_CHECKS = 1;
