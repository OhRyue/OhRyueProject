SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id   := 1;
SET @tp_31401  := 31401; -- 트랜잭션/격리수준
SET @tp_31402  := 31402; -- 동시성/락
SET @tp_31501  := 31501; -- 백업/복구/RPO/RTO
SET @tp_31502  := 31502; -- 장애분석/포스트모템


/* =======================================================
 * 31401 – 트랜잭션 / 격리수준
 * ======================================================= */

-- [31401] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'OX', 'NORMAL',
       '트랜잭션의 “원자성(Atomicity)”은 트랜잭션 내부의 작업이 모두 성공하거나 모두 실패해야 함을 의미한다. (O/X)',
       NULL,
       'O',
       '은행 이체 예시처럼 일부만 성공하면 안 되는 작업을 하나의 단위로 묶는 것이 원자성입니다.',
       'seed:prac:tx_isolation:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'OX', 'NORMAL',
       'READ COMMITTED 격리수준에서는 Dirty Read와 Non-Repeatable Read가 모두 허용된다. (O/X)',
       NULL,
       'X',
       'READ COMMITTED는 Dirty Read는 막지만, Non-Repeatable Read는 여전히 발생할 수 있습니다.',
       'seed:prac:tx_isolation:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31401, 'PRACTICAL', 'OX', 'NORMAL',
       'REPEATABLE READ 격리수준에서는 동일 트랜잭션 내에서 같은 조건으로 두 번 조회하면 항상 같은 결과를 보장하는 것을 목표로 한다. (O/X)',
       NULL,
       'O',
       'REPEATABLE READ는 Non-Repeatable Read를 방지하는 것이 주요 목표입니다. (팬텀 리드는 DB 구현에 따라 다름)',
       'seed:prac:tx_isolation:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:tx_isolation:ox6');

/* =======================================================
 * 31402 – 동시성 제어 / 락
 *  - OX 3개 추가 + MCQ 10개
 * ======================================================= */

-- [31402] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'OX', 'NORMAL',
       'Lost Update를 줄이기 위한 방법 중 하나로, 행 수준 락(SELECT ... FOR UPDATE)을 사용하는 것이 있다. (O/X)',
       NULL,
       'O',
       '비관적 락을 사용해 동시에 같은 행을 수정하지 못하게 막는 전형적인 방법입니다.',
       'seed:prac:concurrency:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'OX', 'NORMAL',
       '낙관적 락(Optimistic Lock)은 항상 DB 수준의 락을 먼저 잡고 시작하는 방식이다. (O/X)',
       NULL,
       'X',
       '낙관적 락은 버전 필드 등을 이용해 “커밋 시점에만” 충돌을 감지하는 방식으로, DB 락을 미리 잡지 않습니다.',
       'seed:prac:concurrency:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31402, 'PRACTICAL', 'OX', 'NORMAL',
       '데드락을 예방하기 위한 일반적인 방법 중 하나는 여러 자원을 잠글 때 항상 같은 순서로 락을 획득하도록 규칙을 두는 것이다. (O/X)',
       NULL,
       'O',
       '락 획득 순서를 통일하면 순환 대기가 줄어들어 데드락 가능성이 감소합니다.',
       'seed:prac:concurrency:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:concurrency:ox6');


/* =======================================================
 * 31501 – 백업 / 복구 / RPO / RTO
 * ======================================================= */

-- [31501] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31501, 'PRACTICAL', 'OX', 'NORMAL',
       '증분 백업은 전체 백업 이후 변경된 데이터만 저장하므로, 일반적으로 전체 백업보다 저장 공간을 적게 사용한다. (O/X)',
       NULL,
       'O',
       '증분/로그 백업은 변경분만 저장하기 때문에 저장 공간을 줄이는 데 유리합니다.',
       'seed:prac:backup:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31501, 'PRACTICAL', 'OX', 'NORMAL',
       'RPO를 줄이기 위해서는 전체 백업 주기를 늘리고, 로그/증분 백업 주기를 촘촘히 가져가는 전략이 도움이 될 수 있다. (O/X)',
       NULL,
       'O',
       '로그/증분 백업 주기를 짧게 가져가면 장애 시점과 마지막 백업 시점 간 간격(RPO)을 줄일 수 있습니다.',
       'seed:prac:backup:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:ox5');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT @cert_id, @tp_31501, 'PRACTICAL', 'OX', 'NORMAL',
       'RTO는 “얼마 전 시점까지의 데이터만 복구되어도 되는가”를 의미하는 지표이다. (O/X)',
       NULL,
       'X',
       'RPO가 “데이터 손실 허용 시점”, RTO는 “얼마 안에 서비스를 복구해야 하는가”를 의미합니다.',
       'seed:prac:backup:ox6'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:backup:ox6');


/* =======================================================
 * 31502 – 장애 분석 / 포스트모템
 * ======================================================= */

-- [31502] OX 추가 3개

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31502, 'PRACTICAL', 'OX', 'NORMAL',
  '장애 분석 보고서(Postmortem)에는 장애 원인뿐 아니라 영향 범위와 재발 방지 대책도 함께 포함하는 것이 일반적이다. (O/X)',
  NULL,
  'O',
  '원인·영향·재발 방지 대책은 거의 모든 포스트모템 템플릿의 공통 요소입니다.',
  'seed:prac:incident:ox3'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:incident:ox3');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31502, 'PRACTICAL', 'OX', 'NORMAL',
  '장애가 재발하지 않았다면, 유사 장애를 예방하기 위한 개선 과제를 정의하는 것은 의미가 없다. (O/X)',
  NULL,
  'X',
  '재발 여부와 무관하게, 유사 상황에서 더 빨리 감지/대응하기 위한 대책을 고민하는 것이 포스트모템의 핵심입니다.',
  'seed:prac:incident:ox4'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:incident:ox4');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, payload_json, answer_key, solution_text, source)
SELECT
  @cert_id, @tp_31502, 'PRACTICAL', 'OX', 'NORMAL',
  '포스트모템 문화에서는 “개인을 비난하기보다는 시스템/프로세스 개선점”에 초점을 맞추는 것이 바람직하다. (O/X)',
  NULL,
  'O',
  'Blameless Postmortem은 개인 탓이 아닌 시스템/프로세스 개선을 지향합니다.',
  'seed:prac:incident:ox5'
WHERE NOT EXISTS (SELECT 1 FROM question WHERE source = 'seed:prac:incident:ox5');


/* =========================================================
 * PRACTICAL SHORT/LONG 추가 시드 (31401 ~ 31502)
 * 각 토픽당 Short 5개 + Long 3개 추가
 * ========================================================= */

INSERT INTO question
  (cert_id, topic_id, mode, type, difficulty, stem, payload_json, answer_key, solution_text, source, image_url)
VALUES
  -- =========================
  -- 31401 트랜잭션/격리수준 (실기)
  -- =========================
  -- SHORT
  (1, 31401, 'PRACTICAL', 'SHORT', 'EASY',
   '트랜잭션의 원자성(Atomicity)을 한 문장으로 설명하세요.',
   NULL,
   '트랜잭션에 포함된 작업들이 모두 성공하거나 모두 실패해야 하는 성질',
   '계좌 이체처럼 일부만 반영되면 안 되는 작업을 하나로 묶어 처리하기 위한 기본 특성입니다.',
   'seed:prac:tx_isolation:short2', NULL),

  (1, 31401, 'PRACTICAL', 'SHORT', 'NORMAL',
   'Repeatable Read 수준에서 방지되는 이상현상을 한 가지 쓰고, 한 문장으로 설명하세요.',
   NULL,
   'Non-Repeatable Read; 같은 조건으로 두 번 조회했을 때 중간에 다른 트랜잭션이 값을 바꿔 결과가 달라지는 현상',
   'Repeatable Read는 같은 조건의 반복 조회에서 행 데이터가 바뀌지 않도록 보장합니다.',
   'seed:prac:tx_isolation:short3', NULL),

  (1, 31401, 'PRACTICAL', 'SHORT', 'NORMAL',
   'Serializable 격리수준의 장단점을 각각 한 문장으로 설명하세요.',
   NULL,
   '장점은 동시성 문제를 대부분 막을 수 있을 정도로 일관성이 강하다는 것, 단점은 동시성이 낮아져 성능 저하가 크다는 것',
   '가장 강한 수준이지만 현실 시스템에서는 부하를 견디기 어려워 제한적으로만 사용됩니다.',
   'seed:prac:tx_isolation:short4', NULL),

  (1, 31401, 'PRACTICAL', 'SHORT', 'HARD',
   '트랜잭션 격리수준을 너무 낮게 설정했을 때와 너무 높게 설정했을 때 각각의 위험을 한 문장씩 설명하세요.',
   NULL,
   '너무 낮으면 Dirty Read 등으로 데이터 일관성 문제가 생기고, 너무 높으면 락 경합으로 성능 문제와 타임아웃이 발생할 수 있다',
   '실기에서는 기능/성능 두 측면을 모두 짚어주는 답이 좋습니다.',
   'seed:prac:tx_isolation:short5', NULL),

  (1, 31401, 'PRACTICAL', 'SHORT', 'HARD',
   '트랜잭션 경계를 애플리케이션 레벨에서 잘못 잡았을 때 나타날 수 있는 문제를 두 가지 쓰세요.',
   NULL,
   '불필요하게 긴 트랜잭션으로 인한 락 홀딩 증가, 필요한 작업이 서로 다른 트랜잭션에 나뉘어 데이터 불일치 발생',
   '서비스/DAO 레벨에서 트랜잭션을 어떻게 묶느냐에 따라 동시성과 무결성에 큰 차이가 납니다.',
   'seed:prac:tx_isolation:short6', NULL),

  -- LONG
  (1, 31401, 'PRACTICAL', 'LONG', 'NORMAL',
   '쇼핑몰에서 “장바구니 담기 → 주문 생성 → 결제 요청”이 하나의 트랜잭션으로 묶여 있을 때와, 단계별로 나뉘어 있을 때 각각 장단점을 설명하고, 적절한 트랜잭션 경계를 어떻게 잡을지 제안하세요.',
   NULL,
   '하나로 묶으면 일관성은 좋지만 트랜잭션이 길어짐, 단계별 분리는 동시성은 좋지만 보상/재시도가 필요',
   '예시: 장바구니 담기와 주문 생성은 비교적 가벼운 작업이지만, 결제는 외부 시스템과 연계되어 지연이 크다. 세 단계를 모두 하나의 트랜잭션으로 묶으면 롤백이 단순하지만, 락이 오래 잡혀 동시성이 떨어진다. 따라서 주문 생성까지를 하나의 트랜잭션으로 처리하고, 결제는 별도 트랜잭션과 보상 로직(주문 취소)을 두는 것이 현실적인 설계다.',
   'seed:prac:tx_isolation:long2', NULL),

  (1, 31401, 'PRACTICAL', 'LONG', 'HARD',
   '은행 코어 시스템에서 계좌 조회/입금/출금 기능에 서로 다른 격리수준을 적용해야 한다고 가정한다. 각 기능에 어떤 격리수준을 적용하는 것이 적절한지, 이유와 함께 서술하세요.',
   NULL,
   '조회는 Read Committed 이상, 입금/출금은 Repeatable Read 또는 그 이상',
   '정답 예시: (1) 단순 잔액 조회는 Read Committed로도 충분하며, Dirty Read만 막으면 된다. (2) 입금/출금과 같이 잔액이 변경되는 트랜잭션은 같은 트랜잭션 내 재조회 결과가 바뀌지 않도록 Repeatable Read 수준이 필요하다. (3) 특정 고위험 처리에는 Serializable을 부분적으로 적용해 동시성보다 일관성을 우선할 수 있다.',
   'seed:prac:tx_isolation:long3', NULL),

  (1, 31401, 'PRACTICAL', 'LONG', 'HARD',
   '분산 환경에서 두 개 이상의 시스템이 참여하는 트랜잭션을 설계할 때, 전통적인 2PC(2-Phase Commit) 대신 “보상 트랜잭션(Saga 패턴)”을 사용하는 이유와 개념을 설명하세요.',
   NULL,
   '2PC는 강한 일관성이지만 복잡하고 장애에 취약, Saga는 로컬 트랜잭션 + 보상 작업으로 최종 일관성 추구',
   '예시: 2PC는 코디네이터 장애 시 교착 상태나 락 장기 보유 문제가 발생하기 쉽다. Saga 패턴에서는 각 서비스가 자신의 로컬 트랜잭션을 커밋하고, 실패 시 보상 트랜잭션으로 이전 상태로 되돌린다. 이렇게 하면 분산 환경에서 가용성을 높이면서도 최종 일관성을 확보할 수 있다.',
   'seed:prac:tx_isolation:long4', NULL),

  -- =========================
  -- 31402 동시성/락 (실기)
  -- =========================
  -- SHORT
  (1, 31402, 'PRACTICAL', 'SHORT', 'EASY',
   '비관적 락(Pessimistic Lock)의 기본 아이디어를 한 문장으로 설명하세요.',
   NULL,
   '충돌이 자주 발생한다고 가정하고, 먼저 락을 걸어 다른 트랜잭션이 동시에 수정하지 못하게 막는 방식',
   'SELECT ... FOR UPDATE 같은 구문이 대표적인 비관적 락 구현입니다.',
   'seed:prac:concurrency:short2', NULL),

  (1, 31402, 'PRACTICAL', 'SHORT', 'NORMAL',
   '낙관적 락(Optimistic Lock)이 유리한 상황의 예를 한 문장으로 설명하세요.',
   NULL,
   '실제 갱신 충돌이 드물고, 잠금을 길게 잡으면 성능에 악영향이 큰 환경에서 유리하다',
   '예: 쇼핑몰 장바구니처럼 같은 데이터를 동시에 수정할 가능성이 낮은 경우입니다.',
   'seed:prac:concurrency:short3', NULL),

  (1, 31402, 'PRACTICAL', 'SHORT', 'NORMAL',
   '데드락(Deadlock)의 정의를 한 문장으로 설명하세요.',
   NULL,
   '두 개 이상의 트랜잭션이 서로가 가진 락을 기다리며 영원히 대기 상태에 빠지는 현상',
   '현업에서도 자주 나오는 중요 개념이라 정의를 정확히 외워두는 것이 좋습니다.',
   'seed:prac:concurrency:short4', NULL),

  (1, 31402, 'PRACTICAL', 'SHORT', 'HARD',
   '락 경합을 줄이기 위해 트랜잭션 설계에서 적용할 수 있는 일반적인 원칙 두 가지를 쓰세요.',
   NULL,
   '트랜잭션을 짧게 유지, 항상 같은 순서로 자원을 잠그기',
   '또한 불필요한 조회를 트랜잭션 밖으로 빼는 것도 중요합니다.',
   'seed:prac:concurrency:short5', NULL),

  (1, 31402, 'PRACTICAL', 'SHORT', 'HARD',
   '애플리케이션 레벨에서 분산 락(예: Redis 기반)을 사용할 때 주의해야 할 점을 한 문장으로 설명하세요.',
   NULL,
   '락 만료/해제 실패 등으로 인해 중복 실행이나 영구 락이 생기지 않도록 만료 시간과 해제 로직을 신중히 설계해야 한다',
   '분산 락은 DB 락보다 자유도가 높은 대신, 직접 만료·재시도·오류 처리를 구현해야 합니다.',
   'seed:prac:concurrency:short6', NULL),

  -- LONG
  (1, 31402, 'PRACTICAL', 'LONG', 'NORMAL',
   '동시에 여러 사용자가 같은 재고를 차감하는 온라인 쇼핑몰 환경에서, Lost Update와 데드락을 최소화하기 위한 설계 방안을 3가지 이상 서술하세요.',
   NULL,
   '재고 전용 테이블/행 잠금, 낙관적 락, 일관된 락 순서, 짧은 트랜잭션',
   '예시: (1) 재고 차감은 항상 동일한 UPDATE 쿼리 패턴과 WHERE 조건으로 수행해 인덱스를 잘 타게 한다. (2) 재고 테이블을 분리해 락 범위를 최소화한다. (3) 버전 컬럼을 활용한 낙관적 락으로 충돌 시 재시도하도록 설계한다. (4) 여러 자원을 잠글 때는 항상 같은 순서로 잠가 데드락을 줄인다.',
   'seed:prac:concurrency:long2', NULL),

  (1, 31402, 'PRACTICAL', 'LONG', 'HARD',
   '대규모 이벤트 신청 시스템에서 “선착순 100명”에게만 혜택을 주는 기능을 구현해야 한다. 동시성 제어와 락, 큐/토큰 전략을 포함해 어떻게 설계할지 서술하세요.',
   NULL,
   'DB 락만이 아니라 토큰/큐를 통한 선필터링, 카운터 관리',
   '정답 예시: (1) 애플리케이션 앞단에서 대기열(큐)을 두어 동시에 DB에 몰리지 않도록 제어한다. (2) 혜택 카운터를 단일 레코드로 관리하고, UPDATE ... WHERE count < 100 형태로 낙관적 제어를 한다. (3) 실패 시 재시도 대신 “이미 마감” 안내를 빠르게 반환해 락 경합을 줄인다.',
   'seed:prac:concurrency:long3', NULL),

  (1, 31402, 'PRACTICAL', 'LONG', 'HARD',
   '분산 환경에서 여러 인스턴스가 동시에 같은 배치 작업을 수행하는 것을 막기 위해 “리더 선출 + 분산 락” 구조를 사용하려고 한다. 이 구조의 개념과 구현 시 주의사항을 서술하세요.',
   NULL,
   '한 인스턴스를 리더로 뽑고, 분산 락으로 단일 실행 보장, 만료/실패 처리 주의',
   '예시: (1) Zookeeper, Redis, DB 등을 사용해 리더를 선출한다. (2) 리더만 분산 락을 취득하고 배치를 실행한다. (3) 리더 장애나 네트워크 분리 상황에서 중복 실행이 발생하지 않도록 락 만료 시간과 재선출 로직을 설계한다.',
   'seed:prac:concurrency:long4', NULL),

  -- =========================
  -- 31501 백업/RPO/RTO (실기)
  -- =========================
  -- SHORT
  (1, 31501, 'PRACTICAL', 'SHORT', 'EASY',
   '전체 백업(Full Backup)의 장점을 한 문장으로 설명하세요.',
   NULL,
   '해당 시점으로 복구 절차가 단순하고 이해하기 쉽다',
   '전체 백업만 있으면 추가 로그 없이 해당 시점으로 되돌릴 수 있습니다.',
   'seed:prac:backup:short2', NULL),

  (1, 31501, 'PRACTICAL', 'SHORT', 'NORMAL',
   '증분 백업(Incremental Backup)의 장점을 두 가지 쓰세요.',
   NULL,
   '백업 시간 단축, 저장 공간 절약',
   '변경분만 백업하므로 전체 백업보다 빠르고 공간도 적게 사용합니다.',
   'seed:prac:backup:short3', NULL),

  (1, 31501, 'PRACTICAL', 'SHORT', 'NORMAL',
   'RPO와 RTO를 함께 줄이려고 할 때 늘어나는 비용/복잡도를 한 문장으로 설명하세요.',
   NULL,
   '백업/복제 인프라와 자동화 수준을 높여야 하므로 인프라 비용과 운영 복잡도가 크게 증가한다',
   '둘 다 강화할수록 고가용성/재해복구 체계를 갖춰야 하므로 투자 규모가 커집니다.',
   'seed:prac:backup:short4', NULL),

  (1, 31501, 'PRACTICAL', 'SHORT', 'HARD',
   'Off-site 백업(원격지 백업)을 구성해야 하는 대표적인 이유를 한 문장으로 설명하세요.',
   NULL,
   '화재나 홍수 같은 물리적 재해가 발생해도 다른 지역의 백업으로 복구할 수 있도록 하기 위해서',
   '동일 센터 내 백업만으로는 재해 복구 요구사항을 충족하기 어렵습니다.',
   'seed:prac:backup:short5', NULL),

  (1, 31501, 'PRACTICAL', 'SHORT', 'HARD',
   '로그 배송(Log Shipping) 또는 실시간 복제를 사용할 때 주의해야 할 점을 한 문장으로 설명하세요.',
   NULL,
   '원본 장애나 논리적 오류가 그대로 복제본에도 전파될 수 있으므로, 별도의 스냅샷이나 포인트 인 타임 복구 전략이 필요하다',
   '실시간 복제는 RPO를 줄여주지만, 잘못된 데이터도 함께 복제된다는 점을 항상 인식해야 합니다.',
   'seed:prac:backup:short6', NULL),

  -- LONG
  (1, 31501, 'PRACTICAL', 'LONG', 'NORMAL',
   '중소 규모 서비스에서 “RPO 1시간, RTO 4시간” 목표를 만족하는 백업/복구 전략을 설계해야 한다. 전체 백업, 증분/로그 백업, 테스트 복구 절차를 포함해 설계안을 서술하세요.',
   NULL,
   '일 1회 전체 + 1시간 주기 증분/로그 백업, 정기 복구 리허설',
   '예시: (1) 새벽 시간에 전체 백업을 수행한다. (2) 업무 시간 동안 1시간 간격으로 증분 또는 로그 백업을 수행해 RPO 1시간을 맞춘다. (3) 복구 절차를 문서화하고 분기마다 테스트 복구를 수행해 RTO 4시간 이내 복구가 가능한지 검증한다.',
   'seed:prac:backup:long2', NULL),

  (1, 31501, 'PRACTICAL', 'LONG', 'HARD',
   '대규모 금융 시스템에서 “RPO 0, RTO 수분 이내”를 요구하는 경우, 동기식 복제, 이중화, 자동 장애 조치를 조합해 어떻게 설계해야 하는지 개략적으로 서술하세요.',
   NULL,
   '동기식 이중화로 RPO 0, 자동 페일오버로 RTO 단축',
   '정답 예시: (1) 주 DB와 대기 DB 간 동기식 복제를 구성해 커밋이 양쪽에 동시에 반영되도록 한다. (2) 애플리케이션은 가상 IP나 로드밸런서를 통해 활성 노드로 접속하며, 장애 시 헬스 체크로 자동 페일오버한다. (3) DR 센터를 별도로 두어 사이트 장애에도 대응한다.',
   'seed:prac:backup:long3', NULL),

  (1, 31501, 'PRACTICAL', 'LONG', 'HARD',
   'S3와 같은 오브젝트 스토리지를 활용해 애플리케이션 로그/백업을 장기 보관하려고 한다. 버전 관리, 라이프사이클 정책, 암호화를 포함해 설계 시 고려해야 할 사항을 서술하세요.',
   NULL,
   '버전 관리로 실수 삭제 대비, 라이프사이클로 보관 기간 관리, 암호화로 보안 강화',
   '예시: (1) 중요한 백업 버킷에는 버전 관리를 활성화해 실수 삭제나 덮어쓰기에 대비한다. (2) 라이프사이클 규칙으로 일정 기간 후 저비용 스토리지로 이동하거나 자동 삭제한다. (3) 서버 측 암호화와 접근 제어 정책으로 민감 데이터를 보호한다.',
   'seed:prac:backup:long4', NULL),

  -- =========================
  -- 31502 장애/Incident Postmortem (실기)
  -- =========================
  -- SHORT
  (1, 31502, 'PRACTICAL', 'SHORT', 'EASY',
   '장애 분석 보고서(Postmortem)의 주요 목적을 한 문장으로 설명하세요.',
   NULL,
   '책임 추궁이 아니라 원인 분석과 재발 방지 대책 수립을 통해 시스템을 개선하는 것이다',
   '현업에서도 “Blameless Postmortem”이 중요한 문화로 자리잡고 있습니다.',
   'seed:prac:incident:short2', NULL),

  (1, 31502, 'PRACTICAL', 'SHORT', 'NORMAL',
   '장애 타임라인을 정리할 때 반드시 포함해야 할 정보 두 가지를 쓰세요.',
   NULL,
   '각 시점에 발생한 주요 이벤트, 조치 내용과 담당자',
   '언제 무엇을 했는지를 시간축으로 정리해야 이후 회고와 개선 작업이 수월합니다.',
   'seed:prac:incident:short3', NULL),

  (1, 31502, 'PRACTICAL', 'SHORT', 'NORMAL',
   '장애 영향 범위를 정리할 때 확인해야 할 관점을 두 가지 쓰세요.',
   NULL,
   '영향을 받은 사용자/거래 수, 영향의 기간과 기능 범위',
   '예: 몇 명의 고객이, 어느 시간 동안, 어떤 기능을 사용하지 못했는지 등입니다.',
   'seed:prac:incident:short4', NULL),

  (1, 31502, 'PRACTICAL', 'SHORT', 'HARD',
   '장애의 “직접 원인”과 “근본 원인”을 구분해야 하는 이유를 한 문장으로 설명하세요.',
   NULL,
   '즉각적인 증상뿐 아니라 구조적 문제를 찾아내 동일 유형의 장애가 반복되는 것을 막기 위해서',
   '예: 설정 실수(직접 원인) 뒤에 있는 교육/검증 프로세스 부재(근본 원인)를 찾아야 합니다.',
   'seed:prac:incident:short5', NULL),

  (1, 31502, 'PRACTICAL', 'SHORT', 'HARD',
   '장애 재발 방지 대책을 작성할 때 “할 일 목록(To-do)” 수준에서 끝내면 안 되는 이유를 한 문장으로 설명하세요.',
   NULL,
   '책임자, 기한, 검증 방법이 없는 대책은 실행되지 않거나 효과를 확인하기 어렵기 때문',
   '대책마다 담당자와 마감일, 완료 기준을 명확히 해야 합니다.',
   'seed:prac:incident:short6', NULL),

  -- LONG
  (1, 31502, 'PRACTICAL', 'LONG', 'NORMAL',
   '야간 배치 작업 중 인덱스 재구성으로 인한 장애가 발생한 사례를 기반으로, Postmortem 보고서에 포함되어야 할 항목(타임라인, 영향, 원인, 재발 방지 대책)을 구조화하여 서술하세요.',
   NULL,
   '언제 무엇이 일어났는지, 누구/무엇이 영향을 받았는지, 직접/근본 원인, 구체적인 대책',
   '예시: (1) 타임라인: 작업 시작 시각, 장애 발생 시각, 탐지 시각, 복구 완료 시각. (2) 영향: 어떤 서비스/고객이 얼마나 오랫동안 영향을 받았는지. (3) 원인: 피크 시간대 직전 대용량 인덱스 재구성 수행, 사전 부하 테스트 미실시 등. (4) 대책: 작업 시간대 조정, 온라인 재구성 도입, 변경 전 성능 검증 프로세스 추가.',
   'seed:prac:incident:long2', NULL),

   (1, 31502, 'PRACTICAL', 'LONG', 'HARD',
   '대규모 장애 발생 후 “블레이멜스(Blameless) 문화”를 유지하면서도 실질적인 개선을 이끌어내기 위한 Postmortem 운영 원칙을 3가지 이상 서술하세요.',
   NULL,
   '개인 비난 금지, 데이터 기반 분석, 실행 가능한 대책과 추적',
   '예시: (1) 누구의 잘못인지가 아니라 무엇이 시스템을 이런 상태로 만들었는지에 집중한다. (2) 로그/지표 등 객관적 데이터를 기반으로 논의한다. (3) 재발 방지 대책은 구체적인 작업 항목과 담당자, 기한을 포함해 추적한다. (4) 모든 내용을 공유해 조직 학습 기회로 활용한다.',
   'seed:prac:incident:long3', NULL),

  (1, 31502, 'PRACTICAL', 'LONG', 'HARD',
   '장애 Postmortem 결과를 DevOps 파이프라인 개선과 어떻게 연결할 수 있는지, 예를 들어 배포 자동화, 모니터링, 알림 체계 측면에서 구체적인 개선 예시를 들어 서술하세요.',
   NULL,
   '장애 원인을 기준으로 배포/모니터링/알림을 개선하는 선순환 구조',
   '예시: (1) 설정 오류로 인한 장애가 있었다면, 배포 파이프라인에 설정 검증 단계와 린트/테스트를 추가한다. (2) 장애를 늦게 인지했다면, 관련 지표에 대한 경고 임계값과 알림 채널(Slack, SMS)을 조정한다. (3) 롤백이 늦었다면, 원클릭 롤백 기능이나 블루/그린 배포 전략을 도입한다.',
   'seed:prac:incident:long4', NULL);


/* =========================================================
 * QUESTION_TAG 매핑 (31401 ~ 31502)
 * - 허용되지 않은 태그 제거
 * - 토픽별 대표 태그로 재매핑
 *   31401 → 트랜잭션
 *   31402 → 동시성
 *   31501 → 백업복구
 *   31502 → 장애분석
 * ========================================================= */

-- 31401 트랜잭션/격리수준 → '트랜잭션'
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31401
  AND qt.tag <> '트랜잭션';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '트랜잭션'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '트랜잭션'
WHERE q.topic_id = @tp_31401
  AND qt.question_id IS NULL;

-- 31402 동시성/락 → '동시성'
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31402
  AND qt.tag <> '동시성';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '동시성'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '동시성'
WHERE q.topic_id = @tp_31402
  AND qt.question_id IS NULL;

-- 31501 백업/복구/RPO/RTO → '백업복구'
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31501
  AND qt.tag <> '백업복구';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '백업복구'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '백업복구'
WHERE q.topic_id = @tp_31501
  AND qt.question_id IS NULL;

-- 31502 장애 분석/포스트모템 → '장애분석'
DELETE qt
FROM question_tag qt
JOIN question q ON qt.question_id = q.id
WHERE q.topic_id = @tp_31502
  AND qt.tag <> '장애분석';

INSERT INTO question_tag (question_id, tag)
SELECT q.id, '장애분석'
FROM question q
LEFT JOIN question_tag qt
  ON qt.question_id = q.id
 AND qt.tag = '장애분석'
WHERE q.topic_id = @tp_31502
  AND qt.question_id IS NULL;


SET FOREIGN_KEY_CHECKS = 1;
