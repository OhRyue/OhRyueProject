SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_cert;

-- =========================================
-- 실기 P.4.x 개념 (트랜잭션 / 동시성 / 락 전략)
--  - topic_id = 31401 (P.4.1), 31402 (P.4.2)
-- =========================================

SET @tp_tx_isolation := 31401; -- P.4.1 트랜잭션 특성 및 격리수준
SET @tp_concurrency  := 31402; -- P.4.2 동시성 제어 및 락 전략 설계

/* ========================================================
 * P.4.1 트랜잭션 특성 및 격리수준
 * topic_id = 31401
 * ======================================================== */
INSERT INTO concept (topic_id, sections_json)
VALUES (
  @tp_tx_isolation,
  JSON_OBJECT(
    'sections', JSON_ARRAY(

      /* -------------------------------------
         P.4.1.1 트랜잭션과 ACID 특성
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 1,
        'subCode', 'P.4.1.1',
        'title', '트랜잭션 기본 개념과 ACID 특성',
        'importance', 5,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','트랜잭션(Transaction)이란?'),
          JSON_OBJECT(
            'type','paragraph',
            'text',
            '트랜잭션은 데이터베이스에서 논리적으로 하나의 작업 단위입니다. 계좌 이체, 주문 처리, 장바구니 결제와 같이 “모두 성공하거나, 아예 반영되지 않아야 하는 작업 묶음”을 말합니다.'
          ),

          JSON_OBJECT(
            'type','table',
            'caption','ACID 네 가지 특성 요약',
            'headers', JSON_ARRAY('특성','영문','설명','시험 포인트'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '원자성',
                'Atomicity',
                '트랜잭션 내부 작업은 전부 성공하거나 전부 실패해야 한다.',
                '중간에 실패 시 ROLLBACK 으로 이전 상태로 되돌릴 수 있어야 한다.'
              ),
              JSON_ARRAY(
                '일관성',
                'Consistency',
                '트랜잭션 수행 전·후에 데이터 무결성이 항상 유지되어야 한다.',
                '제약조건(UNIQUE, FK 등)을 깨지 않도록 설계·검증하는 것이 핵심.'
              ),
              JSON_ARRAY(
                '격리성',
                'Isolation',
                '동시에 실행되는 트랜잭션이 서로의 중간 결과를 보지 않도록 격리하는 정도.',
                '격리 수준(READ COMMITTED 등)을 통해 어느 정도까지 허용할지 조절.'
              ),
              JSON_ARRAY(
                '지속성',
                'Durability',
                'COMMIT 된 결과는 장애가 발생해도 보존되어야 한다.',
                '로그(REDO/UNDO)와 백업·복구 메커니즘과 연결되는 개념.'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','실기에서 ACID 가 나오는 전형적인 패턴'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '계좌 이체, 포인트 적립, 재고 차감처럼 “중간까지만 반영되면 안 되는 작업”을 설명하면서 ACID 를 묻는 문제',
              'COMMIT/ROLLBACK 이 누락되었을 때 어떤 문제가 발생하는지 서술하게 하는 문제',
              '특정 트랜잭션이 실패했을 때 원자성과 일관성 관점에서 어떤 조치가 필요한지 고르는 객관식 문제'
            )
          )
        )
      ),

      /* -------------------------------------
         P.4.1.2 격리수준과 이상현상
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 2,
        'subCode', 'P.4.1.2',
        'title', '격리수준(Isolation Level)과 이상현상',
        'importance', 5,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','동시 실행에서 발생하는 대표적인 이상현상'),
          JSON_OBJECT(
            'type','table',
            'caption','트랜잭션 이상현상 정리',
            'headers', JSON_ARRAY('이상현상','설명','예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                'Dirty Read',
                '다른 트랜잭션이 아직 COMMIT 하지 않은 변경 내용을 읽어버리는 현상',
                '계좌 A→B 이체 중 A 잔액이 임시로 감소한 상태를 다른 트랜잭션이 읽어버리는 경우'
              ),
              JSON_ARRAY(
                'Non-Repeatable Read',
                '같은 쿼리를 두 번 실행했을 때, 그 사이에 다른 트랜잭션이 데이터를 수정/삭제하여 결과가 달라지는 현상',
                '사용자 정보를 조회 후 다시 조회했더니 중간에 누군가가 수정해서 값이 바뀐 경우'
              ),
              JSON_ARRAY(
                'Phantom Read',
                '같은 조건으로 여러 번 조회하는 동안, 다른 트랜잭션이 행을 INSERT/DELETE 해서 “새로운 행”이 생겨나는 현상',
                '“매출 100만 이상 고객 목록”을 두 번 조회하는 사이에 새로운 고객이 조건을 만족하게 되어 목록이 바뀌는 경우'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','격리수준별로 허용되는 이상현상'),
          JSON_OBJECT(
            'type','table',
            'caption','대표 격리수준과 허용되는 이상현상',
            'headers', JSON_ARRAY('격리수준','허용되는 이상현상'),
            'rows', JSON_ARRAY(
              JSON_ARRAY('READ UNCOMMITTED','Dirty Read, Non-Repeatable Read, Phantom Read'),
              JSON_ARRAY('READ COMMITTED','Non-Repeatable Read, Phantom Read'),
              JSON_ARRAY('REPEATABLE READ','Phantom Read'),
              JSON_ARRAY('SERIALIZABLE','없음 (가장 엄격, 완전 격리)')
            )
          ),

          JSON_OBJECT('type','heading','text','실기에서 격리수준을 선택할 때 생각해야 할 것'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '온라인 트랜잭션 처리(OLTP) 시스템은 보통 READ COMMITTED 또는 REPEATABLE READ 를 사용해 성능과 일관성을 타협한다.',
              '금융·결제 같이 정합성이 매우 중요한 시스템은 더 높은 격리수준을 요구할 수 있다.',
              'SERIALIZABLE 은 동시성이 크게 떨어지므로, 특정 업무나 배치 처리 등 제한된 범위에서만 사용하는 경우가 많다.'
            )
          )
        )
      ),

      /* -------------------------------------
         P.4.1.3 COMMIT / ROLLBACK 과 실기 출제 패턴
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 3,
        'subCode', 'P.4.1.3',
        'title', 'COMMIT / ROLLBACK 흐름과 문제 풀이 팁',
        'importance', 4,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','COMMIT / ROLLBACK 기본 흐름'),
          JSON_OBJECT(
            'type','table',
            'caption','트랜잭션 제어문(TCL) 정리',
            'headers', JSON_ARRAY('명령어','설명','주의사항'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                'COMMIT',
                '현재 트랜잭션에서 수행된 변경 내용을 영구적으로 반영한다.',
                'COMMIT 이후에는 ROLLBACK 으로 되돌릴 수 없다.'
              ),
              JSON_ARRAY(
                'ROLLBACK',
                '현재 트랜잭션에서 수행된 변경 내용을 모두 취소하고 이전 상태로 되돌린다.',
                '장애나 오류 발생 시, 전체 작업 단위 취소에 사용된다.'
              ),
              JSON_ARRAY(
                'SAVEPOINT',
                '트랜잭션 내에 중간 지점을 설정하여 부분 롤백을 가능하게 한다.',
                '복잡한 트랜잭션에서 특정 지점까지만 되돌려야 할 때 사용한다.'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','실기에서 자주 나오는 트랜잭션 시나리오'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '“A 계좌에서 B 계좌로 이체하는 과정에서, 중간에 오류가 발생했을 때 올바른 처리 방안은?” → 전체 트랜잭션 ROLLBACK',
              '“여러 단계 처리 중 일부만 성공했을 때, 데이터 정합성을 맞추기 위한 조치는?” → 트랜잭션 경계 재설계 또는 예외 처리 + ROLLBACK',
              '“트랜잭션 격리수준을 낮출 때 얻는 장점과 단점은?” → 동시성 향상 vs 정합성·이상현상 증가'
            )
          )
        )
      )

    )
  )
)
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);

/* ========================================================
 * P.4.2 동시성 제어 및 락 전략 설계
 * topic_id = 31402
 * ======================================================== */
INSERT INTO concept (topic_id, sections_json)
VALUES (
  @tp_concurrency,
  JSON_OBJECT(
    'sections', JSON_ARRAY(

      /* -------------------------------------
         P.4.2.1 락(Lock)의 종류와 기본 전략
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 1,
        'subCode', 'P.4.2.1',
        'title', '락(Lock)의 기본 개념과 유형',
        'importance', 5,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','왜 락이 필요한가?'),
          JSON_OBJECT(
            'type','paragraph',
            'text',
            '여러 트랜잭션이 동시에 같은 데이터를 읽고 쓰는 상황에서, 데이터 정합성을 지키기 위해 필요한 메커니즘이 바로 락입니다. 락을 어떻게 걸고, 얼마나 오래 유지하느냐에 따라 동시성과 성능이 크게 달라집니다.'
          ),

          JSON_OBJECT(
            'type','table',
            'caption','대표적인 락 종류 정리',
            'headers', JSON_ARRAY('락 유형','설명','예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '공유락(Shared Lock, S Lock)',
                '다른 트랜잭션이 데이터를 읽는 것은 허용하지만, 쓰기는 허용하지 않는 락',
                '여러 사용자가 같은 주문 정보를 열람만 하는 상황'
              ),
              JSON_ARRAY(
                '배타락(Exclusive Lock, X Lock)',
                '해당 데이터에 대해 읽기·쓰기 모두 단독으로 점유하는 락',
                '주문 상태를 변경하거나, 계좌 잔액을 수정할 때'
              ),
              JSON_ARRAY(
                '의도락(Intent Lock)',
                '상위 단위(테이블, 페이지)에 “하위 단위에 락을 걸 예정”이라는 의도를 표시하는 락',
                '테이블 전체에 락을 걸지 않고, 일부 행만 잠그는 상황에서 동시성을 관리하는 데 사용'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','락 전략 설계의 기본 원칙'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '락은 최소한의 범위(행 단위 등)와 최소한의 시간 동안만 유지해야 한다.',
              '같은 자원에 대해 일관된 순서로 락을 획득하도록 설계하면 데드락 가능성을 줄일 수 있다.',
              '읽기 위주 시스템에서는 공유락 또는 MVCC 등 낙관적 동시성 제어 방식을 활용해 동시성을 극대화한다.'
            )
          )
        )
      ),

      /* -------------------------------------
         P.4.2.2 데드락(Deadlock)과 해결 전략
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 2,
        'subCode', 'P.4.2.2',
        'title', '데드락의 조건과 예방/해결 방법',
        'importance', 4,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','데드락(Deadlock) 기본 개념'),
          JSON_OBJECT(
            'type','paragraph',
            'text',
            '데드락은 두 개 이상의 트랜잭션이 서로가 보유한 자원의 락을 기다리느라, 영원히 진행되지 못하는 상태를 말합니다. 실무에서는 간헐적인 성능 저하와 에러를 유발하고, 시험에서는 원인과 해결책을 묻는 문제로 자주 등장합니다.'
          ),

          JSON_OBJECT(
            'type','table',
            'caption','데드락 발생의 4가지 필요 조건',
            'headers', JSON_ARRAY('조건','설명'),
            'rows', JSON_ARRAY(
              JSON_ARRAY('상호 배제(Mutual Exclusion)','한 자원은 동시에 하나의 트랜잭션만 사용할 수 있다.'),
              JSON_ARRAY('점유와 대기(Hold and Wait)','이미 자원을 점유한 상태에서 다른 자원을 추가로 요청한다.'),
              JSON_ARRAY('비선점(No Preemption)','다른 트랜잭션이 사용 중인 자원을 강제로 빼앗을 수 없다.'),
              JSON_ARRAY('순환 대기(Circular Wait)','트랜잭션들이 서로가 가진 자원을 기다리며 원형으로 대기한다.')
            )
          ),

          JSON_OBJECT('type','heading','text','데드락 예방/회피/해결 전략'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '락 획득 순서를 일관되게 정의하여 순환 대기(Circular Wait)를 원천적으로 줄인다.',
              '트랜잭션 수행 시간을 짧게 유지하고, 불필요한 사용자 입력 대기 상태에서 락을 잡지 않는다.',
              'DBMS 의 데드락 감지 기능을 활용하여, 특정 트랜잭션을 강제로 롤백하고 나머지를 진행시킨다.',
              '타임아웃(LOCK TIMEOUT)을 설정하여, 일정 시간 이상 기다리면 에러를 발생시키고 재시도 로직을 탄다.'
            )
          )
        )
      ),

      /* -------------------------------------
         P.4.2.3 동시성 설계 체크리스트와 시험 패턴
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 3,
        'subCode', 'P.4.2.3',
        'title', '동시성 제어 설계 체크리스트',
        'importance', 4,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','동시성·락 전략 설계 시 체크할 항목'),
          JSON_OBJECT(
            'type','table',
            'caption','동시성 설계 체크 박스',
            'headers', JSON_ARRAY('항목','질문','예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '락 범위',
                '행 단위로 충분한가, 테이블 전체를 잠가야 하는가?',
                '정산 배치처럼 전체 데이터를 한 번에 갱신해야 하는 경우, 일정 시간 동안 테이블 잠금이 필요할 수 있다.'
              ),
              JSON_ARRAY(
                '락 지속 시간',
                '사용자 입력을 기다리는 동안 락을 쥐고 있지는 않은가?',
                '장바구니 화면에서 결제 버튼을 누를 때까지 주문 테이블을 잠그면 심각한 병목이 발생한다.'
              ),
              JSON_ARRAY(
                '읽기 방식',
                '읽기 작업에 굳이 배타락이 필요한가, 공유락이나 스냅샷으로 충분한가?',
                '조회 화면에서 불필요하게 FOR UPDATE 를 사용하는지 점검한다.'
              ),
              JSON_ARRAY(
                '에러 처리',
                '락 타임아웃이나 데드락 에러 발생 시 재시도 로직이 있는가?',
                '일시적인 충돌로 인한 에러를 사용자가 직접 다시 시도하게 만들 것인지, 시스템이 자동 재시도할 것인지 결정한다.'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','시험에서 잘 나오는 문장 패턴'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '“동시에 여러 사용자가 데이터를 갱신하는 상황에서 데이터 불일치를 방지하기 위한 기법은?” → 락, 타임스탬프, MVCC 등 동시성 제어 기법',
              '“데드락이 자주 발생하는 이유와 해결 방안은?” → 락 획득 순서 통일, 트랜잭션 단위 축소, 타임아웃/감지 기능 활용',
              '“조회 성능을 유지하면서도 정합성을 확보하기 위한 격리수준/락 전략은?” → READ COMMITTED + 적절한 인덱스와 락 범위 조절'
            )
          )
        )
      )

    )
  )
)
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);

SET FOREIGN_KEY_CHECKS = 1;
