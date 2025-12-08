-- =========================================
-- 3.4.1 물리 요소 조사 분석 (concept 보강)
-- topic_id = 13401
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 13401,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              3.4.1.1 스토리지 종류와 특징
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '3.4.1.1',
             'title', '스토리지(Storage) 접속 방식 비교',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','스토리지(Storage)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '스토리지는 데이터를 보존하기 위한 저장장치로, 서버와의 접속 방식에 따라 DAS, NAS, SAN 등으로 구분된다. '
                 '물리 데이터베이스 설계에서는 성능·가용성·확장성 측면에서 어떤 스토리지 방식을 선택할지 분석하는 과정이 중요하다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','스토리지 접속 방식 비교 (DAS / NAS / SAN)',
                 'headers', JSON_ARRAY('방식','설명','장점','단점'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'DAS (Direct Attached Storage)',
                     '데이터 서버와 외장형 저장장치를 전용 케이블로 직접 접속하는 방식',
                     '전용 라인 사용으로 성능이 보장되며, 구조가 단순하고 안정성이 높다.',
                     '저장장치별 접속 방식이 달라 공유가 어렵고, 확장성·유연성이 떨어진다.'
                   ),
                   JSON_ARRAY(
                     'NAS (Network Attached Storage)',
                     'LAN 상에서 파일 서버를 통해 파일 시스템을 공유하는 방식',
                     '서버와 저장장치를 분리해 관리할 수 있고, 데이터 접근·공유가 용이하다.',
                     '파일 서버에 병목이 발생할 수 있고, 파일 서버 장애 시 스토리지 접근이 불가능하다.'
                   ),
                   JSON_ARRAY(
                     'SAN (Storage Area Network)',
                     '광섬유 채널을 이용해 n개의 서버와 m개의 저장장치를 상호 접속하는 방식',
                     'DAS의 한계를 극복하고 대규모 환경에서 고성능·고가용성을 제공한다.',
                     '구성이 복잡하고 비용이 높으며, 이기종 서버 지원과 파일 시스템 형식에 제약이 있을 수 있다.'
                   )
                 )
               )
             )
           ),

           /* -------------------------------------
              3.4.1.2 분산 데이터베이스 개념과 구성
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '3.4.1.2',
             'title', '분산 데이터베이스 개념·장단점·구성',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','분산 데이터베이스(Distributed Database)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '분산 데이터베이스는 네트워크상의 여러 컴퓨터에 데이터가 물리적으로 분산되어 있지만, '
                 '사용자는 하나의 통합된 데이터베이스처럼 접근할 수 있도록 논리적으로 통합한 구조이다.'
               ),
               JSON_OBJECT('type','heading','text','분산 데이터베이스의 장단점'),
               JSON_OBJECT(
                 'type','table',
                 'caption','분산 데이터베이스 장단점',
                 'headers', JSON_ARRAY('구분','내용'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('장점',
                              '분산 제어 용이, 지역 자치성 향상, 효용성과 융통성 증가, '
                              '데이터 복제·분산을 통한 응답 성능 향상, 장애 시 데이터 손실 최소화, 시스템 확장 용이'),
                   JSON_ARRAY('단점',
                              '구성이 복잡해지고, 설계·운영 난이도가 높아지며, '
                              '통신 비용·동기화 오버헤드로 인해 성능 저하와 개발 비용 증가가 발생할 수 있다.')
                 )
               ),
               JSON_OBJECT('type','heading','text','분산 데이터베이스 스키마 구조 (전·분·할·지)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '전역 스키마(Global Schema): 전체 데이터베이스의 논리적 구조 정의',
                   '분할 스키마(Fragment Schema): 릴레이션을 어떻게 분할(수평/수직)할지 정의',
                   '할당 스키마(Allocation Schema): 각 단편을 어느 위치에 저장할지 정의',
                   '지역 스키마(Local Schema): 각 지역 DBMS 내부의 구체적 구조 정의'
                 )
               )
             )
           ),

           /* -------------------------------------
              3.4.1.3 분산 DB의 투명성
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '3.4.1.3',
             'title', '분산 데이터베이스 투명성',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','분산 DB 투명성의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '분산 데이터베이스의 투명성은 사용자가 데이터의 물리적 분산 구조를 의식하지 않고 단일 데이터베이스처럼 사용할 수 있게 하는 성질이다. '
                 '시험에서는 각 투명성의 정의를 구분하는 문제가 자주 출제된다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','분산 데이터베이스 투명성 (위·복·병·분·장)',
                 'headers', JSON_ARRAY('종류','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('위치 투명성',
                              '사용자가 데이터의 물리적 저장 위치를 알 필요 없이 논리적 이름만으로 접근할 수 있는 성질'),
                   JSON_ARRAY('복제 투명성',
                              '데이터가 여러 장소에 복제되어 있어도 사용자는 복제 여부를 의식하지 않고 접근할 수 있는 성질'),
                   JSON_ARRAY('병행 투명성',
                              '여러 사용자가 동시에 트랜잭션을 수행하더라도 결과에 이상이 발생하지 않는 성질(로킹·타임스탬프 등 사용)'),
                   JSON_ARRAY('분할 투명성',
                              '하나의 릴레이션이 여러 단편으로 분할·분산되어 있어도 단일 릴레이션처럼 접근할 수 있는 성질'),
                   JSON_ARRAY('장애 투명성',
                              '어느 지역 시스템이나 통신망에 장애가 발생해도 데이터 무결성을 유지하고, 2단계 커밋(2PC) 등으로 복구 가능한 성질')
                 )
               )
             )
           ),

           /* -------------------------------------
              3.4.1.4 이중화·백업 개요
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '3.4.1.4',
             'title', '데이터베이스 이중화와 백업 전략',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','데이터베이스 이중화(Database Replication)'),
               JSON_OBJECT('type','paragraph','text',
                 '데이터베이스 이중화는 물리적으로 떨어진 복수의 데이터베이스에 변경 사항을 복제해 관리하는 기술이다. '
                 '장애 발생 시 빠른 복구와 서비스 연속성 확보, 읽기 부하 분산 등의 효과가 있다.'
               ),
               JSON_OBJECT('type','heading','text','백업(Backup)의 필요성과 종류'),
               JSON_OBJECT('type','paragraph','text',
                 '백업은 장애나 사용자 실수로 인한 데이터 손실에 대비해 데이터를 별도로 보관하는 작업이다. '
                 '물리 설계에서는 복구 목표 시간(RTO)과 복구 시점(RPO)에 맞는 백업 전략 수립이 중요하다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','데이터베이스 백업 종류 (전·차·증·트)',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('전체 백업 (Full Backup)',
                              '대상 데이터 전체를 주기적으로 백업하는 방식으로, 복구가 단순하지만 백업 시간·공간이 많이 필요하다.'),
                   JSON_ARRAY('차등 백업 (Differential Backup)',
                              '마지막 전체 백업 이후 변경된 모든 데이터를 백업하는 방식으로, 전체+차등 조합으로 복구한다.'),
                   JSON_ARRAY('증분 백업 (Incremental Backup)',
                              '이전 백업 이후 변경된 데이터만 백업하는 방식으로, 백업 부담은 작지만 복구 시 순차 적용이 필요하다.'),
                   JSON_ARRAY('트랜잭션 로그 백업',
                              '데이터 파일이 아니라 로그만 백업해 장애 직전 상태까지 복구할 수 있도록 하는 방식')
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);


-- =========================================
-- 3.4.2 DB 물리 속성 설계 (concept 보강)
-- topic_id = 13402
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 13402,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              3.4.2.1 암호화 개념과 유형
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '3.4.2.1',
             'title', '데이터베이스 암호화 개념과 유형',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','데이터베이스 암호화(Database Encryption)의 필요성'),
               JSON_OBJECT('type','paragraph','text',
                 '데이터베이스 암호화는 민감 정보를 암·복호화해 기밀성을 보장하는 보안 기법이다. '
                 '대용량 데이터 환경에서는 성능과 보안을 모두 고려한 암호화 범위·방식을 설계하는 것이 중요하다.'
               ),
               JSON_OBJECT('type','heading','text','암호화 유형 (응·서·자·기·운)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','데이터베이스 암호화 유형',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('응용 프로그램 자체 암호화',
                              '응용에서 직접 암·복호화를 수행하고, 복호화된 데이터만 DB로 전송하는 방식'),
                   JSON_ARRAY('DB 서버 암호화',
                              'DB 서버의 DBMS 커널이 자체적으로 암·복호화 기능을 수행하는 방식'),
                   JSON_ARRAY('DBMS 자체 암호화',
                              '응용에서 DBMS가 제공하는 암·복호화 API를 호출해 사용하는 방식'),
                   JSON_ARRAY('DBMS 암호화 기능 호출',
                              'DBMS 데이터 파일을 대상으로 OS 레벨 입출력을 암호화하는 방식'),
                   JSON_ARRAY('운영체제 암호화',
                              '파일 시스템·디스크 단위에서 암·복호화를 수행하는 방식')
                 )
               )
             )
           ),

           /* -------------------------------------
              3.4.2.2 암호화 적용 방식
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '3.4.2.2',
             'title', '암호화 적용 방식 (컬럼·블록)',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','컬럼 암호화 방식'),
               JSON_OBJECT('type','paragraph','text',
                 '컬럼 암호화는 특정 민감 컬럼(주민번호, 카드번호 등)에만 선택적으로 암호화를 적용하는 방식이다. '
                 'CPU 부하와 키 관리 오버헤드는 있지만, 필요한 데이터만 암호화해 성능 영향을 줄일 수 있다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'API 방식: 응용에서 암호화 API를 직접 호출해 암·복호화를 수행',
                   'Plug-in 방식: DB 드라이버/엔진에 플러그인을 추가해 접근 시 자동 암·복호화',
                   'Hybrid 방식: API와 Plug-in을 혼합해 유연성과 성능을 균형 있게 조절'
                 )
               ),
               JSON_OBJECT('type','heading','text','블록 암호화 방식'),
               JSON_OBJECT('type','paragraph','text',
                 '블록 암호화는 테이블스페이스·파일·디스크 블록 단위로 데이터 파일 전체를 암호화하는 방식이다. '
                 'TDE(Transparent Data Encryption)와 파일 암호화 방식이 대표적이며, 응용 변경이 적은 대신 I/O 성능에 영향을 줄 수 있다.'
               )
             )
           ),

           /* -------------------------------------
              3.4.2.3 파티셔닝 설계
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '3.4.2.3',
             'title', '파티셔닝 개념·장점·유형',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','파티셔닝(Partitioning)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '파티셔닝은 대용량 테이블을 보다 작은 논리적 단위(파티션)로 분할해 관리·성능·가용성을 향상시키는 기법이다. '
                 '논리적으로는 하나의 테이블이지만, 물리적으로는 여러 파티션으로 나누어 저장한다.'
               ),
               JSON_OBJECT('type','heading','text','파티셔닝 장점 (성·가·백·합)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '성능 향상: 파티션 프루닝으로 필요한 파티션만 스캔해 I/O를 줄일 수 있다.',
                   '가용성 향상: 일부 파티션 장애가 전체 테이블에 미치는 영향을 줄인다.',
                   '백업·복구 용이: 파티션 단위 백업·복구로 운영 부담을 줄일 수 있다.',
                   '경합 감소: 파티션별로 부하를 분산해 락 경합을 줄인다.'
                 )
               ),
               JSON_OBJECT('type','heading','text','파티셔닝 유형 (레·해·리·컴)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','파티셔닝 유형',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('범위 분할 (Range Partitioning)',
                              '날짜·번호 등 연속적인 값의 범위 기준으로 분할하는 방식'),
                   JSON_ARRAY('해시 분할 (Hash Partitioning)',
                              '해시 함수를 사용해 균등하게 데이터를 분산하는 방식'),
                   JSON_ARRAY('리스트 분할 (List Partitioning)',
                              '코드값 등 명시적인 값 목록 기준으로 분할하는 방식'),
                   JSON_ARRAY('컴포지트 분할 (Composite Partitioning)',
                              '범위+해시 등 두 가지 이상 분할 방식을 조합한 방식')
                 )
               )
             )
           ),

           /* -------------------------------------
              3.4.2.4 클러스터링·데이터 지역화
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '3.4.2.4',
             'title', '클러스터링과 데이터 지역화',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','클러스터링(Clustering)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '클러스터링은 지정된 컬럼 값의 순서대로 데이터 행을 물리적으로 저장하는 방법이다. '
                 '범위 조회나 특정 조인 패턴이 반복되는 경우, 관련 데이터를 물리적으로 인접하게 배치해 I/O를 줄이는 목적이 있다.'
               ),
               JSON_OBJECT('type','list','items', JSON_ARRAY(
                 '단일 클러스터링: 하나의 테이블에 대해 클러스터링을 적용',
                 '다중 클러스터링: 여러 테이블을 같은 클러스터 구조에 배치해 조인 성능을 높임'
               )),
               JSON_OBJECT('type','heading','text','데이터 지역화(Data Locality)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '데이터 지역화는 자주 함께 사용되는 데이터를 물리적으로 가까운 위치에 저장해 캐시 효율과 I/O 성능을 높이는 설계 원칙이다. '
                 '시간적 지역성, 공간적 지역성, 순차적 지역성과 같은 개념을 활용해 계층적 기억장치 구조·캐시·작업세트 등을 설계한다.'
               ),
               JSON_OBJECT('type','heading','text','접근제어(Access Control)와 연계'),
               JSON_OBJECT('type','paragraph','text',
                 '물리 속성 설계에서는 암호화·파티셔닝·클러스터링뿐 아니라, 정책·메커니즘·보안 모델(정·메·보) 관점에서 접근제어를 함께 고려해야 한다. '
                 '어떤 파티션·스토리지에 어떤 데이터와 권한을 배치할지까지 설계해야 보안·성능 요구를 동시에 만족시킬 수 있다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);

-- =========================================
-- 3.4.3 데이터베이스 무결성과 키 (concept 보강)
-- topic_id = 13403
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 13403,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              3.4.3.1 데이터 무결성 개념·종류
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '3.4.3.1',
             'title', '데이터베이스 무결성 개념과 종류',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','데이터베이스 무결성(Database Integrity)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '데이터 무결성은 데이터가 허용되지 않은 방법으로 변경되지 않도록 보호하고, '
                 '정의된 제약조건을 항상 만족하도록 유지하는 성질이다. '
                 '정확하고 신뢰할 수 있는 데이터를 유지하는 것이 목적이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','데이터 무결성 종류 (개·참·속·사·키)',
                 'headers', JSON_ARRAY('종류','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('개체 무결성',
                              '한 엔터티(릴레이션)에서 기본 키(PK)는 NULL이 될 수 없고, 중복될 수 없다.'),
                   JSON_ARRAY('참조 무결성',
                              '외래 키(FK)는 참조 대상 릴레이션의 기본 키 값 또는 NULL이어야 한다.'),
                   JSON_ARRAY('속성 무결성',
                              '각 속성 값은 정의된 도메인(데이터 타입·길이·기본값·NULL 허용 여부)을 만족해야 한다.'),
                   JSON_ARRAY('사용자 무결성',
                              '사용자가 정의한 의미적 요구사항(비즈니스 룰)을 위반하지 않아야 한다.'),
                   JSON_ARRAY('키 무결성',
                              '한 릴레이션 내에서 같은 키 값을 가진 튜플이 존재할 수 없도록 보장하는 성질')
                 )
               )
             )
           ),

           /* -------------------------------------
              3.4.3.2 키의 개념과 특성
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '3.4.3.2',
             'title', '키(Key)의 개념과 특성',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','키(Key)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '키는 릴레이션 내에서 각 튜플을 유일하게 식별하기 위한 속성(또는 속성 집합)이다. '
                 '키의 핵심 특성은 유일성과 최소성으로, 어떤 속성 조합이 진짜 식별자로 적합한지 판단하는 기준이 된다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '유일성: 한 릴레이션 내에서 키 값이 서로 중복되지 않는다.',
                   '최소성: 키를 구성하는 속성의 부분 집합만으로는 동일한 유일성을 보장할 수 없다.'
                 )
               )
             )
           ),

           /* -------------------------------------
              3.4.3.3 키의 종류
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '3.4.3.3',
             'title', '주요 키 종류 정리',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','키 종류와 정의'),
               JSON_OBJECT(
                 'type','table',
                 'caption','데이터베이스 키 종류',
                 'headers', JSON_ARRAY('키 종류','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('후보키 (Candidate Key)',
                              '튜플을 유일하게 식별할 수 있는 속성 집합으로, 유일성과 최소성을 모두 만족한다. '
                              '기본 키와 대체 키가 될 수 있는 후보 집합이다.'),
                   JSON_ARRAY('기본키 (Primary Key)',
                              '후보키 중에서 기본 식별자로 선택된 키로, NULL과 중복이 허용되지 않는다.'),
                   JSON_ARRAY('대체키 (Alternate Key)',
                              '후보키 중 기본 키로 채택되지 않은 나머지 후보키를 의미한다.'),
                   JSON_ARRAY('슈퍼키 (Super Key)',
                              '튜플을 유일하게 식별할 수 있지만, 최소성을 만족하지 않을 수도 있는 키이다. '
                              '후보키에 불필요한 속성이 추가된 형태가 될 수 있다.'),
                   JSON_ARRAY('외래키 (Foreign Key)',
                              '다른 릴레이션의 기본 키를 참조하는 속성으로, 테이블 간 참조 무결성을 유지하기 위한 제약 조건이다.')
                 )
               )
             )
           ),

           /* -------------------------------------
              3.4.3.4 무결성과 키 연계
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '3.4.3.4',
             'title', '무결성 제약과 키 설계의 연계',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','무결성 제약 구현 시 키 설계 포인트'),
               JSON_OBJECT('type','paragraph','text',
                 '실제 데이터베이스 스키마를 설계할 때는, 개체·참조·속성 무결성 조건을 키와 제약 조건으로 명확히 표현해야 한다. '
                 '예를 들어 기본 키와 유니크 인덱스, NOT NULL 제약, 외래 키 제약 등을 적절히 조합해 무결성을 구현한다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '기본 키에는 NOT NULL + UNIQUE 제약을 설정해 개체 무결성을 보장한다.',
                   '외래 키에는 ON DELETE / ON UPDATE 옵션을 설정해 참조 무결성 위반을 예방한다.',
                   'CHECK 제약과 도메인 정의를 통해 속성·사용자 무결성을 세밀하게 표현한다.'
                 )
               ),
               JSON_OBJECT('type','paragraph','text',
                 '시험에서는 각 무결성 종류의 정의, 키 종류의 관계, 외래 키와 참조 무결성의 연결 고리를 함께 묻는 경우가 많으므로 '
                 '정의를 단순 암기하는 것을 넘어서 서로의 연관성을 이해해 두는 것이 좋다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);

-- =========================================
-- 3.4.4 DB 반 정규화 (concept 보강)
-- topic_id = 13404
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 13404,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              3.4.4.1 반정규화 개념·목적
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '3.4.4.1',
             'title', '반정규화(De-Normalization)의 개념과 목적',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','반정규화의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '반정규화는 이미 정규화된 엔터티·속성·관계를 성능 향상과 운영 단순화를 위해 의도적으로 중복·통합·분리를 수행하는 데이터 모델링 기법이다. '
                 '비정규화·역정규화라고도 부르며, 조회 성능을 위해 일부 정규화 원칙을 완화하는 과정이다.'
               ),
               JSON_OBJECT('type','heading','text','반정규화의 적용 목적'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '복잡한 조인 구조를 단순화해 조회 성능을 향상시킨다.',
                   '보고서·집계 쿼리 등에서 반복 계산을 줄여 응답 시간을 단축한다.',
                   '운영·개발 측면에서 자주 사용되는 데이터의 접근 경로를 단순화한다.'
                 )
               )
             )
           ),

           /* -------------------------------------
              3.4.4.2 반정규화 장단점
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '3.4.4.2',
             'title', '반정규화의 장단점',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','반정규화 장단점 비교'),
               JSON_OBJECT(
                 'type','table',
                 'caption','반정규화 장단점',
                 'headers', JSON_ARRAY('구분','내용'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('장점',
                              '복잡한 조인 없이 필요한 데이터를 조회할 수 있어 성능이 향상된다. '
                              '업무에 맞춘 테이블 구조로 관리 효율성이 증가할 수 있다.'),
                   JSON_ARRAY('단점',
                              '데이터 중복으로 인해 일관성과 정합성이 떨어질 수 있고, '
                              '데이터 변경 로직이 복잡해져 유지보수 비용과 성능 부담이 증가할 수 있다.')
                 )
               ),
               JSON_OBJECT('type','paragraph','text',
                 '따라서 반정규화는 무조건 수행하는 것이 아니라, 튜닝·인덱스·캐시 등 다른 방법으로 해결이 어려운 경우에만 '
                 '필요 최소 범위에서 적용해야 한다.'
               )
             )
           ),

           /* -------------------------------------
              3.4.4.3 반정규화 기법
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '3.4.4.3',
             'title', '반정규화 주요 기법 정리',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','반정규화 기법 분류 (태·병·분·중 / 컬·중 / 관·중)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '테이블 병합: 자주 조인되는 두 개 이상의 테이블을 하나로 합쳐 조인을 제거',
                   '테이블 분할: 수직·수평 분할을 통해 특정 속성·범위에 대한 접근 성능을 향상',
                   '테이블 중복: 여러 모듈에서 공통으로 사용하는 참조 데이터를 중복 저장',
                   '컬럼 중복: 계산 결과나 코드명을 컬럼으로 추가해 조회 시 조인·계산을 줄임',
                   '관계 중복: 자주 사용되는 경로에 대해 별도의 조인 관계를 추가해 탐색 경로 단축'
                 )
               )
             )
           ),

           /* -------------------------------------
              3.4.4.4 CRUD 분석과 SQL 성능 튜닝
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '3.4.4.4',
             'title', 'CRUD 분석과 SQL 성능 튜닝 개요',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','CRUD 분석의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 'CRUD 분석은 프로세스와 테이블 간의 관계를 Create, Read, Update, Delete 관점에서 매트릭스로 표현해 '
                 '트랜잭션이 어떤 엔터티에 어떻게 영향을 미치는지 분석하는 기법이다. '
                 '반정규화 전·후로 CRUD 매트릭스를 비교하면 모델링 변경 영향 범위를 쉽게 파악할 수 있다.'
               ),
               JSON_OBJECT('type','heading','text','CRUD 매트릭스 구성요소 (엔·단·씨)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '엔터티 타입(Entity Type): 어떤 데이터(테이블)가 대상인지',
                   '단위 프로세스(Unit Process): 어떤 업무 프로세스에서 사용하는지',
                   'CRUD: 각 프로세스가 해당 엔터티에 대해 수행하는 연산 유형(C/R/U/D)'
                 )
               ),
               JSON_OBJECT('type','heading','text','SQL 성능 튜닝 개요'),
               JSON_OBJECT('type','paragraph','text',
                 'SQL 성능 튜닝은 최소한의 자원으로 최적의 응답 시간을 얻기 위한 개선 활동이다. '
                 '옵티마이저 조정, 힌트 사용, 부분 범위 처리, 인덱스 설계 등을 통해 성능을 높이며, '
                 '반정규화는 이러한 튜닝 기법으로 해결이 어려운 경우 마지막 수단으로 고려하는 것이 일반적이다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);