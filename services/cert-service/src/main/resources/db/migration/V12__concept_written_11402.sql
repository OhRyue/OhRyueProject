-- ============================
-- 1.4.2 인터페이스 대상 식별
-- topic_id = 11402
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 11402,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           -- 1.4.2.1 시스템 아키텍처 개념과 설계 원칙 (대확 고운보)
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.4.2.1',
             'title', '시스템 아키텍처 개념과 설계 원칙 (대확 고운보)',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','시스템 아키텍처(System Architecture)란?'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '시스템 아키텍처는 시스템의 구조·행위·동작 원리를 설명하는 프레임워크로, '
                 '어떤 컴포넌트들이 어떤 방식으로 상호작용하는지를 전체적으로 정의합니다.'
               ),
               JSON_OBJECT('type','heading','text','시스템 아키텍처 설계 원칙 (대확 고운보)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','아키텍처 설계 시 고려해야 할 5대 영역',
                 'headers', JSON_ARRAY('원칙','설명','설계 방안 예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '대규모 트랜잭션 처리 및 온라인 성능 보장',
                     '피크 타임에도 안정적인 응답과 처리량 확보',
                     '피크타임 용량 확보, 대용량 배치 처리, 부하 분산, DB 경량화'
                   ),
                   JSON_ARRAY(
                     '아키텍처 확장성 보장',
                     '사용자/요구 증가에 따라 손쉽게 확장 가능해야 함',
                     '다계층 아키텍처, 수평/수직 확장, 모듈화·플러그인 구조'
                   ),
                   JSON_ARRAY(
                     '서비스 고가용성 보장',
                     '장애 발생 시에도 서비스 중단을 최소화',
                     '이중화, Failover, 장애 예방·감시 체계'
                   ),
                   JSON_ARRAY(
                     '운영관리 효율성',
                     '모니터링·백업·장애 대응이 효율적으로 이루어질 것',
                     '통합 모니터링, 자동 백업, 장애 알림·로그 관리'
                   ),
                   JSON_ARRAY(
                     '시스템 보안 강화',
                     '내·외부 공격 방어 및 정보 보호',
                     '보안 전략 수립, 네트워크/시스템/접근 통제, 암호화 적용'
                   )
                 )
               )
             )
           ),

           -- 1.4.2.2 시스템 아키텍처 물리 설계 (1/2/3-Tier)
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.4.2.2',
             'title', '시스템 아키텍처 물리 설계 (1/2/3-Tier)',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','물리 아키텍처 유형'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '1-Tier 아키텍처: 모든 기능이 한 시스템에서 수행되는 구조(클라이언트·서버 분리가 없음).',
                   '2-Tier 아키텍처: 클라이언트–서버 2계층 구조 (예: 클라이언트 + DB 서버).',
                   '3-Tier 아키텍처: 표현 계층–비즈니스 로직 계층–데이터 계층으로 분리된 3계층 구조(웹 시스템의 표준 구조).'
                 )
               ),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '인터페이스 대상 식별 시, 어떤 계층 간에 데이터가 오가는지(프론트–백엔드–DB)를 고려하면 '
                 '어디에 인터페이스를 둘지 더 명확해집니다.'
               )
             )
           ),

           -- 1.4.2.3 시스템 구성 요소 (I-P-O-C-F)와 인터페이스 대상
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.4.2.3',
             'title', '시스템 구성 요소 (I-P-O-C-F)와 인터페이스 대상',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','시스템 기본 구성 요소 (I-P-O-C-F)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','시스템 구성 요소 [입·처·출·제·피]',
                 'headers', JSON_ARRAY('요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('입력(Input)','시스템으로 들어오는 데이터 또는 신호'),
                   JSON_ARRAY('처리(Process)','입력 데이터를 가공·연산·변환하는 기능'),
                   JSON_ARRAY('출력(Output)','외부로 제공되는 결과 데이터·보고서 등'),
                   JSON_ARRAY('제어(Control)','처리 흐름·에러·상태를 제어하는 기능'),
                   JSON_ARRAY('피드백(Feedback)','출력·상태 정보를 다시 입력으로 돌려 성능·품질을 조정하는 기능')
                 )
               ),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '인터페이스 대상 식별 시, 어떤 입력/출력/처리 지점에서 다른 시스템과 데이터가 오가는지를 기준으로 '
                 '연계 포인트를 찾는 것이 중요합니다.'
               )
             )
           ),

           -- 1.4.2.4 인터페이스 시스템 구성과 데이터 표준 (공개종)
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '1.4.2.4',
             'title', '인터페이스 시스템 구성과 데이터 표준 (공개종)',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','인터페이스 시스템(Interface System) 구성'),
               JSON_OBJECT(
                 'type','table',
                 'caption','인터페이스 시스템 구성 요소',
                 'headers', JSON_ARRAY('구성 요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '송신 시스템',
                     '연계 프로그램에서 생성한 데이터를 전송 형식에 맞게 인터페이스 테이블·파일(XML, CSV, TEXT 등)로 변환 후 송신하는 시스템.'
                   ),
                   JSON_ARRAY(
                     '수신 시스템',
                     '수신한 인터페이스 테이블·파일을 자체 연계 프로그램이 처리 가능한 형식으로 변환해 반영하는 시스템.'
                   ),
                   JSON_ARRAY(
                     '연계 서버 / 중계 서버',
                     '송·수신 시스템 사이에서 데이터를 중계하고, 송수신 현황·실패 내역 등을 모니터링하는 역할.'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','인터페이스 데이터 표준 (공·개·종)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '인터페이스 데이터 공통부: 모든 인터페이스에 공통으로 포함되는 헤더(인터페이스 ID, 전송 일시, 송·수신 시스템 코드 등).',
                   '인터페이스 데이터 개별부: 각 인터페이스별 비즈니스 데이터(주문 정보, 회원 정보 등).',
                   '인터페이스 데이터 종료부: 전송 건수, Checksum, EOF 플래그 등 전송 종료·검증 정보.'
                 )
               ),
               JSON_OBJECT(
                 'type','callout',
                 'title','시험 포인트',
                 'body',
                 '“공개종” = 공통부/개별부/종료부 구조를 묻는 문제와, 송신/수신/연계 서버 역할을 묻는 문제에 자주 등장합니다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);
