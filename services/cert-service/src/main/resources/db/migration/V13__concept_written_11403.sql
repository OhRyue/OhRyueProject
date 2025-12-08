-- ============================
-- 1.4.3 인터페이스 상세 설계
-- topic_id = 11403
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 11403,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           -- 1.4.3.1 내·외부 송·수신 연계 방식 및 연계 기술
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.4.3.1',
             'title', '내·외부 송·수신 연계 방식 및 연계 기술',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','내·외부 송·수신 연계 방식'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '직접 연계 방식: 두 시스템이 직접 연결(DB Link, API 호출 등)되어 데이터를 주고받는 방식으로, 구조는 단순하지만 결합도가 높아집니다.',
                   '간접 연계 방식: 연계 서버·메시지 큐·파일 등 중간 매개체를 통해 데이터를 교환하는 방식으로, 느슨한 결합에 유리하지만 구조가 복잡할 수 있습니다.'
                 )
               ),
               JSON_OBJECT('type','heading','text','내·외부 송·수신 연계 기술'),
               JSON_OBJECT(
                 'type','table',
                 'caption','대표 연계 기술',
                 'headers', JSON_ARRAY('연계 기술','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('DB Link','DB에서 제공하는 Link 객체를 이용해 원격 DB에 직접 접근하는 방식'),
                   JSON_ARRAY('DB Connection','애플리케이션에서 JDBC 등으로 직접 DB에 연결해 데이터를 입·출력하는 방식'),
                   JSON_ARRAY('API / Open API','HTTP 기반 API로 데이터를 제공/호출하는 방식 (REST, Open API 등)'),
                   JSON_ARRAY('JDBC','자바 환경에서 표준 DB 접속을 제공하는 인터페이스'),
                   JSON_ARRAY('Hyper Link','URL 링크를 통해 특정 화면·자원으로 바로 이동하는 단순 연계 수단'),
                   JSON_ARRAY('Socket','서버가 소켓과 포트를 열고, 클라이언트와 TCP/IP로 실시간 통신하는 방식'),
                   JSON_ARRAY('연계 솔루션(EAI 등)','다양한 프로토콜·포맷을 통합하는 중계 솔루션 기반 연계'),
                   JSON_ARRAY('Web Service','WSDL·UDDI·SOAP 등을 이용해 표준화된 방식으로 서비스를 호출·연계')
                 )
               )
             )
           ),

           -- 1.4.3.2 통신 유형 (실시간/배치, 동기/비동기, 자연 처리)
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.4.3.2',
             'title', '통신 유형 (실시간/배치, 동기/비동기, 자연 처리)',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','내·외부 송·수신 통신 유형'),
               JSON_OBJECT(
                 'type','table',
                 'caption','대표 통신 유형',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('실시간 단방향','데이터를 이용하고자 하는 시스템에서 거래를 요청하는 방식(한 방향 위주).'),
                   JSON_ARRAY('실시간 양방향','시스템 간에 거래를 상호 요청·응답하는 구조. 양쪽 모두 요청 주체가 될 수 있습니다.'),
                   JSON_ARRAY('동기(Sync) 통신','요청 후 응답이 올 때까지 대기하는 Request-Reply 방식. 응답 시간에 민감하지만 구조는 단순합니다.'),
                   JSON_ARRAY('비동기(Async) 통신','요청과 응답 처리가 분리된 구조로, 큐·MOM을 활용해 지연·버퍼링이 가능하고 느슨한 결합에 유리합니다.'),
                   JSON_ARRAY('자연 처리(Deferred)','순차 처리·지연 처리가 필요한 업무에서 일정 기준에 따라 뒤늦게 처리하는 방식.'),
                   JSON_ARRAY('배치(DB/File 거래)','정해진 시간(예: 매일 00:00)에 일괄로 통신·처리하는 방식으로, 대용량 정산·통계에 활용됩니다.')
                 )
               )
             )
           ),

           -- 1.4.3.3 데이터 명세화와 개체(Entity) 정의서 (논엔 엔주)
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.4.3.3',
             'title', '데이터 명세화와 개체(Entity) 정의서 (논엔 엔주)',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','데이터 명세화'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '인터페이스 요구사항 분석 과정에서 식별한 연계 정보 그룹에 대해, 테이블 정의서·파일 레이아웃·코드 정의서 등을 분석하여 '
                 '실제 전송 데이터 명세를 만드는 작업입니다.'
               ),
               JSON_OBJECT('type','heading','text','개체(Entity) 정의서 명세 지침 (논엔 엔주)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','개체 정의서 기본 항목',
                 'headers', JSON_ARRAY('항목','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('논리 DB명','데이터가 속하는 논리 데이터베이스 이름'),
                   JSON_ARRAY('엔터티명','개체(테이블) 이름'),
                   JSON_ARRAY('엔터티 설명','해당 개체가 어떤 정보를 담는지에 대한 설명'),
                   JSON_ARRAY('주 식별자','개체의 레코드를 고유하게 구분하는 Primary Key 정의')
                 )
               )
             )
           ),

           -- 1.4.3.4 오류 처리 방안과 인터페이스 정의서 항목 (연송데수 / 인최 크시데)
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '1.4.3.4',
             'title', '오류 처리 방안과 인터페이스 정의서 항목 (연송데수 / 인최 크시데)',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','인터페이스 오류 유형 (연·송·데·수)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','인터페이스 오류 유형',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('연계 서버 오류','연계 서버 장애, 중계·모니터링 기능 실패'),
                   JSON_ARRAY('송신 시스템 연계 프로그램 오류','송신 측 변환/전송 로직 오류'),
                   JSON_ARRAY('연계 데이터 오류','필수 값 누락, 포맷 불일치, 코드 매핑 오류 등'),
                   JSON_ARRAY('수신 시스템 연계 프로그램 오류','수신 측 파싱·적재 로직 오류')
                 )
               ),
               JSON_OBJECT('type','heading','text','오류 처리 절차 예시'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '① 인터페이스 오류 발생 시 연계 프로그램에서 오류 로그를 남기도록 설계합니다.',
                   '② 오류 발생 → 로그 확인 → 원인 분석 → 해결 방안 수립 순서로 처리합니다.',
                   '③ 데이터 오류는 데이터 보정 후 재전송, 접속 오류는 네트워크·시스템 복구 후 재전송합니다.'
                 )
               ),
               JSON_OBJECT('type','heading','text','인터페이스 정의서 주요 항목 (인최 크시데)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','인터페이스 정의서 핵심 항목',
                 'headers', JSON_ARRAY('항목','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('인터페이스 ID','각 인터페이스를 구분하는 고유 ID'),
                   JSON_ARRAY('최대 처리 횟수','단위 시간당 처리 가능한 최대 건수·호출 빈도'),
                   JSON_ARRAY('데이터 크기(평균/최대)','전송 데이터의 평균/최대 크기 정보'),
                   JSON_ARRAY('시스템 정보','송·수신 시스템 명, 담당 부서 등'),
                   JSON_ARRAY('데이터 정보','전송 필드 목록, 데이터 타입·길이·코드값 등 상세 명세')
                 )
               )
             )
           ),

           -- 1.4.3.5 미들웨어 솔루션과 FEP
           JSON_OBJECT(
             'orderNo', 5,
             'subCode', '1.4.3.5',
             'title', '미들웨어 솔루션 유형과 FEP',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','미들웨어(Middleware)의 개념'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '미들웨어는 클라이언트와 서버 간의 통신을 담당하는 시스템 소프트웨어로, '
                 '분산 컴퓨팅 환경에서 서로 다른 하드웨어·OS·프로토콜·통신환경을 연결하여 응용 프로그램과 운영환경 간 원활한 통신을 지원합니다.'
               ),
               JSON_OBJECT('type','heading','text','미들웨어 솔루션 유형'),
               JSON_OBJECT(
                 'type','table',
                 'caption','주요 미들웨어 유형',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('DB 미들웨어','DB 벤더가 제공하는 클라이언트/드라이버로 원격 DB와 연결하기 위한 미들웨어'),
                   JSON_ARRAY('RPC (Remote Procedure Call)','원격 프로시저를 로컬 함수처럼 호출할 수 있게 해주는 미들웨어'),
                   JSON_ARRAY('MOM (Message Oriented Middleware)','메시지 기반 비동기 메시지를 전달하는 미들웨어'),
                   JSON_ARRAY('TP-Monitor','대규모 온라인 트랜잭션을 처리·감시하는 미들웨어'),
                   JSON_ARRAY('WAS (Web Application Server)','동적 웹 콘텐츠를 처리하는 서버/미들웨어'),
                   JSON_ARRAY('ORB (Object Request Broker)','CORBA 기반 객체지향 미들웨어로, 분산 객체 간 호출을 중개')
                 )
               ),
               JSON_OBJECT('type','heading','text','FEP(Front End Processor)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'FEP는 입력되는 데이터를 본 프로세서가 처리하기 전에 미리 전처리하여, '
                 '본 시스템의 부담을 줄이고 전체 처리 성능을 높이는 전용 프로그램 또는 하드웨어입니다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);
