-- ============================
-- 2.5 인터페이스 구현
-- 2.5.1 인터페이스 설계 확인
-- topic_id = @topic_iface_design
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 12501,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              2.5.1-1 내·외부 인터페이스 기술 표준 (EAI/ESB)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.5.1',
             'title', '내·외부 인터페이스 기술 표준 – EAI와 ESB',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','EAI(Enterprise Application Integration)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'EAI는 기업 내 다양한 플랫폼·애플리케이션을 통합하여, 비즈니스 프로세스 중심으로 '
                 '여러 애플리케이션 간 네트워크를 통합 관리하기 위한 기술이다. '
                 '대상 시스템에 어댑터를 배포해 서로 다른 시스템을 하나의 통합 환경처럼 사용하게 한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','EAI 특징 요약',
                 'headers', JSON_ARRAY('키워드','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('비즈니스 프로세스 중심',
                              '업무 프로세스 관점에서 애플리케이션을 통합 관리'),
                   JSON_ARRAY('어댑터(Adapter)',
                              '각 시스템에 비표준 어댑터를 배포하여 연동'),
                   JSON_ARRAY('Hub & Spoke + Message Bus',
                              '허브 앤 스포크와 메시지 버스의 혼합 형태로 구현 가능'),
                   JSON_ARRAY('데이터 병목 최소화',
                              '통합 아키텍처로 데이터 흐름을 최적화')
                 )
               ),

               JSON_OBJECT('type','heading','text','EAI 구축 유형 (포허 메하)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','EAI 구축 4유형',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('포인트 투 포인트(Point-to-Point)',
                              '애플리케이션을 1:1로 직접 연결. 구조 단순하지만 변경·재사용이 어려움.'),
                   JSON_ARRAY('허브 앤 스포크(Hub & Spoke)',
                              '중앙 허브를 통해 데이터 전송. 확장·유지보수는 용이하나 허브 장애 시 전체 영향.'),
                   JSON_ARRAY('메시지 버스(Message Bus)',
                              '중앙 메시지 버스(ESB) 미들웨어를 통해 연동. 확장성이 높고 대용량 처리가 가능.'),
                   JSON_ARRAY('하이브리드(Hybrid)',
                              '그룹 내는 Hub & Spoke, 그룹 간은 Message Bus를 사용하는 혼합 구조.')
                 )
               ),

               JSON_OBJECT('type','heading','text','ESB(Enterprise Service Bus)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'ESB는 서비스 지향 통합(SOA) 기반으로, 서로 다른 플랫폼·애플리케이션 간을 느슨한 결합(loose coupling)으로 '
                 '연계하기 위한 버스 기반 통합 기술이다. 개방형 표준인 웹 서비스(예: SOAP, REST)를 활용하며, '
                 '프로토콜·데이터 포맷 변환을 통해 이기종 시스템 간 호환을 지원한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','EAI vs ESB 관점 포인트',
                 'headers', JSON_ARRAY('항목','EAI','ESB'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('통합 관점',
                              '애플리케이션 통합 중심',
                              '서비스(기능) 중심 통합'),
                   JSON_ARRAY('연계 방식',
                              '허브·포인트투포인트·메시지버스 혼합',
                              '버스 기반, 느슨한 결합'),
                   JSON_ARRAY('표준 활용',
                              '제품·솔루션 종속적인 경우 다수',
                              '웹 서비스 등 개방형 표준 기반')
                 )
               ),

               JSON_OBJECT('type','heading','text','EAI/ESB 세부 기술·토폴로지 (허어브메)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','EAI/ESB 구성 요소 키워드',
                 'headers', JSON_ARRAY('키워드','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('허브 앤 스포크(Hub & Spoke)',
                              '중앙 허브를 통한 집중형 연계 구조'),
                   JSON_ARRAY('어댑터(Adapter)',
                              '각 애플리케이션을 EAI/ESB에 연결하는 인터페이스 모듈'),
                   JSON_ARRAY('브로커(Broker)',
                              '메시지 라우팅·변환·필터링 등을 수행하는 중개 컴포넌트'),
                   JSON_ARRAY('메시지 큐(Message Queue)',
                              '비동기 메시지 전송을 위한 큐잉 시스템')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.5.1-2 인터페이스 보안 설계
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '2.5.1',
             'title', '인터페이스 보안 설계 원칙',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','인터페이스 보안 구현 요소'),
               JSON_OBJECT(
                 'type','table',
                 'caption','주요 보안 설계 포인트',
                 'headers', JSON_ARRAY('구분','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('시큐어 코딩 가이드 적용',
                              '입력값 검증, 오류 처리, 캡슐화 등 코드 레벨에서 보안 취약점 방지'),
                   JSON_ARRAY('데이터베이스 보안 적용',
                              '접근 통제·권한 관리·암호화를 통해 DB 기밀성 유지'),
                   JSON_ARRAY('중요 데이터 암호화 전송',
                              'IPSec, SSL/TLS, S-HTTP 등 보안 채널로 암·복호화하여 전송')
                 )
               ),

               JSON_OBJECT('type','heading','text','시큐어 코딩 가이드 적용 대상 (입보시 에코캡아)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','시큐어 코딩 7대 항목',
                 'headers', JSON_ARRAY('항목','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('입력 데이터 검증 및 표현',
                              '모든 외부 입력값에 대해 검증·정규화 수행'),
                   JSON_ARRAY('보안 기능',
                              '인증·인가·암호화·세션 관리 구현 시 보안 기준 준수'),
                   JSON_ARRAY('시간 및 상태',
                              '동시성·타이밍 관련 결함 방지'),
                   JSON_ARRAY('에러 처리',
                              '에러 정보 노출 최소화, 예외 상황 안전 처리'),
                   JSON_ARRAY('코드 오류',
                              '개발 언어·플랫폼별 보안 취약 패턴 제거'),
                   JSON_ARRAY('캡슐화',
                              '중요 데이터·로직을 은닉하고 최소 권한 원칙 적용'),
                   JSON_ARRAY('API 오용',
                              '안전한 API 사용, deprecated·위험 API 사용 자제')
                 )
               ),

               JSON_OBJECT('type','heading','text','DB 암호화 알고리즘 유형 (대비해)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','데이터베이스 암호화 알고리즘',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('대칭 키 암호화',
                              '하나의 키로 암·복호화. 속도가 빠르나 키 관리가 중요.'),
                   JSON_ARRAY('비대칭 키 암호화',
                              '공개키·개인키 쌍 사용. 키 분배는 유리하나 속도가 느림.'),
                   JSON_ARRAY('해시 암호화',
                              '단방향 함수로 암호·무결성 검증에 사용. 복호화 불가.')
                 )
               ),

               JSON_OBJECT('type','heading','text','DB 암호화 기법 유형 (애플하)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','데이터베이스 암호화 적용 방식',
                 'headers', JSON_ARRAY('방식','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('API 방식',
                              '애플리케이션에서 암·복호화 API를 호출하여 데이터 처리'),
                   JSON_ARRAY('Plug-in 방식',
                              'DB 내부 모듈(플러그인)로 암·복호화를 수행'),
                   JSON_ARRAY('Hybrid 방식',
                              'API 방식과 Plug-in 방식을 혼합해 성능·보안을 절충')
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);



-- ============================
-- 2.5.2 인터페이스 기능 구현
-- topic_id = @topic_iface_impl
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 12502,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              2.5.2-1 인터페이스 구현 기술(JSON/REST/AJAX)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.5.2',
             'title', '인터페이스 구현 기술 – JSON / REST / AJAX',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','JSON(JavaScript Object Notation)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'JSON은 속성-값 쌍(키-값 쌍) 구조로 데이터를 표현하는 경량 텍스트 포맷이다. '
                 '비동기 브라우저/서버 통신(AJAX)에서 가장 널리 사용되며, 인간이 읽기 쉽고 기계가 파싱하기 쉬운 구조를 제공한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','JSON 특징',
                 'headers', JSON_ARRAY('항목','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('텍스트 기반',
                              'UTF-8 텍스트로 표현되어 언어·플랫폼에 독립적'),
                   JSON_ARRAY('키-값 구조',
                              '중첩 객체·배열을 사용해 복잡한 구조도 간단히 표현 가능'),
                   JSON_ARRAY('AJAX 핵심 포맷',
                              '브라우저-서버 간 비동기 통신에서 표준처럼 사용')
                 )
               ),

               JSON_OBJECT('type','heading','text','REST(Representational State Transfer)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'REST는 웹과 같은 분산 하이퍼미디어 환경에서 자원의 상태를 HTTP 메서드로 주고받는 아키텍처 스타일이다. '
                 'URI로 자원(Resource)을 식별하고, GET/POST/PUT/DELETE 등 표준 HTTP 메서드로 자원을 조회·생성·수정·삭제한다.'
               ),

               JSON_OBJECT(
                 'type','table',
                 'caption','REST 기본 개념',
                 'headers', JSON_ARRAY('구성요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('자원(Resource)',
                              'URI로 식별되는 대상(예: /api/users/1)'),
                   JSON_ARRAY('표현(Representation)',
                              'JSON, XML 등 자원을 표현하는 데이터 포맷'),
                   JSON_ARRAY('행위(Method)',
                              'HTTP 메서드(GET/POST/PUT/DELETE 등)로 자원에 대한 작업 정의')
                 )
               ),

               JSON_OBJECT('type','heading','text','AJAX(Asynchronous JavaScript and XML)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'AJAX는 JavaScript를 이용해 클라이언트와 서버 간 데이터를 비동기적으로 주고받는 기술이다. '
                 '초기에는 XML을 많이 사용했으나, 현재는 JSON을 사용하는 경우가 대부분이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','AJAX 특징',
                 'headers', JSON_ARRAY('특징','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('비동기 통신',
                              '페이지 전체를 새로고침하지 않고 필요한 데이터만 주고받음'),
                   JSON_ARRAY('부분 갱신',
                              'UI 일부만 갱신하여 사용자 경험(UX) 향상'),
                   JSON_ARRAY('표준 기술 조합',
                              'JavaScript + XMLHttpRequest(Fetch) + JSON/XML 조합으로 구현')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.5.2-2 인터페이스 구현 검증 도구 (엑스피 엔셀웨)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '2.5.2',
             'title', '인터페이스 구현 검증 도구',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','인터페이스 구현 검증 도구 (엑스피 엔셀웨)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','대표 테스트 프레임워크',
                 'headers', JSON_ARRAY('도구','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('xUnit',
                              '언어별 단위 테스트 프레임워크 군. Java(JUnit), C++(CppUnit), .NET(NUnit) 등 지원.'),
                   JSON_ARRAY('STAF',
                              '서비스 호출, 컴포넌트 재사용 등 다양한 환경을 지원하는 테스트 프레임워크.'),
                   JSON_ARRAY('FitNesse',
                              '웹 기반 테스트 케이스 설계·실행·결과 확인을 지원하는 프레임워크.'),
                   JSON_ARRAY('NTAF',
                              'FitNesse와 STAF 장점을 결합한 테스트 자동화 프레임워크.'),
                   JSON_ARRAY('Selenium',
                              '다양한 브라우저·언어를 지원하는 웹 애플리케이션 UI 테스트 도구.'),
                   JSON_ARRAY('watir',
                              'Ruby 기반 웹 애플리케이션 테스트 프레임워크.')
                 )
               ),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '이들 도구를 활용하면 인터페이스 요청·응답 형식, 응답 시간, 예외 처리 등 '
                 '인터페이스 기능 구현이 요구사항을 만족하는지 자동·반자동으로 검증할 수 있다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);
