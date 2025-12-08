-- =========================================
-- 4.1.1 개발환경 구축 (concept 보강)
-- topic_id = 14101
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 14101,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              4.1.1.1 개발환경 구축 개요·절차
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '4.1.1.1',
             'title', '개발환경 구축 개요와 절차',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','개발환경 구축의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '개발환경 구축은 응용 소프트웨어의 개발 편의성과 개발 성능을 향상시키기 위해, '
                 '하드웨어·소프트웨어 개발환경을 종합적으로 구성하는 과정이다.'
               ),
               JSON_OBJECT('type','heading','text','개발환경 구축 절차'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '프로젝트 요구사항 분석: 성능, 보안, 운영 환경 등 비기능 요구사항 포함',
                   '필요 도구 설계: 빌드·테스트·배포·형상관리 도구 선정 기준 정의',
                   '개발 언어 선정: 팀 역량, 프레임워크 지원, 생태계 등을 고려',
                   '구현 도구 선정: IDE, 플러그인, 디버깅/프로파일링 도구 확정',
                   '빌드·테스트 도구 선정: CI 연계, 자동화 수준, 리포트 기능 고려'
                 )
               )
             )
           ),

           /* -------------------------------------
              4.1.1.2 개발환경 구축 도구 분류
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '4.1.1.2',
             'title', '개발환경 구축 도구 분류',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','개발환경 구축 도구의 분류 (빌·구·테·형)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','개발환경 구축 도구 분류',
                 'headers', JSON_ARRAY('분류','설명','예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('빌드 도구',
                              '소스 코드를 컴파일·패키징하고, 의존성을 관리하는 도구',
                              'Maven, Gradle, Ant 등'),
                   JSON_ARRAY('구현 도구',
                              '코드 작성·디버깅·리팩토링을 지원하는 개발 도구',
                              'IntelliJ IDEA, Eclipse, VS Code 등'),
                   JSON_ARRAY('테스트 도구',
                              '단위 테스트·통합 테스트·성능 테스트 자동화를 지원하는 도구',
                              'JUnit, Jest, JMeter, Selenium 등'),
                   JSON_ARRAY('형상 관리 도구',
                              '소스 버전 관리와 협업을 지원하는 도구',
                              'Git, Subversion, GitHub, GitLab 등')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.1.1.3 하드웨어·소프트웨어 개발환경
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '4.1.1.3',
             'title', '하드웨어·소프트웨어 개발환경 구성',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','하드웨어 개발환경 구성요소'),
               JSON_OBJECT(
                 'type','table',
                 'caption','하드웨어 개발환경',
                 'headers', JSON_ARRAY('구분','구성요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('서버 하드웨어',
                              '웹 서버(Web Server)',
                              '정적 자원 제공, 클라이언트의 HTTP 요청 처리'),
                   JSON_ARRAY('서버 하드웨어',
                              '웹 애플리케이션 서버(WAS)',
                              '비즈니스 로직 실행, 트랜잭션·세션 관리'),
                   JSON_ARRAY('서버 하드웨어',
                              'DB 서버(DB Server)',
                              '데이터 저장·조회·트랜잭션 처리 담당'),
                   JSON_ARRAY('서버 하드웨어',
                              '파일 서버(File Server)',
                              '파일 공유·배포를 위한 전용 서버'),
                   JSON_ARRAY('클라이언트 하드웨어',
                              '클라이언트 프로그램 / 웹 브라우저 / 모바일 앱·웹',
                              '개발자가 직접 테스트·디버깅에 사용하는 실행 환경')
                 )
               ),
               JSON_OBJECT('type','heading','text','소프트웨어 개발환경 구성요소'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '운영체제(OS): 서버·클라이언트 OS 선택(Windows, Linux 등)',
                   '미들웨어: WAS, 메시지 브로커, API 게이트웨이 등',
                   'DBMS: RDBMS/NoSQL 선택, 버전·라이선스 정책 고려'
                 )
               )
             )
           ),

           /* -------------------------------------
              4.1.1.4 서버 개발 프레임워크 개요
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '4.1.1.4',
             'title', '서버 개발 프레임워크와 전자정부 프레임워크',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','프레임워크(Framework)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '프레임워크는 공통적으로 재사용되는 설계·구현을 패턴화하여, 협업화된 클래스와 라이브러리 형태로 제공하는 소프트웨어 개발 틀이다.'
               ),
               JSON_OBJECT('type','heading','text','프레임워크의 특징 (모·재·확·역)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '모듈화(Modularity): 기능을 모듈 단위로 분리해 관리·이해를 쉽게 한다.',
                   '재사용성(Reusability): 공통 기능을 재사용해 개발 생산성을 높인다.',
                   '확장성(Extensibility): 변경·확장에 유리한 구조 제공',
                   '제어의 역행(IoC): 객체 생성·제어를 프레임워크가 담당'
                 )
               ),
               JSON_OBJECT('type','heading','text','전자정부 프레임워크 개요'),
               JSON_OBJECT('type','paragraph','text',
                 '전자정부 프레임워크는 공공 정보화 사업에서 표준화된 개발환경을 제공하기 위해 만든 오픈소스 기반 범용 프레임워크로, '
                 '개발환경·실행환경·운영환경·관리환경과 공통 컴포넌트로 구성된다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);


-- =========================================
-- 4.1.2 서버 프로그램 구현 (concept 보강)
-- topic_id = 14102
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 14102,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              4.1.2.1 서버 보안 취약성·침투 테스트
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '4.1.2.1',
             'title', '서버 보안 취약성 식별과 침투 테스트',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','보안 취약성 식별의 목적'),
               JSON_OBJECT('type','paragraph','text',
                 '서버 프로그램 구현 시 보안 취약성 식별은 응용 프로그램의 취약점·위협 요소·허점을 사전에 찾아내어, '
                 '실제 공격 발생 전에 적절한 방어책을 마련하기 위한 과정이다.'
               ),
               JSON_OBJECT('type','heading','text','침투 테스트(Penetration Test) 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '침투 테스트는 전문 보안 인력이 공격자 관점에서 시스템을 모의 공격하여 취약점을 찾는 기법이다. '
                 '실제 공격보다 먼저 취약점을 발견해 패치·보완하는 것이 목적이다.'
               ),
               JSON_OBJECT('type','heading','text','침투 테스트 절차'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '정찰(Reconnaissance): 대상 시스템 정보 수집',
                   '탐색(Scanning): 포트·서비스·취약점 스캔',
                   '접근 권한 취득(Exploitation): 취약점 악용해 권한 확보',
                   '액세스 유지(Maintaining Access): 백도어 설치 등 지속적 접근 확보',
                   '추적 방지(Covering Tracks): 로그 삭제 등 흔적 제거'
                 )
               )
             )
           ),

           /* -------------------------------------
              4.1.2.2 주요 취약성 공격 기법
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '4.1.2.2',
             'title', '취약성에 따른 주요 공격 기법',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','주요 공격 기법 정리'),
               JSON_OBJECT(
                 'type','table',
                 'caption','서버·웹 취약성 공격 기법',
                 'headers', JSON_ARRAY('공격 기법','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('서비스 거부(DDoS/DoS)',
                              '무의미한 요청을 반복 전송해 시스템 자원을 소모시키고, 정상 사용자의 서비스 이용을 방해하는 공격'),
                   JSON_ARRAY('버퍼 오버플로우',
                              '입력 데이터가 버퍼 크기를 초과하도록 유도해, 리턴 주소를 덮어쓰고 임의 코드를 실행시키는 시스템 해킹 기법'),
                   JSON_ARRAY('SQL 삽입(SQL Injection)',
                              '입력 값에 악의적인 SQL을 삽입해 인증 우회·데이터 열람·변조를 수행하는 공격'),
                   JSON_ARRAY('크로스 사이트 스크립팅(XSS)',
                              '게시판·댓글 등에 악성 스크립트를 삽입해, 피해자 브라우저에서 쿠키·개인정보 탈취 또는 악성 행위 실행'),
                   JSON_ARRAY('디렉터리 접근 공격',
                              'HTTP 요청을 조작해 접근이 제한된 디렉터리·파일에 직접 접근하는 공격'),
                   JSON_ARRAY('FTP 바운스 공격',
                              'FTP 서버가 데이터 채널 목적지를 검증하지 않는 취약점을 이용해 제3의 시스템을 공격에 악용하는 기법')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.1.2.3 서버 API 개념과 유형
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '4.1.2.3',
             'title', 'API(Application Programming Interface) 개념',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','API의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 'API는 응용 프로그램이 운영체제나 다른 소프트웨어가 제공하는 기능을 사용하도록 정의된 인터페이스이다. '
                 '서버 프로그램 구현에서는 외부 시스템·클라이언트와 통신하는 표준 창구 역할을 한다.'
               ),
               JSON_OBJECT('type','heading','text','API 유형'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '오픈 API(Open API): 누구나 사용할 수 있도록 공개된 API',
                   '비공개 API(Closed API): 내부 시스템에서만 사용하는 비공개 인터페이스'
                 )
               ),
               JSON_OBJECT('type','heading','text','API 종류 예시'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '윈도우즈 API: 운영체제 수준의 기능(파일, 프로세스 등) 제공',
                   '웹 API: HTTP 기반 REST/JSON API 등',
                   '오픈 API: 공공데이터 포털, 소셜 로그인 API 등',
                   '자바 API: Java 표준 라이브러리 및 프레임워크 제공 API'
                 )
               )
             )
           ),

           /* -------------------------------------
              4.1.2.4 서버 프로그램 보안 고려사항
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '4.1.2.4',
             'title', '서버 프로그램 구현 시 보안 고려사항',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','보안 취약점 예방의 기본 원칙'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '입력 값 검증: 모든 외부 입력에 대해 화이트리스트·길이·포맷 검증 수행',
                   '권한 검증: 기능별 접근 제어와 최소 권한 원칙 적용',
                   '에러 처리: 상세한 내부 정보가 노출되지 않도록 예외 메시지 관리',
                   '로그·모니터링: 공격 징후를 추적할 수 있도록 감사 로그·모니터링 구성',
                   '업데이트: 라이브러리·프레임워크 보안 패치 주기적 적용'
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);


-- =========================================
-- 4.1.3 배치 프로그램 구현 (concept 보강)
-- topic_id = 14103
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 14103,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              4.1.3.1 배치 프로그램 개념·필수 요소
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '4.1.3.1',
             'title', '배치 프로그램 개념과 필수 요소',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','배치 프로그램(Batch Program)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '배치 프로그램은 사용자와의 상호작용 없이, 일련의 작업들을 작업 단위로 묶어 '
                 '정기적으로 반복 수행하거나 정해진 규칙에 따라 일괄 처리하는 프로그램이다.'
               ),
               JSON_OBJECT('type','heading','text','배치 프로그램의 필수 요소'),
               JSON_OBJECT(
                 'type','table',
                 'caption','배치 프로그램 필수 요소',
                 'headers', JSON_ARRAY('요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('자동화',
                              '심각한 오류 상황 외에는 사용자의 개입 없이 자동으로 수행되어야 한다.'),
                   JSON_ARRAY('안정성',
                              '문제 발생 시 언제·어디서 문제가 발생했는지 추적할 수 있어야 한다.'),
                   JSON_ARRAY('대용량 데이터 처리',
                              '대용량 데이터를 효율적으로 처리할 수 있어야 한다.'),
                   JSON_ARRAY('견고성',
                              '유효하지 않은 데이터가 있어도 비정상 종료 없이 적절히 처리해야 한다.'),
                   JSON_ARRAY('성능',
                              '주어진 시간 내 처리를 완료하고, 다른 애플리케이션을 방해하지 않아야 한다.')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.1.3.2 배치 유형·도구
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '4.1.3.2',
             'title', '배치 프로그램 유형과 도구',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','배치 프로그램 유형 (이·온·정)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '이벤트 배치: 특정 이벤트(파일 도착, 메시지 수신 등)에 의해 트리거되는 배치',
                   '온디맨드 배치: 사용자의 요청이나 관리자의 명령으로 필요 시 수행하는 배치',
                   '정기 배치: 일정 주기(매일/매주/매월 등)로 스케줄링되어 수행되는 배치'
                 )
               ),
               JSON_OBJECT('type','heading','text','대표적인 배치 프레임워크'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '스프링 배치(Spring Batch): 스프링 기반의 배치 처리 프레임워크로, 잡(Job)·스텝(Step)·리스너 등을 제공',
                   '쿼츠 스케줄러(Quartz Scheduler): 크론 표현식 기반의 정교한 스케줄링을 지원하는 라이브러리'
                 )
               )
             )
           ),

           /* -------------------------------------
              4.1.3.3 공통 모듈·모듈화와 배치
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '4.1.3.3',
             'title', '공통 모듈 구현과 모듈화 원리',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','재사용과 공통 모듈'),
               JSON_OBJECT('type','paragraph','text',
                 '배치 프로그램에서도 공통 로직(로그 처리, 예외 처리, 공통 조회 쿼리 등)을 모듈화해 재사용성을 높이는 것이 중요하다.'
               ),
               JSON_OBJECT('type','heading','text','모듈화(Modularization)의 원리 (정·분·추·모)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '정보 은닉(Information Hiding): 모듈 내부 구현을 숨기고 인터페이스만 공개',
                   '분할과 정복(Divide & Conquer): 복잡한 문제를 여러 작은 문제로 분할해 해결',
                   '데이터 추상화(Data Abstraction): 데이터와 연산을 하나의 추상 개체로 표현',
                   '모듈 독립성(Module Independence): 결합도는 낮추고 응집도는 높여 독립성 확보'
                 )
               )
             )
           ),

           /* -------------------------------------
              4.1.3.4 배치 설계 시 품질·성능 고려
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '4.1.3.4',
             'title', '배치 설계 시 품질·성능 고려사항',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','배치 설계 시 품질 관점'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '에러·예외 처리: 재시도 정책, 실패 시 롤백 또는 스킵 전략 설계',
                   '로그·모니터링: 실행 이력·처리 건수·에러 건수 기록',
                   '리소스 관리: 동시 실행 배치 간 리소스 경합 최소화'
                 )
               ),
               JSON_OBJECT('type','heading','text','성능 튜닝 포인트'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '대용량 처리 시 배치 단위(commit 단위)·페이징·스트리밍 처리 설계',
                   '인덱스·파티셔닝·SQL 튜닝을 활용한 I/O 감소',
                   '업무 피크 타임을 피한 스케줄링으로 운영 시스템 영향 최소화'
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);