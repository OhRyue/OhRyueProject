-- =========================================
-- 5.3.1 소프트웨어 개발 보안 설계 (concept)
-- topic_id = 15301
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 15301,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------
              5.3.1.1 SW 개발 보안 기본 개념
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '5.3.1.1',
             'title', 'SW 개발 보안 기본 개념 (기밀성·무결성·가용성)',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','SW 개발 보안의 3대 요소 (기·무·가)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','SW 개발 보안 3대 요소',
                 'headers', JSON_ARRAY('요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '기밀성(Confidentiality)',
                     '인가되지 않은 개인이나 시스템이 정보에 접근하여 내용을 노출하거나 유출하지 못하도록 보호하는 특성'
                   ),
                   JSON_ARRAY(
                     '무결성(Integrity)',
                     '정당한 절차를 거치지 않고서는 데이터가 변경될 수 없도록 하여, 데이터의 정확성과 완전성을 보장하는 특성'
                   ),
                   JSON_ARRAY(
                     '가용성(Availability)',
                     '권한을 가진 사용자와 애플리케이션이 필요한 시점에 서비스를 지속적으로 사용할 수 있도록 보장하는 특성'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','SW 개발 보안 관련 기본 용어'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '자산(Asset): 보호 대상이 되는 정보, 시스템, 서비스',
                   '위협(Threat): 자산에 손실을 가져올 수 있는 잠재적 사건이나 행위',
                   '취약점(Vulnerability): 위협이 악용할 수 있는 시스템 혹은 프로세스의 약점',
                   '위험(Risk): 특정 위협이 취약점을 악용하여 발생할 수 있는 피해 가능성과 그 영향도'
                 )
               )
             )
           ),

           /* -------------------------------
              5.3.1.2 Secure SDLC 개요
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '5.3.1.2',
             'title', 'Secure SDLC 개요',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','Secure SDLC의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 'Secure SDLC는 요구사항 분석부터 설계, 구현, 테스트, 유지보수에 이르는 전체 소프트웨어 개발 생명주기 전 단계에 '
                 '보안 활동을 통합하여 보안 취약점을 사전에 예방하는 개발 방법론이다.'
               ),
               JSON_OBJECT('type','heading','text','Secure SDLC 단계 예시'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '계획 단계: 보안 목표와 범위 정의, 보안 정책 및 기준 수립',
                   '분석 단계: 보안 요구사항 도출, 위협 분석과 위험 평가',
                   '설계 단계: 보안 아키텍처 설계, 보안 패턴 적용, 설계 검토',
                   '구현 단계: 안전한 코딩 규칙 적용, 정적 분석 도구 활용, 코드 리뷰 수행',
                   '테스트 단계: 취약점 진단, 동적 분석, 침투 테스트 등 보안 테스트 수행',
                   '배포 및 유지보수 단계: 패치 관리, 로그 모니터링, 보안 사고 대응'
                 )
               )
             )
           ),

           /* -------------------------------
              5.3.1.3 입력 데이터 검증 및 표현
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '5.3.1.3',
             'title', '입력 데이터 검증 및 표현 취약점',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','입력 데이터 검증 및 표현 취약점'),
               JSON_OBJECT(
                 'type','table',
                 'caption','대표 입력 데이터 검증 취약점',
                 'headers', JSON_ARRAY('취약점','설명','대책'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'XSS(Cross Site Script)',
                     '검증되지 않은 외부 입력 데이터가 포함된 웹 페이지를 전송하는 경우, 사용자가 해당 페이지를 열람하면 '
                     '페이지에 포함된 악성 스크립트가 브라우저에서 실행되는 공격. [2020년 4회]',
                     '입력 값에 대한 특수문자 필터링과 이스케이프 처리, 출력 인코딩 적용'
                   ),
                   JSON_ARRAY(
                     'CSRF(Cross-Site Request Forgery)',
                     '사용자의 의지와 무관하게 공격자가 의도한 요청을 특정 웹 사이트에 전송하도록 유도하는 공격',
                     '중요 요청에 대해 CSRF 토큰 사용, Referer 검증, 폼 전송 시 GET 대신 POST 사용'
                   ),
                   JSON_ARRAY(
                     'SQL 삽입(SQL Injection)',
                     '응용 프로그램의 취약한 입력 검증을 이용하여 악의적인 SQL 구문을 삽입하고 실행하여, '
                     '데이터베이스 정보 탈취나 조작을 수행하는 공격',
                     '바인딩 매개변수 방식 사용, 준비된 쿼리 사용, 화이트리스트 기반 입력 검증으로 위험한 문자를 차단'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','XSS 공격 유형'),
               JSON_OBJECT(
                 'type','table',
                 'caption','XSS 주요 유형',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'Stored XSS',
                     '게시글, 댓글 등 서버 저장소에 악성 스크립트를 저장하고, 다른 사용자가 해당 페이지를 열람할 때 '
                     '스크립트가 브라우저에서 실행되는 방식'
                   ),
                   JSON_ARRAY(
                     'Reflected XSS',
                     '공격용 악성 URL을 이메일 등으로 전달하고 사용자가 해당 링크를 클릭하면, '
                     '요청에 포함된 악성 스크립트가 응답 페이지에 반사되어 실행되는 방식'
                   ),
                   JSON_ARRAY(
                     'DOM XSS',
                     'DOM 조작 취약점이 있는 클라이언트 스크립트를 이용하여, 조작된 URL을 통해 브라우저에서 '
                     '직접 DOM 기반 스크립트가 실행되도록 하는 방식'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','에러 처리 취약점 예시'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '취약한 패스워드 요구 조건 설정으로 인한 계정 탈취 위험',
                   '오류 메시지에 시스템 구조나 경로 등의 민감 정보가 노출되는 문제',
                   '예외 상황 발생 시 적절한 대응이 없거나 오류를 그대로 클라이언트에 노출하는 문제',
                   '예외 처리 미흡으로 인해 프로그램이 비정상 종료되거나 상태가 불안정해지는 문제'
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);


-- =========================================
-- 5.3.2 소프트웨어 개발 보안 구현 (concept)
-- topic_id = 15302
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 15302,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------
              5.3.2.1 암호 알고리즘 분류
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '5.3.2.1',
             'title', '암호 알고리즘 개요와 분류',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','암호 알고리즘의 목적'),
               JSON_OBJECT('type','paragraph','text',
                 '암호 알고리즘은 데이터의 기밀성과 무결성을 확보하기 위해 정보를 쉽게 해독할 수 없는 형태로 변환하는 기법이다.'
               ),
               JSON_OBJECT('type','heading','text','암호 알고리즘 분류'),
               JSON_OBJECT(
                 'type','table',
                 'caption','암호 알고리즘 방식 분류',
                 'headers', JSON_ARRAY('방식','세부 유형','대표 기법'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '양방향 암호 방식',
                     '대칭 키 암호 (블록 암호)',
                     'DES, SEED, AES, ARIA, IDEA 등 (블록 단위 암호화) [2020년 3회]'
                   ),
                   JSON_ARRAY(
                     '양방향 암호 방식',
                     '대칭 키 암호 (스트림 암호)',
                     'RC4, LFSR 기반 스트림 암호'
                   ),
                   JSON_ARRAY(
                     '양방향 암호 방식',
                     '비대칭 키 암호 (공개키 암호)',
                     'RSA, ECC, ElGamal, Diffie Hellman 키 교환 등'
                   ),
                   JSON_ARRAY(
                     '일방향 암호 방식',
                     '메시지 인증 코드(MAC)',
                     'HMAC, NMAC'
                   ),
                   JSON_ARRAY(
                     '일방향 암호 방식',
                     '메시지 다이제스트(MDC)',
                     'MD5, SHA 계열 해시 함수'
                   )
                 )
               )
             )
           ),

           /* -------------------------------
              5.3.2.2 주요 암호 알고리즘
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '5.3.2.2',
             'title', '대표 암호 알고리즘 상세',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','대칭 키 암호 알고리즘'),
               JSON_OBJECT(
                 'type','table',
                 'caption','대칭 키 암호 예시',
                 'headers', JSON_ARRAY('기법','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'DES',
                     '블록 크기 64비트, 키 길이 56비트의 블록 암호 알고리즘으로, 페이스텔 구조를 사용한다.'
                   ),
                   JSON_ARRAY(
                     'SEED, AES, ARIA, IDEA',
                     '국내외에서 널리 사용되는 블록 암호 알고리즘들로, 일정 크기의 블록 단위로 데이터를 암호화한다.'
                   ),
                   JSON_ARRAY(
                     'RC4',
                     '로널드 라이베스트가 설계한 스트림 암호 알고리즘으로, 스트림 형태로 평문을 암호문으로 변환한다.'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','비대칭 키 암호 알고리즘'),
               JSON_OBJECT(
                 'type','table',
                 'caption','비대칭 키 암호 예시',
                 'headers', JSON_ARRAY('기법','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'RSA',
                     '세 명의 수학자 Rivest, Shamir, Adleman이 제안한 공개키 암호 알고리즘으로, '
                     '큰 수의 소인수 분해 문제의 난이도에 기반하여 보안성을 확보한다. [2020년 1회, 3회]'
                   ),
                   JSON_ARRAY(
                     'ECC',
                     '유한체 위에서 정의된 타원 곡선 군에서의 이산 로그 문제에 기반한 공개키 암호 기법으로, '
                     '짧은 키 길이로도 높은 보안성을 제공한다.'
                   )
                 )
               )
             )
           ),

           /* -------------------------------
              5.3.2.3 암호 시스템
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '5.3.2.3',
             'title', '암호 시스템 구조',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','대표 암호 시스템'),
               JSON_OBJECT(
                 'type','table',
                 'caption','암호 시스템 예시',
                 'headers', JSON_ARRAY('구분','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'PKI(Public Key Infrastructure)',
                     '공개키 암호 방식과 디지털 인증서를 기반으로 사용자와 서버를 상호 인증하고, '
                     '암호화 통신과 전자 서명을 제공하는 공개키 기반 구조'
                   ),
                   JSON_ARRAY(
                     'PEM',
                     '암호화, 인증, 무결성 보장을 위한 이메일 보안 시스템으로, 메시지 암호화와 서명 기능을 제공한다.'
                   )
                 )
               )
             )
           ),

           /* -------------------------------
              5.3.2.4 코드 오류와 캡슐화 취약점
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '5.3.2.4',
             'title', '코드 오류와 캡슐화 관련 취약점',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','코드 오류 취약점 예시'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '널 포인터 역참조: null 상태의 참조를 사용하여 프로그램이 비정상 종료되거나 예기치 않은 동작을 유발',
                   '정수와 문자의 잘못된 변환: 형 변환 과정에서 범위 초과나 인코딩 문제로 인한 취약점 발생',
                   '부적절한 자원 해제: 파일 핸들, 소켓, 메모리 등을 제대로 해제하지 않아 자원 누수 혹은 보안 취약점 유발',
                   '초기화되지 않은 변수 사용: 정의되지 않은 값을 사용하는 과정에서 예측 불가능한 동작을 발생'
                 )
               ),
               JSON_OBJECT('type','heading','text','캡슐화 관련 취약점'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '잘못된 세션 관리로 인해 다른 사용자 세션 정보가 노출되는 문제',
                   '테스트 후 제거되지 않은 디버그 코드가 남아 내부 정보나 제어 경로를 노출하는 문제',
                   '민감한 데이터를 내부 클래스나 전역 변수로 그대로 보관하여 불필요한 접근 경로가 생기는 문제',
                   '시스템 설정 정보, 경로, 환경 변수 등 내부 시스템 데이터를 외부로 노출하는 문제'
                 )
               )
             )
           ),

           /* -------------------------------
              5.3.2.5 API 오용 취약점
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 5,
             'subCode', '5.3.2.5',
             'title', 'API 오용 취약점',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','API 오용으로 인한 대표 취약점'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'DNS 조회 결과에만 의존하여 보안 결정을 내리는 오류: 공격자가 DNS 스푸핑 등을 통해 결과를 조작할 수 있음',
                   '위험하다고 알려진 함수 사용: 버퍼 오버플로 등을 유발하기 쉬운 취약한 라이브러리 함수 사용',
                   '널 매개변수 검증 누락: 필수 인자가 null인 경우 예외가 발생하거나 우회 경로가 열릴 수 있음',
                   '약한 패스워드 정책: 최소 길이와 복잡도를 지나치게 낮게 설정하여 계정 탈취 위험을 높이는 경우'
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);