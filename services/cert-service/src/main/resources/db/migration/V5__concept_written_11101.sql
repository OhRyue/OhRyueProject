-- ============================
-- 1.1.1 현행 시스템 분석 (개념 보강)
-- topic_id = 11101
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 11101,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              1.1.1.1 플랫폼 특성 및 성능 분석
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.1.1.1',
             'title', '플랫폼 유형과 성능 특성 분석',
             'importance', 5,
             'blocks', JSON_ARRAY(

               /* 플랫폼 정의 */
               JSON_OBJECT('type','heading','text','플랫폼(Platform)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '플랫폼은 공급자와 수요자 등 복수 그룹이 상호 가치를 교환할 수 있도록 마련된 공통 환경으로, 소프트웨어 실행·운영의 기반 구조를 제공한다.'
               ),

               /* 플랫폼 유형 표 */
               JSON_OBJECT('type','heading','text','플랫폼의 유형'),
               JSON_OBJECT(
                 'type','table',
                 'caption','플랫폼 유형 비교',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('싱글 사이드 플랫폼','제휴 관계 기반으로 소비자와 공급자를 연결'),
                   JSON_ARRAY('투 사이드 플랫폼','두 그룹을 중개하며 개방된 구조'),
                   JSON_ARRAY('멀티 사이드 플랫폼','여러 이해관계자를 연결하는 중개 구조')
                 )
               ),

               /* 성능 특성 분석 기법 */
               JSON_OBJECT('type','heading','text','플랫폼 성능 특성 분석 기법'),
               JSON_OBJECT(
                 'type','table',
                 'caption','플랫폼 성능 분석 방법',
                 'headers',JSON_ARRAY('기법','설명'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('사용자 인터뷰','사용자 관점의 속도·만족도 분석. 산출물: 인터뷰 결과서'),
                   JSON_ARRAY('성능/부하 테스트','현행 플랫폼 대상 성능 지표 수집. 산출물: 성능/부하 테스트 결과서'),
                   JSON_ARRAY('산출물 점검','유사 타사 제품의 성능 자료 벤치마킹. 산출물: 벤치마킹 결과서')
                 )
               ),

               /* 성능 측정 항목 */
               JSON_OBJECT('type','heading','text','플랫폼 성능 특성 측정 항목 (경·사·응·가)'),
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '경과시간(Turnaround Time): 작업 시작~종료까지 소요 시간',
                 '사용률(Utilization): 자원 활용 비율',
                 '응답시간(Response Time): 요청에 대한 첫 응답 시간',
                 '가용성(Availability): 시스템의 정상 운영 가능 시간'
               ))
             )
           ),

           /* -------------------------------------
              1.1.1.2 운영체제(OS) 분석
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.1.1.2',
             'title', '운영체제(OS) 구성·분석 및 고려사항',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','운영체제(OS)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '운영체제는 하드웨어와 소프트웨어 자원을 효율적으로 관리하며, 공통 기능(프로세스 관리·메모리 관리·파일 관리 등)을 제공하는 핵심 소프트웨어이다.'
               ),

               JSON_OBJECT('type','heading','text','운영체제 분석 시 고려사항'),
               JSON_OBJECT(
                 'type','table',
                 'caption','운영체제 현행 분석 고려 요소',
                 'headers',JSON_ARRAY('고려사항','설명'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('신뢰도','장기간 운영 시 장애 가능성, 재기동 위험'),
                   JSON_ARRAY('성능','대량 작업(배치), 파일 처리 성능, 메모리 지원(32/64bit)'),
                   JSON_ARRAY('기술 지원','벤더 기술지원 여부, 오픈소스 유지성'),
                   JSON_ARRAY('주변기기','지원 가능한 하드웨어·I/O 기기'),
                   JSON_ARRAY('총 소유 비용(TCO)','하드웨어·라이선스 비용·유지관리 비용 등')
                 )
               ),

               JSON_OBJECT('type','heading','text','운영체제 종류 및 특징'),
               JSON_OBJECT(
                 'type','table',
                 'caption','컴퓨터/모바일 OS 비교',
                 'headers',JSON_ARRAY('구분','종류','특징'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('컴퓨터','Windows','중/소규모 서버·PC에 적합, 유지관리 용이'),
                   JSON_ARRAY('컴퓨터','UNIX','대용량 처리·안정성 우수(엔터프라이즈 환경)'),
                   JSON_ARRAY('컴퓨터','Linux','보안성·유연성 우수, 비용 효율적'),
                   JSON_ARRAY('모바일','Android','다양한 기기 호환성'),
                   JSON_ARRAY('모바일','iOS','보안·성능 우수, 폐쇄형 구조')
                 )
               )
             )
           ),

           /* -------------------------------------
              1.1.1.3 네트워크/OSI 분석
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.1.1.3',
             'title', '네트워크 구성 요소 및 OSI 계층 분석',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','네트워크의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '네트워크는 컴퓨터 장치들이 노드 간 연결을 통해 데이터를 송수신하는 구조로, 시스템 간 통신을 위한 기반 기술이다.'
               ),

               /* 네트워크 구성 요소 */
               JSON_OBJECT('type','heading','text','네트워크 구성 요소'),
               JSON_OBJECT(
                 'type','table',
                 'caption','네트워크 장비 비교',
                 'headers',JSON_ARRAY('장비','설명'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('리피터','물리층 장비. 신호 증폭·전달로 망 길이 확장'),
                   JSON_ARRAY('허브','물리층 장비. 성형 구조 기반 단순 중계'),
                   JSON_ARRAY('브리지','데이터링크층. 큰 네트워크를 세그먼트로 분할'),
                   JSON_ARRAY('스위치','고성능 브리지(2계층). MAC 기반 프레임 전달'),
                   JSON_ARRAY('라우터','3계층 장비. 패킷 라우팅 및 경로 설정'),
                   JSON_ARRAY('게이트웨이','모든 계층 지원. 프로토콜 변환 및 이기종 시스템 연결'),
                   JSON_ARRAY('방화벽','불법 접근 차단 및 내부 정보 유출 방지')
                 )
               ),

               /* OSI 7계층 */
               JSON_OBJECT('type','heading','text','OSI 7계층 구조'),
               JSON_OBJECT(
                 'type','table',
                 'caption','OSI 계층별 기능 요약',
                 'headers',JSON_ARRAY('계층','주요 기능'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('Application','원격 로그인, 파일전송 등 사용자 서비스'),
                   JSON_ARRAY('Presentation','데이터 변환·암호화·압축'),
                   JSON_ARRAY('Session','세션 연결/관리, 동기화'),
                   JSON_ARRAY('Transport','포트 지정, 흐름/오류 제어'),
                   JSON_ARRAY('Network','주소 지정, 라우팅, 패킷 전달'),
                   JSON_ARRAY('Data Link','프레임 구성·MAC 주소·오류 제어'),
                   JSON_ARRAY('Physical','전기적 신호 전달, 비트 전송')
                 )
               ),

               JSON_OBJECT('type','paragraph','text',
                 'OSI 계층은 통신 구조를 논리적으로 분리해 프로토콜 확장성·호환성을 높이는 역할을 하며, 시험에서 계층 순서·영문명·기능이 자주 출제된다.'
               )
             )
           ),

           /* -------------------------------------
              1.1.1.4 DBMS·미들웨어·비즈니스 융합 분석
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '1.1.1.4',
             'title', 'DBMS / 미들웨어 / 비즈니스 융합 분석',
             'importance', 4,
             'blocks', JSON_ARRAY(

               /* DBMS */
               JSON_OBJECT('type','heading','text','DBMS의 개념과 핵심 기능'),
               JSON_OBJECT('type','paragraph','text',
                 'DBMS는 데이터베이스 생성·저장·조회·관리 기능을 제공하는 소프트웨어로, 중복 제어·무결성 관리·접근 제어 등 데이터 관리의 중심 역할을 수행한다.'
               ),
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '중복 제어 및 무결성 제약 유지',
                 '관계 표현 및 인터페이스 제공(JDBC/ODBC)',
                 '샤딩·파티셔닝 지원',
                 '백업 및 회복 기능'
               )),
               JSON_OBJECT(
                 'type','table',
                 'caption','DBMS 분석 시 고려사항 (가성호기구)',
                 'headers',JSON_ARRAY('항목','설명'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('가용성','서비스 중단 허용 수준'),
                   JSON_ARRAY('성능','쿼리 처리 성능, 동시성'),
                   JSON_ARRAY('호환성','JDBC/ODBC 등 인터페이스 지원'),
                   JSON_ARRAY('기술지원','벤더 지원 여부'),
                   JSON_ARRAY('구축 비용','하드웨어/라이선스/TCO')
                 )
               ),

               /* 미들웨어 */
               JSON_OBJECT('type','heading','text','미들웨어 및 WAS'),
               JSON_OBJECT('type','paragraph','text',
                 '미들웨어는 운영체제와 애플리케이션 사이에서 추가 기능을 제공하는 계층이며, WAS는 세션 관리·트랜잭션·데이터 접근 기능을 제공해 웹 서비스 구현을 지원한다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items',JSON_ARRAY(
                   'WAS 고려사항: 가용성 / 성능 / 기술지원 / 구축비용 (가·성·기·구)',
                   '오픈 소스 고려: 라이선스 / 사용자 규모 / 지속 가능성'
                 )
               ),

               /* 비즈니스 융합 */
               JSON_OBJECT('type','heading','text','비즈니스 융합 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '비즈니스 융합은 산업·시장 간 경계를 허물고 ICT 기술을 결합해 새로운 비즈니스 모델을 만들거나 기존 제품을 혁신하는 활동이다.'
               )
             )
           )

         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = 11101);
