-- ============================
-- 2.4 애플리케이션 테스트 관리
-- 2.4.1 애플리케이션 테스트 케이스 설계
-- topic_id = @topic_app_testcase
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 12401,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              2.4.1-1 테스트 케이스 개요·절차
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.4.1',
             'title', '테스트 케이스 개요와 작성 절차',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','테스트 케이스(Test Case)란?'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '테스트 케이스는 특정 요구사항을 만족하는지 확인하기 위해 정의한 입력값, 실행 조건, 예상 결과의 집합이다. '
                 '즉, 어떤 입력과 환경에서 어떤 결과가 나와야 하는지를 명확히 정의한 테스트 단위이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','테스트 케이스 구성 3요소',
                 'headers', JSON_ARRAY('구성요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('테스트 조건(Test Condition)',
                              '어떤 기능/상황을 테스트할 것인지에 대한 조건'),
                   JSON_ARRAY('테스트 데이터(Test Data)',
                              '테스트 시 실제로 사용할 입력값'),
                   JSON_ARRAY('예상 결과(Expected Result)',
                              '정상 동작 시 기대되는 출력값·상태')
                 )
               ),

               JSON_OBJECT('type','heading','text','테스트 케이스 작성 절차'),
               JSON_OBJECT(
                 'type','table',
                 'caption','표준적인 테스트 케이스 작성 흐름',
                 'headers', JSON_ARRAY('단계','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('1. 테스트 계획 검토 및 자료 확보',
                              '테스트 전략·범위·일정이 정의된 계획서를 검토하고 관련 산출물을 확보한다.'),
                   JSON_ARRAY('2. 위험 평가 및 우선순위 결정',
                              '기능의 중요도·장애 영향·사용 빈도 등을 기준으로 테스트 우선순위를 정한다.'),
                   JSON_ARRAY('3. 테스트 요구사항 정의',
                              '요구사항을 분석해 어떤 항목을 어떻게 검증할지 테스트 요구사항을 도출한다.'),
                   JSON_ARRAY('4. 테스트 구조 설계 및 테스트 방법 결정',
                              '기능별·시나리오별 테스트 구조를 설계하고 블랙박스/화이트박스 등 기법을 선택한다.'),
                   JSON_ARRAY('5. 테스트 케이스 정의',
                              '각 요구사항에 대해 입력·절차·예상 결과를 포함한 구체적인 테스트 케이스를 작성한다.'),
                   JSON_ARRAY('6. 테스트 케이스 타당성 확인 및 유지보수',
                              '누락·중복·모호성을 검토하고, 변경된 요구사항에 맞추어 케이스를 계속 보완한다.')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.4.1-2 테스트 케이스 구성요소 & 오라클
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '2.4.1',
             'title', '테스트 케이스 구성요소와 테스트 오라클',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','ISO/IEC/IEEE 29119-3 테스트 케이스 구성요소'),
               JSON_OBJECT(
                 'type','table',
                 'caption','표준에서 정의하는 테스트 케이스 항목',
                 'headers', JSON_ARRAY('항목','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('식별자(Identifier)',
                              '테스트 케이스를 고유하게 식별하는 ID'),
                   JSON_ARRAY('테스트 항목(Test Item)',
                              '테스트 대상이 되는 모듈/기능/컴포넌트'),
                   JSON_ARRAY('입력 명세(Input Specification)',
                              '테스트에 사용되는 입력값과 입력 조건 정의'),
                   JSON_ARRAY('출력 명세(Output Specification)',
                              '기대되는 출력값·상태·로그 등 결과 정의'),
                   JSON_ARRAY('환경 설정(Environmental Needs)',
                              'OS, 브라우저, DB, 네트워크 등 필요한 실행 환경'),
                   JSON_ARRAY('특수 절차 요구(Special Procedure Requirement)',
                              '특별한 사전 조건, 초기화 절차, 후처리 등'),
                   JSON_ARRAY('의존성 기술(Inter-case Dependencies)',
                              '실행 순서, 선행·후행 테스트 케이스와의 의존 관계')
                 )
               ),

               JSON_OBJECT('type','heading','text','테스트 오라클(Test Oracle)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '테스트 오라클은 테스트 결과가 참인지 거짓인지를 판단하기 위해 사전에 정의한 기준값 또는 참값이다. '
                 '실제 결과와 오라클을 비교하여 성공/실패를 판정한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','테스트 오라클 종류 (참샘휴일)',
                 'headers', JSON_ARRAY('종류','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('참(True) 오라클',
                              '모든 입력 값에 대해 기대 결과를 정확히 판단할 수 있는 오라클'),
                   JSON_ARRAY('샘플링(Sampling) 오라클',
                              '입력 값의 일부(표본)에 대해서만 결과를 판단하는 오라클'),
                   JSON_ARRAY('휴리스틱(Heuristic) 오라클',
                              '경험·추정에 기반해 결과의 타당성을 판단하는 오라클'),
                   JSON_ARRAY('일관성 검사(Consistent) 오라클',
                              '동일 입력에 대해 항상 동일 결과가 나오는지 여부로 판단하는 오라클')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.4.1-3 테스트 레벨 & 기법 분류
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '2.4.1',
             'title', '테스트 레벨과 주요 테스트 기법',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','테스트 레벨(Test Level) (단통시인)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','테스트 레벨 4단계',
                 'headers', JSON_ARRAY('레벨','설명','대표 기법·예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('단위 테스트(Unit Test)',
                              '단위 모듈·서브루틴 등 가장 작은 단위에서 수행',
                              '인터페이스 테스트, 자료 구조 테스트, 실행 경로 테스트, 오류 처리 테스트'),
                   JSON_ARRAY('통합 테스트(Integration Test)',
                              '단위 테스트를 통과한 모듈 간 인터페이스를 검증',
                              '빅뱅, 상향식, 하향식 통합 테스트'),
                   JSON_ARRAY('시스템 테스트(System Test)',
                              '전체 시스템이 요구된 기능·비기능 요구사항을 만족하는지 검증',
                              '기능/성능/보안/사용성 등 종합 테스트'),
                   JSON_ARRAY('인수 테스트(Acceptance Test)',
                              '계약·요구사항이 만족되는지 사용자 관점에서 최종 확인',
                              '알파/베타 테스트')
                 )
               ),

               JSON_OBJECT('type','heading','text','정적 테스트 vs 동적 테스트'),
               JSON_OBJECT(
                 'type','table',
                 'caption','실행 여부에 따른 테스트 분류',
                 'headers', JSON_ARRAY('구분','설명','예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('정적 테스트',
                              '프로그램을 실행하지 않고 문서·코드 구조를 분석',
                              '동료 검토, 워크스루, 인스펙션, 정적 분석 도구'),
                   JSON_ARRAY('동적 테스트',
                              '소프트웨어를 실제 실행해 결함을 검출',
                              '블랙박스/화이트박스 테스트, 경험 기반 테스트')
                 )
               ),

               JSON_OBJECT('type','heading','text','블랙박스/화이트박스 테스트'),
               JSON_OBJECT(
                 'type','table',
                 'caption','테스트 관점에 따른 분류',
                 'headers', JSON_ARRAY('구분','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('블랙박스 테스트',
                              '명세 기반 테스트. 내부 코드 구조는 보지 않고 요구사항·명세를 기준으로 기능을 검증하는 테스트'),
                   JSON_ARRAY('화이트박스 테스트',
                              '구조 기반 테스트. 소스 코드 내부 구조와 제어 흐름을 기준으로 테스트 케이스를 설계하는 테스트')
                 )
               ),

               JSON_OBJECT('type','heading','text','블랙박스 테스트 유형 (동경결상 유분페원비 …)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '대표 유형: 동등 분할, 경곗값 분석, 결정 테이블, 상태전이, 유스케이스, 분류 트리, 페어와이즈, '
                 '원인-결과 그래프, 비교 테스트 등. 시험에서는 이름과 개념 정도만 빠르게 구분할 수 있으면 충분한 수준으로 자주 출제된다.'
               ),

               JSON_OBJECT('type','heading','text','화이트박스 테스트 유형 (구결조 조변다 기제데)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','화이트박스 커버리지 유형',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('구문(문장) 커버리지',
                              '모든 명령문이 최소 한 번은 수행되도록 테스트'),
                   JSON_ARRAY('결정(분기) 커버리지',
                              '각 분기에서 조건식 전체가 참/거짓을 최소 한 번씩 갖도록 테스트'),
                   JSON_ARRAY('조건 커버리지',
                              '결정 내 각 개별 조건이 참/거짓을 최소 한 번씩 갖도록 테스트'),
                   JSON_ARRAY('조건/결정 커버리지',
                              '결정 커버리지 + 조건 커버리지를 동시에 만족하도록 테스트'),
                   JSON_ARRAY('변경 조건/결정 커버리지(MCDC)',
                              '각 조건이 다른 조건 영향 없이 전체 결과에 독립적으로 영향을 주도록 설계'),
                   JSON_ARRAY('다중 조건 커버리지',
                              '모든 조건 조합을 100% 테스트'),
                   JSON_ARRAY('기본 경로·제어/데이터 흐름 테스트',
                              '제어 흐름 그래프와 데이터 사용 경로를 기반으로 내부 로직을 검증')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.4.1-4 테스트 목적·성능 테스트·원리
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '2.4.1',
             'title', '테스트 목적·성능 테스트·테스트 원리',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','테스트 목적에 따른 분류 (회안성구회병)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '회복 테스트, 안전(보안) 테스트, 성능 테스트, 구조 테스트, 회귀 테스트, 병행 테스트 등으로 나뉜다. '
                 '시험에서는 분류 이름과 대표 목적을 매칭하는 문제가 자주 나온다.'
               ),

               JSON_OBJECT('type','heading','text','성능 테스트의 상세 유형 (부스스내)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','성능 테스트 4유형',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('부하(Load) 테스트',
                              '정상 범위 내에서 사용량을 점진적으로 증가시키며 성능을 측정'),
                   JSON_ARRAY('스트레스(Stress) 테스트',
                              '정상 범위를 초과하는 높은 부하에서 시스템 한계와 복구 능력을 확인'),
                   JSON_ARRAY('스파이크(Spike) 테스트',
                              '짧은 시간 동안 급격한 부하 증가가 발생할 때의 반응을 확인'),
                   JSON_ARRAY('내구성(Endurance) 테스트',
                              '장시간 연속 사용 시 메모리 누수·성능 저하 등 장기적인 품질을 확인')
                 )
               ),

               JSON_OBJECT('type','heading','text','소프트웨어 테스트의 7원리 (결완초집 살정오)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '대표적으로 다음과 같은 원리를 기억해두면 된다.\n'
                 '· 결함이 존재함을 드러낼 수는 있지만, 결함이 없음을 증명할 수는 없다.\n'
                 '· 완벽한 테스팅은 불가능하다.\n'
                 '· 테스트는 가능한 한 초기부터 시작해야 한다.\n'
                 '· 결함은 특정 모듈·기능에 집중되는 경향이 있다(결함 집중).\n'
                 '· 항상 같은 테스트만 반복하면 새로운 결함을 찾기 어렵다(살충제 패러독스).\n'
                 '· 테스트는 상황·정황에 의존한다.\n'
                 '· 요구사항이 잘못되면 오류가 없어도 실패한 시스템이 될 수 있다(오류 부재의 궤변).'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);



-- ============================
-- 2.4.2 애플리케이션 통합 테스트
-- topic_id = @topic_app_integration
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 12402,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              2.4.2-1 코드 커버리지 & 자동화 도구
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.4.2',
             'title', '코드 커버리지와 테스트 자동화 도구',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','코드 커버리지 유형 (구결조 조변다)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '코드 커버리지는 테스트가 소스 코드를 얼마나 실행했는지 정량적으로 나타내는 지표이다. '
                 '구문, 결정, 조건, 조건/결정, 변경 조건/결정, 다중 조건 커버리지 등이 있으며, '
                 '화이트박스 테스트의 핵심 개념이다.'
               ),

               JSON_OBJECT('type','heading','text','테스트 자동화 도구 개요'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '테스트 자동화 도구는 반복적인 테스트 작업을 스크립트나 도구로 자동화하여 '
                 '테스트 시간과 인력을 줄이고, 테스트 품질을 일정 수준 이상으로 유지하도록 돕는다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','테스트 자동화 도구 유형',
                 'headers', JSON_ARRAY('구분','대표 도구','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('정적 분석 도구',
                              'PMD, Checkstyle, Splint, Cppcheck, SonarQube',
                              '코드를 실행하지 않고 문법·규칙·잠재 결함을 분석'),
                   JSON_ARRAY('테스트 실행 도구',
                              'JMeter, OpenSTA',
                              '테스트 스크립트를 기반으로 부하·기능 테스트를 자동 실행'),
                   JSON_ARRAY('성능 테스트 도구',
                              'Cobertura, Clover',
                              '테스트 실행 시 커버리지·성능을 측정·분석'),
                   JSON_ARRAY('테스트 통제 도구',
                              'Hudson, Ant, xUnit',
                              '빌드·테스트·보고서 생성을 자동으로 오케스트레이션')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.4.2-2 테스트 하네스(Test Harness)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '2.4.2',
             'title', '테스트 하네스(Test Harness) 구성',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','테스트 하네스(Test Harness)란?'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '테스트 하네스는 테스트 실행을 지원하기 위한 드라이버, 스텁, 스크립트 등의 집합으로, '
                 '통합 테스트를 자동·반자동으로 수행할 수 있게 해주는 테스트 환경이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','테스트 하네스 구성요소 (드스슈 시스목)',
                 'headers', JSON_ARRAY('구성요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('테스트 드라이버(Driver)',
                              '상위 모듈 없이 하위 모듈을 직접 호출하기 위한 코드. 매개변수를 전달하고 결과를 수집한다.'),
                   JSON_ARRAY('테스트 스텁(Stub)',
                              '아직 구현되지 않은 하위 모듈을 대신하는 간이 모듈. 상위 모듈이 호출할 수 있도록 임시 기능 제공.'),
                   JSON_ARRAY('테스트 슈트(Suite)',
                              '관련된 테스트 케이스들을 논리적으로 묶어놓은 집합'),
                   JSON_ARRAY('테스트 시나리오',
                              '테스트가 어떤 흐름과 단계로 수행될지 서술한 시나리오 문서'),
                   JSON_ARRAY('테스트 스크립트',
                              '테스트 실행을 자동화하기 위한 절차·명령 집합'),
                   JSON_ARRAY('목(Mock) 오브젝트',
                              '실제 컴포넌트 대신 동작을 흉내내는 가짜 객체. 외부 의존성을 분리하기 위해 사용.')
                 )
               ),

               JSON_OBJECT('type','heading','text','드라이버 vs 스텁'),
               JSON_OBJECT(
                 'type','table',
                 'caption','드라이버와 스텁 비교',
                 'headers', JSON_ARRAY('구분','드라이버(Driver)','스텁(Stub)'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('개념',
                              '테스트 대상 하위 모듈을 직접 호출하는 상위 역할의 도구',
                              '상위 모듈이 호출하는 아직 구현되지 않은 하위 모듈을 대신하는 임시 코드'),
                   JSON_ARRAY('필요 시기',
                              '상위 모듈 없이 하위 모듈만 있을 때(상향식 테스트)',
                              '상위 모듈은 있으나 하위 모듈이 없는 경우(하향식 테스트)'),
                   JSON_ARRAY('테스트 방식',
                              '상향식(Bottom-Up) 통합 테스트에서 주로 사용',
                              '하향식(Top-Down) 통합 테스트에서 주로 사용'),
                   JSON_ARRAY('특징',
                              '실제 하위 모듈과의 인터페이스를 검증하는 데 초점',
                              '간단한 리턴값만 제공하는 경우가 많아 작성이 상대적으로 용이')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.4.2-3 통합 테스트 개념·방법
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '2.4.2',
             'title', '통합 테스트(Integration Test) 개념과 수행 방법',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','통합 테스트의 목적'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '통합 테스트는 단위 테스트를 통과한 모듈들을 서로 결합했을 때, '
                 '인터페이스·데이터 흐름·제어 흐름에서 오류나 결함이 없는지 확인하는 단계이다. '
                 '설계 단계에서 정의한 모듈 구조·연계 방식이 실제 구현과 일치하는지 검증한다.'
               ),

               JSON_OBJECT('type','heading','text','통합 테스트 수행 방법 (하스 상드)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','하향식 vs 상향식 통합 테스트',
                 'headers', JSON_ARRAY('방법','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('하향식 통합 테스트(Top-Down Test)',
                              '메인 제어 모듈에서 시작해 점차 하위 모듈로 내려가며 통합하는 방식. '
                              '상위 모듈은 실제 코드, 아직 구현되지 않은 하위 모듈은 스텁으로 대체한다.'),
                   JSON_ARRAY('상향식 통합 테스트(Bottom-Up Test)',
                              '최하위 모듈부터 점진적으로 상위 모듈과 결합해가는 방식. '
                              '아직 구현되지 않은 상위 모듈은 드라이버로 대체한다.')
                 )
               ),
               JSON_OBJECT(
                 'type','callout',
                 'title','시험 포인트',
                 'body',
                 '· 하향식 ↔ 스텁, 상향식 ↔ 드라이버 매칭을 반드시 기억\n'
                 '· 통합 테스트의 핵심은 “모듈 간 인터페이스 검증”이라는 점을 놓치지 않기'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);



-- ============================
-- 2.4.3 애플리케이션 성능 개선
-- topic_id = @topic_app_perf
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 12403,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              2.4.3-1 성능 테스트와 성능 지표
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.4.3',
             'title', '애플리케이션 성능 테스트 개요',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','성능 테스트의 목적'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '애플리케이션 성능 개선의 출발점은 성능 테스트이다. '
                 '성능 테스트는 응답 시간, 처리량, 동시 사용자 수, 자원 사용량 등이 '
                 '요구 수준을 만족하는지 확인하고, 병목 구간을 찾아내기 위해 수행한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','주요 성능 지표 예시',
                 'headers', JSON_ARRAY('지표','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('응답 시간(Response Time)',
                              '사용자 요청에 대한 응답이 돌아오는 데 걸리는 시간'),
                   JSON_ARRAY('처리량(Throughput)',
                              '단위 시간당 처리 가능한 거래·요청 수'),
                   JSON_ARRAY('동시 사용자 수(Concurrency)',
                              '동시에 시스템을 사용하는 사용자/세션 수'),
                   JSON_ARRAY('자원 사용률(Resource Utilization)',
                              'CPU, 메모리, 디스크, 네트워크의 사용 비율')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.4.3-2 성능 테스트 유형 (부스스내)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '2.4.3',
             'title', '성능 테스트 유형과 활용',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','성능 테스트 유형 (부스스내)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','성능 테스트 4가지 유형',
                 'headers', JSON_ARRAY('유형','설명','주용도'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('부하 테스트(Load Test)',
                              '점진적으로 부하를 증가시키며 시스템이 목표 성능을 유지하는지 확인',
                              '정상·최대 부하 범위에서 용량·성능 확인'),
                   JSON_ARRAY('스트레스 테스트(Stress Test)',
                              '정상 범위를 초과하는 과도한 부하를 가해 시스템의 한계와 회복 능력을 점검',
                              '임계치·장애 상황에서의 동작 확인'),
                   JSON_ARRAY('스파이크 테스트(Spike Test)',
                              '짧은 시간 동안 트래픽이 급격하게 증가·감소할 때의 반응을 확인',
                              '이벤트·프로모션 등 단기간 폭주 상황 대비'),
                   JSON_ARRAY('내구성 테스트(Endurance / Soak Test)',
                              '장시간 지속적인 부하 하에서 성능 저하·메모리 누수·리소스 고갈 여부를 확인',
                              '메모리 누수·리소스 해제 문제 탐지')
                 )
               ),
               JSON_OBJECT(
                 'type','callout',
                 'title','성능 테스트와 성능 개선의 연결',
                 'body',
                 '성능 개선은 “감으로 튜닝”하는 것이 아니라, 성능 테스트를 통해 수치로 병목을 확인한 후 '
                 '원인을 분석하고, 그 결과에 따라 튜닝하는 순서로 진행해야 한다.'
               )
             )
           ),

           /* -------------------------------------
              2.4.3-3 성능 개선(튜닝) 기본 절차
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '2.4.3',
             'title', '애플리케이션 성능 개선 절차',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','성능 개선(튜닝) 5단계'),
               JSON_OBJECT(
                 'type','table',
                 'caption','일반적인 성능 개선 절차',
                 'headers', JSON_ARRAY('단계','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('1. 목표 성능 정의',
                              '요구사항·SLA를 기준으로 응답 시간, 처리량 등 목표 값을 수치로 정의한다.'),
                   JSON_ARRAY('2. 측정·모니터링',
                              '성능 테스트와 모니터링 도구를 통해 현재 성능과 병목 구간을 수집한다.'),
                   JSON_ARRAY('3. 병목 원인 분석',
                              '어느 계층(클라이언트, 서버, DB, 네트워크, 알고리즘)에서 지연이 발생하는지 분석한다.'),
                   JSON_ARRAY('4. 개선안 적용',
                              '쿼리 튜닝, 캐시 적용, 알고리즘·자료구조 개선, 스레드·커넥션 풀 조정 등 구체적 튜닝을 적용한다.'),
                   JSON_ARRAY('5. 회귀 테스트 및 재평가',
                              '개선 후 동일한 성능 테스트를 반복하여 목표 달성 여부와 부작용 여부를 확인한다.')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.4.3-4 성능 모니터링·정적 분석 도구 연계
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '2.4.3',
             'title', '모니터링·정적 분석 도구와 성능 개선',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','모니터링 도구 활용'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '애플리케이션 모니터링 도구(Jennifer, Nmon 등)는 트랜잭션 수, 처리 시간, 응답 시간, 자원 사용률을 '
                 '실시간으로 수집하여 병목 구간을 시각화해준다. 성능 개선 작업 전후로 지표를 비교할 때 매우 유용하다.'
               ),
               JSON_OBJECT('type','heading','text','정적 분석 도구와 성능 품질'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '정적 분석 도구(PMD, Cppcheck, Checkstyle, SonarQube 등)는 복잡도, 중복 코드, 잠재적인 버그를 찾아 '
                 '코드 품질을 높이도록 돕는다. 복잡도가 높은 코드와 중복 로직을 줄이면 유지보수성이 좋아질 뿐만 아니라, '
                 '불필요한 연산 제거로 성능 향상에도 긍정적인 영향을 줄 수 있다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);
