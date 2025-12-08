-- ============================
-- 2.2 통합 구현
-- 2.2.1 모듈 구현
-- topic_id = @topic_module_impl
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 12201,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              2.2.1-1 단위 모듈 구현 개요
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.2.1',
             'title', '단위 모듈 구현 개요',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','단위 모듈 구현의 개념'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '단위 모듈 구현은 소프트웨어 기능을 여러 개의 단위 모듈로 분할·추상화하여 구현하는 기법이다. '
                 '성능 향상과 유지보수 용이성, 재사용성 향상이 주요 목적이다.'
               ),
               JSON_OBJECT('type','heading','text','단위 모듈 구현 원리 (정분추모)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','단위 모듈 구현 4대 원리',
                 'headers', JSON_ARRAY('원리','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('정보 은닉(Information Hiding)',
                              '모듈 내부 데이터·처리 로직을 감추고, 공개된 인터페이스만 통해 접근하도록 하는 원리'),
                   JSON_ARRAY('분할과 정복(Divide & Conquer)',
                              '복잡한 문제를 작게 분할해 모듈 단위로 해결한 뒤 통합하는 방식'),
                   JSON_ARRAY('데이터 추상화(Data Abstraction)',
                              '데이터의 상세 표현은 숨기고, 필요한 속성과 연산만 노출하는 기법'),
                   JSON_ARRAY('모듈 독립성(Module Independency)',
                              '모듈 간 결합도는 낮게, 응집도는 높게 설계하여 변경 영향 범위를 최소화')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.2.1-2 재사용(Reuse) 종류
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '2.2.1',
             'title', '재사용(Reuse) 종류',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','재사용(Reuse)의 세 가지 형태'),
               JSON_OBJECT(
                 'type','table',
                 'caption','재공학 / 역공학 / 재개발',
                 'headers', JSON_ARRAY('구분','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('재공학',
                              '기존 소프트웨어를 분석·개선하여 품질을 향상시키거나, 동일 기능을 재사용하는 기법. '
                              '위험 감소·비용 절감·기간 단축 효과가 있다.'),
                   JSON_ARRAY('역공학',
                              '완성된 소프트웨어를 디버깅·디컴파일·분석하여 구조·원리·알고리즘을 역으로 도출하는 기법'),
                   JSON_ARRAY('재개발',
                              '기존 시스템을 참고하되, 완전히 새로운 시스템을 다시 개발하는 방식. '
                              '새로운 기능 추가나 큰 구조 변경에 사용.')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.2.1-3 단위 모듈 테스트 종류
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '2.2.1',
             'title', '단위 모듈 테스트 종류',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','단위 모듈 테스트 방법'),
               JSON_OBJECT(
                 'type','table',
                 'caption','테스트 유형과 설명',
                 'headers', JSON_ARRAY('종류','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('블랙박스 테스트',
                              '명세 기반 테스트. 내부 코드를 보지 않고 요구사항·명세서를 기준으로 기능을 검증하는 테스트'),
                   JSON_ARRAY('화이트박스 테스트',
                              '구조 기반 테스트. 모듈 내부 소스 코드를 보면서 분기·경로를 기준으로 테스트'),
                   JSON_ARRAY('메서드 기반 테스트',
                              '개별 메서드를 서로 다른 파라미터로 반복 호출하여 동작을 검증하는 테스트'),
                   JSON_ARRAY('화면 기반 테스트',
                              '사용자 시나리오를 기준으로 실제 화면에 데이터를 입력·조작하면서 수행하는 테스트')
                 )
               ),
               JSON_OBJECT(
                 'type','callout',
                 'title','시험 포인트',
                 'body',
                 '· “블랙박스 = 명세 기반”, “화이트박스 = 구조 기반” 용어 매칭\n'
                 '· 단위 테스트 관점에서 메서드 기반·화면 기반 테스트도 함께 정리'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);



-- ============================
-- 2.2.2 통합 구현 관리
-- topic_id = @topic_integration_mgmt
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 12202,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              2.2.2-1 IDE와 협업 도구
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.2.2',
             'title', 'IDE와 협업 도구',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','IDE(Integrated Development Environment)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'IDE는 코딩, 디버깅, 컴파일, 배포 등 개발에 필요한 작업을 하나의 통합 환경에서 수행하도록 지원하는 도구이다. '
                 '대표 예: Eclipse, Visual Studio, Xcode 등.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','IDE가 제공하는 대표 기능',
                 'headers', JSON_ARRAY('구분','내용'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('개발환경 지원','에디터, 프로젝트 구조 관리, 빌드 스크립트 연동'),
                   JSON_ARRAY('컴파일·디버깅','코드 컴파일, 브레이크포인트, 변수 관찰 등'),
                   JSON_ARRAY('외부 연계','형상 관리 도구, 빌드 도구, 서버 등과 연동'),
                   JSON_ARRAY('DB 연동','개발 중 데이터베이스 조회·수정·테스트 지원')
                 )
               ),

               JSON_OBJECT('type','heading','text','협업 도구'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '협업 도구는 통합 구현 과정에서 개발자 간 커뮤니케이션, 산출물 공유, 일정·이슈 관리를 지원하는 도구이다. '
                 '문서·소스·아이디어·디자인·프로젝트·일정 관리 등 다양한 형태로 구분된다.'
               ),
               JSON_OBJECT(
                 'type','callout',
                 'title','협업 도구의 핵심 기능',
                 'body',
                 '· 개발자 간 커뮤니케이션 채널 제공\n'
                 '· 일정/이슈 공유 및 진행 상황 시각화\n'
                 '· 집단 지성을 활용한 문제 해결 및 아이디어 공유'
               )
             )
           ),

           /* -------------------------------------
              2.2.2-2 형상 관리 도구
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '2.2.2',
             'title', '형상 관리(Configuration Management)',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','형상 관리 개념'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '형상 관리는 개발 과정에서 발생하는 각종 산출물(요구사항, 설계, 소스 코드, 매뉴얼 등)의 변경 이력을 관리하는 활동이다. '
                 'CVS, SVN, Git과 같은 형상 관리 도구로 구현한다.'
               ),

               JSON_OBJECT('type','heading','text','형상관리 관리 항목 (분코지)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','형상관리 관리 대상 예',
                 'headers', JSON_ARRAY('구분','예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('프로젝트 문서','요구 분석서, 설계서, 테스트 케이스'),
                   JSON_ARRAY('소스 코드','애플리케이션 코드, 스크립트'),
                   JSON_ARRAY('운영·설치 지침','운영 매뉴얼, 설치 매뉴얼 등')
                 )
               ),

               JSON_OBJECT('type','heading','text','형상 관리 도구의 주요 기능 (인아커)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','형상 관리 도구 기본 동작',
                 'headers', JSON_ARRAY('기능','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('체크아웃(Check-Out)',
                              '저장소의 특정 버전을 작업 공간으로 가져와 수정할 수 있도록 하는 기능'),
                   JSON_ARRAY('체크인(Check-In)',
                              '수정된 파일을 저장소에 반영하는 기능'),
                   JSON_ARRAY('커밋(Commit)',
                              '버전 이력 단위로 변경 사항을 확정·기록하는 작업')
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);



-- ============================
-- 2.3.1 제품 소프트웨어 패키징
-- topic_id = @topic_sw_packaging
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 12301,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              2.3.1-1 애플리케이션 패키징 개요
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.3.1',
             'title', '애플리케이션 패키징',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','애플리케이션 패키징의 개념'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '애플리케이션 패키징은 개발이 완료된 소프트웨어를 배포·설치 가능한 형태로 묶고, '
                 '설치·사용 방법을 포함한 매뉴얼까지 준비하여 고객에게 전달하는 활동이다. '
                 '“사용자 중심”으로 진행된다는 점이 중요하다.'
               ),
               JSON_OBJECT('type','heading','text','패키징 고려사항 (환유관변)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','패키징 시 고려해야 할 항목',
                 'headers', JSON_ARRAY('키워드','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('환경(사용자 시스템 환경 정의)',
                              '운영체제, 하드웨어, 네트워크 등 설치 대상 환경을 명확히 정의'),
                   JSON_ARRAY('UI 제공',
                              '설치·실행 과정에서 사용자에게 직관적인 UI 제공'),
                   JSON_ARRAY('관리 서비스 제공',
                              '설치 이후에도 유지보수·업데이트·문의 대응이 가능하도록 서비스 설계'),
                   JSON_ARRAY('변경·개선 관리',
                              '패키징 변경·패치·업데이트에 대한 관리 체계 마련')
                 )
               ),
               JSON_OBJECT(
                 'type','heading',
                 'text',
                 '애플리케이션 패키징 프로세스'
               ),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '기능 식별 → 모듈화 → 빌드 진행 → 사용자 환경 분석 → 패키징 적용 시험 → 패키징 변경·개선 순으로 진행한다.'
               )
             )
           ),

           /* -------------------------------------
              2.3.1-2 배포 도구와 모니터링 도구
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '2.3.1',
             'title', '애플리케이션 배포·모니터링 도구',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','애플리케이션 배포 도구'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '애플리케이션 배포 도구는 패키징된 디지털 콘텐츠의 지적 재산권을 보호·관리하고, '
                 '안전한 유통과 배포를 보장하는 솔루션이다. DRM 기술 요소를 그대로 활용한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','배포 도구 기술 요소 (DRM 기술 요소 동일)',
                 'headers', JSON_ARRAY('키워드','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('암호화',
                              '콘텐츠를 암호화하여 비인가 사용자가 이용하지 못하게 보호'),
                   JSON_ARRAY('키 관리',
                              '암·복호화에 사용되는 키의 생성·배포·저장·폐기를 관리'),
                   JSON_ARRAY('식별·저작권 표현',
                              '콘텐츠와 사용자에 대한 식별 체계, 저작권 정책 표현'),
                   JSON_ARRAY('정책·크랙 방지·인증',
                              '사용 정책 관리, 크랙 방지, 정당한 사용자 인증 기능 제공')
                 )
               ),
               JSON_OBJECT(
                 'type','heading',
                 'text',
                 '배포 도구 활용 시 고려사항 (암이복최)'
               ),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '· 암호화/보안 확보\n'
                 '· 이기종 환경·DRM 연동 고려\n'
                 '· 과도한 복잡성·비효율성 방지\n'
                 '· 환경에 최적인 암호 알고리즘 선택'
               ),

               JSON_OBJECT('type','heading','text','애플리케이션 모니터링 도구'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '애플리케이션 모니터링 도구는 배포된 제품 소프트웨어의 기능·성능·운영 상태를 모니터링하여 '
                 '서비스 가용성과 성능을 최적화하기 위한 도구이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','모니터링 도구 기능 예시',
                 'headers', JSON_ARRAY('기능','설명','예시 도구'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('변경 관리',
                              '구성요소 간 종속 관계·변경 이력을 모니터링',
                              'ChangeMiner'),
                   JSON_ARRAY('성능 관리',
                              '트랜잭션 수, 처리 시간, 응답 시간 등 모니터링',
                              'Jennifer, Nmon'),
                   JSON_ARRAY('정적 분석',
                              '소스 코드 잠재적 결함, 코딩 규칙 위반 탐지',
                              'PMD, Cppcheck, Checkstyle, SonarQube'),
                   JSON_ARRAY('동적 분석',
                              '실행 중 발생 가능한 에러·취약점 탐지',
                              'Avalanche, Valgrind')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.3.1-3 디지털 저작권 관리(DRM)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '2.3.1',
             'title', '디지털 저작권 관리(DRM)',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','DRM 개요'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'DRM(Digital Rights Management)은 디지털 콘텐츠에 권리 정보를 부여하고, '
                 '암호화 기술을 이용하여 허가된 사용자·허가된 범위 내에서만 콘텐츠를 사용할 수 있도록 통제하는 기술이다.'
               ),
               JSON_OBJECT(
                 'type','heading',
                 'text',
                 'DRM 구성요소 (제콘패 클 소컨보)'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','DRM 주요 구성요소',
                 'headers', JSON_ARRAY('구분','주요 역할'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('콘텐츠 제공자',
                              '암호화된 콘텐츠와 메타데이터, 사용 정보를 포함한 DRM 콘텐츠를 제공'),
                   JSON_ARRAY('패키저(Packager)',
                              '콘텐츠를 암호화하고, 라이선스·메타데이터를 포함한 패키지로 변환'),
                   JSON_ARRAY('클리어링 하우스',
                              '키 관리와 라이선스 발급·정책 관리를 담당'),
                   JSON_ARRAY('콘텐츠 소비자 + DRM 컨트롤러',
                              '배포된 콘텐츠에 대한 이용 권한을 확인·통제'),
                   JSON_ARRAY('보안 컨트롤러',
                              '콘텐츠 유통 경로에서의 보안·무결성을 보장')
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);



-- ============================
-- 2.3.2 제품 소프트웨어 매뉴얼 작성
-- topic_id = @topic_sw_manual
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 12302,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              2.3.2-1 제품 소프트웨어 매뉴얼
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.3.2',
             'title', '제품 소프트웨어 매뉴얼',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','매뉴얼의 개념과 종류'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '제품 소프트웨어 매뉴얼은 사용자 관점에서 기능과 사용 방법을 설명하는 문서(설명서·안내서)이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','설치 매뉴얼 vs 사용자 매뉴얼',
                 'headers', JSON_ARRAY('종류','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('설치 매뉴얼',
                              '제품 구매 후 최초 설치 시 참고하는 문서. 설치 절차에 필요한 내용만 포함하며, '
                              '설치 시작부터 완료까지 전 과정을 순서대로 설명한다.'),
                   JSON_ARRAY('사용자 매뉴얼',
                              '설치와 사용에 필요한 전체 절차·환경 정보를 포함하는 문서. '
                              '사용자가 컴포넌트를 사용할 때 알아야 할 내용을 중심으로 기술한다.')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.3.2-2 국제 표준 제품 품질 특성
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '2.3.2',
             'title', '국제 표준 제품 품질 특성',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','주요 품질 표준'),
               JSON_OBJECT(
                 'type','table',
                 'caption','소프트웨어 품질 관련 국제 표준',
                 'headers', JSON_ARRAY('표준','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('ISO/IEC 9126',
                              '소프트웨어 품질 특성과 품질 측정 기준을 정의한 표준'),
                   JSON_ARRAY('ISO/IEC 14598',
                              '소프트웨어 제품 평가 프로세스 및 평가 모듈을 정의'),
                   JSON_ARRAY('ISO/IEC 12119',
                              '소프트웨어 패키지 제품의 품질 요구사항 및 테스트 표준'),
                   JSON_ARRAY('ISO/IEC 25000 (SQuaRE)',
                              '소프트웨어 품질 요구·평가 통합 모델 표준. 9126·14598·12119를 통합함.')
                 )
               ),

               JSON_OBJECT('type','heading','text','ISO/IEC 9126 품질 특성 (기신사효유이)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','6대 품질 특성',
                 'headers', JSON_ARRAY('특성','설명','예시 키워드'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('기능성(Functionality)',
                              '요구된 기능을 정확히 제공하는 능력',
                              '적합성, 정확성, 상호 운용성, 보안성'),
                   JSON_ARRAY('신뢰성(Reliability)',
                              '주어진 시간 동안 오류 없이 수행하는 정도',
                              '성숙성, 결함 허용성, 회복성'),
                   JSON_ARRAY('사용성(Usability)',
                              '사용자가 이해·학습·사용하기 쉬운 정도',
                              '이해성, 학습성, 운용성, 친밀성'),
                   JSON_ARRAY('효율성(Efficiency)',
                              '사용 자원에 대한 성능 수준',
                              '시간 반응성, 자원 효율성'),
                   JSON_ARRAY('유지보수성(Maintainability)',
                              '변경·수정·개선이 용이한 정도',
                              '분석성, 변경성, 안정성, 시험성'),
                   JSON_ARRAY('이식성(Portability)',
                              '다른 환경으로 옮겨 사용할 수 있는 능력',
                              '적응성, 설치성, 대체성')
                 )
               ),

               JSON_OBJECT('type','heading','text','ISO/IEC 14598 평가 특성 (반재공객)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','평가 결과가 가져야 할 성질',
                 'headers', JSON_ARRAY('특성','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('반복성(Repeatability)','같은 평가자가 같은 조건에서 평가하면 결과가 동일해야 함'),
                   JSON_ARRAY('재현성(Reproducibility)','다른 평가자라도 같은 조건이면 유사한 결과가 나와야 함'),
                   JSON_ARRAY('공정성(Impartiality)','평가가 특정 이해관계에 치우치지 않아야 함'),
                   JSON_ARRAY('객관성(Objectivity)','객관적 근거에 기반해 결과를 도출해야 함')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.3.2-3 소프트웨어 공학 기본 원칙
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '2.3.2',
             'title', '소프트웨어 공학 기본 원칙',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','소프트웨어 위기와 극복 방안'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '소프트웨어 위기는 소프트웨어의 복잡도·규모 증가, 관리 부재, 수명주기 단축 등으로 인해 '
                 '품질 저하·비용 증가·일정 지연이 발생하는 현상을 말한다. '
                 '공학적 접근, 표준화, 자동화 도구, 품질 보증 등을 통해 극복한다.'
               ),
               JSON_OBJECT('type','heading','text','대표 법칙'),
               JSON_OBJECT(
                 'type','table',
                 'caption','소프트웨어 공학 관련 법칙',
                 'headers', JSON_ARRAY('법칙','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('브룩스의 법칙',
                              '지연된 프로젝트에 인력을 추가 투입하면 오히려 더 지연될 수 있다는 법칙'),
                   JSON_ARRAY('파레토 법칙(80/20)',
                              '소프트웨어 오류의 80%는 전체 모듈의 20%에서 발견된다는 경험 법칙'),
                   JSON_ARRAY('롱테일 법칙',
                              '소수의 핵심보다도 다수의 비주류 항목이 더 큰 가치를 낼 수 있다는 개념')
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);



-- ============================
-- 2.3.3 제품 소프트웨어 버전 관리
-- topic_id = @topic_sw_versioning
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 12303,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              2.3.3-1 버전 관리와 도구 유형
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.3.3',
             'title', '제품 소프트웨어 버전 관리 개요',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','버전 관리 개념'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '소프트웨어 버전 관리는 코드·라이브러리·문서 등 산출물의 시간에 따른 변경 이력을 체계적으로 관리하는 활동이다. '
                 '형상 관리 지침을 기반으로 버전 관리 도구를 사용해 수행한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','버전 관리 도구 유형',
                 'headers', JSON_ARRAY('유형','예시','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('공유 폴더 방식','RCS, SCCS',
                              '공유 폴더에 파일을 복사해 관리하는 초기 방식'),
                   JSON_ARRAY('클라이언트/서버 방식','CVS, SVN',
                              '중앙 서버에서 버전을 관리하고 클라이언트가 접속해 사용하는 구조'),
                   JSON_ARRAY('분산 저장소 방식','Git, BitKeeper',
                              '각 클라이언트가 완전한 로컬 저장소를 가지며, 필요 시 원격 저장소와 동기화')
                 )
               ),
               JSON_OBJECT(
                 'type','callout',
                 'title','도구별 특징',
                 'body',
                 '· CVS: 가장 오래된 형상 관리 도구 중 하나\n'
                 '· SVN: CVS 단점을 보완해 널리 사용되는 중앙집중형 도구\n'
                 '· Git: 분산형, 로컬 작업에 강하고 브랜치·머지에 최적화'
               )
             )
           ),

           /* -------------------------------------
              2.3.3-2 빌드 자동화와 품질 분석 도구
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '2.3.3',
             'title', '빌드 자동화 & 소스 코드 품질 분석',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','빌드 자동화 도구 구성요소'),
               JSON_OBJECT(
                 'type','table',
                 'caption','빌드 자동화 구성 요소와 예시',
                 'headers', JSON_ARRAY('구성요소','설명','대표 도구'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('CI 서버',
                              '지속적 통합(Continuous Integration)을 위해 빌드·테스트를 자동 수행하는 서버',
                              'Jenkins, Hudson'),
                   JSON_ARRAY('SCM',
                              '소스 코드 형상 관리 시스템',
                              'SVN, Git'),
                   JSON_ARRAY('빌드 도구',
                              '컴파일·테스트·정적 분석 등을 자동 실행해 동작 가능한 산출물을 만드는 도구',
                              'Ant, Maven, Gradle'),
                   JSON_ARRAY('테스트 도구',
                              '작성된 테스트 코드를 자동으로 실행하는 도구',
                              'JUnit, Selenium'),
                   JSON_ARRAY('테스트 커버리지 도구',
                              '테스트가 소스 코드를 얼마나 커버하는지 분석',
                              'Emma'),
                   JSON_ARRAY('인스펙션 도구',
                              '소스 코드를 실행하지 않고 정적 분석으로 품질을 판단',
                              'Checkstyle, Cppcheck')
                 )
               ),

               JSON_OBJECT('type','heading','text','소스 코드 품질 분석 도구'),
               JSON_OBJECT(
                 'type','table',
                 'caption','정적/동적 분석 도구 예시',
                 'headers', JSON_ARRAY('구분','도구','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('정적 분석','PMD, Cppcheck, Checkstyle, SonarQube',
                              '코딩 스타일, 잠재 버그, 규칙 위반, 복잡도 등을 정적으로 분석'),
                   JSON_ARRAY('동적 분석','Avalanche, Valgrind',
                              '실행 중 메모리 누수, 스레드 결함, 취약점 등을 분석')
                 )
               )
             )
           ),

           /* -------------------------------------
              2.3.3-3 복잡도 측정과 클린 코드
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '2.3.3',
             'title', '맥케이브 복잡도 & 클린 코드',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','맥케이브 순환 복잡도(McCabe Cyclomatic Complexity)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '맥케이브 복잡도는 제어 흐름 그래프를 기반으로 소스 코드의 복잡도를 정량화한 지표이다. '
                 '독립적인 실행 경로 수와 테스트 최소 케이스 수를 파악하는 데 사용된다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','복잡도 계산식',
                 'headers', JSON_ARRAY('계산식','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('V(G) = E - N + 2',
                              '간선 수(E)와 노드 수(N)로부터 복잡도 V(G)를 계산'),
                   JSON_ARRAY('V(G) = P + 1',
                              '조건 분기문 개수(P)에 1을 더해 복잡도를 계산')
                 )
               ),

               JSON_OBJECT('type','heading','text','배드 코드와 클린 코드'),
               JSON_OBJECT(
                 'type','table',
                 'caption','배드 코드 유형 예',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('외계인 코드',
                              '개발자·문서가 없어 이해·유지보수가 매우 어려운 코드'),
                   JSON_ARRAY('스파게티 코드',
                              '흐름이 복잡하게 얽혀 구조 파악이 어려운 코드'),
                   JSON_ARRAY('높은 결합도·중복 로직',
                              '변경 영향이 크고 오류 가능성이 높은 구조')
                 )
               ),
               JSON_OBJECT(
                 'type','heading',
                 'text',
                 '클린 코드 원칙 (가단의 중추)'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','클린 코드 5대 원칙',
                 'headers', JSON_ARRAY('원칙','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('가독성',
                              '누구나 읽기 쉽게 이름·주석·들여쓰기를 명확히 한다.'),
                   JSON_ARRAY('단순성',
                              '한 번에 한 가지 일만 하도록 코드를 단순하게 유지한다.'),
                   JSON_ARRAY('의존성 최소',
                              '다른 모듈에 대한 의존을 줄여 변경 영향 범위를 축소한다.'),
                   JSON_ARRAY('중복성 제거',
                              '중복 코드를 공통화하여 유지보수성을 높인다.'),
                   JSON_ARRAY('추상화',
                              '상위 수준에서는 개념만, 세부 구현은 하위 수준에 두어 계층을 명확히 한다.')
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);
