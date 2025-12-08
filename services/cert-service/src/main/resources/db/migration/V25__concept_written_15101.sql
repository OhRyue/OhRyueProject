-- =========================================
-- 5.1.1 소프트웨어 개발방법론 선정 (concept)
-- topic_id = 15101
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 15101,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------
              5.1.1.1 소프트웨어 생명주기 모델
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '5.1.1.1',
             'title', '소프트웨어 생명주기(SDLC)와 주요 모델',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','소프트웨어 생명주기(SDLC; Software Development Life Cycle)'),
               JSON_OBJECT('type','paragraph','text',
                 '소프트웨어 생명주기는 시스템 요구분석부터 유지보수까지의 전 공정을 체계화한 절차로, '
                 '개발 과정 전반을 관리하기 위한 기본 틀이다.'
               ),
               JSON_OBJECT('type','heading','text','SDLC 공통 프로세스 (요·설·구·테·유)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '요구사항 분석: 사용자의 요구 및 시스템 범위 정의',
                   '설계: 구조·아키텍처·인터페이스 설계',
                   '구현: 설계에 따른 실제 코드 작성',
                   '테스트: 단위·통합·시스템·인수 테스트 수행',
                   '유지보수: 운영 중 오류 수정 및 기능 개선'
                 )
               ),
               JSON_OBJECT('type','heading','text','소프트웨어 생명주기 모델 종류 (폭·프·나·반)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','대표 SDLC 모델',
                 'headers', JSON_ARRAY('모델','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '폭포수 모델(Waterfall Model)',
                     '각 단계를 명확히 마무리한 후 다음 단계로 진행하는 선형 순차 모형. '
                     '타당성 검토→계획→요구분석→설계→구현→테스트→유지보수 순으로 진행하는 고전적 생명주기 모형이다. [2020년 4회]'
                   ),
                   JSON_ARRAY(
                     '프로토타이핑 모델(Prototyping Model)',
                     '고객 요구 기반 주요 기능을 시제품(프로토타입)으로 구현하고, 피드백을 반영해 점진적으로 완성해 가는 모델.'
                   ),
                   JSON_ARRAY(
                     '나선형 모델(Spiral Model)',
                     '보헴(Bohem)이 제안한 위험 중심 반복형 모델. '
                     '계획 수립→위험 분석→개발 및 검증→고객 평가 단계를 반복하며 점진적으로 완성한다. [2020년 4회, 2022년 3회]'
                   ),
                   JSON_ARRAY(
                     '반복적 모델(Iteration Model)',
                     '구축 대상을 여러 부분으로 나누어 병렬·반복 개발 후 통합하여 점증적으로 완성하는 모델.'
                   )
                 )
               )
             )
           ),

           /* -------------------------------
              5.1.1.2 소프트웨어 개발방법론 개요
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '5.1.1.2',
             'title', '소프트웨어 개발방법론 개요와 분류',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','소프트웨어 개발방법론의 정의'),
               JSON_OBJECT('type','paragraph','text',
                 '소프트웨어 개발방법론은 소프트웨어 개발 전 과정에 걸쳐 반복·지속적으로 적용 가능한 '
                 '방법·절차·기법을 체계화한 것이다.'
               ),
               JSON_OBJECT('type','heading','text','개발방법론의 주요 종류 (구·정·객·컴·애·제)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','소프트웨어 개발방법론 분류',
                 'headers', JSON_ARRAY('방법론','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '구조적 방법론(Structured Development)',
                     '프로세스 중심 하향식(Top-Down) 방법론. '
                     '구조적 프로그래밍 표현을 위해 나씨-슈나이더만 차트를 사용한다.'
                   ),
                   JSON_ARRAY(
                     '정보공학 방법론(Information Engineering)',
                     '정보시스템 개발 절차와 작업 기법을 체계화한 방법론으로, '
                     '개발 주기를 기준으로 대형 프로젝트를 수행하는 데 적합.'
                   ),
                   JSON_ARRAY(
                     '객체지향 방법론(Object-Oriented)',
                     '현실 세계를 객체 단위로 모델링하여 분석·설계하는 방법론.'
                   ),
                   JSON_ARRAY(
                     '컴포넌트 기반 방법론(CBD; Component Based Development)',
                     '재사용 가능한 컴포넌트를 조립·구성하여 시스템을 개발하는 방법론. '
                     '개발 기간 단축, 확장성 향상, 재사용성 증대에 강점이 있다. [2020년 4회]'
                   ),
                   JSON_ARRAY(
                     '애자일 방법론(Agile Development)',
                     '사람·협업 중심의 경량 개발방법론. 변화에 유연·신속하게 대응하며 짧은 주기의 반복 개발과 '
                     '지속적인 고객 피드백을 강조한다.'
                   ),
                   JSON_ARRAY(
                     '제품 계열 방법론(Product Line Development)',
                     '특정 제품군에 공통적으로 적용되는 기능을 정의하고, 이를 기반으로 관련 제품들을 '
                     '계열 형태로 개발하는 방법론.'
                   )
                 )
               )
             )
           ),

           /* -------------------------------
              5.1.1.3 나씨-슈나이더만 차트
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '5.1.1.3',
             'title', '나씨-슈나이더만 차트',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','나씨-슈나이더만(Nassi-Shneiderman) 차트 [2020년 4회]'),
               JSON_OBJECT('type','paragraph','text',
                 '나씨-슈나이더만 차트는 논리 기술에 중점을 둔 도형식 표현 방법으로, '
                 '연속·선택·다중 선택·반복 등의 제어 논리 구조를 직사각형 블록으로 표현한다.'
               ),
               JSON_OBJECT('type','paragraph','text',
                 '조건이 복합된 부분을 시각적으로 명확히 식별할 수 있어, '
                 '구조적 프로그램 설계에 적합한 표현 방법이다.'
               )
             )
           ),

           /* -------------------------------
              5.1.1.4 요구공학 방법론
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '5.1.1.4',
             'title', '요구공학 방법론(Requirements Engineering)',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','요구공학 방법론의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '요구공학 방법론은 사용자의 요구가 올바르게 반영된 시스템을 개발하기 위해 '
                 '요구사항 도출, 분석, 명세, 확인 및 검증을 구조화된 절차로 수행하는 방법론이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '요구 도출: 이해관계자로부터 요구를 수집',
                   '요구 분석: 상충 요구 해결, 범위·우선순위 결정',
                   '요구 명세: 요구사항을 문서화·모델링',
                   '요구 확인 및 검증: 요구가 일관·완전·타당한지 점검'
                 )
               )
             )
           ),

           /* -------------------------------
              5.1.1.5 비용산정 모델(분류·LOC·MM·COCOMO·Putnam·FP)
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 5,
             'subCode', '5.1.1.5',
             'title', '비용산정 모델 분류와 대표 기법',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','비용산정 모델의 분류 [2020년 4회]'),
               JSON_OBJECT(
                 'type','table',
                 'caption','하향식·상향식 비용산정 방법',
                 'headers', JSON_ARRAY('분류','설명','대표 기법'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '하향식 산정방법',
                     '경험이 풍부한 전문가에게 비용 산정을 의뢰하거나 '
                     '여러 전문가 의견을 조정·통합하여 추정하는 방식.',
                     '전문가 판단, 델파이 기법'
                   ),
                   JSON_ARRAY(
                     '상향식 산정방법',
                     '세부 기능·요구사항 단위로 필요한 작업량·비용을 산정한 후 합산하는 방식.',
                     'LOC, Man Month, COCOMO, Putnam, FP 모형'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','주요 비용산정 모델'),
               JSON_OBJECT(
                 'type','table',
                 'caption','비용산정 모델 정리',
                 'headers', JSON_ARRAY('모델','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'LOC(Lines of Code)',
                     '각 기능의 원시 코드 라인 수(LOC)의 낙관·중간·비관치를 측정해 예측치를 구하고, '
                     '이를 통해 인력·기간을 산정하는 방법.'
                   ),
                   JSON_ARRAY(
                     'Man Month',
                     '한 사람이 1개월 동안 수행할 수 있는 작업량을 기준으로 비용을 산정하는 기법. '
                     'Man Month = LOC / 프로그래머 월간 생산성, '
                     '프로젝트 기간 = Man Month / 인력 수.'
                   ),
                   JSON_ARRAY(
                     'COCOMO(COnstructive COst MOdel)',
                     '보헴이 제안한 모형으로, 프로그램 규모(DSI)에 따라 개발 노력(Man-Month)을 산정하는 경험적 모델.'
                   ),
                   JSON_ARRAY(
                     'Putnam 모형',
                     'Rayleigh-Norden 곡선의 노력 분포도를 이용한 프로젝트 비용산정 기법. '
                     '이를 기반으로 하는 자동화 추정 도구로 SLIM이 있다. [2020년 3회]'
                   ),
                   JSON_ARRAY(
                     '기능점수(FP; Function Point)',
                     '자료 입력·정보 출력·사용자 질의·데이터 파일·외부 인터페이스 등 기능점수를 기반으로 '
                     '규모와 비용을 산정하는 기법. [2020년 3회]'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','LOC 기반 개발 소요 기간 계산 예시'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '2020년 1회: LOC=50,000, 생산성=200라인/월, 인력=10명 → 기간=25개월',
                   '2022년 1회: LOC=36,000, 생산성=300라인/월, 인력=6명 → 기간=20개월'
                 )
               )
             )
           ),

           /* -------------------------------
              5.1.1.6 COCOMO 개발 유형
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 6,
             'subCode', '5.1.1.6',
             'title', 'COCOMO 개발 유형 [2020년 1회]',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','COCOMO의 소프트웨어 개발 유형'),
               JSON_OBJECT(
                 'type','table',
                 'caption','COCOMO 개발 유형과 규모',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '조직형(단순형, Organic Mode)',
                     '기관 내부에서 개발되는 중·소규모 소프트웨어로, '
                     '일괄 자료 처리·과학기술 계산·비즈니스 자료 처리 등에 적용. '
                     '약 50KDSI 이하 규모에 적합.'
                   ),
                   JSON_ARRAY(
                     '반 분리형(중간형, Semi-Detached Mode)',
                     '조직형과 임베디드형 중간 수준. '
                     '약 300KDSI 이하 규모 시스템에 적용.'
                   ),
                   JSON_ARRAY(
                     '임베디드형(Embedded Mode)',
                     '운영체제, 실시간 처리, 초대형 트랜잭션 처리 시스템 등 '
                     '복잡 제약 조건을 갖는 대규모 시스템 개발에 적용. '
                     '약 300KDSI 이상 규모.'
                   )
                 )
               ),
               JSON_OBJECT('type','paragraph','text',
                 'KDSI(Kilo Delivered Source Instruction)는 인도된 소스 코드 라인 수를 '
                 '1,000라인 단위로 표현한 규모 단위이다.'
               )
             )
           ),

           /* -------------------------------
              5.1.1.7 일정관리 모델 (CPM/PERT/CCPM)
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 7,
             'subCode', '5.1.1.7',
             'title', '일정관리 모델과 임계 경로(CPM)',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','대표 일정관리 모델'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'CPM(Critical Path Method): 공정 네트워크에서 프로젝트 기간을 결정하는 임계 경로를 찾는 기법.',
                   'PERT(Program Evaluation Review Technique): 낙관·중간·비관치를 기반으로 확률적 기간을 추정하는 기법.',
                   'CCPM(Critical Chain Project Management): 자원 제약을 고려하여 여유 buffer를 관리하는 기법.'
                 )
               ),
               JSON_OBJECT('type','heading','text','CPM 임계 경로 계산 예시(2020년 3회)'),
               JSON_OBJECT('type','paragraph','text',
                 'CPM에서는 네트워크 상에서 소요 기간의 합이 가장 긴 경로가 임계 경로가 되며, '
                 '그 경로의 기간이 프로젝트 전체 소요 기간이 된다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '경로1: A→B→C→D→H = 2+2+2+3 = 9일',
                   '경로2: A→B→E→G→H = 2+3+5+4 = 14일 (임계 경로)',
                   '경로3: A→F→G→H = 3+5+4 = 12일'
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);


-- =========================================
-- 5.1.2 소프트웨어 개발방법론 테일러링 (concept)
-- topic_id = 15102
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 15102,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------
              5.1.2.1 소프트웨어 개발 표준 (ISO/IEC 12207)
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '5.1.2.1',
             'title', '소프트웨어 개발 표준과 ISO/IEC 12207',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','소프트웨어 생명주기 프로세스 표준 – ISO/IEC 12207'),
               JSON_OBJECT('type','paragraph','text',
                 'ISO/IEC 12207은 소프트웨어 생명주기 전 과정에 대한 프로세스를 규정한 국제 표준으로, '
                 '프로세스 정의와 역할·활동·산출물 기준을 제공한다.'
               ),
               JSON_OBJECT('type','heading','text','ISO/IEC 12207의 프로세스 분류 (기·조·지)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','ISO/IEC 12207 프로세스 구성',
                 'headers', JSON_ARRAY('분류','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('기본 공정(Basic Processes)','개발·운영·유지보수 등 핵심 생명주기 공정'),
                   JSON_ARRAY('조직 공정(Organizational Processes)','조직 수준의 관리·개선·인프라 관련 공정'),
                   JSON_ARRAY('지원 공정(Supporting Processes)','형상관리·품질보증·검증·검사·감사 등 지원 공정')
                 )
               )
             )
           ),

           /* -------------------------------
              5.1.2.2 CMMI – 성숙도 모델
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '5.1.2.2',
             'title', 'CMMI 성숙도 모델과 구성',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','CMMI(Capability Maturity Model Integration)'),
               JSON_OBJECT('type','paragraph','text',
                 'CMMI는 기존 CMM을 통합·발전시킨 모델로, '
                 '소프트웨어 개발 조직의 프로세스 성숙도와 역량을 평가·개선하기 위한 프레임워크이다.'
               ),
               JSON_OBJECT('type','paragraph','text',
                 '표현 방식은 단계적(단계별 성숙도 레벨) 모델과 연속적(개별 프로세스 영역별 능력 수준) 모델이 있다.'
               ),
               JSON_OBJECT('type','heading','text','단계적 표현 모델의 성숙도 레벨 (초·관·정·관·최) [2020년 1회, 4회]'),
               JSON_OBJECT(
                 'type','table',
                 'caption','CMMI 성숙도 레벨',
                 'headers', JSON_ARRAY('레벨','단계명','특징'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('Level 1','초기(Initial)',
                              '프로세스가 비형식적·임시적 수준으로, 성공 여부가 개인 역량에 크게 의존.'),
                   JSON_ARRAY('Level 2','관리(Managed)',
                              '프로젝트 단위로 계획·추적·관리되는 수준. 기본적인 프로젝트 관리 프로세스 수립.'),
                   JSON_ARRAY('Level 3','정의(Defined)',
                              '조직 차원의 표준 프로세스를 수립하고 각 프로젝트에 맞게 Tailoring하여 사용하는 단계.'),
                   JSON_ARRAY('Level 4','정량적 관리(Quantitatively Managed)',
                              '프로세스·품질에 대해 정량적 측정과 통계적 관리가 수행되는 단계.'),
                   JSON_ARRAY('Level 5','최적화(Optimized)',
                              '지속적인 프로세스 개선과 혁신 활동이 이루어지는 최적화 단계.')
                 )
               ),
               JSON_OBJECT('type','heading','text','CMMI 구성 영역'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'SW-CMM: 소프트웨어 개발 프로세스',
                   'CE-CMM: 시스템 공학(시스템 엔지니어링)',
                   'IPD-CMM: 통합 제품 개발',
                   'People-CMM: 인적 역량·역량 관리',
                   'SA-CMM: 소프트웨어 획득',
                   'SECAM 등: 보안·기타 영역'
                 )
               )
             )
           ),

           /* -------------------------------
              5.1.2.3 SPICE (ISO/IEC 15504)
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '5.1.2.3',
             'title', 'SPICE 프로세스 수행 능력 수준',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','SPICE(Software Process Improvement and Capability dEtermination)'),
               JSON_OBJECT('type','paragraph','text',
                 'SPICE는 소프트웨어 프로세스 개선 및 능력 측정을 위한 국제 표준으로, '
                 '프로세스 수행 능력 수준을 0~5 단계로 정의한다. [2020년 4회]'
               ),
               JSON_OBJECT('type','heading','text','SPICE 프로세스 수행 능력 수준 (불·수·관·확·예·최)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','SPICE 능력 수준',
                 'headers', JSON_ARRAY('레벨','단계명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('Level 0','불안정(Incomplete)'),
                   JSON_ARRAY('Level 1','수행(Performed)'),
                   JSON_ARRAY('Level 2','관리(Managed)'),
                   JSON_ARRAY('Level 3','확립(Established)'),
                   JSON_ARRAY('Level 4','예측(Predictable)'),
                   JSON_ARRAY('Level 5','최적화(Optimizing)')
                 )
               )
             )
           ),

           /* -------------------------------
              5.1.2.4 개발방법론 테일러링
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '5.1.2.4',
             'title', '소프트웨어 개발방법론 테일러링',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','테일러링(Tailoring)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '테일러링은 조직의 표준 프로세스를 프로젝트 특성(비즈니스·기술·조직 환경)에 맞게 '
                 '커스터마이징하여 최적의 수행 프로세스를 도출하는 과정이다.'
               ),
               JSON_OBJECT('type','heading','text','테일러링 프로세스 (정·표·상·세·문)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '특징 정의: 프로젝트 목표·범위·제약·리스크 등 특성 분석',
                   '표준 프로세스 선정 및 검증: 조직 표준 프로세스 중 후보 선정·적합성 검토',
                   '상위 커스터마이징: 상위 단계에서 불필요 프로세스 제거·추가·병합 등 조정',
                   '세부 커스터마이징: 활동 수준에서 역할·산출물·절차를 구체 조정',
                   '문서화: 테일러링 결과를 문서화하여 이해관계자와 공유·승인'
                 )
               ),
               JSON_OBJECT('type','heading','text','테일러링 개발방법론 기준 (목·요·프·구·국·법) [2020년 1회]'),
               JSON_OBJECT(
                 'type','table',
                 'caption','테일러링 시 고려해야 할 내부·외부 기준',
                 'headers', JSON_ARRAY('구분','항목','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('내부 기준','목표 환경',
                              '적용 대상 시스템의 규모, 성격, 운영 환경, 인프라 등'),
                   JSON_ARRAY('내부 기준','요구사항',
                              '기능·비기능 요구사항, 품질 수준, 규제 요구 등'),
                   JSON_ARRAY('내부 기준','프로젝트 특성',
                              '일정·예산·리스크·조직 구조·협력업체 구성 등'),
                   JSON_ARRAY('내부 기준','구성원 능력',
                              '프로젝트 팀의 경험, 역량, 도메인 이해도'),
                   JSON_ARRAY('외부 기준','국제 표준·품질 기준',
                              'ISO/IEC 12207, ISO/IEC 25000, CMMI, SPICE 등 준수 여부'),
                   JSON_ARRAY('외부 기준','법적 규제',
                              '산업별 컴플라이언스, 개인정보·보안 관련 법규 등')
                 )
               ),
               JSON_OBJECT('type','heading','text','테일러링 추가 고려사항'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '프로젝트 도메인(금융·통신·공공·특수산업 등) 특성 반영',
                   'AS-IS 시스템 및 기존 산출물 재사용성 고려',
                   '다중 플랫폼(모바일·웹·임베디드 등) 여부에 따른 복수 방법론 적용 필요성',
                   '외부 환경·적용 기술·표준·산출물 요구사항 등에 대한 검토',
                   '테일러링 내역의 산출물화 및 관련 교육·공유'
                 )
               )
             )
           ),

           /* -------------------------------
              5.1.2.5 SW 개발 프레임워크 적용 효과
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 5,
             'subCode', '5.1.2.5',
             'title', '소프트웨어 개발 프레임워크와 기대 효과',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','소프트웨어 개발 프레임워크'),
               JSON_OBJECT('type','paragraph','text',
                 '소프트웨어 개발 프레임워크는 공통 구조와 컴포넌트, 개발·운영 도구를 제공하여 '
                 '일관된 아키텍처와 생산성을 확보하도록 지원하는 틀이다.'
               ),
               JSON_OBJECT('type','heading','text','프레임워크 적용 시 기대 효과 [2020년 1회]'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '품질 보증: 검증된 구조·컴포넌트 사용으로 품질 향상',
                   '개발 용이성: 공통 기능·도구 제공으로 개발 생산성 향상',
                   '변경 용이성: 계층 구조·모듈화로 변경·확장 용이',
                   '복잡도 감소: 반복·공통 기능을 프레임워크에 위임하여 시스템 복잡도 축소',
                   '상호 운용성 향상: 표준 기반 설계로 시스템 간 연계 용이',
                   '유지보수 용이: 공통 구조·코딩 규칙으로 유지보수 효율 향상',
                   '중복 예산 절감: 공통 컴포넌트 재사용으로 중복 개발 비용 절감'
                 )
               )
             )
           ),

           /* -------------------------------
              5.1.2.6 소프트웨어 재사용과 방법
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 6,
             'subCode', '5.1.2.6',
             'title', '소프트웨어 재사용 요소와 재사용 방법',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','소프트웨어 재사용 요소'),
               JSON_OBJECT('type','paragraph','text',
                 '소프트웨어 재사용은 기존 자산(코드·지식·문서 등)을 반복 활용해 품질과 생산성을 향상시키는 활동이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '전체 프로그램',
                   '부분 코드(모듈·클래스·컴포넌트)',
                   '응용된 도메인 지식',
                   '데이터 모형·구조',
                   '아키텍처·설계 구조',
                   '테스트 계획·케이스',
                   '문서화 템플릿·방법론'
                 )
               ),
               JSON_OBJECT('type','heading','text','소프트웨어 재사용 방법 [2020년 3회]'),
               JSON_OBJECT(
                 'type','table',
                 'caption','합성 중심 vs 생성 중심 재사용',
                 'headers', JSON_ARRAY('방법','설명','별칭'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '합성 중심(Composition-Based)',
                     '전자 칩과 같은 소프트웨어 부품(블록, 모듈, 컴포넌트)을 미리 만들어 두고 '
                     '이를 조립·결합하여 새로운 소프트웨어를 완성하는 방식.',
                     '블록 구성 방법'
                   ),
                   JSON_ARRAY(
                     '생성 중심(Generation-Based)',
                     '추상화된 명세를 기반으로 자동 생성 도구·패턴을 활용해 프로그램을 생성하는 방식.',
                     '패턴 구성 방법'
                   )
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);