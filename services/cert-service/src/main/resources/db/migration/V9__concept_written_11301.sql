-- ============================
-- 1.3.1 공통 모듈 설계
-- topic_id = 11301 (@topic_common_module)
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 11301,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              1.3.1.1 공통 모듈과 모듈 독립성
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.3.1.1',
             'title', '공통 모듈과 모듈 독립성',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','모듈 / 공통 모듈 개념'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '모듈(Module): 독립된 하나의 소프트웨어/하드웨어 단위로, 서브루틴·작업 단위 등으로 기능을 캡슐화한 것.',
                   '모듈화(Modularity): 복잡한 시스템을 이해·수정·재사용·유지보수가 쉽도록 기능 단위로 분해하는 설계·구현 기법.',
                   '공통 모듈: 전체 프로그램 기능 중 반복적으로 사용하는 특정 기능을 별도 실행 코드로 묶어, 여러 프로그램에서 재사용할 수 있도록 만든 모듈.'
                 )
               ),

               JSON_OBJECT('type','heading','text','좋은 모듈의 기본 특징'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '각 모듈은 상대적으로 독립성이 높을수록 좋다.',
                   '단독 컴파일·테스트가 가능하고, 다른 곳에서 재사용하기 쉽다.',
                   '독립성이 높을수록 수정 시 다른 모듈에 미치는 영향이 작고, 오류 추적이 쉽다.',
                   '독립성은 “결합도는 낮게, 응집도는 높게, 크기는 적당히 작게” 설계하는 것으로 향상된다.'
                 )
               ),

               JSON_OBJECT('type','heading','text','공통 모듈 명세 원칙 (정·명·완·일·추)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','공통 모듈 명세 작성 원칙',
                 'headers', JSON_ARRAY('원칙','키워드','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '정확성 (Correctness)',
                     '필요성',
                     '시스템 구현 시 해당 기능이 “왜 필요한지”를 알 수 있도록 정확하게 작성한다.'
                   ),
                   JSON_ARRAY(
                     '명확성 (Clarity)',
                     '단일 해석',
                     '누가 읽어도 한 가지 의미로만 해석되도록, 모호한 표현을 피하고 일관된 용어를 사용한다.'
                   ),
                   JSON_ARRAY(
                     '완전성 (Completeness)',
                     '누락 없음',
                     '해당 기능을 구현하는 데 필요한 입력·출력·예외·제약사항을 빠짐없이 기술한다.'
                   ),
                   JSON_ARRAY(
                     '일관성 (Consistency)',
                     '충돌 방지',
                     '다른 공통 기능들과 정책·형식·인터페이스가 충돌하지 않도록 규칙을 맞춘다.'
                   ),
                   JSON_ARRAY(
                     '추적성 (Traceability)',
                     '출처 연결',
                     '요구사항 ID, 관련 시스템, 연관 모듈 등 “어디에서 왔는지”를 추적할 수 있게 연결 정보를 남긴다.'
                   )
                 )
               )
             )
           ),

           /* -------------------------------------
              1.3.1.2 결합도와 팬인·팬아웃
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.3.1.2',
             'title', '결합도와 팬인·팬아웃',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','결합도(Coupling) – 약할수록 좋은 이유'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '결합도는 모듈 간 “얼마나 강하게 연결되어 있는지”를 나타내는 지표로, 약할수록(낮을수록) 모듈 교체·수정·테스트가 쉬워진다. '
                 '시험에서는 결합도 단계의 순서와 각 결합도의 정의를 자주 묻는다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','결합도 단계 (약한 → 강한)',
                 'headers', JSON_ARRAY('순서','결합도','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('①','자료 결합도 (Data)','단순 값(파라미터)만 주고받는 가장 바람직한 형태.'),
                   JSON_ARRAY('②','스탬프 결합도 (Stamp)','배열·레코드 등 자료 구조를 통째로 전달하는 결합도.'),
                   JSON_ARRAY('③','제어 결합도 (Control)','제어 플래그·코드 등을 전달해 상대 모듈의 내부 흐름까지 제어하는 결합도.'),
                   JSON_ARRAY('④','공통 결합도 (Common)','공유되는 공통 데이터 영역을 여러 모듈이 함께 사용하는 결합도.'),
                   JSON_ARRAY('⑤','내용 결합도 (Content)','한 모듈이 다른 모듈의 내부 기능·자료를 직접 참조/수정하는 최악의 결합도.')
                 )
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','자주 언급되는 결합도 유형 정리',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('자료 결합도','인터페이스가 순수한 데이터 요소(값)로만 구성된다.'),
                   JSON_ARRAY('스탬프 결합도','구조체·객체·배열 등 복합 자료구조를 전달한다.'),
                   JSON_ARRAY('제어 결합도','분기 여부 등 제어 정보를 전달해 상대 모듈의 처리 경로를 결정한다.'),
                   JSON_ARRAY('외부 결합도','외부 장치·파일·환경 등 공통 외부 자원을 함께 참조한다.'),
                   JSON_ARRAY('공통 결합도','공용(글로벌) 데이터 영역을 여러 모듈이 읽고/쓴다.'),
                   JSON_ARRAY('내용 결합도','다른 모듈의 내부 변수·코드를 직접 참조하거나 점프한다.')
                 )
               ),

               JSON_OBJECT('type','heading','text','팬인(Fan-In) / 팬아웃(Fan-Out)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','팬인·팬아웃 비교',
                 'headers', JSON_ARRAY('구분','정의','설계 관점 포인트'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '팬인 (Fan-In)',
                     '해당 모듈을 호출하는 상위 모듈의 수.',
                     '팬인이 높다는 것은 “여러 곳에서 재사용된다”는 의미지만, 단일 장애점(SPOF)이 될 수 있어 변경·테스트 시 영향 범위를 주의해야 한다.'
                   ),
                   JSON_ARRAY(
                     '팬아웃 (Fan-Out)',
                     '해당 모듈이 호출하는 하위 모듈의 수.',
                     '팬아웃이 너무 높으면 한 모듈이 지나치게 많은 다른 모듈을 직접 제어하는 구조로, 설계를 단순화하거나 중간 계층 모듈을 도입할 필요가 있다.'
                   )
                 )
               )
             )
           ),

           /* -------------------------------------
              1.3.1.3 모듈화와 설계 기법 (상향식 / 하향식)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.3.1.3',
             'title', '모듈화와 설계 기법 (상향식 / 하향식)',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','모듈화(Modularity)의 목적'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '소프트웨어 성능 향상: 불필요한 중복을 줄이고, 책임을 분리해 최적화 지점을 명확히 한다.',
                   '변경·유지보수 용이: 변경 범위를 한 모듈 안으로 가둬 다른 부분에 미치는 영향을 최소화한다.',
                   '재사용성 향상: 자주 쓰는 기능을 공통 모듈로 만들어 여러 시스템에서 공유한다.',
                   '이해도 향상: 복잡한 시스템을 “의미 있는 작은 단위”로 쪼개 전체 구조를 파악하기 쉽게 한다.'
                 )
               ),

               JSON_OBJECT('type','heading','text','하향식 / 상향식 설계 기법'),
               JSON_OBJECT(
                 'type','table',
                 'caption','설계 기법 비교',
                 'headers', JSON_ARRAY('기법','설명','특징·유의점'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '하향식 설계',
                     '시스템 상위 수준의 개략적인 구조에서 시작해 점점 세부 기능으로 분해해 나가는 방식.',
                     '상위에서 인터페이스가 먼저 정의되어 통합이 쉽지만, 초기 단계에 세부 데이터 구조를 충분히 고려해야 한다.'
                   ),
                   JSON_ARRAY(
                     '상향식 설계',
                     '낮은 레벨(세부 기능) 모듈들을 먼저 설계·구현하고, 이를 결합해 상위 구조를 형성하는 방식.',
                     '각 모듈의 재사용성을 높이기 좋지만, 인터페이스가 이미 굳어 있으면 새로운 기능 통합 시 제약이 생길 수 있다.'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','바람직한 모듈 설계 방안 요약'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '결합도는 낮게, 응집도는 높게 설계해 독립성을 최대화한다.',
                   '모듈의 복잡도와 중복을 줄이고, 이름·인터페이스·예외 처리 규칙 등을 일관되게 맞춘다.',
                   '모듈 기능은 “한눈에 예측 가능”해야 하며, 지나치게 범위가 크거나 제한적이지 않도록 균형을 맞춘다.',
                   '계층 구조를 명확히 해 상위 모듈이 “무엇을”, 하위 모듈이 “어떻게”를 담당하도록 역할을 분리한다.'
                 )
               )
             )
           ),

           /* -------------------------------------
              1.3.1.4 설계 모델링·코드·HIPO 한눈에 보기
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '1.3.1.4',
             'title', '설계 모델링·코드·HIPO 한눈에 보기',
             'importance', 3,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','설계 모델링과 설계 유형'),
               JSON_OBJECT(
                 'type','table',
                 'caption','상위 설계 / 하위 설계',
                 'headers', JSON_ARRAY('구분','유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '상위 설계',
                     '자료 구조 설계',
                     '요구분석에서 도출된 정보를 실제 구현에 사용할 자료 구조로 변환한다.'
                   ),
                   JSON_ARRAY(
                     '상위 설계',
                     '아키텍처 설계',
                     '시스템 전체 구조와 주요 컴포넌트, 컴포넌트 간 관계를 정의한다.'
                   ),
                   JSON_ARRAY(
                     '상위 설계',
                     '인터페이스 설계',
                     '시스템과 외부 시스템·사용자 간의 통신 방식과 인터페이스 규격을 정의한다.'
                   ),
                   JSON_ARRAY(
                     '상위 설계',
                     '프로시저 설계',
                     '아키텍처의 컴포넌트를 구체적인 절차(Procedure) 설계로 내려 적는다.'
                   ),
                   JSON_ARRAY(
                     '하위 설계',
                     '모듈 설계',
                     '각 컴포넌트를 실제 코드 수준의 모듈 단위로 분해·구체화한다.'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','코드 설계 – 자주 나오는 코드 유형'),
               JSON_OBJECT(
                 'type','table',
                 'caption','대표 코드 설계 기법',
                 'headers', JSON_ARRAY('유형','핵심 개념','예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '연상 코드 (Mnemonic)',
                     '코드만 보고 대상을 연상할 수 있도록 의미 있는 문자·약어를 사용하는 코드.',
                     '국가 코드: KR(한국), US(미국) 등'
                   ),
                   JSON_ARRAY(
                     '블록 코드 (Block)',
                     '공통성이 있는 대상들을 블록으로 나누고, 각 블록 내에서 일련번호를 부여하는 방식.',
                     '전화번호의 지역번호-국번-일련번호 구조'
                   ),
                   JSON_ARRAY(
                     '순차 코드 (Sequence)',
                     '특정 기준에 따라 순서대로 번호를 부여하는 방식.',
                     '사번·학번을 0001, 0002, … 순서로 부여'
                   ),
                   JSON_ARRAY(
                     '표의 숫자 코드 (Significant Digit)',
                     '길이·넓이·용량 등 물리적 수치를 코드에 직접 반영하는 방식.',
                     '20-10-300 (길이-넓이-용량)'
                   ),
                   JSON_ARRAY(
                     '10진 코드 (Decimal)',
                     '10진수만으로 표현하는 코드.',
                     '상품 바코드의 숫자 코드'
                   ),
                   JSON_ARRAY(
                     '그룹 분류식 코드',
                     '대상들을 대·중·소 분류로 나누고 계층적으로 번호를 부여하는 방식.',
                     '학번: 입학년도-전공-일련번호'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','HIPO(Hierarchy Input Process Output) 개요'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'HIPO는 입력(Input)–처리(Process)–출력(Output) 관점에서 시스템 기능을 계층적으로 표현하는 문서화 도구로, '
                 '하향식 설계와 잘 어울리며 분석·설계·문서화를 동시에 지원한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','HIPO 차트 종류 (가·총·세)',
                 'headers', JSON_ARRAY('종류','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '가시적 도표 (Visual TOC)',
                     '시스템의 전체 기능과 계층 구조를 한눈에 보여주는 목차형 구조도.'
                   ),
                   JSON_ARRAY(
                     '총체적 도표 (Overview Diagram)',
                     '각 기능 블록에 대해 입력·처리·출력의 개요를 보여주는 도표.'
                   ),
                   JSON_ARRAY(
                     '세부적 도표 (Detail Diagram)',
                     '총체적 도표의 각 기능을 더 작은 단계로 세분화해 상세 흐름을 기술하는 도표.'
                   )
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);
