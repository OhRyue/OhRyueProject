-- ============================
-- 1.2.1 UI 요구사항 / 화면 설계
-- topic_id = 11201 (@topic_ui_req)
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 11201,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              1.2.1.1 UI / UX 기본 개념과 UI 유형(CGNO)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.2.1.1',
             'title', 'UI / UX 기본 개념과 UI 유형(CGNO)',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','UI와 UX의 기본 개념'),
               JSON_OBJECT(
                 'type','table',
                 'caption','UI vs UX 비교',
                 'headers', JSON_ARRAY('구분','정의'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'UI (User Interface)',
                     '사용자와 시스템이 상호작용하는 화면·입력 장치 등 “눈에 보이는 접점”으로, 배우기 쉽고 사용하기 쉬운 형태로 설계되어야 한다.'
                   ),
                   JSON_ARRAY(
                     'UX (User eXperience)',
                     '제품·서비스를 사용하면서 사용자가 직·간접적으로 느끼는 총체적 경험(만족감, 편의성, 감정 등)을 의미한다.'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','UI 유형 – CGNO'),
               JSON_OBJECT(
                 'type','table',
                 'caption','UI 유형(CGNO)',
                 'headers', JSON_ARRAY('유형','특징','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'CLI',
                     '정적 텍스트 기반',
                     '명령어를 텍스트로 입력해 조작하는 인터페이스로, DOS·UNIX에서 사용하는 명령행 기반 UI.'
                   ),
                   JSON_ARRAY(
                     'GUI',
                     '그래픽 반응 기반',
                     '아이콘·버튼·창 등 그래픽 요소와 마우스/포인터를 이용하는 인터페이스.'
                   ),
                   JSON_ARRAY(
                     'NUI',
                     '직관적 사용자 반응 기반',
                     '키보드·마우스 없이 제스처, 터치, 음성 등 “자연스러운 동작”으로 조작하는 인터페이스.'
                   ),
                   JSON_ARRAY(
                     'OUI',
                     '유기적 상호작용 기반',
                     '현실 사물 자체가 입·출력 장치가 되는 형태로, 입출력 장치의 경계가 흐려지는 유기적 인터페이스.'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','NUI – 모바일 제스처 예시'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'Tap: 한 번 가볍게 누르기',
                   'Double Tap: 두 번 빠르게 누르기',
                   'Drag / Pan: 누른 채로 계속 움직이며 위치 변경',
                   'Press: 길게 누르기(롱프레스)',
                   'Flick: 빠르게 스크롤',
                   'Pinch: 두 손가락으로 벌리거나 모아 확대/축소'
                 )
               )
             )
           ),

           /* -------------------------------------
              1.2.1.2 UI 설계 원칙(직·유·학·유)과 설계 지침
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.2.1.2',
             'title', 'UI 설계 원칙(직·유·학·유)과 설계 지침',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','UI 설계 4원칙 – 직·유·학·유'),
               JSON_OBJECT(
                 'type','table',
                 'caption','UI 설계 원칙과 부특성',
                 'headers', JSON_ARRAY('원칙','설명','부특성 예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '직관성(Intuitiveness)',
                     '누구나 쉽게 이해하고 사용할 수 있어야 한다.',
                     '쉬운 탐색, 일관된 배치, 익숙한 용어 사용'
                   ),
                   JSON_ARRAY(
                     '유효성(Efficiency)',
                     '사용자의 목표를 정확하고 완전하게 달성할 수 있어야 한다.',
                     '단계 최소화, 오류 처리 및 복구 용이'
                   ),
                   JSON_ARRAY(
                     '학습성(Learnability)',
                     '초보자와 숙련자 모두 쉽게 배우고 기억할 수 있어야 한다.',
                     '쉬운 학습, 명확한 피드백, 일관된 패턴'
                   ),
                   JSON_ARRAY(
                     '유연성(Flexibility)',
                     '다양한 사용자의 인터랙션을 수용하고, 실수를 예방·포용할 수 있어야 한다.',
                     '오류 예방, 실수 포용, 오류 감지 및 안내'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','UI 설계 일반 지침'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '사용자 중심: 사용자의 직무·연령·환경을 고려한 설계.',
                   '일관성: 화면 간 용어·배치·동작이 일관되도록 구성.',
                   '단순성: 핵심 기능을 메인 화면에 노출해 조작 경로를 최소화.',
                   '결과 예측 가능: 버튼·링크 클릭 시 어떤 결과가 나올지 예측 가능해야 함.',
                   '가시성: 현재 상태, 가능한 행동, 오류 메시지가 눈에 잘 띄게 표시되어야 함.',
                   '접근성: 다양한 사용 계층(장애 여부, 기기 환경 등)을 고려한 접근성 확보.',
                   '오류 처리: 오류 발생 시 원인과 해결 방법을 명확히 안내.'
                 )
               )
             )
           ),

           /* -------------------------------------
              1.2.1.3 UI 표준/스타일 가이드 및 설계 도구
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.2.1.3',
             'title', 'UI 표준 / 스타일 가이드와 설계 도구·산출물',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','UI 표준과 스타일 가이드'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'UI 표준은 공통 디자인 철학과 원칙을 기반으로, 화면 간 이동·화면 구성·패턴 등을 시스템 전체에 일관되게 적용하기 위한 규약이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','UI 표준 구성 요소 (액·정·스·패·조)',
                 'headers', JSON_ARRAY('구성 요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('전체 UX 원칙','서비스 전반에 적용될 사용자 경험 방향성 정의'),
                   JSON_ARRAY('정책 및 철학','브랜드·서비스 철학, 디자인 정책'),
                   JSON_ARRAY('UI 스타일 가이드','색상, 폰트, 버튼, 입력창 등 UI 요소 규격'),
                   JSON_ARRAY('UI 패턴 모델 정의','반복되는 화면 패턴·컴포넌트 정의'),
                   JSON_ARRAY('조직/관리 체계','표준 유지·개선을 담당하는 조직·프로세스')
                 )
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'UI 스타일 가이드 예: 구동 환경(브라우저·OS), 레이아웃, 내비게이션, 공통 컴포넌트, 인터랙션 규칙 등 정의.',
                   '표준은 많은 업무 케이스를 포괄하면서도, 변경·확장이 쉽도록 관리 조직이 필요하다.'
                 )
               ),

               JSON_OBJECT('type','heading','text','UI 설계 도구와 산출물'),
               JSON_OBJECT(
                 'type','table',
                 'caption','UI 설계 도구 비교',
                 'headers', JSON_ARRAY('도구','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '와이어프레임(Wireframe)',
                     '화면 단위 레이아웃·구성 요소를 단순하게 표현해, 이해관계자 간 화면 구성을 빠르게 공유하기 위한 스케치.'
                   ),
                   JSON_ARRAY(
                     '스토리보드(Storyboard)',
                     '정책, 프로세스, 콘텐츠 구성, 와이어프레임, 기능 정의, DB 연동 정보까지 포함한 “종합 설계 문서”.'
                   ),
                   JSON_ARRAY(
                     '프로토타입(Prototype)',
                     '와이어프레임/스토리보드에 동작·전환 효과를 입혀 실제처럼 시뮬레이션할 수 있는 동적 모형.'
                   ),
                   JSON_ARRAY(
                     '목업(Mockup)',
                     '실제 화면과 매우 유사하게 디자인된 정적 모형으로, 주로 디자인·사용 방법 설명·평가에 사용되며 실제 기능은 동작하지 않는다.'
                   ),
                   JSON_ARRAY(
                     '유스케이스(Use Case)',
                     '사용자가 목표를 달성하기 위해 수행하는 시나리오를 텍스트·다이어그램 형태로 표현한 것.'
                   )
                 )
               )
             )
           ),

           /* -------------------------------------
              1.2.1.4 UI 설계 프로세스 / 분석 기법 / 감성공학
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '1.2.1.4',
             'title', 'UI 설계 프로세스와 분석 기법, 감성공학',
             'importance', 3,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','UI 설계 프로세스 (문·사·작·컴·인·디)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','UI 설계 프로세스 단계',
                 'headers', JSON_ARRAY('단계','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('문제 정의','해결해야 할 비즈니스·사용자 문제를 정의한다.'),
                   JSON_ARRAY('사용자 모델 정의','타깃 사용자, 페르소나, 사용 맥락을 정의한다.'),
                   JSON_ARRAY('작업 분석','사용자가 수행하는 작업·시나리오를 분석한다.'),
                   JSON_ARRAY('컴퓨터 오브젝트 및 기능 정의','작업을 시스템 기능·오브젝트로 매핑한다.'),
                   JSON_ARRAY('UI 정의','화면 요소·흐름·입력/출력 방식을 정의한다.'),
                   JSON_ARRAY('디자인 평가','프로토타입·사용성 테스트 등을 통해 UI를 검증·개선한다.')
                 )
               ),

               JSON_OBJECT('type','heading','text','UI 흐름 설계 & 분석 기법'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'UI 흐름 설계: (1) 화면에 표현될 기능 정리 → (2) 화면 입력 요소 확인 → (3) UI 요구사항 기반 유스케이스 설계 → (4) 기능·양식(폼) 확인.',
                   '3C 분석: 고객(Customer)·자사(Company)·경쟁사(Competitor)를 비교해 차별화 전략을 도출.',
                   'SWOT 분석: 강점(Strength), 약점(Weakness), 기회(Opportunity), 위협(Threat)을 분석해 전략을 수립.',
                   '시나리오 플래닝: 불확실한 미래 상황을 여러 시나리오로 가정해 대응 전략을 세우는 기법.',
                   '사용성 테스트: 실제 사용자가 과제를 수행하는 과정을 관찰하며 문제점을 분석.',
                   '워크숍: 소규모 그룹이 모여 아이디어·지식·방법을 공유·정리하는 협업 활동.'
                 )
               ),

               JSON_OBJECT('type','heading','text','감성공학(Sensibility Ergonomics) 개요'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '감성공학은 “인간이 느끼는 감성”을 제품 설계에 반영해, 사용자가 느끼는 이미지·만족감·편안함 등을 체계적으로 구현하려는 공학적 접근이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','감성공학 접근 방법 (1·2·3류)',
                 'headers', JSON_ARRAY('구분','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '1류 접근 방법',
                     '감성을 표현하는 어휘(예: 고급스러운, 친근한 등)를 조사·분석해 제품 디자인 요소와 연결하는 방법.'
                   ),
                   JSON_ARRAY(
                     '2류 접근 방법',
                     '연령·성별·생활 방식 등 개인 특성을 기반으로, 심리적 감성을 구체화하는 방법.'
                   ),
                   JSON_ARRAY(
                     '3류 접근 방법',
                     '인간의 감각을 계측하고 수학적 모델로 표현하는 등, 공학적·정량적 기법으로 감성을 다루는 방법.'
                   )
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);