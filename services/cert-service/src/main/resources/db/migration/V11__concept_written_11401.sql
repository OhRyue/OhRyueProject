-- ============================
-- 1.4.1 인터페이스 요구사항
-- topic_id = 11401
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 11401,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           -- 1.4.1.1 내·외부 인터페이스 요구사항의 개념
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.4.1.1',
             'title', '내·외부 인터페이스 요구사항의 개념',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','내·외부 인터페이스 요구사항이란?'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '내·외부 인터페이스 요구사항은 조직 내·외부에 존재하는 시스템들이 상호 접속을 통해 '
                 '특정 기능을 수행하기 위한 접속 방법·규칙에 대한 필수적인 요구사항입니다.'
               ),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '즉, “어떤 시스템이, 어떤 데이터를, 어떤 방식으로, 어느 정도의 품질로 주고받을 것인가?”를 '
                 '명확하게 정의한 것이 내·외부 인터페이스 요구사항이라고 볼 수 있습니다.'
               )
             )
           ),

           -- 1.4.1.2 인터페이스 요구사항 구성 요소
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.4.1.2',
             'title', '인터페이스 요구사항 구성 요소',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','인터페이스 요구사항 구성 항목'),
               JSON_OBJECT(
                 'type','table',
                 'caption','내·외부 인터페이스 요구사항 구성',
                 'headers', JSON_ARRAY('항목','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('인터페이스 이름','연계 기능을 식별할 수 있는 고유 명칭'),
                   JSON_ARRAY('연계 대상 시스템','어떤 시스템과 연계하는지(내부/외부, 시스템명)'),
                   JSON_ARRAY('연계 범위 및 내용','어떤 업무/데이터가 연계되는지에 대한 범위'),
                   JSON_ARRAY('연계 방식','직접/간접, 실시간/배치 등 연계 유형'),
                   JSON_ARRAY('송신 데이터','전송되는 데이터 항목·형식·코드 체계'),
                   JSON_ARRAY('인터페이스 주기','실시간, 분/시간/일 단위, 배치 시간 등'),
                   JSON_ARRAY('기타 고려사항','보안, 장애 처리, 로깅, 모니터링 등 특이 사항')
                 )
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '요구사항 정의서에서 “시스템 간 연계가 필요한 부분”을 따로 모아 인터페이스 요구사항으로 정리합니다.',
                   '각 항목은 시험에서 그대로 보기로 출제되므로, 구성 요소 이름과 의미를 세트로 암기해 두는 것이 좋습니다.'
                 )
               )
             )
           ),

           -- 1.4.1.3 기능/비기능 요구사항 (기완일 / 신사효유이)
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.4.1.3',
             'title', '기능/비기능 요구사항 분류 (기완일 / 신사효유이)',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','기능 요구사항 (기완일)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','기능 요구사항(Functional Requirements)',
                 'headers', JSON_ARRAY('키워드','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '기능성(Functionality)',
                     '시스템이 제공해야 하는 기능의 종류(무엇을 할 것인가?)를 정의.'
                   ),
                   JSON_ARRAY(
                     '완전성(Completeness)',
                     '사용자가 기대하는 기능이 빠짐없이 포함되었는지 여부.'
                   ),
                   JSON_ARRAY(
                     '일관성(Consistency)',
                     '기능 간 정의·동작·규칙이 서로 충돌 없이 일관되게 유지되는지 여부.'
                   )
                 )
               ),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '기능 요구사항은 “시스템이 무엇을 해야 하는가?”를 정의합니다. '
                 '예를 들어, 주문 등록, 결제 처리, 포인트 적립과 같이 사용자가 기대하는 서비스 기능이 여기에 포함됩니다.'
               ),

               JSON_OBJECT('type','heading','text','비기능 요구사항 (신사효유이)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','비기능 요구사항(Non-functional Requirements)',
                 'headers', JSON_ARRAY('키워드','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('신뢰성(Reliability)','오류 없이 안정적으로 동작하는 정도'),
                   JSON_ARRAY('사용성(Usability)','배우기 쉽고 사용하기 쉬운 정도'),
                   JSON_ARRAY('효율성(Efficiency)','응답 시간, 처리량 등 성능 관련 요구'),
                   JSON_ARRAY('유지보수성(Maintainability)','변경·수정·보수가 얼마나 쉬운지'),
                   JSON_ARRAY('이식성(Portability)','다른 환경으로 옮겨가도 잘 동작하는 정도')
                 )
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '추가로 장비 구성, 성능, 인터페이스, 데이터, 테스트, 보안, 품질, 제약, 관리, 지원 요구사항 등이 비기능 요구사항에 포함됩니다.',
                   '시험에서는 “기완일 / 신사효유이” 암기어와 함께 기능/비기능 예시를 구분하는 문제가 자주 출제됩니다.'
                 )
               )
             )
           ),

           -- 1.4.1.4 인터페이스 요구사항 식별·분류 프로세스
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '1.4.1.4',
             'title', '내·외부 인터페이스 요구사항 식별·분류 프로세스',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','요구사항 식별 및 분류 순서'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '① 요구사항 식별: 전체 요구사항 중 시스템 간 연계가 필요한 부분을 찾아냅니다.',
                   '② 내·외부 인터페이스 관련 명세서·현황 자료 준비: 기존 인터페이스 문서, 운영 매뉴얼, 로그 등을 수집합니다.',
                   '③ 기능/비기능 요구사항 분류: 기능(무엇을)과 비기능(어떻게)을 구분해 인터페이스 관점에서 재정리합니다.'
                 )
               ),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '요구사항을 제대로 분류해 두어야, 이후 상세 설계 단계에서 누락·중복을 줄이고 성능·보안 같은 품질 요구를 명확하게 반영할 수 있습니다.'
               )
             )
           ),

           -- 1.4.1.5 인터페이스 요구사항 명세서 구체화 (세이신정)
           JSON_OBJECT(
             'orderNo', 5,
             'subCode', '1.4.1.5',
             'title', '인터페이스 요구사항 명세서 구체화 (세이신정)',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','내·외부 인터페이스 요구사항 명세서 구체화 프로세스'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '① 요구사항 정의서 세분화: “내·외부 연계 관점”에서 필요한 세부 항목으로 쪼갭니다.',
                   '② 내·외부 인터페이스 요구사항 내용 이해 및 수정: 용어·범위를 명확히 다듬습니다.',
                   '③ 누락된 요구사항 신규 정의: 기존 시스템, 운영 시나리오를 보며 빠진 연계를 보완합니다.',
                   '④ 내·외부 인터페이스 요구사항 정리: 표나 정의서 양식으로 정리해 공유 가능한 문서를 만듭니다.'
                 )
               ),
               JSON_OBJECT(
                 'type','callout',
                 'title','암기 포인트',
                 'body',
                 '“세이신정” 순서를 물어보는 문제에 대비해, 각 단계의 의미와 함께 흐름을 기억해 두는 것이 좋습니다.'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);
