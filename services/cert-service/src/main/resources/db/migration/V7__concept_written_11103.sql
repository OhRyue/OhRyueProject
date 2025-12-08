-- ============================
-- 1.1.3 애자일 / 분석 모델 / 분석 자동화 도구
-- topic_id = 11103
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 11103,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              1.1.3.1 애자일 개념과 선언문
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.1.3.1',
             'title', '애자일(Agile) 개념과 애자일 선언문',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','애자일(Agile) 개발 방법론의 개념'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '애자일 방법론은 소프트웨어를 짧은 주기로 개발하면서, 고객과의 지속적인 피드백을 통해 요구 변화에 유연하게 대응하는 반복·점진적 개발 방법론이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '요구사항을 기능 중심으로 정의하고, 작게 나누어 빠르게 구현·검증한다.',
                   '절차와 도구보다 개인과 상호작용(소통)을 중시한다.',
                   '짧은 반복 주기(스프린트 등)로 계획을 세워 요구 변화에 신속하게 대응한다.',
                   '문서보다는 실행 가능한 소프트웨어를 더 큰 가치로 둔다.',
                   '고객과의 피드백을 적극적으로 반영해 품질을 높인다.'
                 )
               ),

               JSON_OBJECT('type','heading','text','애자일 선언문 (개·변·동·고)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','애자일 선언문 – 네 가지 핵심 가치',
                 'headers', JSON_ARRAY('항목','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('개인과 상호작용','공정과 도구보다 개인과 상호작용을 더 중시한다.'),
                   JSON_ARRAY('변화 대응','계획을 따르기보다 변화에 대응하는 것을 더 중시한다.'),
                   JSON_ARRAY('동작하는 소프트웨어','포괄적인 문서보다 동작하는 소프트웨어를 더 중시한다.'),
                   JSON_ARRAY('고객과의 협력','계약 협상보다 고객과의 협력을 더 중시한다.')
                 )
               ),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '시험에서는 “공정과 도구 vs 개인과 상호작용, 문서 vs 동작하는 소프트웨어”처럼 대비되는 표현과, 약어(개·변·동·고)를 함께 묻는 문제가 자주 출제된다.'
               )
             )
           ),

           /* -------------------------------------
              1.1.3.2 XP / 린 / 스크럼
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.1.3.2',
             'title', 'XP / 린(Lean) / 스크럼(Scrum) 핵심 정리',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','XP(eXtreme Programming)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 'XP는 의사소통과 즉각적인 피드백을 통해 소프트웨어 품질을 높이고, 단순한 설계와 짧은 반복 주기(1~3주)를 통해 빠르게 개발하는 대표적인 애자일 방법론이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '변경이 잦고 불확실한 요구에 적합한 소규모 팀에 잘 어울린다.',
                   '개발 문서보다 “읽기 좋은 코드”와 자동화된 테스트에 더 큰 비중을 둔다.',
                   '5가지 가치(용·단·의·피·존): 용기(Courage), 단순성(Simplicity), 의사소통(Communication), 피드백(Feedback), 존중(Respect).',
                   '12가지 기본 원리: 짝 프로그래밍, 공통 코드 소유, 지속적 통합, 작은 릴리즈, TDD, 리팩토링, 40시간 작업, 고객 상주, 코드 표준 등.'
                 )
               ),

               JSON_OBJECT('type','heading','text','린(Lean) 소프트웨어 개발'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '린은 도요타 생산 방식의 린 시스템 개념을 소프트웨어 개발에 적용해, 낭비를 제거하고 품질과 흐름을 최적화하는 방법론이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','린(Lean)의 7가지 원칙 (낭·품·지·확·인·사·전)',
                 'headers', JSON_ARRAY('원칙','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('낭비 제거','가치가 없는 활동·중복 작업 제거'),
                   JSON_ARRAY('품질 내재화','개발 과정에 품질 활동을 내재시켜 결함을 조기에 제거'),
                   JSON_ARRAY('지식 창출','지속적인 학습과 개선을 통해 조직 지식을 축적'),
                   JSON_ARRAY('늦은 확정','충분한 정보가 모일 때까지 의사결정을 늦춰 리스크를 줄임'),
                   JSON_ARRAY('빠른 인도','작은 단위로 자주 전달해 피드백을 빠르게 받음'),
                   JSON_ARRAY('사람 존중','개발자·팀원의 자율성과 책임을 존중'),
                   JSON_ARRAY('전체 최적화','부분 최적화가 아닌 전체 흐름과 가치 사슬을 최적화')
                 )
               ),

               JSON_OBJECT('type','heading','text','스크럼(Scrum)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '스크럼은 매일 정해진 시간·장소에서 짧은 미팅을 진행하며, 2~4주 스프린트를 반복해 제품을 점진적으로 개선하는 프로젝트 관리 중심 애자일 프레임워크이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','스크럼의 역할',
                 'headers', JSON_ARRAY('역할','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('제품 책임자(Product Owner)','제품 백로그 우선순위를 관리하고, 고객 가치 극대화를 책임진다.'),
                   JSON_ARRAY('스크럼 마스터(Scrum Master)','스크럼 규칙을 지키도록 돕고, 장애 요인을 제거하는 퍼실리테이터 역할을 한다.'),
                   JSON_ARRAY('개발팀(Development Team)','스프린트 내 백로그 항목을 구현하는 자기 조직화 개발팀이다.')
                 )
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '제품 백로그(Product Backlog): 구현해야 할 요구사항·아키텍처 항목 목록.',
                   '스프린트(Sprint): 2~4주 반복 개발 기간, 각 스프린트마다 동작하는 증분 결과물을 목표로 한다.',
                   '속도(Velocity): 한 스프린트에서 처리 가능한 백로그 양의 추정치.',
                   '주요 이벤트: 스프린트 계획 회의, 일일 스크럼, 스프린트 검토, 스프린트 회고.'
                 )
               )
             )
           ),

           /* -------------------------------------
              1.1.3.3 모델링 개념과 절차(요·개·논·물)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.1.3.3',
             'title', '모델링(Modeling)의 개념과 절차 (요·개·논·물)',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','모델링의 개념'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '모델링은 실세계의 복잡한 물리·업무 현상을 특정 목적에 맞게 단순화하여, 이해·분석·설계에 활용하기 쉬운 형식으로 표현하는 기법이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '요구사항 분석의 핵심 활동으로, 문제 도메인의 엔티티와 관계를 구조화한다.',
                   '모델을 통해 문제 상황에 대한 이해를 높이고, 해결 방안을 설명할 수 있다.',
                   '개념 모델은 도메인 엔티티, 속성, 관계, 종속성을 반영한다.'
                 )
               ),

               JSON_OBJECT('type','heading','text','모델링 절차 (요·개·논·물)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','모델링 절차 단계',
                 'headers', JSON_ARRAY('순서','절차','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '1',
                     '요구사항 분석',
                     '현행 데이터·업무의 문제점과 개선 요구를 파악하고, 향후 개선 방향을 도출한다.'
                   ),
                   JSON_ARRAY(
                     '2',
                     '개념 모델링',
                     '업무 중심으로 엔티티·속성·관계를 추상화하여 ERD 등 개념적 구조를 정의한다.'
                   ),
                   JSON_ARRAY(
                     '3',
                     '논리 모델링',
                     '관계, 키, 정규화 등을 수행해 논리적 스키마(테이블 구조)를 도출한다.'
                   ),
                   JSON_ARRAY(
                     '4',
                     '물리 모델링',
                     'DBMS 특성에 맞게 컬럼 타입, 제약조건, 인덱스 등을 설계해 물리 스키마를 만든다.'
                   )
                 )
               )
             )
           ),

           /* -------------------------------------
              1.1.3.4 분석 자동화 도구(CASE)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '1.1.3.4',
             'title', '분석 자동화 도구(CASE) 개념',
             'importance', 3,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','분석 자동화 도구(CASE)의 개념'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '분석 자동화 도구는 요구사항을 자동으로 분석하고, 요구사항 명세서·다이어그램 등의 산출물을 생성·관리하도록 지원하는 CASE(Computer Aided Software Engineering) 도구이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '요구사항 캡처 및 추적: 요구 변경 이력과 연관 관계를 관리한다.',
                   '모델·다이어그램 자동 생성: UML·ERD 등 분석 모델을 시각적으로 표현한다.',
                   '일관성 검사: 모델·명세 간의 정합성을 자동으로 점검한다.',
                   '문서화 지원: 요구사항 명세서·설계 문서를 자동으로 생성·갱신한다.'
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);