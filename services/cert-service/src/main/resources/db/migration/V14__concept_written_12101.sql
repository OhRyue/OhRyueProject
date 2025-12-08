-- ============================
-- 2.1 데이터 입출력 구현
-- 2.1.1 논리 데이터 저장소 확인
-- topic_id = 20101 (@topic_logical_data_store)
-- ============================

INSERT INTO concept (topic_id, sections_json)
SELECT 12101,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              2.1.1-1 자료 구조 개요
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.1.1',
             'title', '자료 구조 개요',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','자료 구조(Data Structure)의 개념'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '자료 구조는 컴퓨터 상에서 자료를 효율적으로 저장·처리하기 위해 정의한 논리적인 구조이다. '
                 '저장·탐색·삽입·삭제 등의 연산을 얼마나 빠르고 효율적으로 할 수 있는지가 핵심 포인트이다.'
               ),

               JSON_OBJECT('type','heading','text','자료 구조의 분류'),
               JSON_OBJECT(
                 'type','table',
                 'caption','선형 구조 vs 비선형 구조',
                 'headers', JSON_ARRAY('구조','설명','대표 예'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '선형 구조',
                     '데이터가 1차원적으로, 일렬(순서)로 연결된 구조',
                     '리스트(List), 스택(Stack), 큐(Queue), 데크(Deque)'
                   ),
                   JSON_ARRAY(
                     '비선형 구조',
                     '데이터가 계층적·망형으로 연결된 구조',
                     '트리(Tree), 그래프(Graph)'
                   )
                 )
               ),

               JSON_OBJECT('type','callout',
                 'title','시험 포인트',
                 'body',
                 '· “선형 / 비선형” 구분과 각각의 대표 예시를 정확히 매칭\n'
                 '· 자료 구조 정의: “효율적인 저장과 처리를 위한 논리적 구조”라는 키워드 기억'
               )
             )
           ),

           /* -------------------------------------
              2.1.1-2 선형 구조 (리스트 / 스택 / 큐 / 데크)
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '2.1.1',
             'title', '선형 구조 (리스트·스택·큐·데크)',
             'importance', 5,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','리스트(List)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','선형 리스트 vs 연결 리스트',
                 'headers', JSON_ARRAY('구조','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '선형 리스트(Linear List)',
                     '배열처럼 연속된 기억 장소에 데이터를 저장하는 리스트. '
                     '인덱스로 빠른 접근이 가능하지만, 삽입/삭제 시 이동 비용이 크다.'
                   ),
                   JSON_ARRAY(
                     '연결 리스트(Linked List)',
                     '노드(Node)의 포인터 부분으로 서로 연결한 리스트. '
                     '포인터를 위한 추가 공간이 필요하고 탐색 속도는 느리지만, '
                     '중간 삽입·삭제가 쉬운 구조이다.'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','스택(Stack) – LIFO'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '스택(Stack)은 한 방향으로만 자료를 넣고 꺼내는 LIFO(Last-In First-Out) 구조이다. '
                 'Top(스택 포인터)에 대해 PUSH/POP을 수행하며, 함수 호출, 인터럽트 처리, 수식 계산 등에 활용된다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','스택 연산과 특징',
                 'headers', JSON_ARRAY('구분','내용'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('PUSH','스택의 Top 위치에 새로운 데이터를 삽입하는 연산'),
                   JSON_ARRAY('POP','스택의 Top에 있는 데이터를 꺼내는 연산'),
                   JSON_ARRAY('Top','현재 스택에서 가장 위(마지막) 데이터의 위치로, 스택 포인터 역할을 한다.')
                 )
               ),

               JSON_OBJECT('type','heading','text','큐(Queue) – FIFO'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '큐(Queue)는 한쪽 끝에서는 삽입(ENQUEUE), 다른 쪽 끝에서는 삭제(DEQUEUE)가 이뤄지는 '
                 'FIFO(First-In First-Out) 구조이다. '
                 '꺼내는 쪽을 Front, 넣는 쪽을 Rear라고 한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','큐 연산',
                 'headers', JSON_ARRAY('연산','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('ENQUEUE','Rear 방향으로 데이터를 차례대로 삽입'),
                   JSON_ARRAY('DEQUEUE','Front에서부터 데이터를 차례대로 삭제')
                 )
               ),

               JSON_OBJECT('type','heading','text','데크(Deque; Double Ended Queue)'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '데크(Deque)는 큐의 양쪽 끝에서 삽입과 삭제가 모두 가능한 자료 구조이다. '
                 '양 끝에 대한 포인터를 두어 스택/큐 동작을 모두 구현할 수 있다.'
               ),

               JSON_OBJECT('type','callout',
                 'title','시험 포인트',
                 'body',
                 '· 스택: LIFO, PUSH/POP, Top → 함수 호출·인터럽트 응용\n'
                 '· 큐: FIFO, ENQUEUE/DEQUEUE, Front/Rear 용어 구분\n'
                 '· 데크: 양쪽에서 삽입/삭제 가능 – “양방향 큐”라는 키워드 기억'
               )
             )
           ),

           /* -------------------------------------
              2.1.1-3 비선형 구조 (트리·그래프) & 논리 데이터 저장소
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '2.1.1',
             'title', '비선형 구조 & 논리 데이터 저장소',
             'importance', 4,
             'blocks', JSON_ARRAY(

               JSON_OBJECT('type','heading','text','트리(Tree) 기초'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '트리(Tree)는 데이터들을 계층(structure)으로 표현한 비선형 자료 구조이다. '
                 '노드(Node)와 링크(Link)로 구성되며, 인덱스·디렉터리 등 계층 구조 표현에 많이 사용된다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','트리 기본 용어',
                 'headers', JSON_ARRAY('용어','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('루트 노드(Root)','부모가 없는 최상위 노드'),
                   JSON_ARRAY('단말 노드(Leaf)','자식이 없는 말단 노드'),
                   JSON_ARRAY('부모/자식(Parent/Child)','상위/하위 레벨로 직접 연결된 관계'),
                   JSON_ARRAY('형제(Sibling)','같은 부모를 가진 노드'),
                   JSON_ARRAY('레벨(Level)','루트에서 특정 노드까지의 단계'),
                   JSON_ARRAY('깊이(Depth)','루트에서 해당 노드까지의 간선 수'),
                   JSON_ARRAY('차수(Degree)','특정 노드가 가진 자식 노드의 수')
                 )
               ),

               JSON_OBJECT('type','heading','text','그래프(Graph) 기초'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '그래프(Graph)는 정점(노드) V와 간선(엣지) E로 구성된 비선형 구조이며, 네트워크·경로 등 복잡한 관계를 표현할 때 사용된다. '
                 '트리는 사이클이 없는 그래프의 특수한 형태이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','그래프 유형',
                 'headers', JSON_ARRAY('구분','최대 간선 수','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '방향 그래프',
                     'n(n-1)',
                     '간선에 방향이 있는 그래프. 정점 간의 관계가 단방향.'
                   ),
                   JSON_ARRAY(
                     '무방향 그래프',
                     'n(n-1)/2',
                     '간선에 방향이 없는 그래프. 연결만 존재하고 방향 정보는 없음.'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','논리 데이터 저장소 개념'),
               JSON_OBJECT(
                 'type','paragraph',
                 'text',
                 '논리 데이터 저장소는 업무를 모델링 표기법(E-R 다이어그램 등)으로 형상화한 데이터의 저장소를 의미한다. '
                 '실제 DBMS·파일 구조에 독립적인 “업무 관점의 데이터 모델”이라고 볼 수 있다.'
               ),

               JSON_OBJECT('type','heading','text','논리 데이터 저장소 구조 (개속관)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','논리 데이터 저장소를 구성하는 세 가지 요소',
                 'headers', JSON_ARRAY('구조','설명','표기 예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '개체(Entity)',
                     '관리 대상이 되는 실체(사람, 물건, 사건 등). 테이블 단위에 해당.',
                     '사원, 고객, 주문, 상품 등'
                   ),
                   JSON_ARRAY(
                     '속성(Attribute)',
                     '개체가 관리해야 할 구체적인 정보 항목.',
                     '사원번호, 성명, 입사일, 급여 등'
                   ),
                   JSON_ARRAY(
                     '관계(Relationship)',
                     '개체와 개체 사이의 대응·연관 관계.',
                     '사원–부서 소속 관계, 주문–고객 관계 등'
                   )
                 )
               ),

               JSON_OBJECT('type','callout',
                 'title','시험 포인트',
                 'body',
                 '· 논리 데이터 저장소 = 업무 모델 관점의 “데이터 저장소”라는 정의를 기억\n'
                 '· 논리 구조 요소 3가지: 개체/속성/관계 (개속관)\n'
                 '· 트리·그래프의 기본 용어(루트/단말/차수, 방향/무방향 최대 간선 수)와 함께 정리'
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);
