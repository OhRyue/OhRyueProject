-- =========================================
-- 4.3.1 운영체제 기초 활용 (concept 보강)
-- topic_id = 14301 (제4과목 프로그래밍 언어 활용 > 운영체제 기초 활용)
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 14301,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              4.3.1.1 운영체제 개요와 운용 기법
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '4.3.1.1',
             'title', '운영체제(OS) 개요와 운용 기법',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','운영체제(OS; Operating System)의 역할'),
               JSON_OBJECT('type','paragraph','text',
                 '운영체제는 사용자가 컴퓨터 하드웨어를 보다 쉽게 사용할 수 있도록 인터페이스를 제공하는 '
                 '시스템 소프트웨어이다. CPU, 메모리, 저장장치, 입출력 장치 등의 자원을 관리하고, '
                 '다중 사용자·다중 프로그램 환경에서 스케줄링과 자원 분배를 수행한다.'
               ),
               JSON_OBJECT('type','heading','text','운영체제 구조 – 쉘(Shell)과 커널(Kernel)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','쉘과 커널의 역할',
                 'headers', JSON_ARRAY('구성요소','역할'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('쉘(Shell)',
                              '운영체제의 가장 바깥 부분에서 사용자 명령을 해석·처리하는 인터페이스(명령어 해석기)'),
                   JSON_ARRAY('커널(Kernel)',
                              '프로세스·메모리·파일 시스템·입출력 장치 등 하드웨어 자원을 직접 관리하는 핵심 부분')
                 )
               ),
               JSON_OBJECT('type','heading','text','운영체제 운용 기법'),
               JSON_OBJECT(
                 'type','table',
                 'caption','주요 운용 기법 비교',
                 'headers', JSON_ARRAY('구분','설명','특징'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('실시간 처리 시스템',
                              '데이터 발생 즉시 처리하여 결과를 산출',
                              '처리 시간이 짧고 처리 비용이 낮음'),
                   JSON_ARRAY('시분할 시스템(Time Sharing)',
                              'CPU 시간을 여러 사용자에게 균등 분할하여 사용',
                              '라운드 로빈 스케줄링 사용, 모든 사용자에게 유사한 서비스 제공'),
                   JSON_ARRAY('일괄 처리 시스템(Batch)',
                              '일정 기간 또는 일정량 데이터를 모아 한꺼번에 처리',
                              '자원 활용 효율 높고 CPU 유휴 시간 감소, 반응 시간이 길 수 있음'),
                   JSON_ARRAY('분산 처리 시스템',
                              '물리적으로 떨어진 여러 컴퓨터를 네트워크로 연결하여 하나의 시스템처럼 동작',
                              '확장성 및 가용성 향상')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.3.1.2 Windows / Unix / Linux 특징
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '4.3.1.2',
             'title', 'Windows / Unix / Linux 운영체제 특징',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','Windows 계열 운영체제'),
               JSON_OBJECT('type','paragraph','text',
                 'Windows는 DOS 기반에서 출발해 현재는 GUI 중심 환경을 제공하는 OS로, '
                 '소스가 공개되어 있지 않은 상용 운영체제이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '트리 디렉터리 구조 사용',
                   '그래픽 사용자 인터페이스(GUI) 제공',
                   '선점형 멀티태스킹 방식 지원',
                   '자동 감지(Plug & Play) 기능 제공',
                   'OLE(Object Linking and Embedding) 사용'
                 )
               ),
               JSON_OBJECT('type','heading','text','유닉스(UNIX) 계열 운영체제 특징 (대다 사 이계)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '대화식 운영체제 기능 제공',
                   '다중 작업(Multi-tasking) 기능',
                   '다중 사용자(Multi-user) 기능',
                   '이식성(Portability) 우수',
                   '계층적 트리 구조 파일 시스템 제공'
                 )
               ),
               JSON_OBJECT('type','heading','text','Linux와 Unix의 차이점 요약'),
               JSON_OBJECT(
                 'type','table',
                 'caption','Linux vs Unix 비교',
                 'headers', JSON_ARRAY('항목','Linux','Unix'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('비용','대부분 무료, 일부 유료 배포판','대부분 유료 상용 OS'),
                   JSON_ARRAY('주 사용자',
                              '개발자, 일반 사용자까지 폭넓게 사용',
                              '메인프레임·워크스테이션 등 대형 시스템 관리자'),
                   JSON_ARRAY('배포 방식',
                              '오픈소스 기반, 다양한 배포판 존재',
                              '사업자에 의해 배포, 라이선스 비용 수반'),
                   JSON_ARRAY('사용자 편의',
                              'GUI, 파일 시스템, Bash 셸 등 폭넓게 지원',
                              '전통적으로 CLI 중심이나 GUI 환경도 지원'),
                   JSON_ARRAY('주 활용 분야',
                              '스마트폰, 태블릿 등 다양한 디바이스와 서버',
                              '인터넷 서버·워크스테이션 등 대형 시스템에 주로 사용')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.3.1.3 메모리 관리와 배치·할당·교체
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '4.3.1.3',
             'title', '메모리 관리 기법과 배치·할당·교체',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','메모리 관리 4대 기법 (반·배·할·교)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','메모리 관리 기법',
                 'headers', JSON_ARRAY('기법','키워드','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('반입 기법 (When)',
                              '언제',
                              '디스크의 프로세스를 언제 주기억장치에 적재할지 결정'),
                   JSON_ARRAY('배치 기법 (Where)',
                              '어디에',
                              '적재할 프로세스를 주기억장치의 어느 위치에 둘지 결정'),
                   JSON_ARRAY('할당 기법 (How)',
                              '어떻게',
                              '어떤 방식으로 메모리를 나누어 프로세스에 할당할지 결정'),
                   JSON_ARRAY('교체 기법 (Who)',
                              '누구를',
                              '어떤 프로세스를 메모리에서 내보낼지(교체할지) 결정')
                 )
               ),
               JSON_OBJECT('type','heading','text','메모리 배치 기법 (초·적·악)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','배치 기법 유형',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('최초 적합(First Fit)',
                              '가용 공간 목록에서 처음 발견한 충분히 큰 공간에 프로세스 적재'),
                   JSON_ARRAY('최적 적합(Best Fit)',
                              '크기가 가장 비슷한(가장 작은 여유 공간) 파티션에 할당 – 내부 공백 최소화'),
                   JSON_ARRAY('최악 적합(Worst Fit)',
                              '가장 큰 가용 공간에 할당 – 큰 공백을 남겨 다른 프로세스 적재 기회 확보')
                 )
               ),
               JSON_OBJECT('type','heading','text','주기억장치 할당 기법 (연·단·다, 분·페·세)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','연속/분산 할당 기법',
                 'headers', JSON_ARRAY('분류','기법','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('연속 할당',
                              '단일 분할 / 다중 분할',
                              '하나의 프로세스가 연속된 메모리 영역을 점유하는 방식'),
                   JSON_ARRAY('분산 할당',
                              '페이징(Paging)',
                              '가상기억장치의 프로세스를 고정 크기 페이지로 나누고, '
                              '주기억장치를 같은 크기의 프레임으로 나누어 분산 적재'),
                   JSON_ARRAY('분산 할당',
                              '세그먼테이션(Segmentation)',
                              '프로세스를 서로 다른 크기의 논리 단위(세그먼트)로 나누어 할당 – 함수/배열 등 논리 단위 기준'),
                   JSON_ARRAY('분산 할당',
                              '페이징+세그먼테이션',
                              '세그먼트 하나를 다시 페이지 단위로 분할해 관리하는 혼합 방식')
                 )
               ),
               JSON_OBJECT('type','heading','text','페이지 교체 알고리즘 요약'),
               JSON_OBJECT(
                 'type','table',
                 'caption','주요 페이지 교체 알고리즘',
                 'headers', JSON_ARRAY('알고리즘','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('FIFO','가장 먼저 들어온 페이지를 먼저 교체 (선입선출)'),
                   JSON_ARRAY('LRU','가장 오랫동안 사용되지 않은 페이지 교체'),
                   JSON_ARRAY('LFU','참조 횟수가 가장 적은 페이지 교체'),
                   JSON_ARRAY('OPT','앞으로 가장 오랫동안 참조되지 않을 페이지를 교체하는 이론적 최적 알고리즘'),
                   JSON_ARRAY('NUR','최근 사용 여부를 참조비트·변형비트로 판단해 교체하는 근사 LRU'),
                   JSON_ARRAY('SCR','Second-Chance – FIFO에 최근 사용 여부(참조 비트)를 반영해 개선한 기법')
                 )
               ),
               JSON_OBJECT('type','heading','text','단편화와 해결 방안'),
               JSON_OBJECT(
                 'type','table',
                 'caption','메모리 단편화 해결 (내슬 / 외버·공통·압)',
                 'headers', JSON_ARRAY('단편화 유형','대표 해결책'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('내부 단편화','Slab Allocator 등 고정 크기 캐시 활용'),
                   JSON_ARRAY('외부 단편화','버디 메모리 할당, 공통 통합·압축(Coalescing, Compaction) 기법')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.3.1.4 지역성, 스레싱, 워킹셋
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '4.3.1.4',
             'title', '지역성(Locality)·스레싱·워킹셋',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','지역성(Locality)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '지역성은 프로세스가 기억장치 내 정보 전체를 균일하게 접근하는 것이 아니라, '
                 '특정 시점에 특정 부분(일부 페이지)을 집중적으로 참조하는 성질이다.'
               ),
               JSON_OBJECT('type','heading','text','지역성의 유형 (시·공·순)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','지역성 유형과 예시',
                 'headers', JSON_ARRAY('유형','설명·예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('시간 지역성(Temporal)',
                              '최근에 참조한 페이지·데이터를 가까운 미래에도 다시 참조하는 경향 '
                              '(예: 반복문, 서브루틴 호출, 스택 등)'),
                   JSON_ARRAY('공간 지역성(Spatial)',
                              '참조된 주소 주변의 인접 주소들도 함께 참조되는 경향 '
                              '(예: 순차적인 코드·배열 접근)'),
                   JSON_ARRAY('순차 지역성(Sequential)',
                              '프로그램 명령이 순차적으로 실행되며 인접한 명령어를 계속 참조하는 성질')
                 )
               ),
               JSON_OBJECT('type','heading','text','지역성과 관련된 현상'),
               JSON_OBJECT(
                 'type','table',
                 'caption','지역성 관련 주요 개념',
                 'headers', JSON_ARRAY('개념','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('지역성(Locality)',
                              '하나의 페이지가 일정 시간 동안 집중적으로 액세스되는 현상'),
                   JSON_ARRAY('스레싱(Thrashing)',
                              '프로세스 처리 시간보다 페이지 교체 시간이 더 많아지는 비정상 상태'),
                   JSON_ARRAY('워킹 셋(Working Set)',
                              '일정 시간 동안 프로세스가 자주 참조하는 페이지들의 집합'),
                   JSON_ARRAY('프리페이징(Prepaging)',
                              '곧 사용될 것으로 예측되는 페이지를 미리 메모리에 적재하는 기법')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.3.1.5 프로세스·스레드·스케줄링
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 5,
             'subCode', '4.3.1.5',
             'title', '프로세스·스레드·스케줄링과 HRN',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','프로세스 상태와 PCB'),
               JSON_OBJECT(
                 'type','table',
                 'caption','프로세스 상태 (생·준·실·대·완)',
                 'headers', JSON_ARRAY('상태','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('Create','프로세스가 생성되는 상태'),
                   JSON_ARRAY('Ready','CPU 할당을 기다리는 준비 상태'),
                   JSON_ARRAY('Running','CPU를 할당받아 실행 중인 상태'),
                   JSON_ARRAY('Waiting(Blocked)','입출력 등 이벤트를 기다리는 상태'),
                   JSON_ARRAY('Complete/Exit','실행이 종료된 상태')
                 )
               ),
               JSON_OBJECT('type','heading','text','PCB(Process Control Block) 주요 정보 (프·상·카·레·스·계·입·메)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'PID(프로세스 식별자)',
                   '프로세스 상태',
                   '프로그램 카운터',
                   '레지스터 저장 영역',
                   '프로세서 스케줄링 정보',
                   '계정 정보',
                   '입출력 상태 정보',
                   '메모리 관리 정보'
                 )
               ),
               JSON_OBJECT('type','heading','text','스레드(Thread)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '스레드는 프로세스보다 가벼운 실행 단위로, 같은 프로세스 내 다른 스레드들과 '
                 '코드·데이터·자원을 공유하면서 독립적인 제어 흐름을 가진다. '
                 '이를 통해 응용 프로그램의 처리율 향상과 응답성 개선이 가능하다.'
               ),
               JSON_OBJECT('type','heading','text','프로세스 스케줄링 – 선점/비선점'),
               JSON_OBJECT(
                 'type','table',
                 'caption','선점형 vs 비선점형 스케줄링',
                 'headers', JSON_ARRAY('분류','예시 알고리즘','특징'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('선점형',
                              'RR, SRT, MLQ, MFQ 등',
                              '이미 실행 중인 프로세스의 CPU를 다른 프로세스가 빼앗을 수 있음'),
                   JSON_ARRAY('비선점형',
                              '우선순위, FCFS, SJF, HRN, Deadline 등',
                              '한 번 CPU를 할당받으면 자발적으로 종료·대기 상태가 될 때까지 점유')
                 )
               ),
               JSON_OBJECT('type','heading','text','선점형 알고리즘 요약'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'RR(Round Robin): 고정된 시간 할당량(Time Quantum) 단위로 순환, Time Sharing System용',
                   'SRT(Shortest Remaining Time): 남은 실행 시간이 가장 짧은 프로세스에 우선 할당',
                   'MLQ(Multi Level Queue): 우선순위별로 독립된 대기 큐를 구성',
                   'MFQ(Multi Level Feedback Queue): FIFO+RR 혼합, 단계별로 다른 큐와 규칙 적용'
                 )
               ),
               JSON_OBJECT('type','heading','text','비선점형 알고리즘과 HRN'),
               JSON_OBJECT(
                 'type','table',
                 'caption','비선점형 스케줄링 (우·기·H·F·S)',
                 'headers', JSON_ARRAY('알고리즘','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('우선순위(Priority)','우선순위가 높은 프로세스에 CPU 먼저 할당, 동순위는 FCFS'),
                   JSON_ARRAY('기한부(Deadline)','요청된 기한 내에 작업을 완료하도록 계획'),
                   JSON_ARRAY('FCFS','먼저 도착한 순서대로 처리 – FIFO'),
                   JSON_ARRAY('SJF','서비스 시간이 가장 짧은 작업을 먼저 수행 – 기아 현상 가능'),
                   JSON_ARRAY('HRN','(대기시간+서비스시간)/서비스시간으로 우선순위를 계산해 기아를 완화한 방식')
                 )
               ),
               JSON_OBJECT('type','paragraph','text',
                 'HRN 우선순위 계산식: (대기시간 + 서비스시간) / 서비스시간. '
                 '값이 클수록 우선순위가 높으며, 대기 시간이 긴 작업의 우선순위가 자연스럽게 증가해 '
                 'SJF의 기아 현상을 보완한다.'
               )
             )
           ),

           /* -------------------------------------
              4.3.1.6 교착상태(Deadlock)와 해결
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 6,
             'subCode', '4.3.1.6',
             'title', '교착상태(Deadlock) 개념·조건·해결',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','교착상태(Deadlock)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '교착상태는 둘 이상의 프로세스가 서로가 점유한 자원을 기다리며 '
                 '무한정 대기하는 상태로, 각 프로세스가 더 이상 진행할 수 없는 상황을 말한다.'
               ),
               JSON_OBJECT('type','heading','text','교착상태 발생 조건 (상·점·비·환)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '상호배제(Mutual Exclusion): 자원은 동시에 하나의 프로세스만 사용 가능',
                   '점유와 대기(Hold & Wait): 최소 한 개의 자원을 점유한 채로 다른 자원을 추가로 요청하여 대기',
                   '비선점(Non Preemption): 이미 할당된 자원을 강제로 빼앗을 수 없음',
                   '환형 대기(Circular Wait): 프로세스들이 원형으로 자원을 서로 요구하며 대기'
                 )
               ),
               JSON_OBJECT('type','heading','text','교착상태 해결 방법 (예·회·발·복)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','교착상태 해결 전략',
                 'headers', JSON_ARRAY('전략','기법 예시','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('예방(Prevention)',
                              '자원 요청 순서 강제, 점유 후 대기 금지 등',
                              '4가지 발생 조건 중 하나 이상이 성립하지 않도록 미리 설계'),
                   JSON_ARRAY('회피(Avoidance)',
                              '은행가 알고리즘, Wait-Die, Wound-Wait',
                              '시스템 상태를 계속 점검하면서 안전 상태에서만 자원 할당'),
                   JSON_ARRAY('발견(Detection)',
                              '자원할당 그래프, Wait-for 그래프',
                              '주기적으로 시스템을 검사하여 교착 상태를 탐지'),
                   JSON_ARRAY('복구(Recovery)',
                              '프로세스 Kill, 자원 선점',
                              '교착 상태에 있는 프로세스를 종료하거나 자원을 회수해서 복구')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.3.1.7 환경변수·쉘·리눅스 명령어
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 7,
             'subCode', '4.3.1.7',
             'title', '환경변수·UNIX Shell·기본 명령어',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','환경변수(Environment Variable)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '환경변수는 프로세스가 동작하는 환경에 영향을 주는 동적 값들의 모음으로, '
                 'PATH, HOME, LANG 등 다양한 설정 정보를 담고 있다.'
               ),
               JSON_OBJECT('type','heading','text','UNIX Shell과 환경변수'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'UNIX Shell은 명령어 해석기로, 사용자와 시스템 사이 인터페이스 역할을 수행',
                   '환경변수 출력 명령: printenv, env, setenv 등'
                 )
               ),
               JSON_OBJECT('type','heading','text','CLI와 GUI'),
               JSON_OBJECT(
                 'type','table',
                 'caption','운영체제 제어 방식',
                 'headers', JSON_ARRAY('방식','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('CLI (Command Line Interface)',
                              '사용자가 명령어를 직접 입력해 시스템을 제어하는 방식'),
                   JSON_ARRAY('GUI (Graphic User Interface)',
                              '아이콘·창·버튼 등 그래픽 요소를 클릭하여 조작하는 방식')
                 )
               ),
               JSON_OBJECT('type','heading','text','리눅스/유닉스 계열 기본 명령어 모음'),
               JSON_OBJECT(
                 'type','table',
                 'caption','자주 나오는 Linux/Unix 명령어',
                 'headers', JSON_ARRAY('구분','명령어','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('시스템','uname / uname -a / uname -r',
                              '커널·시스템 정보 확인'),
                   JSON_ARRAY('시스템','uptime',
                              '시스템 가동 시간, 사용자 수, 평균 부하 확인'),
                   JSON_ARRAY('사용자','id',
                              '사용자 이름, UID, GID 등 출력'),
                   JSON_ARRAY('사용자','who / last',
                              '현재 로그인 사용자 / 과거 로그인 기록 조회'),
                   JSON_ARRAY('파일','ls / pwd / cp / mv / rm',
                              '목록 보기, 현재 경로, 복사, 이동, 삭제'),
                   JSON_ARRAY('프로세스','ps / kill',
                              '현재 프로세스 목록 조회 / 프로세스 종료'),
                   JSON_ARRAY('권한','chmod / chown',
                              '파일·디렉터리 권한 및 소유자 변경'),
                   JSON_ARRAY('네트워크','ifconfig / host',
                              '네트워크 인터페이스 설정 확인 / 도메인↔IP 조회'),
                   JSON_ARRAY('압축','tar / gzip',
                              '파일 묶기/압축 및 해제'),
                   JSON_ARRAY('검색','grep / find',
                              '파일 내용 검색 / 파일 위치 검색'),
                   JSON_ARRAY('디스크','df / du',
                              '디스크 사용량, 디렉터리별 용량 확인'),
                   JSON_ARRAY('디렉터리 이동','cd',
                              '현재 작업 디렉터리 변경')
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);