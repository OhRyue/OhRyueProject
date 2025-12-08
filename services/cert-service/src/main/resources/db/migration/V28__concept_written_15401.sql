-- =========================================
-- 5.4.1 시스템 보안 설계 (concept)
-- topic_id = 15401
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 15401,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------
              5.4.1.1 서비스 공격 유형 – DoS / DDoS / DRDoS
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '5.4.1.1',
             'title', '서비스 공격 유형 (DoS / DDoS / DRDoS)',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','DoS(서비스 거부) 공격 개요'),
               JSON_OBJECT('type','paragraph','text',
                 'DoS(Denial of Service) 공격은 시스템 자원을 악의적으로 고갈시켜 정상 사용자가 '
                 '서비스를 이용하지 못하도록 만드는 공격이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','주요 DoS 공격 기법',
                 'headers', JSON_ARRAY('공격 기법','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'SYN 플러딩(SYN Flooding)',
                     'TCP 3-way 핸드셰이크 과정의 구조적 특성을 악용하여, 다수의 SYN만 보내고 응답을 완료하지 않아 '
                     '서버의 연결 대기 큐를 고갈시키는 공격'
                   ),
                   JSON_ARRAY(
                     'UDP 플러딩(UDP Flooding)',
                     '대량의 UDP 패킷을 임의의 포트로 전송하여, 수신 측에서 ICMP Destination Unreachable 메시지를 '
                     '반복 생성하게 만들어 자원을 고갈시키는 공격'
                   ),
                   JSON_ARRAY(
                     '스머프(Smurf / Smurfing)',
                     '출발지 주소를 공격 대상 IP로 위조한 뒤, 네트워크 전체에 ICMP Echo를 브로드캐스트하여 '
                     '대상 시스템에 과도한 응답 트래픽이 집중되도록 하는 공격 [2020년 1회]'
                   ),
                   JSON_ARRAY(
                     '죽음의 핑(Ping of Death, PoD)',
                     '정상 크기를 초과하는 큰 ICMP 패킷을 전송하여 과도한 단편화와 재조합 과정에서 버퍼 오버플로우를 유발하고 '
                     '서비스를 중단시키는 공격'
                   ),
                   JSON_ARRAY(
                     '랜드 어택(Land Attack)',
                     '출발지 IP와 목적지 IP를 동일하게 설정한 패킷을 전송하여, 시스템이 자기 자신에게 응답을 보내도록 만들어 '
                     '가용성을 침해하는 공격'
                   ),
                   JSON_ARRAY(
                     '티어 드롭(Tear Drop)',
                     '잘못된 Fragment Offset 값을 가진 IP 단편화 패킷을 전송하여, 재조합 과정에서 오류를 유발하고 '
                     '시스템 동작을 마비시키는 공격'
                   ),
                   JSON_ARRAY(
                     '봉크(Bonk) / 보잉크(Boink)',
                     '프로토콜의 오류 제어 및 재전송·재조립 기능을 악용하여 과부하를 일으키는 공격 기법'
                   )
                 )
               ),

               JSON_OBJECT('type','heading','text','DDoS / DRDoS 공격 개요'),
               JSON_OBJECT('type','paragraph','text',
                 'DDoS(Distributed DoS)는 다수의 공격자(좀비 PC)를 분산 배치하여 동시다발적으로 '
                 '공격 트래픽을 발생시키는 방식이며, DRDoS는 반사 서버를 활용해 공격 근원을 숨기고 '
                 '효율을 높인 반사형 서비스 거부 공격이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','DDoS 공격 구성 요소 (HAMAD)',
                 'headers', JSON_ARRAY('구성 요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('핸들러(Handler)','여러 에이전트를 제어하는 마스터 프로그램'),
                   JSON_ARRAY('에이전트(Agent)','공격 대상에 실제 공격 트래픽을 전송하는 시스템'),
                   JSON_ARRAY('마스터(Master)','공격자 명령을 받아 핸들러를 제어하는 시스템'),
                   JSON_ARRAY('공격자(Attacker)','전체 공격을 계획하고 지휘하는 해커의 컴퓨터'),
                   JSON_ARRAY('데몬(Daemon)','에이전트 역할을 수행하는 악성 프로그램')
                 )
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'DDoS 공격 도구 예: Trinoo, Tribe Flood Network, Stacheldraht [2020년 3회]',
                   'DDoS 대응: 차단 정책 업데이트, 좀비 PC IP 확보, 보안 솔루션 운영, 홈페이지 보안 강화, 시스템 패치 등'
                 )
               ),
               JSON_OBJECT('type','heading','text','DRDoS(Distributed Reflection DoS) 공격'),
               JSON_OBJECT('type','paragraph','text',
                 'DRDoS는 출발지 IP를 공격 대상 IP로 위조하여 다수의 반사 서버로 요청을 보내고, '
                 '해당 반사 서버들이 공격 대상에게 대량의 응답을 보내도록 유도하는 공격이다. '
                 'DDoS보다 공격 근원 파악이 어렵고 트래픽 증폭 효율이 높다.'
               )
             )
           ),

           /* -------------------------------
              5.4.1.2 버퍼 오버플로우 / 백도어 및 방어
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '5.4.1.2',
             'title', '버퍼 오버플로우 / 백도어 및 방어 기법',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','버퍼 오버플로우 공격'),
               JSON_OBJECT('type','paragraph','text',
                 '버퍼 오버플로우(Buffer Overflow)는 메모리에 할당된 버퍼 크기를 초과하는 데이터 입력을 통해 '
                 '프로세스의 제어 흐름(복귀 주소 등)을 덮어쓰고 악성 코드를 실행시키는 공격이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '스택 버퍼 오버플로우: 호출 스택 영역의 버퍼를 넘치게 하여 복귀 주소를 덮어쓰는 공격',
                   '힙 버퍼 오버플로우: 동적 할당(힙) 영역의 메모리를 넘치게 하여 데이터 구조나 함수 포인터를 조작하는 공격'
                 )
               ),
               JSON_OBJECT('type','heading','text','버퍼 오버플로우 방어 기법'),
               JSON_OBJECT(
                 'type','table',
                 'caption','버퍼 오버플로우 대응 방안',
                 'headers', JSON_ARRAY('대응 방안','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '스택가드(StackGuard)',
                     '복귀 주소와 지역 변수 사이에 카나리(canary) 값을 삽입하여, 함수 종료 시 카나리 값이 변경되었으면 '
                     '버퍼 오버플로우로 판단하고 실행을 중단하는 기술 [2020년 1회]'
                   ),
                   JSON_ARRAY(
                     '스택쉴드(StackShield)',
                     '함수 시작 시 복귀 주소를 별도의 Global RET 영역에 저장해 두었다가, '
                     '함수 종료 시 스택의 복귀 주소와 비교해 다르면 오버플로우로 간주해 중단하는 기술'
                   ),
                   JSON_ARRAY(
                     'ASLR(Address Space Layout Randomization)',
                     '실행 시마다 코드/스택/힙 등의 메모리 주소를 난수화하여, 공격자가 특정 주소를 정확히 '
                     '예측하지 못하도록 하는 방어 기법'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','백도어(Backdoor)와 탐지'),
               JSON_OBJECT('type','paragraph','text',
                 '백도어는 인증 절차를 우회하여 시스템에 몰래 접속할 수 있도록 숨겨둔 비정상적인 '
                 '접근 경로(계정, 기능)를 의미한다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '프로세스 및 열린 포트 확인',
                   'Setuid 파일 검사',
                   '백신·백도어 탐지 도구 활용',
                   '파일 무결성 검사 도구 활용',
                   '시스템·보안 로그 분석'
                 )
               )
             )
           ),

           /* -------------------------------
              5.4.1.3 주요 시스템 보안 공격기법 & 용어
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '5.4.1.3',
             'title', '주요 시스템 보안 공격기법 및 용어',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','주요 시스템 보안 공격기법'),
               JSON_OBJECT(
                 'type','table',
                 'caption','대표 시스템 보안 공격기법',
                 'headers', JSON_ARRAY('공격 기법','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '포맷 스트링 공격',
                     'printf 계열 함수 등 포맷 문자열 인자를 검증 없이 외부 입력으로 사용할 때, '
                     '%x, %n 등의 포맷을 악용하여 메모리 내용을 유출하거나 수정하는 공격'
                   ),
                   JSON_ARRAY(
                     '레이스 컨디션 공격(Race Condition)',
                     '둘 이상의 프로세스/스레드가 공유 자원에 동시에 접근할 때, 접근 순서에 따라 '
                     '예기치 않은 결과가 발생하는 상황을 악용하는 공격'
                   ),
                   JSON_ARRAY(
                     '키로거(Key Logger)',
                     '사용자의 키보드 입력을 가로채어 ID, 비밀번호, 계좌·카드번호 등 민감 정보를 탈취하는 공격 [2020년 1회]'
                   ),
                   JSON_ARRAY(
                     '루트킷(Rootkit)',
                     '시스템에 침입한 후, 침입 사실을 숨기고 향후 침입을 유지하기 위해 '
                     '백도어, 트로이 목마 등 여러 기능을 묶어 제공하는 도구 세트'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','보안 관련 용어 및 공격'),
               JSON_OBJECT(
                 'type','table',
                 'caption','보안 관련 주요 용어',
                 'headers', JSON_ARRAY('용어','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '스미싱(Smishing)',
                     '문자메시지를 이용해 신뢰할 수 있는 기관이나 지인으로 위장하여, '
                     '개인정보 입력이나 소액결제를 유도하는 피싱 공격'
                   ),
                   JSON_ARRAY(
                     '큐싱(Qshing)',
                     'QR 코드를 악용하여 악성 앱 설치나 피싱 사이트 접속을 유도, '
                     '금융 정보 등을 탈취하는 공격'
                   ),
                   JSON_ARRAY(
                     'APT 공격',
                     '특정 대상을 장기간 집요하게 노리는 지능형 지속 공격(Advanced Persistent Threat)'
                   ),
                   JSON_ARRAY(
                     '제로데이 공격',
                     '취약점이 공개되기 전에, 패치가 존재하지 않는 시점의 보안 약점을 악용하는 공격'
                   ),
                   JSON_ARRAY(
                     '사이버 킬체인(Cyber Kill Chain)',
                     '록히드 마틴이 제안한 APT 공격 분석·방어 모델로, 정찰→무기화→전달→취약점 악용→설치→명령·제어→목표 수행의 7단계로 공격을 분석'
                   ),
                   JSON_ARRAY(
                     '이블 트윈(Evil Twin)',
                     '합법적인 무선 AP인 것처럼 위장한 가짜 AP를 구축하여, 사용자 트래픽을 가로채는 Wi-Fi 피싱 공격'
                   ),
                   JSON_ARRAY(
                     '웜(Worm)',
                     '스스로를 복제하여 네트워크를 통해 전파되는 악성 프로그램'
                   ),
                   JSON_ARRAY(
                     '랜섬웨어(Ransomware)',
                     '사용자 파일을 암호화하거나 잠그고, 복호화 대가로 금전을 요구하는 악성 코드 [2020년 1회]'
                   ),
                   JSON_ARRAY(
                     'Tripwire',
                     '파일·설정의 변조 여부를 탐지하여, 크래커의 침입·백도어 설치·설정 변경을 분석하는 무결성 점검 도구 [2020년 1회]'
                   )
                 )
               )
             )
           ),

           /* -------------------------------
              5.4.1.4 서버 인증 / 접근통제
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '5.4.1.4',
             'title', '서버 인증 및 접근통제',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','서버 인증 개요'),
               JSON_OBJECT('type','paragraph','text',
                 '서버 인증은 다중 사용자 시스템과 네트워크 환경에서 접속자의 로그인 정보를 확인하여 '
                 '정당한 사용자만 시스템에 접근하도록 보장하는 절차이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '스니핑(패킷 가로채기) 방지',
                   '피싱 사이트로의 접속 방지',
                   '데이터 변조 방지',
                   '기업 및 서비스 신뢰도 향상'
                 )
               ),
               JSON_OBJECT('type','heading','text','인증 기술 유형 (지·소·생·특)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '지식 기반 인증: 사용자가 알고 있는 정보(비밀번호, PIN 등)',
                   '소지 기반 인증: 사용자가 소유한 매체(스마트카드, OTP 토큰 등)',
                   '생체 기반 인증: 지문, 홍채, 얼굴, 음성 등 생체 정보',
                   '특징 기반 인증: 위치, 행동 패턴 등 사용자 특성을 활용한 인증'
                 )
               ),
               JSON_OBJECT('type','heading','text','서버 접근통제 개요'),
               JSON_OBJECT('type','paragraph','text',
                 '서버 접근통제는 사람·프로세스가 서버 내 파일과 자원에 대해 읽기, 쓰기, 실행 권한을 '
                 '가질 수 있는지 여부를 결정·통제하는 기능이다. 목표는 비인가자로부터 기밀성, 무결성, '
                 '가용성을 보장하는 것이다.'
               ),
               JSON_OBJECT('type','heading','text','접근통제 유형: DAC / MAC / RBAC'),
               JSON_OBJECT(
                 'type','table',
                 'caption','접근통제 정책 비교',
                 'headers', JSON_ARRAY('정책','권한 부여 주체','접근 결정 기준','정책 변경','특징'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'DAC (Discretionary Access Control)',
                     '데이터 소유자',
                     '신분(Identity), 사용자/그룹',
                     '변경 용이',
                     '구현이 쉽고 유연하지만, 세밀한 정책 관리 시 복잡도가 증가할 수 있음'
                   ),
                   JSON_ARRAY(
                     'MAC (Mandatory Access Control)',
                     '시스템',
                     '보안 등급(Label)',
                     '고정적(변경 어려움)',
                     '강제적 정책에 기반한 접근통제로, 군·정부 등 높은 보안 환경에 적합'
                   ),
                   JSON_ARRAY(
                     'RBAC (Role Based Access Control)',
                     '중앙 관리자',
                     '역할(Role)',
                     '변경 용이',
                     '조직 내 역할에 기반하여 권한을 부여하므로, 대규모 조직에서 관리 효율이 높음'
                   )
                 )
               )
             )
           ),

           /* -------------------------------
              5.4.1.5 보호 모델 / 보안 아키텍처 / 보안 프레임워크
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 5,
             'subCode', '5.4.1.5',
             'title', '보호 모델·보안 아키텍처·보안 프레임워크',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','접근통제 보호 모델'),
               JSON_OBJECT(
                 'type','table',
                 'caption','벨-라파둘라 & 비바 모델',
                 'headers', JSON_ARRAY('모델','규칙','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '벨-라파둘라(BLP, 기밀성 모델)',
                     'No Read Up',
                     '낮은 등급의 주체는 높은 등급의 객체를 읽을 수 없음 (기밀성 보호)'
                   ),
                   JSON_ARRAY(
                     '벨-라파둘라(BLP, 기밀성 모델)',
                     'No Write Down',
                     '높은 등급의 주체는 낮은 등급의 객체에 기록할 수 없음 (상위 정보가 하위로 새어 나가지 않도록 보호)'
                   ),
                   JSON_ARRAY(
                     '비바 모델(Biba, 무결성 모델)',
                     'No Read Down',
                     '높은 등급의 주체는 낮은 등급의 객체를 읽을 수 없음 (무결성 보호)'
                   ),
                   JSON_ARRAY(
                     '비바 모델(Biba, 무결성 모델)',
                     'No Write Up',
                     '낮은 등급의 주체는 높은 등급의 객체를 수정할 수 없음'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','보안 아키텍처(Security Architecture)'),
               JSON_OBJECT('type','paragraph','text',
                 '보안 아키텍처는 정보 자산의 기밀성·무결성·가용성을 강화하기 위해, 관리적·물리적·기술적 '
                 '보안 요소와 그 관계를 구조적으로 설계한 청사진이다. [2023년 3회]'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '종합적인 보안 개념 수립',
                   '보안 관리 능력 향상',
                   '일관된 보안 수준 유지',
                   '표준 모델 재사용을 통한 비용 절감'
                 )
               ),
               JSON_OBJECT('type','heading','text','보안 아키텍처 영역'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '관리적 보안: 정책·지침·조직·절차 등',
                   '물리적 보안: 출입통제, 설비 보호, 환경 보호 등',
                   '기술적 보안: 암호, 방화벽, 침입 탐지/방지, 접근통제 등'
                 )
               ),
               JSON_OBJECT('type','heading','text','보안 프레임워크(Security Framework)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '구성 요소: 정보보호 요소, 전략 및 거버넌스, 침해사고 관리, 계층적 보안 기술',
                   '정보보호 요소(컴·위·요·기): 컴플라이언스, 보안 위협, 비즈니스 요구, 비즈니스 기회'
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);


-- =========================================
-- 5.4.2 시스템 보안 구현 (concept)
-- topic_id = 15402
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 15402,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------
              5.4.2.1 로그 분석
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '5.4.2.1',
             'title', '시스템 로그 분석',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','Linux 로그'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '대부분의 로그 파일은 /var/log 디렉터리에 저장된다.',
                   '로그 파일은 일반적으로 텍스트 형식으로 저장되며, tail, grep, less 등의 명령으로 분석한다.'
                 )
               ),
               JSON_OBJECT('type','heading','text','Windows 로그'),
               JSON_OBJECT('type','paragraph','text',
                 'Windows 시스템에서는 로그를 이벤트 로그 형식으로 관리하며, 이벤트 뷰어(Event Viewer)를 통해 '
                 '응용 프로그램, 보안, 시스템 로그 등을 조회·분석할 수 있다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '응용 프로그램 로그: 애플리케이션 관련 이벤트 기록',
                   '보안 로그: 로그인 시도, 권한 변경 등 보안 관련 이벤트 기록',
                   '시스템 로그: 시스템 구성 요소(드라이버, 서비스 등)의 상태 및 오류 기록'
                 )
               )
             )
           ),

           /* -------------------------------
              5.4.2.2 네트워크 보안 솔루션
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '5.4.2.2',
             'title', '네트워크 보안 솔루션',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','대표 네트워크 보안 솔루션'),
               JSON_OBJECT(
                 'type','table',
                 'caption','네트워크 보안 솔루션 정리',
                 'headers', JSON_ARRAY('솔루션','약어','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '방화벽(Firewall)',
                     'Firewall',
                     '내부 네트워크와 외부 네트워크 간 트래픽을 모니터링하여, 정책에 따라 접근을 허용하거나 차단하는 장비'
                   ),
                   JSON_ARRAY(
                     '웹 방화벽',
                     'WAF',
                     '웹 애플리케이션에 특화된 방화벽으로, SQL Injection, XSS 등 웹 공격을 탐지·차단하는 보안 장비'
                   ),
                   JSON_ARRAY(
                     '네트워크 접근 제어',
                     'NAC',
                     '단말이 내부 네트워크에 접속하려 할 때, 단말의 보안 상태를 점검하고 접속을 제어·통제하는 솔루션'
                   ),
                   JSON_ARRAY(
                     '침입 탐지 시스템',
                     'IDS',
                     '네트워크 트래픽과 시스템 이벤트를 모니터링하여 비인가 접근이나 보안 정책 위반 행위를 탐지하는 시스템'
                   ),
                   JSON_ARRAY(
                     '침입 방지 시스템',
                     'IPS',
                     '침입 탐지 기능에 더해, 공격 트래픽을 실시간으로 차단하고 유해 트래픽을 능동적으로 처리하는 시스템'
                   ),
                   JSON_ARRAY(
                     '무선 침입 방지 시스템',
                     'WIPS',
                     '비인가 무선 단말 및 취약 무선 AP를 탐지·차단하여, 무선 LAN 환경에서의 보안 위협을 방지하는 시스템'
                   ),
                   JSON_ARRAY(
                     '통합 보안 시스템',
                     'UTM',
                     '방화벽, IDS/IPS, VPN, 안티바이러스, 이메일 필터링 등 다양한 보안 기능을 하나의 장비로 통합 제공하는 솔루션'
                   ),
                   JSON_ARRAY(
                     '가상사설망',
                     'VPN',
                     '공중망(인터넷) 상에 암호화된 터널을 구성하여, 마치 전용망처럼 안전하게 통신할 수 있도록 하는 기술 [2020년 4회]'
                   )
                 )
               )
             )
           ),

           /* -------------------------------
              5.4.2.3 시스템 보안·콘텐츠 유출 방지 솔루션
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '5.4.2.3',
             'title', '시스템 보안 솔루션 및 콘텐츠 유출 방지',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','시스템 보안 솔루션'),
               JSON_OBJECT(
                 'type','table',
                 'caption','시스템 보안 솔루션',
                 'headers', JSON_ARRAY('솔루션','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '스팸 차단 솔루션',
                     '이메일 서버 앞단에서 프록시 메일 서버로 동작하며, 스팸 메일·메일 바이러스 검사를 수행하고 '
                     '내/외부 메일 본문 검색을 통해 정보 유출을 방지한다.'
                   ),
                   JSON_ARRAY(
                     '보안 운영체제(Secure OS)',
                     '운영체제 커널에 보안 기능을 추가하여, OS 수준의 취약점을 악용하는 해킹으로부터 시스템을 보호하는 OS [2020년 4회]'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','콘텐츠 유출 방지 보안 솔루션'),
               JSON_OBJECT(
                 'type','table',
                 'caption','콘텐츠 유출 방지 관련 솔루션',
                 'headers', JSON_ARRAY('솔루션','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '보안 USB',
                     '데이터 암·복호화, 사용자 식별·인증, 임의 복제 방지, 분실 시 원격 삭제 기능 등을 통해 '
                     '중요 데이터 유출을 방지하는 USB 매체'
                   ),
                   JSON_ARRAY(
                     'DLP(Data Loss Prevention)',
                     '조직 내부의 중요 자료가 외부로 유출되는 것을 탐지하고 차단하는 솔루션으로, '
                     '정보 흐름 모니터링과 실시간 차단 기능을 제공한다.'
                   ),
                   JSON_ARRAY(
                     'DRM(Digital Rights Management)',
                     '디지털 저작물의 사용 권한(열람, 복사, 인쇄 등)을 제어하여, 콘텐츠의 불법 복제·배포를 방지하는 기술'
                   )
                 )
               )
             )
           ),

           /* -------------------------------
              5.4.2.4 취약점 분석 대상 및 절차
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '5.4.2.4',
             'title', '취약점 분석 대상 및 절차',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','취약점 분석 대상'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '환경 및 시설: 전산실, 전원, 냉방, 출입통제 등 물리적 환경',
                   '하드웨어: 서버, 네트워크 장비, 단말 등',
                   '소프트웨어: OS, 미들웨어, 애플리케이션, 데이터베이스 등'
                 )
               ),
               JSON_OBJECT('type','heading','text','취약점 분석 절차 (자·진·제·진·결·보)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '자산 조사 및 분석: 보호해야 할 자산과 중요도, 영향도를 파악',
                   '진단 대상 선정: 분석 범위와 대상 시스템을 선정',
                   '제약사항 확인: 일정, 인력, 접근 권한 등 진단 시 제약 요소 확인',
                   '진단 수행: 자동화 도구 및 수동 분석을 통해 취약점 점검',
                   '결과 분석: 진단 결과를 정리하고 위험도 평가',
                   '보고서 작성 및 개선 권고: 결과 보고 및 개선 방안 제시'
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);