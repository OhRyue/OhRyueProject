-- =========================================
-- 4.3.2 네트워크 기초 활용 (concept 보강)
-- topic_id = 14302 (제4과목 > 응용 SW 기초 기술 활용 > 네트워크 기초 활용)
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 14302,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------------
              4.3.2.1 LAN / WLAN / IEEE 802.11
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '4.3.2.1',
             'title', 'LAN·WLAN과 IEEE 802.11',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','근거리 통신망(LAN)의 개요'),
               JSON_OBJECT('type','paragraph','text',
                 'LAN(Local Area Network)은 근거리(건물·층·사무실 등)를 연결하는 통신망으로, '
                 '높은 전송 속도와 낮은 오류율을 특징으로 한다.'
               ),
               JSON_OBJECT('type','heading','text','LAN 전송망 구성 형태 (버·트·링·메·성)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','LAN 토폴로지 구성 방식',
                 'headers', JSON_ARRAY('형태','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('버스형(Bus)','하나의 공용 버스(케이블)에 여러 노드가 접속하는 구조'),
                   JSON_ARRAY('트리형(Tree)','버스형을 계층 구조로 확장한 형태'),
                   JSON_ARRAY('링형(Ring)','노드들이 고리 형태로 연결되어 토큰 등을 순환시키는 구조'),
                   JSON_ARRAY('메시형(Mesh)','각 노드가 여러 노드와 직접 연결되는 고신뢰 구조'),
                   JSON_ARRAY('성형(Star)','중앙 허브/스위치에 모든 단말이 연결되는 구조')
                 )
               ),
               JSON_OBJECT('type','heading','text','무선 LAN(WLAN)의 개념'),
               JSON_OBJECT('type','paragraph','text',
                 '무선 LAN(WLAN; Wireless LAN)은 유선 LAN과 무선 단말 사이를 무선 주파수로 연결하는 네트워크로, '
                 'AP(Access Point)를 중심으로 단말이 무선으로 접속한다.'
               ),
               JSON_OBJECT('type','heading','text','IEEE 802.11 무선 LAN 표준(와이파이) [2020년 1회]'),
               JSON_OBJECT(
                 'type','table',
                 'caption','주요 IEEE 802.11 표준',
                 'headers', JSON_ARRAY('표준','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('802.11a','5GHz 대역, 최대 54Mbps 전송 속도'),
                   JSON_ARRAY('802.11b','2.4GHz 대역, 최대 11Mbps 전송 속도'),
                   JSON_ARRAY('802.11e','MAC 수준 QoS 강화, IP 전화·비디오용 품질 보장'),
                   JSON_ARRAY('802.11f','AP 간 로밍 기능 향상'),
                   JSON_ARRAY('802.11g','2.4GHz, 802.11b와 유사하지만 22Mbps 이상 고속 지원'),
                   JSON_ARRAY('802.11i','무선 LAN 보안 기능 강화'),
                   JSON_ARRAY('802.11h','전파 간섭 방지 기능 추가(802.11e 확장)'),
                   JSON_ARRAY('802.11ac','최소 1Gbit/s급 무선 LAN, 단일 링크 최소 500Mbit/s'),
                   JSON_ARRAY('802.11ad','60GHz 대역, 무압축 HD 비디오 등 대용량 고속 전송'),
                   JSON_ARRAY('802.11ax','1개의 AP로 다수 디바이스를 밀집 지원하는 고용량 Wi-Fi')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.3.2.2 WAN·프로토콜 기본요소·인터넷 접속
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '4.3.2.2',
             'title', 'WAN과 프로토콜 기본 요소·인터넷 접속',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','원거리 통신망(WAN; Wide Area Network)'),
               JSON_OBJECT('type','paragraph','text',
                 'WAN은 국가·대륙 등 넓은 지리적 범위를 연결하는 통신망으로, '
                 'LAN보다 전송 속도가 느리고 에러율이 높은 경향이 있다.'
               ),
               JSON_OBJECT('type','heading','text','WAN 연결 기술 (전·회·패)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '전용 회선 방식(Dedicated Line): 특정 사용자 간에 전용 회선을 항상 연결',
                   '회선 교환 방식(Circuit Switching): 통화 시점에만 회선을 설정해 사용하는 방식',
                   '패킷 교환 방식(Packet Switching): 데이터를 패킷 단위로 분할·전송·재조립하는 방식'
                 )
               ),
               JSON_OBJECT('type','heading','text','프로토콜의 기본 요소 (구·의·타)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','프로토콜 기본 요소',
                 'headers', JSON_ARRAY('요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('구문(Syntax)','데이터 형식, 부호, 코드, 신호 수준 등 형식 정의'),
                   JSON_ARRAY('의미(Semantic)','제어 정보의 의미, 오류 처리, 동기 제어 등'),
                   JSON_ARRAY('타이밍(Timing)','속도 조정, 전송 순서, 동기화 등 시간 관련 규칙')
                 )
               ),
               JSON_OBJECT('type','heading','text','인터넷 접속 구성 요소'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'ISP(Internet Service Provider): 인터넷 접속 서비스를 제공하는 사업자',
                   'URL(Uniform Resource Locator): 인터넷 자원의 위치를 표현하는 표기',
                   '웹 브라우저(Web Browser): URL을 해석하여 웹 페이지를 표시하는 클라이언트 프로그램'
                 )
               )
             )
           ),

           /* -------------------------------------
              4.3.2.3 OSI 7계층 모델
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '4.3.2.3',
             'title', 'OSI 7계층 구조와 계층별 역할',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','OSI 7계층(아·파·서·티·내·다·피)'),
               JSON_OBJECT('type','paragraph','text',
                 'OSI(Open Systems Interconnection) 7계층 모델은 통신 기능을 7개의 계층으로 분할해 '
                 '각 계층이 독립적으로 설계되도록 한 참조 모델로, '
                 '하위 계층의 기능을 이용해 상위 계층에 서비스를 제공한다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','OSI 7계층과 대표 프로토콜',
                 'headers', JSON_ARRAY('계층','이름','역할','대표 프로토콜'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('7계층',
                              '응용 계층(Application)',
                              '사용자 응용 프로세스와 직접 연계되는 서비스 제공',
                              'HTTP, SMTP, FTP'),
                   JSON_ARRAY('6계층',
                              '표현 계층(Presentation)',
                              '데이터 형식 변환, 암·복호화, 압축 등 표현 형태 변환',
                              'JPEG, MPEG'),
                   JSON_ARRAY('5계층',
                              '세션 계층(Session)',
                              '대화(세션) 설정·유지·종료 관리',
                              'RPC, NetBIOS'),
                   JSON_ARRAY('4계층',
                              '전송 계층(Transport)',
                              '종단 간 신뢰성 있는 데이터 전송, 오류·흐름 제어',
                              'TCP, UDP'),
                   JSON_ARRAY('3계층',
                              '네트워크 계층(Network)',
                              '라우팅, 논리 주소(IP) 기반 패킷 전달 및 QoS 수단 제공',
                              'IP, ARP, RARP, ICMP, IGMP'),
                   JSON_ARRAY('2계층',
                              '데이터 링크 계층(Data Link)',
                              '링크 설정·유지·해제, 노드 간 오류·흐름·회선 제어',
                              'HDLC, LAPB, LLC, LAPD, PPP'),
                   JSON_ARRAY('1계층',
                              '물리 계층(Physical)',
                              '전압, 커넥터, 전송 매체 등 전기·물리적 특성 정의',
                              'RS-232C, X.21')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.3.2.4 네트워크 계층·라우팅 프로토콜
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '4.3.2.4',
             'title', '네트워크 계층 프로토콜과 라우팅',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','네트워크 계층(L3) 주요 프로토콜'),
               JSON_OBJECT(
                 'type','table',
                 'caption','네트워크 계층 프로토콜 정리',
                 'headers', JSON_ARRAY('프로토콜','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('IP(Internet Protocol)',
                              '패킷 교환망에서 송·수신 간 데이터그램 전달을 위한 기본 규약'),
                   JSON_ARRAY('ARP(Address Resolution Protocol)',
                              'IP 주소를 물리 주소(MAC 주소)로 변환'),
                   JSON_ARRAY('RARP(Reverse ARP)',
                              'MAC 주소로부터 자신의 IP 주소를 알아낼 때 사용'),
                   JSON_ARRAY('ICMP(Internet Control Message Protocol)',
                              'IP 패킷 처리 중 발생하는 오류·상태 정보를 전달'),
                   JSON_ARRAY('IGMP(Internet Group Management Protocol)',
                              '호스트와 인접 라우터 간 멀티캐스트 그룹 멤버십 관리')
                 )
               ),
               JSON_OBJECT('type','heading','text','라우팅 프로토콜'),
               JSON_OBJECT('type','paragraph','text',
                 '라우팅 프로토콜은 목적지까지의 여러 경로 중 최적 경로를 선택하기 위해 '
                 '라우터 간 정보를 교환하는 통신 규약이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','내부/외부 라우팅 프로토콜',
                 'headers', JSON_ARRAY('분류','프로토콜','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('내부 라우팅(IGP)',
                              'RIP, OSPF',
                              '자율 시스템(AS) 내부에서 사용하는 라우팅 프로토콜'),
                   JSON_ARRAY('외부 라우팅(EGP)',
                              'EGP, BGP',
                              'AS 간 경로 설정을 위한 라우팅 프로토콜')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.3.2.5 L4~L7 주요 프로토콜
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 5,
             'subCode', '4.3.2.5',
             'title', '전송·세션·표현·응용 계층 프로토콜',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','전송 계층(4계층) – TCP와 UDP'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'TCP(Transmission Control Protocol): 연결 지향, 신뢰성 있는 전송 보장',
                   'UDP(User Datagram Protocol): 비연결, 단순·빠른 전송, 실시간·멀티캐스트에 적합'
                 )
               ),
               JSON_OBJECT('type','heading','text','세션 계층(5계층) 프로토콜'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'RPC(Remote Procedure Call): 원격 프로시저 호출',
                   'NetBIOS(Network Basic Input/Output System): 세션 관리 인터페이스'
                 )
               ),
               JSON_OBJECT('type','heading','text','표현 계층(6계층) 프로토콜'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'JPEG: 이미지 압축 표준',
                   'MPEG: 멀티미디어(영상·음성) 압축 표준'
                 )
               ),
               JSON_OBJECT('type','heading','text','응용 계층(7계층) 주요 프로토콜'),
               JSON_OBJECT(
                 'type','table',
                 'caption','응용 계층 프로토콜 정리',
                 'headers', JSON_ARRAY('프로토콜','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('HTTP',
                              '텍스트 기반의 하이퍼텍스트 전송 프로토콜'),
                   JSON_ARRAY('FTP',
                              'TCP/IP 기반 서버·클라이언트 간 파일 전송 프로토콜'),
                   JSON_ARRAY('SMTP',
                              '포트 25, 이메일 발송용 프로토콜'),
                   JSON_ARRAY('POP3',
                              '원격 서버로부터 이메일을 가져오는 프로토콜'),
                   JSON_ARRAY('IMAP',
                              '원격 서버에 보관된 이메일을 폴더 등 구조로 관리하며 가져오는 프로토콜'),
                   JSON_ARRAY('Telnet',
                              '원격 호스트에 접속해 터미널 세션을 제공하는 프로토콜')
                 )
               )
             )
           ),

           /* -------------------------------------
              4.3.2.6 IP·IPv4·IPv6·전환·헤더·주소
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 6,
             'subCode', '4.3.2.6',
             'title', 'IP, IPv4·IPv6, 전환 방식과 헤더·주소',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','인터넷 프로토콜(IP)의 역할'),
               JSON_OBJECT('type','paragraph','text',
                 'IP는 패킷 교환 네트워크에서 송신 호스트와 수신 호스트 간 데이터그램을 전달하기 위한 '
                 '네트워크 계층 프로토콜로, 주소 지정과 패킷 분할·조립 기능을 제공한다.'
               ),
               JSON_OBJECT('type','heading','text','IP의 특징'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '비신뢰성(Unreliable) – 에러·흐름 제어를 직접 제공하지 않음',
                   '비연결형(Connectionless) – 연결 설정 없이 독립적인 패킷 전송',
                   '현재 표준은 IPv4이나, 주소 고갈 문제로 IPv6가 점차 확산 중'
                 )
               ),
               JSON_OBJECT('type','heading','text','IPv4 vs IPv6 비교 [2020년 1회, 3회]'),
               JSON_OBJECT(
                 'type','table',
                 'caption','IPv4와 IPv6 비교',
                 'headers', JSON_ARRAY('구분','IPv4','IPv6'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('주소 길이',
                              '32bit',
                              '128bit'),
                   JSON_ARRAY('표기 방법',
                              '8비트씩 4부분 10진수 (예: 192.168.10.1)',
                              '16비트씩 8부분 16진수 (예: 2001:9e76:...:e11c)'),
                   JSON_ARRAY('주소 개수',
                              '약 45억 개',
                              '약 4.3 × 10^38개'),
                   JSON_ARRAY('품질(QoS)',
                              'Best Effort, 품질 보장 곤란',
                              '등급·서비스별 패킷 구분 가능, 품질 보장 용이'),
                   JSON_ARRAY('헤더 크기',
                              '가변 길이 헤더',
                              '고정 길이(단순 헤더)'),
                   JSON_ARRAY('보안 기능',
                              'IPsec 별도 구성',
                              'IPsec 기반 인증·암호화 기능 내장'),
                   JSON_ARRAY('Plug & Play',
                              '지원 안함',
                              '자동 구성(Plug & Play) 지원'),
                   JSON_ARRAY('모바일 IP·웹캐스팅',
                              '구현 곤란',
                              '용이'),
                   JSON_ARRAY('전송 방식',
                              'Unicast, Multicast, Broadcast',
                              'Unicast, Multicast, Anycast')
                 )
               ),
               JSON_OBJECT('type','heading','text','IPv4 → IPv6 전환 방법 (듀·터·주)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '듀얼 스택(Dual Stack): 한 장비가 IPv4와 IPv6를 모두 지원',
                   '터널링(Tunneling): IPv6 패킷을 IPv4 망을 통해 캡슐화 전송',
                   '주소 변환(Address Translation): IPv4·IPv6 주소 변환 장비 사용'
                 )
               ),
               JSON_OBJECT('type','heading','text','IPv4 헤더 구조(요소 기억용)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'Version, Header Length, Type of Service, Total Length',
                   'Identification, Flags, Fragment Offset',
                   'TTL(Time to Live), Protocol, Header Checksum',
                   'Source Address, Destination Address',
                   'Option(선택), Data'
                 )
               ),
               JSON_OBJECT('type','heading','text','IPv4 주소 클래스'),
               JSON_OBJECT(
                 'type','table',
                 'caption','IPv4 주소 클래스',
                 'headers', JSON_ARRAY('클래스','범위','용도'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('A','0.0.0.0 ~ 127.255.255.255','국가·대형 통신망'),
                   JSON_ARRAY('B','128.0.0.0 ~ 191.255.255.255','중·대형 통신망'),
                   JSON_ARRAY('C','192.0.0.0 ~ 223.255.255.255','소규모 통신망'),
                   JSON_ARRAY('D','224.0.0.0 ~ 239.255.255.255','멀티캐스트용'),
                   JSON_ARRAY('E','240.0.0.0 ~ 255.255.255.255','실험용')
                 )
               ),
               JSON_OBJECT('type','heading','text','IPv6 헤더 주요 필드'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'Version, Traffic Class, Flow Label',
                   'Payload Length, Next Header, Hop Limit',
                   'Source Address, Destination Address'
                 )
               )
             )
           ),

           /* -------------------------------------
              4.3.2.7 TCP / UDP 상세 – 헤더·제어 기법
              ------------------------------------- */
           JSON_OBJECT(
             'orderNo', 7,
             'subCode', '4.3.2.7',
             'title', 'TCP/UDP 특징·헤더·흐름·혼잡 제어',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','TCP(Transmission Control Protocol)의 특징'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '연결 지향(Connection-oriented) – 세션을 설정 후 데이터 전송',
                   '신뢰성 보장 – 재전송, 순서 제어, 오류 제어 제공',
                   '흐름 제어(Flow Control) 및 혼잡 제어(Congestion Control) 지원',
                   '기본 헤더 크기 20byte, 최대 60byte까지 확장 가능 [2020년 3회]'
                 )
               ),
               JSON_OBJECT('type','heading','text','TCP 헤더 주요 필드'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'Source Port, Destination Port',
                   'Sequence Number, Acknowledgement Number',
                   'HLEN(Header Length), Flag bits',
                   'Window Size, Checksum, Urgent Pointer',
                   'Options & Padding'
                 )
               ),
               JSON_OBJECT('type','heading','text','TCP 흐름 제어 기법 [2020년 4회]'),
               JSON_OBJECT(
                 'type','table',
                 'caption','TCP 흐름 제어',
                 'headers', JSON_ARRAY('기법','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('정지-대기(Stop & Wait)',
                              '프레임 하나 전송 후 ACK를 기다린 뒤 다음 프레임 전송 – 단순하지만 대역폭 활용도 낮음'),
                   JSON_ARRAY('슬라이딩 윈도(Sliding Window)',
                              '수신 측 윈도 크기만큼 여러 프레임을 연속 전송, ACK에 따라 윈도 범위를 이동')
                 )
               ),
               JSON_OBJECT('type','heading','text','TCP 혼잡 제어(SCFF; Slow, Avoid, Fast, Fast)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '느린 출발(Slow Start)',
                   '혼잡 회피(Congestion Avoidance)',
                   '빠른 재전송(Fast Retransmit)',
                   '빠른 회복(Fast Recovery)'
                 )
               ),
               JSON_OBJECT('type','heading','text','UDP(User Datagram Protocol)의 특징'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '비연결형(Connectionless), 비신뢰성 – 순서 보장·재전송 없음',
                   '헤더 구조가 단순해 오버헤드가 작고 전송 속도가 빠름 [2020년 4회]',
                   '순서화되지 않은 데이터그램 서비스 제공',
                   '실시간 스트리밍, 음성·영상, 멀티캐스트에 적합'
                 )
               ),
               JSON_OBJECT('type','heading','text','UDP 헤더 구조 (소·데·렝·체·다)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'Source Port Number',
                   'Destination Port Number',
                   'Length(UDP Length)',
                   'Checksum',
                   'Data'
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);