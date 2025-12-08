-- =========================================
-- 5.2.1 네트워크 구축관리 (concept)
-- topic_id = 15201
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 15201,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------
              5.2.1.1 SDN / OpenFlow
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '5.2.1.1',
             'title', 'SDN과 OpenFlow',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','SDN(Software Defined Network)'),
               JSON_OBJECT('type','paragraph','text',
                 'SDN은 개방형 API(OpenFlow 등)를 기반으로 네트워크 장비의 트래픽 경로를 지정하는 컨트롤 플레인과, '
                 '실제 트래픽 전송을 담당하는 데이터 플레인을 분리하여 네트워크를 중앙집중적으로 제어하는 기술이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','SDN의 계층 구조',
                 'headers', JSON_ARRAY('계층','구성요소'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('응용 계층','네트워크를 사용하는 애플리케이션'),
                   JSON_ARRAY('제어 계층(네트워크 OS)','컨트롤러, 정책 기반 경로 제어'),
                   JSON_ARRAY('데이터 플레인(Data Plane)','실제 패킷 포워딩을 수행하는 스위치·라우터')
                 )
               ),
               JSON_OBJECT('type','heading','text','오픈플로우(OpenFlow)'),
               JSON_OBJECT('type','paragraph','text',
                 '오픈플로우는 네트워크 장치의 컨트롤 플레인과 데이터 플레인 사이의 연계와 제어를 담당하는 '
                 '대표적인 개방형 표준 인터페이스이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '파이프라이닝(Pipelining): 여러 플로우 테이블을 순차적으로 검사하며 패킷 처리',
                   '그룹 테이블(Group Table): 멀티캐스트·로드밸런싱 등 그룹 동작 지원',
                   '보안 채널(Secure Channel): 컨트롤러와 스위치 간 제어 메시지 보호'
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.1.2 NFV / 오버레이 네트워크
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '5.2.1.2',
             'title', 'NFV와 오버레이 네트워크',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','NFV(Network Function Virtualization)'),
               JSON_OBJECT('type','paragraph','text',
                 'NFV는 범용 서버·스토리지·스위치 등 하드웨어에 가상화 기술을 적용하여 '
                 '스위치, 라우터 같은 네트워크 기능을 소프트웨어 모듈(VNF)로 제공하는 기술이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','NFV의 구성요소 (펑·인·마)',
                 'headers', JSON_ARRAY('구성요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('VNFs(Virtual Network Functions)','네트워크 기능을 구현한 소프트웨어 패키지'),
                   JSON_ARRAY('NFVI(Network Functions Virtualization Infrastructure)','VNF가 동작하는 물리·가상 인프라 계층'),
                   JSON_ARRAY('MANO(Management & Orchestration)','NFVI 자원 관리와 VNF 라이프사이클 조율·관리')
                 )
               ),
               JSON_OBJECT('type','heading','text','오버레이 네트워크(Overlay Network)'),
               JSON_OBJECT('type','paragraph','text',
                 '오버레이 네트워크는 기존 물리 네트워크 위에 논리적인 가상 네트워크를 중첩시켜 구성하는 구조로, '
                 '기존 망을 변경하지 않고도 새로운 네트워크 서비스를 유연하게 제공할 수 있다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'DHT(Distributed Hash Table)',
                   '오버레이 노드(Overlay Node)',
                   '베이스 노드(Base Node)',
                   '식별자(Identifier)와 매핑(Mapping) 구조'
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.1.3 네트워크 신기술
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '5.2.1.3',
             'title', '네트워크 관련 신기술',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','주요 네트워크 신기술 정리'),
               JSON_OBJECT(
                 'type','table',
                 'caption','네트워크 신기술 요약',
                 'headers', JSON_ARRAY('기술','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('Wi-SUN',
                              '스마트 그리드와 연계하여 전기·수도·가스 등의 공급자와 사용자가 '
                              '무선 네트워크로 효율적인 관리를 수행하도록 하는 IEEE 802.15.4 기반 무선 통신 기술'),
                   JSON_ARRAY('NFC',
                              '약 10cm 이내 거리에서 동작하는 저전력 비접촉식 근거리 무선 통신 기술'),
                   JSON_ARRAY('스몰 셀(Small Cell)',
                              '낮은 출력·좁은 커버리지의 소형 기지국으로, 트래픽 밀집 구간 커버 확장에 사용'),
                   JSON_ARRAY('SON(Self Organizing Network)',
                              '무선 접속망의 계획·구성·최적화·장애 복구를 자동화하여 운영 효율을 높이는 기술'),
                   JSON_ARRAY('Bluetooth / BLE',
                              '2.4GHz 대역을 사용하는 근거리 무선 통신 규격과 저전력 버전(BLE)'),
                   JSON_ARRAY('Ad-hoc Network',
                              '고정된 기반망 없이 이동 노드들이 자율적으로 구성하는 네트워크'),
                   JSON_ARRAY('Mesh Network',
                              '각 노드가 다수의 노드와 메쉬 형태로 연결된 구조로, 대규모 디바이스 네트워크 구성에 최적화된 방식'),
                   JSON_ARRAY('UWB',
                              '초광대역 주파수와 매우 낮은 전력을 이용해 고속 데이터 전송을 수행하는 무선 기술'),
                   JSON_ARRAY('USN(UsN)',
                              '센서와 RFID 태그로부터 주변 환경 정보를 탐지하여 실시간으로 네트워크에 연결하는 센서 네트워크'),
                   JSON_ARRAY('WBAN',
                              '체내 또는 인체 주변 3m 이내에서 동작하는 신체 영역 근거리 무선 네트워크'),
                   JSON_ARRAY('NDN',
                              'IP 주소 대신 데이터 이름을 활용하여 콘텐츠 중심으로 정보를 전달하는 미래 인터넷 구조'),
                   JSON_ARRAY('네트워크 슬라이싱',
                              '하나의 물리 코어 네트워크를 다수의 가상 네트워크로 분리해 고객 맞춤형 서비스를 제공하는 5G 핵심 기술'),
                   JSON_ARRAY('NOMA',
                              '동일 자원 상에서 여러 단말 데이터를 동시에 전송하여 주파수 효율을 향상시키는 비직교 다중접속 기술'),
                   JSON_ARRAY('MEC',
                              '기지국 인근에 분산 클라우드 인프라를 배치하여 엣지에서 서비스·캐시를 제공하는 컴퓨팅 구조'),
                   JSON_ARRAY('피코넷(Piconet)',
                              '블루투스·UWB 장치들이 소규모 무선 네트워크를 형성하는 구조 [2020년 1회]')
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.1.4 네트워크 설치 구조/장비/라우팅
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '5.2.1.4',
             'title', '네트워크 설치 구조와 장비, 라우팅',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','네트워크 설치 구조 (버스·트리·링·성)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','LAN 구성 형태별 특징',
                 'headers', JSON_ARRAY('구조','장점','단점'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     '버스형(Bus)',
                     '구조 단순, 설치 용이, 비용 저렴, 노드 추가·삭제가 쉬움',
                     '노드 과도 추가 시 성능 저하, 특정 구간 장애 시 전체에 영향'
                   ),
                   JSON_ARRAY(
                     '트리형(Tree)',
                     '계층적 구조로 확장 용이, 허브 추가로 많은 단말 연결 가능',
                     '허브 장애 시 해당 하위 노드에 영향'
                   ),
                   JSON_ARRAY(
                     '링형(Ring)',
                     '링 구성 변경·추가·삭제가 비교적 용이',
                     '링 일부 장애가 전체 통신에 영향'
                   ),
                   JSON_ARRAY(
                     '성형(Star)',
                     '소규모 네트워크에 적합, 설치·재구성 용이',
                     '중앙 허브 장애 시 전체 네트워크 중단'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','3계층 네트워크 구축 모델 (코·분·액)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '코어 계층(Core Layer): 백본 역할, 고속·고신뢰 백본 스위칭',
                   '분배 계층(Distribution Layer): 액세스 계층 집계, 정책·라우팅 적용',
                   '액세스 계층(Access Layer): 단말·서버가 직접 접속하는 계층'
                 )
               ),
               JSON_OBJECT('type','heading','text','주요 네트워크 장비'),
               JSON_OBJECT(
                 'type','table',
                 'caption','네트워크 장비 유형',
                 'headers', JSON_ARRAY('장비','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('스위치',
                              'OSI 2계층 장비로, 프레임의 목적지 MAC 주소를 기반으로 포워딩을 수행'),
                   JSON_ARRAY('라우터',
                              'OSI 3계층 장비로, 네트워크 간 최적 경로를 계산·선택하여 패킷을 전달'),
                   JSON_ARRAY('광전송 장비',
                              '광케이블을 이용해 스위칭 노드를 연결하며 장거리 고속 전송에 사용'),
                   JSON_ARRAY('액세스 장비',
                              '최종 사용자와 공중망(통신사업자망) 사이를 연결하는 장비'),
                   JSON_ARRAY('이동 통신 장비',
                              '기지국·제어국·교환기 등으로 구성된 이동통신 인프라'),
                   JSON_ARRAY('다중화기(Multiplexer)',
                              '하나의 회선을 시간·주파수·코드로 분할해 여러 신호를 전송하는 장비')
                 )
               ),
               JSON_OBJECT('type','heading','text','라우팅 프로토콜 (ROB)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','라우팅 프로토콜 특징',
                 'headers', JSON_ARRAY('프로토콜','특징'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'RIP',
                     '거리 벡터 라우팅, 메트릭으로 홉 수 사용, 최대 홉 수 15, 소규모 망에 적합. '
                     '최단 경로 계산에 Bellman-Ford 알고리즘 사용. [2020년 1·3회]'
                   ),
                   JSON_ARRAY(
                     'OSPF',
                     '링크 상태 라우팅, 지연·처리량 등 다양한 메트릭 사용, 홉 수 제한 없음.'
                   ),
                   JSON_ARRAY(
                     'BGP',
                     '자율 시스템(AS) 간 경로 선택에 사용되는 외부 게이트웨이 프로토콜로, ISP 간 상호 연동에 사용.'
                   )
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.1.5 광전송 / 이동통신 / 다중화
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 5,
             'subCode', '5.2.1.5',
             'title', '광전송 기술과 이동통신, 다중화 방식',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','광전송 기술 [2020년 4회]'),
               JSON_OBJECT(
                 'type','table',
                 'caption','광전송 표준과 기술',
                 'headers', JSON_ARRAY('기술','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('SONET',
                              '고속 디지털 통신을 위한 북미 표준 광전송 규격. 동기식 디지털 다중화 신호 계위를 정의.'),
                   JSON_ARRAY('SDH',
                              'SONET을 기초로 한 ITU 국제 표준 동기식 디지털 계층 규격.'),
                   JSON_ARRAY('WDM',
                              '서로 다른 파장의 복수 광신호를 하나의 광섬유로 동시에 전송하는 파장 분할 다중화 기술.'),
                   JSON_ARRAY('DWDM',
                              '파장 간격을 더욱 촘촘하게 나누어 대용량 데이터 전송을 지원하는 고밀도 WDM 기술.'),
                   JSON_ARRAY('CET',
                              '광역 통신망에서 고속 패킷 전송을 지원하는 차세대 Carrier Ethernet Transport 기술.')
                 )
               ),
               JSON_OBJECT('type','heading','text','이동 통신 방식'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'CDMA(Code Division Multiple Access)',
                   'GSM(Global System for Mobile Communications)',
                   'WCDMA(Wideband CDMA)'
                 )
               ),
               JSON_OBJECT('type','heading','text','다중화기 종류'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   'FDM(Frequency Division Multiplexing): 주파수 대역을 분할하여 다중 전송',
                   'TDM(Time Division Multiplexing): 시간을 슬롯으로 나누어 다중 전송',
                   'CDM(Code Division Multiplexing): 코드 확산을 이용한 다중 접속'
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);

-- =========================================
-- 5.2.2 SW 구축관리 (concept)
-- topic_id = 15202
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 15202,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------
              5.2.2.1 SW 개발 트렌드: AI / VR / 블록체인
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '5.2.2.1',
             'title', '소프트웨어 개발 트렌드: AI · VR/AR · 블록체인',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','인공지능(AI; Artificial Intelligence)'),
               JSON_OBJECT('type','paragraph','text',
                 '인공지능은 인간의 지적 능력을 소프트웨어로 구현하여 컴퓨터가 지능적인 행동과 판단을 수행하도록 하는 기술이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '기계학습(Machine Learning)',
                   '딥러닝(Deep Learning)'
                 )
               ),
               JSON_OBJECT('type','heading','text','가상 현실과 증강 현실'),
               JSON_OBJECT(
                 'type','table',
                 'caption','VR / AR / MR 비교',
                 'headers', JSON_ARRAY('구분','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('가상 현실(VR)',
                              '컴퓨터 기술로 만들어낸 실제와 유사하지만 가상의 환경 전체를 체험하는 기술'),
                   JSON_ARRAY('증강 현실(AR)',
                              '실세계 영상 위에 가상의 정보나 물체를 합성하여 현실에 겹쳐 보이게 하는 기술'),
                   JSON_ARRAY('혼합 현실(MR)',
                              '실세계와 가상세계 요소를 동시에 표현하고, 양쪽과 실시간 상호작용이 가능한 하이브리드 현실')
                 )
               ),
               JSON_OBJECT('type','heading','text','블록체인(Block Chain)'),
               JSON_OBJECT('type','paragraph','text',
                 '블록체인은 분산 데이터베이스의 한 형태로, 임의 조작이 어려운 블록들의 연결 구조를 이용해 '
                 '거래 내역을 안전하게 분산 저장하는 기술이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '동작 과정: 거래(Transaction) → 암호화(Block) → 상호 연결(Chaining) → 분산 저장(Propagation)',
                   '주요 기술: 분산원장, 공개키 기반 구조, 암호화 해시, 스마트 계약, 합의 알고리즘, 분산 애플리케이션'
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.2.2 AI · ML · DL
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '5.2.2.2',
             'title', '인공지능 수준 분류와 기계학습/딥러닝',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','인공지능의 지능 수준에 따른 분류'),
               JSON_OBJECT(
                 'type','table',
                 'caption','AI 수준별 예시',
                 'headers', JSON_ARRAY('수준','내용','예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('1단계','단순 제어 프로그램','에어컨, 청소기, 세탁기 등 단순 제어 로직'),
                   JSON_ARRAY('2단계','고전적 인공지능(탐색·추론·지식)','전문가 시스템 등 지식 기반 추론 시스템'),
                   JSON_ARRAY('3단계','기계학습 인공지능','정제된 데이터를 학습하고 결과를 예측하는 추천 시스템 등'),
                   JSON_ARRAY('4단계','딥러닝 인공지능','대규모 데이터를 자동 학습하여 복잡 문제를 해결하는 시스템')
                 )
               ),
               JSON_OBJECT('type','heading','text','기계학습 방법 분류 (지·비·강·준)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','기계학습 방법',
                 'headers', JSON_ARRAY('방법','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('지도학습',
                              '입력 데이터에 대한 정답(목표값)을 함께 제공하며 학습하는 방식'),
                   JSON_ARRAY('비지도학습',
                              '정답 없이 데이터의 구조·패턴을 스스로 발견하는 방식'),
                   JSON_ARRAY('강화학습',
                              '행위에 대한 보상·벌점을 통해 최적 행동을 학습하는 방식'),
                   JSON_ARRAY('준지도학습',
                              '레이블이 있는 데이터와 없는 데이터를 함께 활용하는 방식')
                 )
               ),
               JSON_OBJECT('type','heading','text','딥러닝 주요 알고리즘 (심·합·순)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','딥러닝 알고리즘',
                 'headers', JSON_ARRAY('알고리즘','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('DNN(심층신경망)',
                              '입력·출력 사이에 다수의 은닉 계층을 두어 비선형 관계를 모델링하는 신경망 구조'),
                   JSON_ARRAY('CNN(합성곱 신경망)',
                              '컨볼루션과 풀링을 반복해 특징을 추출하며 차원을 축소하는 구조로, 영상 처리에 널리 사용'),
                   JSON_ARRAY('RNN(순환 신경망)',
                              '이전 시점의 은닉 상태를 다음 시점 입력에 함께 사용하여 시계열·연속 데이터 패턴을 학습')
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.2.3 블록체인 합의 알고리즘
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '5.2.2.3',
             'title', '블록체인 합의 알고리즘',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','블록체인 합의 알고리즘'),
               JSON_OBJECT(
                 'type','table',
                 'caption','합의 알고리즘 비교',
                 'headers', JSON_ARRAY('알고리즘','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('PoW(Proof of Work)',
                              '난이도가 높은 연산 문제를 가장 먼저 푼 참여자에게 블록 생성 권한을 부여하는 방식'),
                   JSON_ARRAY('PoS(Proof of Stake)',
                              '보유 지분이 많은 참여자에게 유리하게 블록 생성 권한을 분배하는 방식으로, '
                              '에너지 소모를 줄이는 것을 목표로 함'),
                   JSON_ARRAY('PBFT(Practical Byzantine Fault Tolerance)',
                              '참가자 중 1명이 리더 역할을 수행하고, 전체 응답을 집계해 다수 의견으로 블록을 확정하는 합의 방식')
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.2.4 소프트웨어 관련 신기술
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '5.2.2.4',
             'title', '소프트웨어 관련 신기술',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','소프트웨어 관련 신기술'),
               JSON_OBJECT(
                 'type','table',
                 'caption','대표 SW 신기술',
                 'headers', JSON_ARRAY('기술','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('디지털 트윈(Digital Twin)',
                              '물리적 자산과 동일한 가상 모델을 소프트웨어로 구현하여, 설계·운영·정비 전 과정의 효율을 높이는 기술. '
                              '자산 최적화, 사고 최소화, 생산성 향상에 활용된다. [2020년 3회]'),
                   JSON_ARRAY('SOA(Service Oriented Architecture)',
                              '서비스 단위로 분할된 애플리케이션 조각을 느슨하게 결합하여 하나의 완성된 애플리케이션을 구성하는 아키텍처. '
                              '비즈니스 계층·표현 계층·프로세스 계층 등으로 구성된다. [2020년 4회]'),
                   JSON_ARRAY('N-Screen',
                              '하나의 콘텐츠를 PC, 스마트폰 등 여러 기기에서 끊김 없이 이용할 수 있도록 하는 서비스'),
                   JSON_ARRAY('Mashup',
                              '웹에서 제공하는 여러 정보와 서비스를 조합하여 새로운 서비스나 응용을 만드는 기술 [2020년 3회]')
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.2.5 Secure SDLC와 보안 개발 방법론
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 5,
             'subCode', '5.2.2.5',
             'title', 'Secure SDLC와 보안 개발 프레임워크',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','소프트웨어 개발 생명주기(SDLC) 단계별 보안 활동'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '요구사항 분석: 보안 요구사항 식별·정의',
                   '설계: 위협 모델링, 보안 설계 패턴 적용',
                   '구현: 안전한 코딩 규칙 적용, 코드 리뷰',
                   '테스트: 취약점 진단·보안 테스트 수행',
                   '유지보수: 취약점 패치, 로그 분석·모니터링'
                 )
               ),
               JSON_OBJECT('type','heading','text','Secure SDLC(보안 통합 개발 생명주기)'),
               JSON_OBJECT('type','paragraph','text',
                 'Secure SDLC는 설계와 구현 단계에서 발생할 수 있는 보안 약점을 최소화하기 위해, '
                 '개발 생명주기 전 단계에 보안 활동을 통합한 개발 방법론이다.'
               ),
               JSON_OBJECT('type','heading','text','대표 Secure SDLC 모델'),
               JSON_OBJECT(
                 'type','table',
                 'caption','Secure SDLC 관련 프레임워크',
                 'headers', JSON_ARRAY('모델','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('BSIMM',
                              '보안 활동 성숙도 수준을 영역별로 측정하여, 조직의 소프트웨어 보안 능력을 향상시키는 데 초점을 둔 프레임워크'),
                   JSON_ARRAY('Open SAMM',
                              'OWASP에서 개발한 개방형 보안 프레임워크로, 규모에 따라 점진적으로 확대 적용 가능한 구조. '
                              '설계 리뷰·코드 리뷰·보안 테스팅 3가지 검증 활동의 연계성을 강조한다.'),
                   JSON_ARRAY('Seven TouchPoints',
                              '실무에서 검증된 보안 모범 사례를 SDLC에 통합한 개발 보안 생명주기 방법론 [2020년 3회]'),
                   JSON_ARRAY('MS SDL',
                              '마이크로소프트가 자사 제품에 의무 적용하는 보안 강화 프레임워크로, '
                              '개발 전후(pre/post) SDL 비교를 통해 보안 강도를 평가한다.'),
                   JSON_ARRAY('OWASP CLASP',
                              '역할 기반·활동 중심 보안 프레임워크로, 기존 운영 시스템에 적용하기 쉬운 구조. '
                              '설계·코딩 오류를 찾아 개선할 수 있도록 취약점 목록을 개발팀에 제공한다. [2021년 3회]')
                 )
               )
             )
           )

         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = 15202);



-- =========================================
-- 5.2.3 HW 구축관리 (concept)
-- topic_id = 15203
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 15203,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------
              5.2.3.1 클라우드 컴퓨팅
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '5.2.3.1',
             'title', '클라우드 컴퓨팅 개요와 분류',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','클라우드 컴퓨팅(Cloud Computing)'),
               JSON_OBJECT('type','paragraph','text',
                 '클라우드 컴퓨팅은 인터넷을 통해 가상화된 컴퓨팅 자원과 서비스를 제공하여, '
                 '사용자가 자체 인프라 없이 필요한 IT 리소스를 온디맨드로 사용하는 구조이다.'
               ),
               JSON_OBJECT('type','heading','text','클라우드 유형 (사·공·하)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','클라우드 컴퓨팅 분류',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('사설 클라우드(Private Cloud)',
                              '조직 내부 인프라(IDC, 서버 등)를 기반으로 자체 구축·운영하는 클라우드'),
                   JSON_ARRAY('공용 클라우드(Public Cloud)',
                              '클라우드 서비스 제공업체가 다수 고객에게 공용 인프라를 제공하는 형태'),
                   JSON_ARRAY('하이브리드 클라우드(Hybrid Cloud)',
                              '사설 클라우드와 공용 클라우드를 함께 사용하여 유연성과 보안성을 동시에 확보하는 형태')
                 )
               ),
               JSON_OBJECT('type','heading','text','클라우드 서비스 유형 (IaaS / PaaS / SaaS)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','클라우드 서비스 모델',
                 'headers', JSON_ARRAY('유형','약어','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('인프라형 서비스','IaaS',
                              '서버·스토리지 같은 인프라 자원을 가상머신 형태로 제공하는 서비스'),
                   JSON_ARRAY('플랫폼형 서비스','PaaS',
                              '애플리케이션 개발·실행·관리를 위한 플랫폼과 런타임 환경을 제공하는 서비스'),
                   JSON_ARRAY('소프트웨어형 서비스','SaaS',
                              '응용 소프트웨어를 중앙에서 호스팅하고, 사용자는 웹 브라우저 등으로 이용하는 서비스')
                 )
               ),
               JSON_OBJECT('type','heading','text','클라우드 컴퓨팅 핵심 기술요소 (컴·스·아·컨·분·네)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '컴퓨터 가상화',
                   '스토리지 가상화',
                   'I/O 가상화',
                   '컨테이너 기술',
                   '분산 처리 기술(Distributed Computing)',
                   '네트워크 가상화 기술'
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.3.2 엣지 컴퓨팅 / SDDC
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '5.2.3.2',
             'title', '엣지 컴퓨팅과 SDDC',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','엣지 컴퓨팅(Edge Computing)'),
               JSON_OBJECT('type','paragraph','text',
                 '엣지 컴퓨팅은 네트워크 엣지(사용자·디바이스 근처)에 연산 능력을 배치하여, '
                 '데이터 처리와 연산을 분산시키는 구조이다. 지연을 줄이고 대역폭 사용을 최적화할 수 있다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '데이터 소스(센서·단말기기)',
                   '인텔리전스 레이어(엣지 분석·처리 기능)'
                 )
               ),
               JSON_OBJECT('type','heading','text','SDDC(Software-Defined Data Center)'),
               JSON_OBJECT('type','paragraph','text',
                 'SDDC는 데이터센터 내 모든 하드웨어 자원을 가상화하여 자원 풀로 구성하고, '
                 '소프트웨어로 컴퓨팅·스토리지·네트워크·관리를 일괄 제어하는 데이터센터 아키텍처이다.'
               ),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '컴퓨팅·네트워킹·스토리지·관리를 모두 소프트웨어로 정의',
                   '인력 개입 없이 소프트웨어 기반 자동 제어·관리',
                   '데이터센터 자원을 가상화하여 서비스 형태로 제공'
                 )
               ),
               JSON_OBJECT('type','heading','text','SDDC 구성요소 (컴·네·스·프) [2020년 4회]'),
               JSON_OBJECT(
                 'type','table',
                 'caption','SDDC 구성 요소',
                 'headers', JSON_ARRAY('요소','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('SDC(Computing)',
                              '소프트웨어 정의 컴퓨팅 환경으로, CPU·메모리를 소프트웨어 명령으로 제어'),
                   JSON_ARRAY('SDN(Networking)',
                              '개방형 API 기반으로 트래픽 전달 동작을 소프트웨어 컨트롤러에서 제어·관리하는 가상 네트워크'),
                   JSON_ARRAY('SDS(Storage)',
                              '이기종 물리 디스크를 하나의 논리 스토리지로 통합하는 가상화 스토리지 기술'),
                   JSON_ARRAY('프로비저닝',
                              '가상 자원에 대한 자동 할당·회수를 담당하는 자원 관리 기술')
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.3.3 스토리지 / 백업 / 고가용성
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '5.2.3.3',
             'title', '스토리지, 백업, 고가용성',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','스토리지 시스템 유형 (DAS / NAS / SAN)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','저장 장치 유형 비교',
                 'headers', JSON_ARRAY('유형','설명','장점','단점'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY(
                     'DAS',
                     '호스트 버스 어댑터에 디스크를 직접 연결하는 구조 [2020년 3·4회]',
                     '전용 케이스로 안정성이 높고, 전송 성능 보장',
                     '서버 간 파일 공유가 어렵고, 확장성·유연성이 부족'
                   ),
                   JSON_ARRAY(
                     'NAS',
                     '서버와 저장 장치를 네트워크로 연결하여 파일 단위로 접근하는 구조',
                     '이기종 간 파일 공유 가능, 설치·관리 용이',
                     '네트워크 대역폭을 점유하고, OLTP 성능 저하와 장애 포인트 증가 가능'
                   ),
                   JSON_ARRAY(
                     'SAN',
                     '서버와 스토리지를 전용 스토리지 네트워크로 연결하여 블록 단위로 데이터 접근',
                     '무정지 확장성, 고성능, 관리 효율 우수',
                     '구축 비용이 높고, 호환성 관리가 필요'
                   )
                 )
               ),
               JSON_OBJECT('type','heading','text','백업(Backup) 유형'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '데이터 범위: 전체 백업, 차분 백업, 증분 백업',
                   '백업 주기: 일일·주간·월간·연간·임시/수시 백업',
                   '백업 매체: 테이프, 디스크, VTL(Virtual Tape Library), 가상화 백업'
                 )
               ),
               JSON_OBJECT('type','heading','text','고가용성(HA; High Availability)'),
               JSON_OBJECT('type','paragraph','text',
                 '고가용성은 두 개 이상의 시스템을 클러스터로 구성하여, 한 시스템에 장애가 발생하더라도 '
                 '다른 시스템이 빠르게 서비스 역할을 대신 수행하도록 하는 기술이다.'
               ),
               JSON_OBJECT(
                 'type','table',
                 'caption','고가용성 유형 (핫·뮤·콘)',
                 'headers', JSON_ARRAY('유형','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('상시 대기 방식(Hot Standby)',
                              '운영 시스템과 별도로 대기 시스템을 두었다가, 장애 발생 시 대기 시스템으로 전환'),
                   JSON_ARRAY('상호 인수(Mutual Take-Over)',
                              '두 시스템이 각각 업무를 수행하다가 한쪽 장애 시 남은 시스템이 두 업무를 동시에 처리'),
                   JSON_ARRAY('동시적 접근(Concurrent Access)',
                              '여러 시스템이 모두 Active 상태로 병렬 처리하며, 하나 장애 시 나머지가 처리량을 분담')
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.3.4 서버 SW 및 운영
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '5.2.3.4',
             'title', '서버 소프트웨어와 운영 개요',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','서버 탑재 소프트웨어'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '운영체제(OS)',
                   'DBMS',
                   '보안 솔루션',
                   '클라우드·가상화 솔루션',
                   '웹 서버 및 WAS'
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);


-- =========================================
-- 5.2.4 DB 구축관리 (concept)
-- topic_id = 15204
-- =========================================
INSERT INTO concept (topic_id, sections_json)
SELECT 15204,
       JSON_OBJECT(
         'sections', JSON_ARRAY(

           /* -------------------------------
              5.2.4.1 빅데이터와 하둡 생태계
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '5.2.4.1',
             'title', '빅데이터 특성 및 하둡 생태계',
             'importance', 5,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','빅데이터(Big Data)의 3V 특성'),
               JSON_OBJECT(
                 'type','table',
                 'caption','빅데이터의 3가지 핵심 특성',
                 'headers', JSON_ARRAY('특성','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('Volume','페타바이트(PB) 수준의 대규모 데이터 양'),
                   JSON_ARRAY('Variety','정형·반정형·비정형 등 다양한 형태의 데이터'),
                   JSON_ARRAY('Velocity','빠른 속도로 수집·생성·처리되는 데이터')
                 )
               ),
               JSON_OBJECT('type','heading','text','빅데이터 기술 구성'),
               JSON_OBJECT(
                 'type','table',
                 'caption','빅데이터 주요 기술',
                 'headers', JSON_ARRAY('구분','기술','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('비정형 데이터 수집','Chukwa',
                              '각 서버 에이전트가 수집한 데이터를 컬렉터가 받아 HDFS에 저장'),
                   JSON_ARRAY('정형 데이터 수집','Sqoop',
                              'RDBMS에서 HDFS로 데이터를 수집하는 도구'),
                   JSON_ARRAY('분산 데이터 저장','HDFS',
                              '대용량 파일을 분산 서버에 저장하고 고속 처리할 수 있게 하는 하둡 파일 시스템'),
                   JSON_ARRAY('분산 처리','Hadoop',
                              '일반 PC급 서버를 묶어 가상화된 대용량 스토리지를 구성하고, 빅데이터를 분산 저장·처리하는 플랫폼 [2020년 1회]'),
                   JSON_ARRAY('분산 처리 모델','MapReduce',
                              '대용량 데이터를 분산 처리한 뒤 다시 합치는 병렬 처리 모델 [2020년 4회]'),
                   JSON_ARRAY('분산 데이터베이스','HBase',
                              '컬럼 기반 분산 저장소로 HDFS와 연동되는 NoSQL DB'),
                   JSON_ARRAY('데이터 가공','Pig',
                              'Pig Latin 스크립트로 MapReduce를 추상화하여 대량 데이터 분석을 지원'),
                   JSON_ARRAY('데이터 웨어하우스','Hive',
                              '하둡 기반 DW 솔루션으로 HiveQL을 제공'),
                   JSON_ARRAY('분석·시각화','R',
                              '통계 분석과 시각화를 위한 오픈소스 프로그래밍 언어')
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.4.2 NoSQL
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '5.2.4.2',
             'title', 'NoSQL 개요와 유형',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','NoSQL(Not Only SQL)의 BASE 특성'),
               JSON_OBJECT(
                 'type','table',
                 'caption','BASE 특성',
                 'headers', JSON_ARRAY('특성','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('Basically Available',
                              '분산 환경에서 언제든지 데이터에 접근 가능하도록 가용성을 중시'),
                   JSON_ARRAY('Soft-State',
                              '노드 상태가 외부 전송 정보에 의해 결정되며, 특정 시점에는 일관성이 보장되지 않을 수 있음'),
                   JSON_ARRAY('Eventually Consistent',
                              '일정 시간이 지나면 전체 데이터가 결국 일관된 상태에 도달하는 특성')
                 )
               ),
               JSON_OBJECT('type','heading','text','NoSQL 유형 (키·컬·도·그)'),
               JSON_OBJECT(
                 'type','table',
                 'caption','NoSQL 유형과 예시',
                 'headers', JSON_ARRAY('유형','사례'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('Key-Value Store','Redis, DynamoDB'),
                   JSON_ARRAY('Column Family Store','HBase, Cassandra'),
                   JSON_ARRAY('Document Store','MongoDB, Couchbase'),
                   JSON_ARRAY('Graph Store','Neo4j, AllegroGraph')
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.4.3 DB 관련 신기술
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '5.2.4.3',
             'title', 'DB 관련 신기술',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','데이터베이스 관련 신기술'),
               JSON_OBJECT(
                 'type','table',
                 'caption','DB 관련 신기술',
                 'headers', JSON_ARRAY('기술','설명'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('Data Mining',
                              '대량 데이터에서 변수 간 상관관계를 분석하고 유의미한 패턴을 찾아내는 기법 [2020년 3회]'),
                   JSON_ARRAY('Digital Twin',
                              '물리적 사물을 컴퓨터에 동일하게 표현하는 가상 모델'),
                   JSON_ARRAY('Zigbee',
                              '저전력 디지털 라디오를 사용하는 근거리 무선 통신 프로토콜')
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.4.4 DB 무결성과 보안
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 4,
             'subCode', '5.2.4.4',
             'title', '데이터베이스 무결성과 보안',
             'importance', 4,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','데이터베이스 무결성 관리 방법'),
               JSON_OBJECT(
                 'type','table',
                 'caption','무결성 구현 방식',
                 'headers', JSON_ARRAY('유형','설명','예시'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('선언적 방법',
                              'DBMS 기능을 이용하여 제약조건으로 무결성을 구현',
                              'DDL, 기본키(PK), 외래키(FK) 제약 등'),
                   JSON_ARRAY('절차적 방법',
                              '애플리케이션 로직이나 데이터베이스 객체로 무결성을 구현',
                              '트리거, 프로시저, 애플리케이션 코드')
                 )
               ),
               JSON_OBJECT('type','heading','text','데이터베이스 보안의 3요소 (기·무·가)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '기밀성(Confidentiality): 인가된 사용자만 데이터에 접근',
                   '무결성(Integrity): 데이터가 정확·일관되게 유지',
                   '가용성(Availability): 필요 시 데이터와 서비스에 접근 가능'
                 )
               )
             )
           ),

           /* -------------------------------
              5.2.4.5 데이터베이스 표준화와 관리 조직
              ------------------------------- */
           JSON_OBJECT(
             'orderNo', 5,
             'subCode', '5.2.4.5',
             'title', '데이터베이스 표준화와 관리 조직',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','데이터베이스 표준화 구성요소 (표·관·절)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '데이터 표준',
                   '데이터 표준 관리 조직',
                   '데이터 표준화 절차'
                 )
               ),
               JSON_OBJECT('type','heading','text','데이터 표준 관리 대상 (용·단·도·코)'),
               JSON_OBJECT(
                 'type','list',
                 'items', JSON_ARRAY(
                   '표준 용어',
                   '표준 단어',
                   '표준 도메인',
                   '표준 코드'
                 )
               ),
               JSON_OBJECT('type','heading','text','데이터 표준 관리 조직 역할'),
               JSON_OBJECT(
                 'type','table',
                 'caption','데이터 표준 관리 조직',
                 'headers', JSON_ARRAY('조직','역할'),
                 'rows', JSON_ARRAY(
                   JSON_ARRAY('전사 데이터 관리자',
                              '조직 전체 데이터 표준 정책·전략 수립 및 관리'),
                   JSON_ARRAY('업무 데이터 관리자',
                              '각 업무 영역 데이터 표준 정의·관리'),
                   JSON_ARRAY('업무 시스템 관리자',
                              '운영 시스템에서 데이터 표준 적용·준수 관리')
                 )
               )
             )
           )

         )
       )
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);