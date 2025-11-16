SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_cert;

-- topic id 매핑 (WRITTEN)
SET @topic_analysis      := 11101;  -- 1.1.1 현행 시스템 분석
SET @topic_req_methods   := 11102;  -- 1.1.2 요구사항 확인 기법
SET @topic_modeling      := 11103;  -- 1.1.3 분석 모델/요구 관리
SET @topic_ui_req        := 11201;  -- 1.2.1 UI 요구사항/화면흐름
SET @topic_common_module := 11301;  -- 1.3.1 공통 모듈 설계
SET @topic_oop_design    := 11302;  -- 1.3.2 객체 지향 설계
SET @topic_if_req        := 11401;  -- 1.4.1 인터페이스 요구사항 확인
SET @topic_if_target     := 11402;  -- 1.4.2 인터페이스 대상 식별
SET @topic_if_detail     := 11403;  -- 1.4.3 인터페이스 상세 설계

-- ============================
-- 6-1) 1.1.1 현행 시스템 분석
-- ============================
INSERT INTO concept (topic_id, sections_json)
SELECT @topic_analysis,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.1.1.1',
             'title', '플랫폼 기능 및 구조 분석',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','(1) 플랫폼의 개념'),
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '플랫폼은 애플리케이션을 구동시키기 위한 공통 실행 환경입니다.',
                 'OS, 미들웨어, 런타임, 공통 라이브러리 등이 포함됩니다.',
                 '동일 플랫폼에서는 배포/운영 표준이 맞춰져 유지보수 효율이 높아집니다.'
               )),
               JSON_OBJECT('type','heading','text','(2) 현행 시스템 분석 목적'),
               JSON_OBJECT('type','paragraph','text',
                 '성능, 보안, 가용성, 확장성 관점에서 현재 시스템의 한계를 파악하여 요구사항 정의와 목표 성능을 설정하는 것이 핵심입니다.'
               ),
               JSON_OBJECT('type','heading','text','(3) 수집해야 할 대표 지표'),
               JSON_OBJECT(
                 'type','table',
                 'caption','현행 시스템 성능/운영 지표 예시',
                 'headers',JSON_ARRAY('구분','내용','예시'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('처리량(Throughput)','단위 시간당 처리 건수','분당 주문 처리 300건'),
                   JSON_ARRAY('응답시간(Response Time)','요청~첫 응답까지 시간','평균 250ms / 95% 500ms'),
                   JSON_ARRAY('자원 사용률(Utilization)','CPU/메모리/디스크 사용 비율','CPU 평균 65%'),
                   JSON_ARRAY('가용성(Availability)','서비스 가능한 시간 비율','월 가용성 99.5%')
                 )
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.1.1.2',
             'title', '비기능 요구의 기반 데이터 수집',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text',
                 '현행 분석 단계에서 수집한 지표는 이후 성능/용량/장애 대응 요구사항의 근거가 됩니다. '
                 '특히 피크 타임 트래픽, 과거 장애 이력, 배치 처리 시간 등을 놓치지 않고 정리하는 것이 중요합니다.'
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_analysis);

-- ============================
-- 6-2) 1.1.2 요구사항 확인 기법
-- ============================
INSERT INTO concept (topic_id, sections_json)
SELECT @topic_req_methods,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.1.2.1',
             'title', '요구 수집(Elicitation) 기법',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','(1) 대표 수집 기법'),
               JSON_OBJECT('type','table',
                 'caption','요구 수집 기법 비교',
                 'headers',JSON_ARRAY('기법','설명','장점','주의점'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('인터뷰','핵심 이해관계자와 1:1 또는 소규모 대면 질의','심층적인 맥락 파악 가능','인터뷰어 역량에 따라 편향 가능'),
                   JSON_ARRAY('설문','다수 사용자에게 동일 질문 배포','정량 데이터 수집 용이','응답률과 질문 설계 품질에 영향'),
                   JSON_ARRAY('워크숍/브레인스토밍','여러 이해관계자가 함께 아이디어 도출','아이디어 확장, 합의 도출에 유리','강한 의견에 이끌릴 수 있음'),
                   JSON_ARRAY('관찰','사용자의 실제 업무/행동을 관찰','숨은 요구 파악 가능','시간과 비용이 많이 들 수 있음')
                 )
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.1.2.2',
             'title', '요구 분석/명세 기법',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','(1) 명세 기법 유형'),
               JSON_OBJECT('type','table',
                 'caption','요구 명세 기법 정리',
                 'headers',JSON_ARRAY('분류','기법','특징'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('비형식','자연어 명세','익숙하지만 모호성/중복 위험이 큼'),
                   JSON_ARRAY('반형식','표/다이어그램 기반','가독성과 구조성이 좋아 협업에 유리'),
                   JSON_ARRAY('정형','수학적 기호/형식 언어','검증 가능하지만 난이도가 높음')
                 )
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_req_methods);

-- ============================
-- 6-3) 1.1.3 분석 모델/요구 관리
-- ============================
INSERT INTO concept (topic_id, sections_json)
SELECT @topic_modeling,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.1.3.1',
             'title', '모델링 관점',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text',
                 '복잡한 문제를 구조화하기 위해 기능/정적 구조/동작 관점으로 나누어 모델링합니다.'
               ),
               JSON_OBJECT('type','table',
                 'caption','대표 모델링 관점과 예시',
                 'headers',JSON_ARRAY('관점','핵심 질문','대표 기법'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('기능','무엇을 하는가?','DFD, 유스케이스 다이어그램'),
                   JSON_ARRAY('정적 구조','어떤 요소로 구성되는가?','ERD, 클래스 다이어그램'),
                   JSON_ARRAY('동작','어떻게 동작하는가?','상태/시퀀스/활동 다이어그램')
                 )
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.1.3.2',
             'title', '요구사항 관리 도구',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','table',
                 'caption','요구 관리 도구의 주요 기능',
                 'headers',JSON_ARRAY('기능','설명'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('추적성(Traceability)','요구–설계–테스트 간 링크를 통해 변경 영향 분석'),
                   JSON_ARRAY('버전관리','요구 변경 이력/승인 내역 관리'),
                   JSON_ARRAY('우선순위 관리','가치·비용 기반 요구 우선순위 설정')
                 )
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_modeling);

-- ============================
-- 6-4) 1.2.1 UI 요구사항/화면흐름
-- ============================
INSERT INTO concept (topic_id, sections_json)
SELECT @topic_ui_req,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.2.1.1',
             'title', 'UI 요구사항 식별',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text',
                 'UI 요구사항은 단순히 화면 모양뿐 아니라 사용성, 접근성, 응답시간, 오류 처리 방식 등을 포함합니다.'
               ),
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '핵심 사용자 시나리오(가입, 로그인, 결제 등)를 중심으로 화면흐름 정의',
                 '반응형 레이아웃, 폰트/컬러 가이드 등 스타일 가이드 정리',
                 '에러/경고/알림 메시지 정책 정의'
               ))
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.2.1.2',
             'title', '화면흐름(스토리보드)',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text',
                 '와이어프레임/스토리보드를 활용해 화면 간 이동경로, 입력 데이터 흐름을 시각적으로 표현하면 개발/디자인/기획 간 의사소통에 도움이 됩니다.'
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_ui_req);

-- ============================
-- 6-5) 1.3.1 공통 모듈 설계
-- ============================
INSERT INTO concept (topic_id, sections_json)
SELECT @topic_common_module,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.3.1.1',
             'title', '공통 모듈의 개념',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text',
                 '공통 모듈은 여러 서브시스템에서 반복 사용되는 기능을 재사용 가능하게 묶어놓은 컴포넌트입니다. 예: 인증, 로그, 공통 검증, 메시지 포맷 변환 등.'
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.3.1.2',
             'title', '설계 시 고려사항',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '의존성 방향: 도메인 서비스가 공통 모듈에만 의존하도록 일관성 유지',
                 '확장성: 신규 요구가 생겼을 때 기존 API를 가능한 한 깨지 않고 확장할 수 있도록 설계',
                 '성능/장애 영향: 공통 모듈 장애 시 전체 서비스에 미치는 영향 고려'
               ))
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_common_module);

-- ============================
-- 6-6) 1.3.2 객체 지향 설계
-- ============================
INSERT INTO concept (topic_id, sections_json)
SELECT @topic_oop_design,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.3.2.1',
             'title', 'SOLID 원칙 개요',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','table',
                 'caption','SOLID 설계 원칙 요약',
                 'headers',JSON_ARRAY('원칙','영문','핵심 의미'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('단일 책임 원칙','SRP','클래스는 한 가지 이유로만 변경되어야 한다'),
                   JSON_ARRAY('개방-폐쇄 원칙','OCP','확장에는 열려 있고 변경에는 닫혀 있어야 한다'),
                   JSON_ARRAY('리스코프 치환 원칙','LSP','하위 타입은 언제나 상위 타입으로 대체 가능해야 한다'),
                   JSON_ARRAY('인터페이스 분리 원칙','ISP','클라이언트에 특화된 여러 인터페이스 사용'),
                   JSON_ARRAY('의존 역전 원칙','DIP','고수준/저수준 모두 추상화에 의존')
                 )
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_oop_design);

-- ============================
-- 6-7) 1.4.1~1.4.3 인터페이스 설계
-- ============================
INSERT INTO concept (topic_id, sections_json)
SELECT @topic_if_req,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.4.1.1',
             'title', '인터페이스 요구사항 식별',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text',
                 '외부/내부 시스템 간 주고받을 데이터, 응답시간, 오류 처리, 보안(인증/인가/암호화) 요구사항을 명확히 기록해야 합니다.'
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_if_req);

INSERT INTO concept (topic_id, sections_json)
SELECT @topic_if_target,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.4.2.1',
             'title', '인터페이스 대상 식별',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','table',
                 'caption','인터페이스 대상 식별 관점',
                 'headers',JSON_ARRAY('관점','질문 예시'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('업무 프로세스','어떤 업무 단계에서 어느 시스템과 연계되는가?'),
                   JSON_ARRAY('데이터 흐름','어떤 데이터가 어디에서 생성·소비되는가?'),
                   JSON_ARRAY('장애 영향도','어떤 시스템 장애가 전체 서비스에 큰 영향을 주는가?')
                 )
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_if_target);

INSERT INTO concept (topic_id, sections_json)
SELECT @topic_if_detail,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.4.3.1',
             'title', '인터페이스 상세 설계 요소',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','table',
                 'caption','상세 설계 체크리스트',
                 'headers',JSON_ARRAY('항목','내용'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('프로토콜/포맷','HTTP/HTTPS, gRPC, 메시지 큐, JSON, XML 등'),
                   JSON_ARRAY('요청/응답 구조','필수/옵션 필드, 에러 코드 체계'),
                   JSON_ARRAY('타임아웃/재시도','호출 제한 시간, 재시도 횟수/간격, 백오프 전략'),
                   JSON_ARRAY('서킷 브레이커','장애 전파를 막기 위한 차단 조건/반개방 정책'),
                   JSON_ARRAY('보안','TLS, 토큰 기반 인증(JWT 등), IP 제한, 감사 로그')
                 )
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id = @topic_if_detail);

SET FOREIGN_KEY_CHECKS = 1;
