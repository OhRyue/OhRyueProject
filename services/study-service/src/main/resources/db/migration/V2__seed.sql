SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_study;

SET @cert_id := 1;

/* =========================================================
 * 토픽 기본 데이터
 * ========================================================= */
INSERT INTO topic (cert_id, code, title, exam_mode, order_no, emoji)
SELECT @cert_id, 'INF-W-ANALYSIS', '현행 시스템 분석', 'WRITTEN', 10, '🔍'
WHERE NOT EXISTS (SELECT 1 FROM topic WHERE cert_id=@cert_id AND code='INF-W-ANALYSIS');
SET @topic_analysis := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='INF-W-ANALYSIS' LIMIT 1);

INSERT INTO topic (cert_id, code, title, exam_mode, order_no, emoji)
SELECT @cert_id, 'INF-W-OOP', '객체지향 설계', 'WRITTEN', 20, '🧱'
WHERE NOT EXISTS (SELECT 1 FROM topic WHERE cert_id=@cert_id AND code='INF-W-OOP');
SET @topic_oop := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='INF-W-OOP' LIMIT 1);

INSERT INTO topic (cert_id, code, title, exam_mode, order_no, emoji)
SELECT @cert_id, 'INF-W-INTERFACE', '인터페이스 설계', 'WRITTEN', 30, '🔗'
WHERE NOT EXISTS (SELECT 1 FROM topic WHERE cert_id=@cert_id AND code='INF-W-INTERFACE');
SET @topic_interface := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='INF-W-INTERFACE' LIMIT 1);

INSERT INTO topic (cert_id, code, title, exam_mode, order_no, emoji)
SELECT @cert_id, 'INF-P-DESIGN', '실기 설계 패턴', 'PRACTICAL', 40, '🛠️'
WHERE NOT EXISTS (SELECT 1 FROM topic WHERE cert_id=@cert_id AND code='INF-P-DESIGN');
SET @topic_practical := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='INF-P-DESIGN' LIMIT 1);

/* =========================================================
 * 개념(리치 블록) 샘플
 * ========================================================= */
INSERT INTO concept (topic_id, blocks_json)
SELECT @topic_analysis,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.1.1.1',
             'title', '플랫폼 기능 분석',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','(1) 플랫폼의 개념'),
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '플랫폼은 애플리케이션을 구동시키는데 필요한 소프트웨어 환경이다.',
                 '동일 플랫폼 내에서는 상호 호환이 가능하도록 만들어진 결합체를 의미한다.',
                 '공급자와 수요자 등 복수 그룹이 참여하여 각 그룹이 얻고자 하는 가치를 공정한 거래를 통해 교환할 수 있도록 구축된 환경이다.'
               )),
               JSON_OBJECT('type','heading','text','(2) 플랫폼 성능 특성 분석'),
               JSON_OBJECT('type','paragraph','text','플랫폼 성능 분석을 통해 사용자의 서비스 이용 시 속도의 적정성을 알 수 있다.'),
               JSON_OBJECT('type','heading','text','(3) 플랫폼 성능 특성 측정 항목'),
               JSON_OBJECT('type','paragraph','text','측정 항목에는 경과시간, 사용률, 응답시간, 가용성이 있다.'),
               JSON_OBJECT(
                 'type','table',
                 'caption','플랫폼 성능 특성 측정 항목',
                 'headers',JSON_ARRAY('항목','설명'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('경과시간','작업 시작~종료까지의 총 소요시간'),
                   JSON_ARRAY('사용률','자원 사용 비율'),
                   JSON_ARRAY('응답시간','요청~첫 응답까지의 시간'),
                   JSON_ARRAY('가용성','서비스 가능한 시간 비율')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/concepts/platform_metrics.png',
                 'alt','플랫폼 성능 지표 개념도',
                 'caption','성능 지표 개념도'
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.1.1.2',
             'title', '운영체제 분석',
             'importance', 1,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text','...운영체제 분석 내용...')
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id=@topic_analysis);

INSERT INTO concept (topic_id, blocks_json)
SELECT @topic_oop,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.1.2.1',
             'title', '요구분석 기법',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','(1) 요구수집 기법'),
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '인터뷰(Stakeholder 중심 질적정보 수집)',
                 '설문(다수의 정량적 의견 수렴)',
                 '워크숍/브레인스토밍(아이디어 확장)'
               )),
               JSON_OBJECT('type','heading','text','(2) 분석/명세 기법'),
               JSON_OBJECT(
                 'type','table',
                 'caption','요구 명세 기법 비교',
                 'headers',JSON_ARRAY('기법','설명','장점','주의점'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('자연어 명세','문장 기반 명세','친숙/빠름','모호성 위험'),
                   JSON_ARRAY('정형 명세','수학적/형식언어 기반','명확/검증 용이','난이도 높음'),
                   JSON_ARRAY('유스케이스','행위 중심 시나리오','이해 용이','비기능 요구 반영 한계')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/req/elicitation.png',
                 'alt','요구수집 방법 개요',
                 'caption','요구수집 방법 개요'
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/req/spec_comparison.png',
                 'alt','명세 기법 비교',
                 'caption','명세 기법 비교 다이어그램'
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.1.2.2',
             'title', 'UML',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text','UML은 표준 시각적 모델링 언어로 구조/행위를 표현한다.'),
               JSON_OBJECT(
                 'type','table',
                 'caption','주요 UML 다이어그램',
                 'headers',JSON_ARRAY('분류','다이어그램','핵심 목적'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('구조','클래스','정적 구조/관계'),
                   JSON_ARRAY('구조','컴포넌트','배치 단위/인터페이스'),
                   JSON_ARRAY('행위','유스케이스','액터-시스템 상호작용'),
                   JSON_ARRAY('행위','시퀀스','메시지 시간 순서'),
                   JSON_ARRAY('행위','활동','절차 흐름')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/uml/uc_sample.png',
                 'alt','유스케이스 예시',
                 'caption','유스케이스 다이어그램 예시'
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/uml/seq_sample.png',
                 'alt','시퀀스 예시',
                 'caption','시퀀스 다이어그램 예시'
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.1.2.3',
             'title', '애자일(Agile)',
             'importance', 3,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','heading','text','(1) 핵심 가치'),
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '개인/상호작용 > 프로세스/도구',
                 '작동하는 소프트웨어 > 포괄적 문서',
                 '고객 협력 > 계약 협상',
                 '변경 대응 > 계획 준수'
               )),
               JSON_OBJECT('type','heading','text','(2) 스크럼 핵심'),
               JSON_OBJECT(
                 'type','table',
                 'caption','스크럼 이벤트/산출물',
                 'headers',JSON_ARRAY('구분','내용','주기'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('이벤트','스프린트 기획/데일리/리뷰/회고','1~4주'),
                   JSON_ARRAY('산출물','백로그/증분','지속 갱신')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/agile/scrum_cycle.png',
                 'alt','스크럼 사이클',
                 'caption','스크럼 사이클'
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id=@topic_oop);

INSERT INTO concept (topic_id, blocks_json)
SELECT @topic_interface,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '1.1.3.1',
             'title', '모델링 기법',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text','문제 영역을 추상화해 의사소통/분석을 돕는 기법.'),
               JSON_OBJECT(
                 'type','table',
                 'caption','모델링 관점',
                 'headers',JSON_ARRAY('관점','설명','예시'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('기능','무엇을 하는가','DFD, 유스케이스'),
                   JSON_ARRAY('정적 구조','구성/관계','ERD, 클래스'),
                   JSON_ARRAY('동작','어떻게 동작하는가','상태/시퀀스/활동')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/modeling/erd.png',
                 'alt','ERD 샘플',
                 'caption','ERD 예시'
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 2,
             'subCode', '1.1.3.2',
             'title', '분석 자동화 도구',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','list','items',JSON_ARRAY(
                 '정적 분석(코드 규칙 위반 탐지)',
                 '성능 분석(프로파일링/부하테스트 도구)',
                 '모델 검증(일관성/제약 확인)'
               )),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/tools/static_analysis.png',
                 'alt','정적 분석 개념도',
                 'caption','정적 분석 개념도'
               )
             )
           ),
           JSON_OBJECT(
             'orderNo', 3,
             'subCode', '1.1.3.3',
             'title', '요구사항 관리 도구',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT(
                 'type','table',
                 'caption','요구 관리 주요 기능',
                 'headers',JSON_ARRAY('기능','설명'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('추적성','요구~설계~테스트 간 링크'),
                   JSON_ARRAY('버전관리','요구 변경 이력 관리'),
                   JSON_ARRAY('우선순위','가치/노력 기반 정렬')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/req/traceability.png',
                 'alt','추적성',
                 'caption','요구 추적성 개요'
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id=@topic_interface);

INSERT INTO concept (topic_id, blocks_json)
SELECT @topic_practical,
       JSON_OBJECT(
         'sections', JSON_ARRAY(
           JSON_OBJECT(
             'orderNo', 1,
             'subCode', '2.2.1.1',
             'title', '정규화 개요',
             'importance', 2,
             'blocks', JSON_ARRAY(
               JSON_OBJECT('type','paragraph','text','정규화는 이상현상을 줄이고 데이터 무결성을 높이기 위한 설계 절차.'),
               JSON_OBJECT(
                 'type','table',
                 'caption','정규형 요약',
                 'headers',JSON_ARRAY('정규형','핵심 조건','설명'),
                 'rows',JSON_ARRAY(
                   JSON_ARRAY('1NF','원자성','반복/다중값 제거'),
                   JSON_ARRAY('2NF','부분함수 종속 제거','기본키 전체 종속'),
                   JSON_ARRAY('3NF','이행함수 종속 제거','키가 아닌 속성 간 종속 제거')
                 )
               ),
               JSON_OBJECT(
                 'type','image',
                 'url','https://cdn.example.com/db/normalization.png',
                 'alt','정규화 개념도',
                 'caption','정규화 개념도'
               )
             )
           )
         )
       )
WHERE NOT EXISTS (SELECT 1 FROM concept WHERE topic_id=@topic_practical);

/* =========================================================
 * 필기 OX 문제
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_analysis, 'WRITTEN', 'OX', 'NORMAL',
       '현행 시스템 분석 단계에서는 응답 시간과 처리량과 같은 성능 지표를 수집해야 한다. (O/X)',
       'O',
       '성능 지표는 향후 요구사항 정의와 설계 제약을 결정하므로 필수로 수집합니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석 단계에서는 응답 시간%'
);
SET @q_ox_analysis := (
  SELECT id FROM question
   WHERE topic_id=@topic_analysis AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '현행 시스템 분석 단계에서는 응답 시간%' LIMIT 1
);

INSERT INTO question_tag (question_id, tag)
SELECT @q_ox_analysis, '현행시스템'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_ox_analysis AND tag='현행시스템');

/* =========================================================
 * 필기 MCQ 문제
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_oop, 'WRITTEN', 'MCQ', 'NORMAL',
       '다음 중 개방-폐쇄 원칙(OCP)에 대한 설명으로 옳은 것은 무엇인가?',
       'B',
       'OCP는 기능을 확장할 수 있도록 열어두되 기존 코드를 수정하지 않도록 닫아두라는 원칙입니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_oop AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 개방-폐쇄 원칙%' );
SET @q_mcq_ocp := (
  SELECT id FROM question
   WHERE topic_id=@topic_oop AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '다음 중 개방-폐쇄 원칙%' LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_ocp,'A','요구사항이 바뀔 때마다 클래스를 수정한다',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_ocp AND label='A');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_ocp,'B','확장은 허용하되 기존 코드는 변경하지 않는다',1
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_ocp AND label='B');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_ocp,'C','추상화보다 상속을 우선 사용한다',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_ocp AND label='C');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_ocp,'D','단일 책임 원칙을 무시해도 된다',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_ocp AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_ocp, 'OOP'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_ocp AND tag='OOP');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_interface, 'WRITTEN', 'MCQ', 'HARD',
       '외부 시스템과의 인터페이스 상세 설계 시 가장 우선 고려할 요소는 무엇인가?',
       'A',
       '트래픽 규모 대비 스루풋/지연시간 목표와 타임아웃·재시도·서킷브레이커 등 안정성 패턴을 우선 정의해야 합니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_interface AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '외부 시스템과의 인터페이스 상세 설계 시%' );
SET @q_mcq_interface := (
  SELECT id FROM question
   WHERE topic_id=@topic_interface AND mode='WRITTEN' AND type='MCQ'
     AND stem LIKE '외부 시스템과의 인터페이스 상세 설계 시%' LIMIT 1
);

INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_interface,'A','요청/응답 포맷과 타임아웃·재시도 정책을 정의한다',1
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_interface AND label='A');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_interface,'B','UI 테마 색상을 맞춘다',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_interface AND label='B');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_interface,'C','사무실 조명을 조정한다',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_interface AND label='C');
INSERT INTO question_choice (question_id,label,content,is_correct)
SELECT @q_mcq_interface,'D','개발자 별명을 정한다',0
WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_mcq_interface AND label='D');

INSERT INTO question_tag (question_id, tag)
SELECT @q_mcq_interface, '인터페이스설계'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_mcq_interface AND tag='인터페이스설계');

/* =========================================================
 * 실기 주관식 문제
 * ========================================================= */
INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_practical, 'PRACTICAL', 'SHORT', 'NORMAL',
       '서킷 브레이커 패턴의 목적을 한 문장으로 설명하세요.',
       '연쇄 실패 방지, 임계치 도달 시 호출 단락 후 복구 확인',
       '서킷 브레이커는 실패율이 임계치를 넘으면 호출을 차단하고 회복 신호가 올 때까지 대체 경로를 사용해 연쇄 실패를 방지합니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='SHORT'
     AND stem LIKE '서킷 브레이커 패턴의 목적%' );
SET @q_short_cb := (
  SELECT id FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='SHORT'
     AND stem LIKE '서킷 브레이커 패턴의 목적%' LIMIT 1
);

INSERT INTO question_tag (question_id, tag)
SELECT @q_short_cb, '서킷브레이커'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_short_cb AND tag='서킷브레이커');

INSERT INTO question (cert_id, topic_id, mode, type, difficulty, stem, answer_key, solution_text)
SELECT @cert_id, @topic_practical, 'PRACTICAL', 'LONG', 'HARD',
       '초당 2,000 RPS를 처리해야 하는 외부 API 연동 모듈을 설계하세요. 요구사항: 평균 지연 150ms 이하, 실패 시 재시도 정책, 백프레셔, 모니터링/알림 포함.',
       '비동기 IO, 타임아웃, 재시도, 서킷브레이커, 큐 기반 백프레셔, 메트릭/알림',
       '비동기 I/O와 연결 풀을 사용하고, 타임아웃·재시도(지수 백오프)·서킷브레이커·큐 기반 백프레셔·메트릭/알림 구성을 포함하도록 설계합니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='LONG'
     AND stem LIKE '초당 2,000 RPS를 처리해야 하는 외부 API 연동 모듈%' );
SET @q_long_api := (
  SELECT id FROM question
   WHERE topic_id=@topic_practical AND mode='PRACTICAL' AND type='LONG'
     AND stem LIKE '초당 2,000 RPS를 처리해야 하는 외부 API 연동 모듈%' LIMIT 1
);

INSERT INTO question_tag (question_id, tag)
SELECT @q_long_api, '고가용성'
WHERE NOT EXISTS (SELECT 1 FROM question_tag WHERE question_id=@q_long_api AND tag='고가용성');

SET FOREIGN_KEY_CHECKS = 1;
