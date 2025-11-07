SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ---------------------------------
-- 0) 공통: 참조 대상(자격증/토픽) 변수 확보
-- ---------------------------------
-- 자격증: 정보처리기사
SET @cert_id := (SELECT id FROM cert WHERE name = '정보처리기사' LIMIT 1);

-- 토픽들 (예시: 질문에서 준 구조)
SET @t_1              := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1' LIMIT 1);         -- 소프트웨어 설계
SET @t_1_1            := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.1' LIMIT 1);       -- 요구사항 확인
SET @t_1_1_1          := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.1.1' LIMIT 1);     -- 현행 시스템 분석
SET @t_1_1_2          := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.1.2' LIMIT 1);     -- 요구사항 확인
SET @t_1_1_3          := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.1.3' LIMIT 1);     -- 분석 모델 확인
SET @t_1_2            := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.2' LIMIT 1);       -- 화면설계
SET @t_1_2_1          := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.2.1' LIMIT 1);     -- UI 요구사항 확인
SET @t_1_3            := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.3' LIMIT 1);       -- 애플리케이션 설계
SET @t_1_3_1          := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.3.1' LIMIT 1);     -- 공통 모듈 설계
SET @t_1_3_2          := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.3.2' LIMIT 1);     -- 객체 지향 설계
SET @t_1_4            := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.4' LIMIT 1);       -- 인터페이스 설계
SET @t_1_4_1          := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.4.1' LIMIT 1);     -- 인터페이스 요구사항 확인
SET @t_1_4_2          := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.4.2' LIMIT 1);     -- 인터페이스 대상 식별
SET @t_1_4_3          := (SELECT id FROM topic WHERE cert_id=@cert_id AND code='1.4.3' LIMIT 1);     -- 인터페이스 상세 설계

-- 사용자 (account-service seed 준수)
SET @u_ohryue := 'ohryue';
SET @u_user2  := 'user2';
SET @u_guest  := 'guest';
SET @u_admin  := 'admin';

-- ---------------------------------
-- 1) 문항뱅크 — 필기 OX 4문항 (1.1.1, 1.1.2, 1.1.3, 1.2.1)
-- ---------------------------------
-- Q1 (OX, 현행 시스템 분석)
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url, ox_answer)
SELECT @t_1_1_1, 'OX', 'NORMAL',
       '현행 시스템 분석 단계에서 성능 지표(응답시간/처리량 등) 수집은 핵심 활동이다. (O/X)',
       '현행 성능은 요구사항 정의와 설계 제약을 결정하므로 주요 수집 항목입니다.',
       NULL, 1
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@t_1_1_1 AND type='OX' AND text LIKE '현행 시스템 분석 단계에서 성능 지표%'
);

-- Q2 (OX, 요구사항 확인)
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url, ox_answer)
SELECT @t_1_1_2, 'OX', 'EASY',
       '요구사항 확인에서는 이해관계자 식별과 요구사항 분류(기능/비기능)가 이루어진다. (O/X)',
       '요구사항 확인의 주요 산출: 이해관계자 목록, 요구사항 목록(기능/비기능), 제약사항 등.',
       NULL, 1
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@t_1_1_2 AND type='OX' AND text LIKE '요구사항 확인에서는 이해관계자 식별%'
);

-- Q3 (OX, 분석 모델 확인)
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url, ox_answer)
SELECT @t_1_1_3, 'OX', 'NORMAL',
       '분석 모델 확인에서는 데이터 흐름/상태 전이/객체 모델 등을 통해 요구사항 일관성을 검증한다. (O/X)',
       'DFD/STD/클래스 모델 등으로 누락·충돌·모호성을 줄입니다.',
       NULL, 1
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@t_1_1_3 AND type='OX' AND text LIKE '분석 모델 확인에서는 데이터 흐름%'
);

-- Q4 (OX, UI 요구사항)
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url, ox_answer)
SELECT @t_1_2_1, 'OX', 'EASY',
       'UI 요구사항 확인 단계에서 접근성/반응성/일관성은 고려 대상이 아니다. (O/X)',
       '오답. UI 품질 속성(접근성/반응성/일관성)은 핵심 비기능 요구사항입니다.',
       NULL, 0
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@t_1_2_1 AND type='OX' AND text LIKE 'UI 요구사항 확인 단계에서 접근성%'
);

-- 태그
INSERT INTO question_tag (question_id, tag)
SELECT q.id, t.tag FROM (
  SELECT '요구사항' AS tag UNION ALL
  SELECT '현행시스템' UNION ALL
  SELECT '분석모델'   UNION ALL
  SELECT 'UI' ) t
JOIN question q ON q.text IN (
  '현행 시스템 분석 단계에서 성능 지표(응답시간/처리량 등) 수집은 핵심 활동이다. (O/X)',
  '요구사항 확인에서는 이해관계자 식별과 요구사항 분류(기능/비기능)가 이루어진다. (O/X)',
  '분석 모델 확인에서는 데이터 흐름/상태 전이/객체 모델 등을 통해 요구사항 일관성을 검증한다. (O/X)',
  'UI 요구사항 확인 단계에서 접근성/반응성/일관성은 고려 대상이 아니다. (O/X)'
)
LEFT JOIN question_tag qt ON qt.question_id=q.id AND qt.tag=t.tag
WHERE qt.id IS NULL;

-- ---------------------------------
-- 2) 문항뱅크 — 필기 MCQ 5문항 (1.3.1, 1.3.2, 1.4.1, 1.4.2, 1.4.3)
-- ---------------------------------
-- MCQ1 (공통 모듈 설계)
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url, ox_answer)
SELECT @t_1_3_1, 'MCQ', 'NORMAL',
       '다음 중 공통 모듈 설계의 이점으로 가장 거리가 먼 것은?',
       '공통 모듈은 재사용/일관성/유지보수성 향상이 핵심 이점입니다.',
       NULL, NULL
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@t_1_3_1 AND type='MCQ' AND text='다음 중 공통 모듈 설계의 이점으로 가장 거리가 먼 것은?'
);
SET @q_m1 := (SELECT id FROM question
              WHERE topic_id=@t_1_3_1 AND type='MCQ'
                AND text='다음 중 공통 모듈 설계의 이점으로 가장 거리가 먼 것은?' LIMIT 1);
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m1,'A','중복 코드 감소',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m1 AND label='A');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m1,'B','일관된 인터페이스 제공',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m1 AND label='B');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m1,'C','유지보수 비용 증가',1 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m1 AND label='C');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m1,'D','재사용성 향상',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m1 AND label='D');

-- MCQ2 (객체지향 설계)
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url, ox_answer)
SELECT @t_1_3_2, 'MCQ', 'EASY',
       '객체지향 설계 원칙(SOLID) 중 개방/폐쇄 원칙(OCP)의 설명으로 옳은 것은?',
       'OCP: 확장에는 열려 있고 변경에는 닫혀 있어야 한다.',
       NULL, NULL
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@t_1_3_2 AND type='MCQ' AND text LIKE '객체지향 설계 원칙(SOLID)%OCP%'
);
SET @q_m2 := (SELECT id FROM question
              WHERE topic_id=@t_1_3_2 AND type='MCQ'
                AND text LIKE '객체지향 설계 원칙(SOLID)%OCP%' LIMIT 1);
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m2,'A','상속보다 합성을 선호한다',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m2 AND label='A');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m2,'B','모듈은 확장 가능하되 기존 코드는 수정 최소화',1 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m2 AND label='B');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m2,'C','인터페이스 분리는 최소화해야 한다',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m2 AND label='C');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m2,'D','단일 책임 원칙을 위배해도 된다',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m2 AND label='D');

-- MCQ3 (인터페이스 요구사항 확인)
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url, ox_answer)
SELECT @t_1_4_1, 'MCQ', 'NORMAL',
       '외부 시스템 연계를 위한 인터페이스 요구사항으로 우선 고려할 요소는?',
       '식별/보안/스루풋/오류처리/재시도 정책 등을 명확히 정의해야 합니다.',
       NULL, NULL
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@t_1_4_1 AND type='MCQ' AND text='외부 시스템 연계를 위한 인터페이스 요구사항으로 우선 고려할 요소는?'
);
SET @q_m3 := (SELECT id FROM question
              WHERE topic_id=@t_1_4_1 AND type='MCQ'
                AND text='외부 시스템 연계를 위한 인터페이스 요구사항으로 우선 고려할 요소는?' LIMIT 1);
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m3,'A','요청/응답 포맷과 인증 방식',1 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m3 AND label='A');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m3,'B','UI 테마 색상',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m3 AND label='B');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m3,'C','사무실 인테리어',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m3 AND label='C');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m3,'D','개발자 별명',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m3 AND label='D');

-- MCQ4 (인터페이스 대상 식별)
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url, ox_answer)
SELECT @t_1_4_2, 'MCQ', 'NORMAL',
       '대상 시스템 식별 시 적절하지 않은 기준은?',
       '업무 흐름/데이터 소유권/보안 등으로 식별합니다. 담당자 취향은 기준이 아닙니다.',
       NULL, NULL
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@t_1_4_2 AND type='MCQ' AND text='대상 시스템 식별 시 적절하지 않은 기준은?'
);
SET @q_m4 := (SELECT id FROM question
              WHERE topic_id=@t_1_4_2 AND type='MCQ'
                AND text='대상 시스템 식별 시 적절하지 않은 기준은?' LIMIT 1);
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m4,'A','업무 기능 경계',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m4 AND label='A');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m4,'B','데이터 소유권',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m4 AND label='B');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m4,'C','보안 등급',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m4 AND label='C');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m4,'D','담당자 개인 취향',1 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m4 AND label='D');

-- MCQ5 (인터페이스 상세 설계)
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url, ox_answer)
SELECT @t_1_4_3, 'MCQ', 'HARD',
       '대량 트래픽 인터페이스 상세 설계 시 가장 우선 고려할 사항은?',
       '스루풋/지연시간/백프레셔/재시도/아이들포인트/서킷브레이커 등 안정성 패턴.',
       NULL, NULL
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@t_1_4_3 AND type='MCQ' AND text='대량 트래픽 인터페이스 상세 설계 시 가장 우선 고려할 사항은?'
);
SET @q_m5 := (SELECT id FROM question
              WHERE topic_id=@t_1_4_3 AND type='MCQ'
                AND text='대량 트래픽 인터페이스 상세 설계 시 가장 우선 고려할 사항은?' LIMIT 1);
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m5,'A','스루풋과 지연시간 예산',1 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m5 AND label='A');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m5,'B','회의실 조명',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m5 AND label='B');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m5,'C','가을 소풍 일정',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m5 AND label='C');
INSERT INTO question_choice (question_id,label,text,is_correct)
SELECT @q_m5,'D','폰트 크기 8pt 고정',0 FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM question_choice WHERE question_id=@q_m5 AND label='D');

-- 태그
INSERT INTO question_tag (question_id, tag)
SELECT q.id, t.tag FROM (
  SELECT '공통모듈' AS tag UNION ALL
  SELECT 'OOP'         UNION ALL
  SELECT '인터페이스요구' UNION ALL
  SELECT '대상식별'     UNION ALL
  SELECT '상세설계' ) t
JOIN question q ON q.id IN (@q_m1,@q_m2,@q_m3,@q_m4,@q_m5)
LEFT JOIN question_tag qt ON qt.question_id=q.id AND qt.tag=t.tag
WHERE qt.id IS NULL;

-- ---------------------------------
-- 3) 문항뱅크 — 실기 SHORT 2 + LONG 1 (예: 1.3.2/OOP, 1.4.3/상세설계)
-- ---------------------------------
-- SHORT1
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url, ox_answer)
SELECT @t_1_3_2, 'SHORT', 'NORMAL',
       '다형성(Polymorphism)을 1~2문장으로 설명하세요.',
       '객체가 동일한 인터페이스로 서로 다른 구현을 제공하는 성질. 호출자는 타입 추상화로 확장 용이.',
       NULL, NULL
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@t_1_3_2 AND type='SHORT' AND text LIKE '다형성(Polymorphism)%설명'
);

-- SHORT2
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url, ox_answer)
SELECT @t_1_4_3, 'SHORT', 'HARD',
       '서킷 브레이커(Circuit Breaker) 패턴의 목적을 1문장으로 쓰세요.',
       '연쇄 실패를 방지하기 위해 실패율이 임계치에 도달하면 호출을 단락하고 복구 후 재시도.',
       NULL, NULL
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@t_1_4_3 AND type='SHORT' AND text LIKE '서킷 브레이커%목적'
);

-- LONG1
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url, ox_answer)
SELECT @t_1_4_3, 'LONG', 'HARD',
       '초당 2,000 RPS를 처리해야 하는 외부 API 연동 모듈을 설계하세요.\n'
       '요구사항: 평균 지연 150ms 이하, 실패 시 재시도 정책, 백프레셔, 모니터링/알림 포함.',
       '비동기 I/O, 연결 풀, 타임아웃/재시도(지수백오프), 서킷브레이커, 큐 기반 백프레셔, 지표(레이턴시/스루풋/실패율) 수집 및 알람.',
       'https://example.com/api-design.png', NULL
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@t_1_4_3 AND type='LONG' AND text LIKE '초당 2,000 RPS를 처리해야 하는 외부 API 연동%'
);

-- ---------------------------------
-- 4) 샘플 세션(MICRO) + 아이템 매핑 + 답안 + 요약/AI로그 + 진행
--    사용자: ohryue, 필기(MICRO) — OX 4 + MCQ 5 = 9문항
-- ---------------------------------
-- 세션 생성
INSERT INTO study_session (user_id, mode, exam_mode, topic_scope_json, question_count, status)
SELECT @u_ohryue, 'MICRO', 'WRITTEN',
       JSON_OBJECT('topics', JSON_ARRAY(@t_1_1_1,@t_1_1_2,@t_1_1_3,@t_1_2_1,@t_1_3_1,@t_1_3_2,@t_1_4_1,@t_1_4_2,@t_1_4_3)),
       9, 'OPEN'
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1 FROM study_session
   WHERE user_id=@u_ohryue AND mode='MICRO' AND exam_mode='WRITTEN' AND status='OPEN'
     AND question_count=9
);
SET @ss := (SELECT id FROM study_session
            WHERE user_id=@u_ohryue AND mode='MICRO' AND exam_mode='WRITTEN'
              AND status='OPEN' ORDER BY id DESC LIMIT 1);

-- 세션 아이템(순서대로 OX 4 + MCQ 5)
-- OX IDs
SET @q_ox1 := (SELECT id FROM question WHERE type='OX' AND topic_id=@t_1_1_1 LIMIT 1);
SET @q_ox2 := (SELECT id FROM question WHERE type='OX' AND topic_id=@t_1_1_2 LIMIT 1);
SET @q_ox3 := (SELECT id FROM question WHERE type='OX' AND topic_id=@t_1_1_3 LIMIT 1);
SET @q_ox4 := (SELECT id FROM question WHERE type='OX' AND topic_id=@t_1_2_1 LIMIT 1);
-- MCQ IDs
SET @q_l := (SELECT id FROM question WHERE id=@q_m1 LIMIT 1);
SET @q_m := (SELECT id FROM question WHERE id=@q_m2 LIMIT 1);
SET @q_n := (SELECT id FROM question WHERE id=@q_m3 LIMIT 1);
SET @q_o := (SELECT id FROM question WHERE id=@q_m4 LIMIT 1);
SET @q_p := (SELECT id FROM question WHERE id=@q_m5 LIMIT 1);

-- 아이템 채우기(멱등)
INSERT INTO study_session_item (session_id, order_no, question_id)
SELECT @ss, 1, @q_ox1 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM study_session_item WHERE session_id=@ss AND order_no=1);
INSERT INTO study_session_item (session_id, order_no, question_id)
SELECT @ss, 2, @q_ox2 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM study_session_item WHERE session_id=@ss AND order_no=2);
INSERT INTO study_session_item (session_id, order_no, question_id)
SELECT @ss, 3, @q_ox3 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM study_session_item WHERE session_id=@ss AND order_no=3);
INSERT INTO study_session_item (session_id, order_no, question_id)
SELECT @ss, 4, @q_ox4 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM study_session_item WHERE session_id=@ss AND order_no=4);

INSERT INTO study_session_item (session_id, order_no, question_id)
SELECT @ss, 5, @q_l FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM study_session_item WHERE session_id=@ss AND order_no=5);
INSERT INTO study_session_item (session_id, order_no, question_id)
SELECT @ss, 6, @q_m FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM study_session_item WHERE session_id=@ss AND order_no=6);
INSERT INTO study_session_item (session_id, order_no, question_id)
SELECT @ss, 7, @q_n FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM study_session_item WHERE session_id=@ss AND order_no=7);
INSERT INTO study_session_item (session_id, order_no, question_id)
SELECT @ss, 8, @q_o FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM study_session_item WHERE session_id=@ss AND order_no=8);
INSERT INTO study_session_item (session_id, order_no, question_id)
SELECT @ss, 9, @q_p FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM study_session_item WHERE session_id=@ss AND order_no=9);

-- 답안 로그(예시: OX 3/4 정답, MCQ 3/5 정답 → 총 6/9 = 66.67%)
-- OX 제출
INSERT INTO user_answer (user_id, question_id, correct, score, answer_text)
SELECT @u_ohryue, @q_ox1, 1, NULL, 'O' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_answer WHERE user_id=@u_ohryue AND question_id=@q_ox1);
INSERT INTO user_answer (user_id, question_id, correct, score, answer_text)
SELECT @u_ohryue, @q_ox2, 1, NULL, 'O' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_answer WHERE user_id=@u_ohryue AND question_id=@q_ox2);
INSERT INTO user_answer (user_id, question_id, correct, score, answer_text)
SELECT @u_ohryue, @q_ox3, 1, NULL, 'O' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_answer WHERE user_id=@u_ohryue AND question_id=@q_ox3);
INSERT INTO user_answer (user_id, question_id, correct, score, answer_text)
SELECT @u_ohryue, @q_ox4, 0, NULL, 'O' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_answer WHERE user_id=@u_ohryue AND question_id=@q_ox4);

-- MCQ 제출(B/D/A/A/A 라고 가정, 정답은 문제별로 위에서 지정)
-- q_m1: 정답 C → 오답
INSERT INTO user_answer (user_id, question_id, correct, score, answer_text)
SELECT @u_ohryue, @q_l, 0, NULL, 'B' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_answer WHERE user_id=@u_ohryue AND question_id=@q_l);
-- q_m2: 정답 B → 정답
INSERT INTO user_answer (user_id, question_id, correct, score, answer_text)
SELECT @u_ohryue, @q_m, 1, NULL, 'B' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_answer WHERE user_id=@u_ohryue AND question_id=@q_m);
-- q_m3: 정답 A → 정답
INSERT INTO user_answer (user_id, question_id, correct, score, answer_text)
SELECT @u_ohryue, @q_n, 1, NULL, 'A' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_answer WHERE user_id=@u_ohryue AND question_id=@q_n);
-- q_m4: 정답 D → 정답
INSERT INTO user_answer (user_id, question_id, correct, score, answer_text)
SELECT @u_ohryue, @q_o, 1, NULL, 'D' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_answer WHERE user_id=@u_ohryue AND question_id=@q_o);
-- q_m5: 정답 A → 오답(예: D)
INSERT INTO user_answer (user_id, question_id, correct, score, answer_text)
SELECT @u_ohryue, @q_p, 0, NULL, 'D' FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_answer WHERE user_id=@u_ohryue AND question_id=@q_p);

-- 세션 상태/제출 시간 반영(제출로 마감)
UPDATE study_session
   SET status='SUBMITTED', closed_at=NOW()
 WHERE id=@ss AND status='OPEN';

-- 요약 (총/정답/정확도/시간) — 예시 정확도 66.67
INSERT INTO study_summary (session_id, user_id, ai_summary_text, total, correct, accuracy, time_spent_sec)
SELECT @ss, @u_ohryue,
       '미니(MICRO) 세션 요약: OX 4/4 중 3 정답, MCQ 5/5 중 3 정답. 개념 재확인 및 인터페이스 품질속성 보완 권장.',
       9, 6, 66.67, 540
FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM study_summary WHERE session_id=@ss);

-- 진행(미니/MCQ) — 예시: 1.1.1/1.1.2/1.1.3/1.2.1 네 토픽에 대해 미니 통과/오답 반영
INSERT INTO user_progress (user_id, topic_id, exam_mode, mini_total, mini_correct, mini_passed, mcq_total, mcq_correct)
SELECT @u_ohryue, @t_1_1_1, 'WRITTEN', 4, 4, 1, 0, 0 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_progress WHERE user_id=@u_ohryue AND topic_id=@t_1_1_1 AND exam_mode='WRITTEN');
INSERT INTO user_progress (user_id, topic_id, exam_mode, mini_total, mini_correct, mini_passed, mcq_total, mcq_correct)
SELECT @u_ohryue, @t_1_1_2, 'WRITTEN', 4, 4, 1, 0, 0 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_progress WHERE user_id=@u_ohryue AND topic_id=@t_1_1_2 AND exam_mode='WRITTEN');
INSERT INTO user_progress (user_id, topic_id, exam_mode, mini_total, mini_correct, mini_passed, mcq_total, mcq_correct)
SELECT @u_ohryue, @t_1_1_3, 'WRITTEN', 4, 4, 1, 0, 0 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_progress WHERE user_id=@u_ohryue AND topic_id=@t_1_1_3 AND exam_mode='WRITTEN');
INSERT INTO user_progress (user_id, topic_id, exam_mode, mini_total, mini_correct, mini_passed, mcq_total, mcq_correct)
SELECT @u_ohryue, @t_1_2_1, 'WRITTEN', 4, 3, 0, 0, 0 FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM user_progress WHERE user_id=@u_ohryue AND topic_id=@t_1_2_1 AND exam_mode='WRITTEN');

-- 실기 채점/해설 AI 로그(샘플) — SHORT2/LONG1
SET @q_s2 := (SELECT id FROM question WHERE type='SHORT' AND topic_id=@t_1_4_3 LIMIT 1);
SET @q_l1 := (SELECT id FROM question WHERE type='LONG'  AND topic_id=@t_1_4_3 LIMIT 1);

INSERT INTO ai_grade_log (user_id, question_id, model, prompt_ref, score, latency_ms)
SELECT @u_ohryue, @q_s2, 'gpt-5-scoring', 'short:circuit_breaker', 85, 420
FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM ai_grade_log WHERE user_id=@u_ohryue AND question_id=@q_s2);

INSERT INTO ai_explain_log (session_id, question_id, model, explanation_text)
SELECT @ss, @q_s2, 'gpt-5-explain',
       '핵심 포인트는 “연쇄 실패 방지/임계치/단락/복구 후 재시도”. 사례/조건을 덧붙이면 가점.'
FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM ai_explain_log WHERE session_id=@ss AND question_id=@q_s2);

INSERT INTO ai_grade_log (user_id, question_id, model, prompt_ref, score, latency_ms)
SELECT @u_ohryue, @q_l1, 'gpt-5-scoring', 'long:rps2000-integration', 78, 680
FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM ai_grade_log WHERE user_id=@u_ohryue AND question_id=@q_l1);

INSERT INTO ai_explain_log (session_id, question_id, model, explanation_text)
SELECT @ss, @q_l1, 'gpt-5-explain',
       '재시도/백오프/서킷브레이커/메트릭/알림은 좋았으나 백프레셔 처리와 타임아웃/연결풀 수치 예산 제시가 약함.'
FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM ai_explain_log WHERE session_id=@ss AND question_id=@q_l1);

INSERT INTO ai_summary_log (session_id, model, summary_text)
SELECT @ss, 'gpt-5-summarize',
       'OX는 개념 숙련 양호, MCQ는 인터페이스 품질속성 강화 필요. 실기는 설계 패턴 수치화/예산 설정 보완 권장.'
FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM ai_summary_log WHERE session_id=@ss);

SET FOREIGN_KEY_CHECKS = 1;
