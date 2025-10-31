-- V3: 필기(OX/MCQ) 샘플 문항 + 보기 + 태그
SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

SET @T_1_1_1 = (SELECT id FROM topic WHERE code='1.1.1');

-- OX 6문 (미니체크용 5문+여유)
INSERT INTO question (topic_id, type, difficulty, text, explanation, ox_answer)
VALUES
(@T_1_1_1, 'OX', 'EASY',   '플랫폼은 애플리케이션 실행 환경을 의미한다.', '플랫폼은 실행 환경을 제공한다.', 1),
(@T_1_1_1, 'OX', 'EASY',   '응답시간은 요청~첫 응답까지의 시간을 의미한다.', '지표 정의 확인.', 1),
(@T_1_1_1, 'OX', 'NORMAL', '가용성은 장애 발생 빈도만 의미한다.', '가용성은 서비스 가능한 시간 비율로 포괄한다.', 0),
(@T_1_1_1, 'OX', 'NORMAL', '운영체제 분석에서 스케줄링은 중요하지 않다.', '스케줄링 정책은 핵심이다.', 0),
(@T_1_1_1, 'OX', 'HARD',   'DBMS 분석 시 인덱스 전략은 고려 대상이다.', '인덱싱 전략은 성능 핵심.', 1),
(@T_1_1_1, 'OX', 'EASY',   '네트워크 분석 지표에는 지연과 손실률이 포함될 수 있다.', 'QoS 지표 중 하나.', 1);

-- MCQ 8문 (문제풀이용 4문+여유)
-- Q1
INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES (@T_1_1_1, 'MCQ', 'EASY',
'다음 중 "응답시간"의 정의로 가장 적절한 것은?',
'응답시간은 요청~첫 응답까지의 시간이다.');
SET @Q1 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, text, is_correct) VALUES
(@Q1,'A','작업 시작~종료까지의 총 소요시간',0),
(@Q1,'B','요청~첫 응답까지의 시간',1),
(@Q1,'C','자원 사용 비율',0),
(@Q1,'D','서비스 가능한 시간 비율',0);

-- Q2
INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES (@T_1_1_1, 'MCQ', 'EASY',
'플랫폼 성능 분석의 주된 목적은?',
'속도/자원/가용성 등 요구 충족 여부를 평가한다.');
SET @Q2 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, text, is_correct) VALUES
(@Q2,'A','보안 정책 문서화',0),
(@Q2,'B','요구 적합성 평가',1),
(@Q2,'C','가격 책정 고도화',0),
(@Q2,'D','스키마 정규화',0);

-- Q3
INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES (@T_1_1_1, 'MCQ', 'NORMAL',
'가용성(Availability)에 대한 설명으로 옳은 것은?',
'가용성=서비스 가능한 시간 비율.');
SET @Q3 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, text, is_correct) VALUES
(@Q3,'A','처리량(TPS)과 동일한 지표다',0),
(@Q3,'B','서비스 가능한 시간 비율이다',1),
(@Q3,'C','요청~응답 소요시간을 의미한다',0),
(@Q3,'D','자원 사용률과 같다',0);

-- Q4
INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES (@T_1_1_1, 'MCQ', 'NORMAL',
'운영체제 분석에서 고려할 항목으로 거리가 가장 먼 것은?',
'스케줄링, 메모리, I/O는 핵심. 색상 테마는 무관.');
SET @Q4 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, text, is_correct) VALUES
(@Q4,'A','스케줄링 정책',0),
(@Q4,'B','메모리 관리',0),
(@Q4,'C','I/O 관리',0),
(@Q4,'D','UI 색상 테마',1);

-- Q5
INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES (@T_1_1_1, 'MCQ', 'HARD',
'DBMS 인덱스 전략과 가장 관련 깊은 것은?',
'인덱스는 탐색/정렬/조인 비용과 직결.');
SET @Q5 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, text, is_correct) VALUES
(@Q5,'A','패킷 재전송율',0),
(@Q5,'B','조인/검색 비용',1),
(@Q5,'C','UI 접근성',0),
(@Q5,'D','로그 색상',0);

-- Q6
INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES (@T_1_1_1, 'MCQ', 'EASY',
'네트워크 품질 지표가 아닌 것은?',
'대역폭/지연/손실률은 품질 지표. 글꼴 크기는 아님.');
SET @Q6 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, text, is_correct) VALUES
(@Q6,'A','대역폭',0),
(@Q6,'B','지연',0),
(@Q6,'C','손실률',0),
(@Q6,'D','글꼴 크기',1);

-- Q7
INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES (@T_1_1_1, 'MCQ', 'NORMAL',
'응답시간을 단축하기 위한 방법으로 거리가 가장 먼 것은?',
'캐시/쿼리튜닝/리소스 증설은 관련. 스플래시 아트는 무관.');
SET @Q7 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, text, is_correct) VALUES
(@Q7,'A','쿼리 튜닝',0),
(@Q7,'B','캐시 활용',0),
(@Q7,'C','리소스 증설',0),
(@Q7,'D','스플래시 아트 교체',1);

-- Q8
INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES (@T_1_1_1, 'MCQ', 'NORMAL',
'사용률(Utilization)에 대한 설명으로 옳은 것은?',
'자원 사용 비율.');
SET @Q8 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id, label, text, is_correct) VALUES
(@Q8,'A','요청~첫 응답까지의 시간',0),
(@Q8,'B','자원 사용 비율',1),
(@Q8,'C','장애 발생 빈도',0),
(@Q8,'D','처리량과 동일',0);

-- 태그(약점 분석용)
INSERT INTO question_tag (question_id, tag)
SELECT id, '플랫폼' FROM question WHERE topic_id=@T_1_1_1;
INSERT INTO question_tag (question_id, tag)
SELECT id, '성능지표' FROM question WHERE topic_id=@T_1_1_1 AND type='MCQ';

SET FOREIGN_KEY_CHECKS = 1;
