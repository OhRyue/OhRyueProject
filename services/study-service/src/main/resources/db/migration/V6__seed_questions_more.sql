-- V6: 1.1.2 / 1.1.3 / 2.2.1 문항 시드(필기 OX/MCQ) + 일부 실기 샘플
SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

SET @T_1_1_2 = (SELECT id FROM topic WHERE code='1.1.2');
SET @T_1_1_3 = (SELECT id FROM topic WHERE code='1.1.3');
SET @T_2_2_1 = (SELECT id FROM topic WHERE code='2.2.1');

-- ================= 1.1.2 (요구사항 확인) =================

-- OX
INSERT INTO question (topic_id, type, difficulty, text, explanation, ox_answer)
VALUES
(@T_1_1_2, 'OX','EASY','인터뷰는 이해관계자에게서 질적 정보를 수집하는 데 유용하다.','요구수집 기법 중 하나.',1),
(@T_1_1_2, 'OX','NORMAL','정형 명세는 모호성이 높고 검증이 어렵다.','반대다: 모호성 낮고 검증 용이.',0);

-- MCQ (3)
INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES
(@T_1_1_2,'MCQ','EASY','다음 중 요구 명세 기법의 장점-주의 조합으로 올바른 것은?','자연어: 친숙/모호성 위험. 정형: 명확/난이도 높음.');
SET @Q12_1 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id,label,text,is_correct) VALUES
(@Q12_1,'A','자연어: 친숙/모호성 위험',1),
(@Q12_1,'B','정형: 친숙/모호성 위험',0),
(@Q12_1,'C','자연어: 난이도 높음/명확',0),
(@Q12_1,'D','정형: 명확/난이도 낮음',0);

INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES
(@T_1_1_2,'MCQ','NORMAL','UML 유스케이스 다이어그램의 주 목적은?','액터와 시스템 간 상호작용 표현.');
SET @Q12_2 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id,label,text,is_correct) VALUES
(@Q12_2,'A','데이터 정규화',0),
(@Q12_2,'B','액터-시스템 상호작용 표현',1),
(@Q12_2,'C','배포 토폴로지 표현',0),
(@Q12_2,'D','상태 전이 규칙 표현',0);

INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES
(@T_1_1_2,'MCQ','NORMAL','스크럼에 대한 설명으로 옳지 않은 것은?','스프린트는 1~4주 고정 타임박스.');
SET @Q12_3 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id,label,text,is_correct) VALUES
(@Q12_3,'A','스프린트는 1~4주 고정 타임박스',1),
(@Q12_3,'B','백로그는 정적 문서로 고정된다',0),
(@Q12_3,'C','데일리는 팀 커뮤니케이션 촉진 목적',0),
(@Q12_3,'D','회고는 개선점 도출 목적',0);

-- 태그
INSERT INTO question_tag (question_id, tag) SELECT id,'요구분석' FROM question WHERE topic_id=@T_1_1_2;
INSERT INTO question_tag (question_id, tag) SELECT id,'UML'   FROM question WHERE topic_id=@T_1_1_2 AND type='MCQ';
INSERT INTO question_tag (question_id, tag) SELECT id,'Agile' FROM question WHERE topic_id=@T_1_1_2;

-- ================= 1.1.3 (분석 모델 확인) =================

-- OX
INSERT INTO question (topic_id, type, difficulty, text, explanation, ox_answer)
VALUES
(@T_1_1_3,'OX','EASY','ERD는 정적 구조를 표현하는 모델이다.','개체/관계 구조.',1),
(@T_1_1_3,'OX','NORMAL','정적 분석 도구는 런타임 성능 계측에 특화된다.','정적분석은 코드 규칙/취약점, 성능 계측은 동적.',0);

-- MCQ (3)
INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES
(@T_1_1_3,'MCQ','EASY','동작 관점 모델의 예시로 옳은 것은?','시퀀스/상태/활동 다이어그램.');
SET @Q13_1 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id,label,text,is_correct) VALUES
(@Q13_1,'A','클래스 다이어그램',0),
(@Q13_1,'B','시퀀스 다이어그램',1),
(@Q13_1,'C','컴포넌트 다이어그램',0),
(@Q13_1,'D','패키지 다이어그램',0);

INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES
(@T_1_1_3,'MCQ','NORMAL','요구 관리 도구의 대표적 기능이 아닌 것은?','정답: 그래픽 렌더링.');
SET @Q13_2 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id,label,text,is_correct) VALUES
(@Q13_2,'A','추적성',0),
(@Q13_2,'B','버전관리',0),
(@Q13_2,'C','우선순위 부여',0),
(@Q13_2,'D','그래픽 렌더링',1);

INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES
(@T_1_1_3,'MCQ','NORMAL','정적 분석의 주 목적은?','코드 규칙 위반/취약점 탐지.');
SET @Q13_3 = LAST_INSERT_ID();
INSERT INTO question_choice (question_id,label,text,is_correct) VALUES
(@Q13_3,'A','실행시간 성능 계측',0),
(@Q13_3,'B','코드 규칙/취약점 탐지',1),
(@Q13_3,'C','부하 테스트 자동화',0),
(@Q13_3,'D','A/B 테스트 관리',0);

-- 태그
INSERT INTO question_tag (question_id, tag) SELECT id,'모델링'  FROM question WHERE topic_id=@T_1_1_3;
INSERT INTO question_tag (question_id, tag) SELECT id,'정적분석' FROM question WHERE topic_id=@T_1_1_3 AND type='MCQ';
INSERT INTO question_tag (question_id, tag) SELECT id,'요구관리' FROM question WHERE topic_id=@T_1_1_3 AND type='MCQ';

-- ================= 2.2.1 (정규화) =================

-- OX
INSERT INTO question (topic_id, type, difficulty, text, explanation, ox_answer)
VALUES
(@T_2_2_1,'OX','EASY','1NF는 모든 속성이 원자값을 갖도록 요구한다.','반복/다중값 제거.',1),
(@T_2_2_1,'OX','NORMAL','3NF는 키가 아닌 속성 간 이행 종속을 허용한다.','허용하지 않는다.',0);

-- MCQ (4)
INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES
(@T_2_2_1,'MCQ','EASY','2NF 달성을 위해 제거해야 하는 것은?','부분 함수 종속 제거.'),
(@T_2_2_1,'MCQ','NORMAL','정규화의 주요 목적이 아닌 것은?','정답: 시스템 테마 색상 통일.');
SET @Q221_1 = LAST_INSERT_ID(); -- Q221_1: 2NF
SET @Q221_2 = @Q221_1 + 1;      -- 추정(연속 AUTO_INCREMENT 환경에서만; 안전 위해 다시 조회)
SET @Q221_2 = (SELECT MAX(id) FROM question WHERE topic_id=@T_2_2_1);

-- 정확히 ID 바인딩
SET @Q221_A = (SELECT id FROM question WHERE topic_id=@T_2_2_1 AND text LIKE '2NF 달성%' ORDER BY id DESC LIMIT 1);
SET @Q221_B = (SELECT id FROM question WHERE topic_id=@T_2_2_1 AND text LIKE '정규화의 주요 목적이 아닌%' ORDER BY id DESC LIMIT 1);

INSERT INTO question_choice (question_id,label,text,is_correct) VALUES
(@Q221_A,'A','부분 함수 종속',1),
(@Q221_A,'B','이행 함수 종속',0),
(@Q221_A,'C','원자성 결여',0),
(@Q221_A,'D','후보키 중복',0);

INSERT INTO question_choice (question_id,label,text,is_correct) VALUES
(@Q221_B,'A','이상현상 감소',0),
(@Q221_B,'B','무결성 향상',0),
(@Q221_B,'C','테마 색상 통일',1),
(@Q221_B,'D','갱신 용이성 향상',0);

-- 추가 MCQ 2개
INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES
(@T_2_2_1,'MCQ','NORMAL','1NF 위반의 예시로 옳은 것은?','반복/다중값 컬럼 보유.'),
(@T_2_2_1,'MCQ','HARD','정규화와 반정규화의 균형에 대한 설명으로 옳은 것은?','성능/복잡도 균형 관점에서 판단.');
SET @Q221_C = (SELECT id FROM question WHERE topic_id=@T_2_2_1 AND text LIKE '1NF 위반의 예시%' ORDER BY id DESC LIMIT 1);
SET @Q221_D = (SELECT id FROM question WHERE topic_id=@T_2_2_1 AND text LIKE '정규화와 반정규화의 균형%' ORDER BY id DESC LIMIT 1);

INSERT INTO question_choice (question_id,label,text,is_correct) VALUES
(@Q221_C,'A','전화번호 컬럼에 "010,011" 같이 다중값 저장',1),
(@Q221_C,'B','정규식 검증 사용',0),
(@Q221_C,'C','인덱스 추가',0),
(@Q221_C,'D','FK 설정',0);

INSERT INTO question_choice (question_id,label,text,is_correct) VALUES
(@Q221_D,'A','항상 최대 정규화를 적용해야 한다',0),
(@Q221_D,'B','성능/조회패턴에 따라 반정규화도 고려한다',1),
(@Q221_D,'C','인덱스는 정규화와 무관하다',0),
(@Q221_D,'D','정규화는 스키마 변경과 무관하다',0);

-- 태그
INSERT INTO question_tag (question_id, tag) SELECT id,'정규화' FROM question WHERE topic_id=@T_2_2_1;
INSERT INTO question_tag (question_id, tag) SELECT id,'데이터모델' FROM question WHERE topic_id=@T_2_2_1 AND type='MCQ';

-- ================= 실기(주관식) 보강 (1.1.2 / 2.2.1) =================

INSERT INTO question (topic_id, type, difficulty, text, explanation)
VALUES
(@T_1_1_2,'SHORT','NORMAL','애자일의 4가지 핵심 가치를 간단히 서술하세요.','가치 4항목 요약.'),
(@T_2_2_1,'LONG','NORMAL','정규화 절차와 반정규화 고려사항을 사례와 함께 서술하세요.','정규형 단계/이상현상/성능 균형.');

SET FOREIGN_KEY_CHECKS = 1;
