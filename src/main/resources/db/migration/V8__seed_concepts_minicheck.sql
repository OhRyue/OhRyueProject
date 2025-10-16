-- 샘플 Concept 1개 + 미니체크 2개
INSERT INTO concept(cert_id, category, title, summary, pitfalls, examples_json, tags_json)
VALUES (
  1,'DB/SQL','트랜잭션과 격리수준',
  '트랜잭션은 작업단위, 격리수준은 동시성 오류 방지 강도. 실무 기본은 RC.',
  'RC와 RR 차이(팬텀/NR 혼동), 엔진별 구현 차이 주의',
  JSON_ARRAY('예: 은행 이체 중 롤백 시나리오'),
  JSON_ARRAY('트랜잭션','격리수준','RC','RR')
);
SET @CID := LAST_INSERT_ID();

INSERT INTO concept_check(concept_id, stem, choices_json, answer_idx, description) VALUES
(@CID,'Read Committed에서 방지되는 현상은?',
 JSON_ARRAY('Dirty Read','Non-Repeatable Read','Phantom Read','Write Skew'),0,
 'RC는 Dirty Read만 방지'),
(@CID,'Repeatable Read에서 여전히 발생 가능한 현상은?',
 JSON_ARRAY('Dirty Read','Non-Repeatable Read','Phantom Read','Lost Update'),2,
 '전통 정의상 RR은 Phantom 미방지(엔진별 상이)');

-- 문제 5문항 샘플(태그: 트랜잭션/격리수준/RC/RR)
INSERT INTO question(stem, choices_json, answer_idx, difficulty, exp, meta_json)
VALUES
('트랜잭션 ACID 중 일관성 설명으로 맞는 것은?',
 JSON_ARRAY('항상 커밋됨','제약조건을 유지','항상 스냅샷 사용','항상 직렬화'),1,2,'제약/무결성 유지', JSON_OBJECT()),
('RC 수준에서 허용되는 현상은?',
 JSON_ARRAY('Dirty Read','Non-Repeatable Read','두 번 커밋','직렬화'),1,2,'NR 허용', JSON_OBJECT()),
('RR 수준에서 일반적으로 방지되는 것은?',
 JSON_ARRAY('Dirty Read','Non-Repeatable Read','Starvation','LiveLock'),1,2,'NR 방지', JSON_OBJECT()),
('Phantom Read를 근본적으로 막는 격리수준은?',
 JSON_ARRAY('RC','RR','Serializable','Read Uncommitted'),2,3,'직렬화 필요', JSON_OBJECT()),
('트랜잭션 롤백이 필요한 상황은?',
 JSON_ARRAY('정상처리','부분성공','오류/예외','캐시미스'),2,1,'오류 시 롤백', JSON_OBJECT());

-- 태깅
SET @Q1 := (SELECT MIN(id) FROM question);
INSERT INTO question_tag(question_id, tag) VALUES
(@Q1,'트랜잭션'),(@Q1+1,'격리수준'),(@Q1+1,'RC'),(@Q1+2,'RR'),(@Q1+3,'Serializable'),(@Q1+4,'트랜잭션');
