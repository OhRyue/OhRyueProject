-- 소프트웨어 설계(1) > 1.1 요구사항 확인 > 1.1.1 현행 시스템 분석 > (세세 6개)

INSERT INTO `topic`(`cert_id`, `code`, `name`, `level`, `parent_id`, `ord`)
VALUES (1,'1','소프트웨어 설계',1,NULL,1)
ON DUPLICATE KEY UPDATE name=VALUES(name), level=VALUES(level), parent_id=VALUES(parent_id), ord=VALUES(ord);
SET @SUBJ := (SELECT `id` FROM `topic` WHERE `cert_id`=1 AND `code`='1');

INSERT INTO `topic`(`cert_id`, `code`, `name`, `level`, `parent_id`, `ord`)
VALUES (1,'1.1','요구사항 확인',2,@SUBJ,1)
ON DUPLICATE KEY UPDATE name=VALUES(name), level=VALUES(level), parent_id=VALUES(parent_id), ord=VALUES(ord);
SET @MAIN := (SELECT `id` FROM `topic` WHERE `cert_id`=1 AND `code`='1.1');

INSERT INTO `topic`(`cert_id`, `code`, `name`, `level`, `parent_id`, `ord`)
VALUES (1,'1.1.1','현행 시스템 분석',3,@MAIN,1)
ON DUPLICATE KEY UPDATE name=VALUES(name), level=VALUES(level), parent_id=VALUES(parent_id), ord=VALUES(ord);
SET @DETAIL := (SELECT `id` FROM `topic` WHERE `cert_id`=1 AND `code`='1.1.1');

INSERT INTO `topic`(`cert_id`, `code`, `name`, `level`, `parent_id`, `ord`) VALUES
(1,'1.1.1.1','플랫폼 기능 분석',4,@DETAIL,1),
(1,'1.1.1.2','플랫폼 성능 특성 분석',4,@DETAIL,2),
(1,'1.1.1.3','운영체제 분석',4,@DETAIL,3),
(1,'1.1.1.4','네트워크 분석',4,@DETAIL,4),
(1,'1.1.1.5','DBMS 분석',4,@DETAIL,5),
(1,'1.1.1.6','비즈니스융합분석',4,@DETAIL,6)
ON DUPLICATE KEY UPDATE name=VALUES(name), level=VALUES(level), parent_id=VALUES(parent_id), ord=VALUES(ord);

-- 세세항목 id 보관 (DBMS 분석)
SET @MICRO_DBMS := (SELECT `id` FROM `topic` WHERE `cert_id`=1 AND `code`='1.1.1.5');

-- [필수] concept 1개를 세세항목에 매핑 (topic_id가 아직 NULL인 최신 1건)
--  - LearnFlow의 /learn/micro/start 가 개념-세세항목 연결을 전제로 동작하기 때문
UPDATE `concept`
   SET `topic_id` = @MICRO_DBMS
 WHERE `topic_id` IS NULL
 ORDER BY `id` DESC
 LIMIT 1;

-- question_topic 매핑 (테스트용 최소 매핑)
--  - Micro/Review에서 topic 기반 출제가 바로 확인됨
--  - 이미 매핑된 경우 중복 삽입 방지를 위해 IGNORE 사용
SET @TID := @MICRO_DBMS;
-- 6문항(id ASC)만 간단히 매핑
INSERT IGNORE INTO `question_topic`(`question_id`, `topic_id`)
SELECT q.`id`, @TID
  FROM `question` q
 ORDER BY q.`id` ASC
 LIMIT 6;
