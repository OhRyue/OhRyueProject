-- topic: 계층형 카테고리(1=과목,2=주요,3=세부,4=세세)
CREATE TABLE IF NOT EXISTS `topic` (
  `id`        BIGINT PRIMARY KEY AUTO_INCREMENT,
  `cert_id`   BIGINT NOT NULL,
  `code`      VARCHAR(50)  NOT NULL,   -- 예: 1.1.1.4
  `name`      VARCHAR(200) NOT NULL,   -- 예: 네트워크 분석
  `level`     TINYINT      NOT NULL,   -- 1~4
  `parent_id` BIGINT NULL,
  `ord`       INT NOT NULL DEFAULT 0,
  UNIQUE KEY `uk_topic_cert_code` (`cert_id`, `code`),
  KEY `idx_topic_parent` (`parent_id`),
  KEY `idx_topic_cert_level` (`cert_id`, `level`, `ord`),
  CONSTRAINT `fk_topic_parent` FOREIGN KEY (`parent_id`) REFERENCES `topic`(`id`)
) ENGINE=InnoDB;

-- concept에 세세항목 연결
SET @col_exists := (
  SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'concept' AND COLUMN_NAME = 'topic_id'
);
SET @sql := IF(@col_exists = 0,
  'ALTER TABLE `concept` ADD COLUMN `topic_id` BIGINT NULL',
  'SELECT 1');
PREPARE s FROM @sql; EXECUTE s; DEALLOCATE PREPARE s;

-- 외래키 안정화를 위해 인덱스(없으면 생성)
SET @idx_exists := (
  SELECT COUNT(*) FROM INFORMATION_SCHEMA.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'concept' AND INDEX_NAME = 'idx_concept_topic'
);
SET @sql := IF(@idx_exists = 0,
  'ALTER TABLE `concept` ADD INDEX `idx_concept_topic` (`topic_id`)',
  'SELECT 1');
PREPARE s FROM @sql; EXECUTE s; DEALLOCATE PREPARE s;

-- concept.topic_id → topic.id FK
SET @fk_exists := (
  SELECT COUNT(*) FROM INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS
  WHERE CONSTRAINT_SCHEMA = DATABASE() AND CONSTRAINT_NAME = 'fk_concept_topic'
);
SET @sql := IF(@fk_exists = 0,
  'ALTER TABLE `concept` ADD CONSTRAINT `fk_concept_topic` FOREIGN KEY (`topic_id`) REFERENCES `topic`(`id`)',
  'SELECT 1');
PREPARE s FROM @sql; EXECUTE s; DEALLOCATE PREPARE s;

-- 문제-토픽 매핑(총정리/보조학습 출제 범위)
CREATE TABLE IF NOT EXISTS `question_topic` (
  `question_id` BIGINT NOT NULL,
  `topic_id`    BIGINT NOT NULL,
  PRIMARY KEY (`question_id`, `topic_id`),
  KEY `idx_qtopic_topic` (`topic_id`)
) ENGINE=InnoDB;
