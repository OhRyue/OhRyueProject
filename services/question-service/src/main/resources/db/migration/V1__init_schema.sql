CREATE TABLE IF NOT EXISTS topic (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(120) NOT NULL,
  level INT NOT NULL,
  parent_id BIGINT NULL,
  ord INT NOT NULL DEFAULT 0,
  created_at TIMESTAMP NULL,
  updated_at TIMESTAMP NULL,
  INDEX idx_topic_parent (parent_id)
);

CREATE TABLE IF NOT EXISTS question (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  stem VARCHAR(400) NOT NULL,
  choices_json JSON NOT NULL,
  answer_idx INT NOT NULL,
  exp TEXT NULL,
  meta_json JSON NULL,
  difficulty INT NULL,
  created_at TIMESTAMP NULL,
  updated_at TIMESTAMP NULL,
  INDEX idx_question_level (difficulty)
);

CREATE TABLE IF NOT EXISTS question_topic (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  question_id BIGINT NOT NULL,
  topic_id BIGINT NOT NULL,
  CONSTRAINT uk_question_topic UNIQUE (question_id, topic_id),
  INDEX idx_qt_topic (topic_id)
);

CREATE TABLE IF NOT EXISTS question_tag (
  question_id BIGINT NOT NULL,
  tag VARCHAR(64) NOT NULL,
  PRIMARY KEY (question_id, tag)
);
