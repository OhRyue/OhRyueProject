CREATE TABLE IF NOT EXISTS question (
  id BIGINT PRIMARY KEY AUTO_INCREMENT,
  source VARCHAR(20) DEFAULT 'custom',      -- year/custom
  year INT NULL,
  round INT NULL,
  difficulty TINYINT NOT NULL DEFAULT 2,    -- 1~3
  stem TEXT NOT NULL,
  choices_json JSON NOT NULL,
  answer_idx INT NOT NULL,
  exp TEXT,
  meta_json JSON
);

CREATE TABLE IF NOT EXISTS question_tag (
  question_id BIGINT NOT NULL,
  tag VARCHAR(100) NOT NULL,
  PRIMARY KEY (question_id, tag)
);
CREATE INDEX idx_qtag_tag ON question_tag(tag);
