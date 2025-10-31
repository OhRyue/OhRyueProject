-- 문제-태그 매핑
CREATE TABLE IF NOT EXISTS question_tag (
    id          BIGINT AUTO_INCREMENT PRIMARY KEY,
    question_id BIGINT       NOT NULL,
    tag         VARCHAR(80)  NOT NULL
) ENGINE=InnoDB;

CREATE INDEX ix_qtag_tag ON question_tag (tag);
CREATE INDEX ix_qtag_qid ON question_tag (question_id);
