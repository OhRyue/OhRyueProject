CREATE TABLE IF NOT EXISTS learn_step (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  session_id   BIGINT       NOT NULL,
  step         VARCHAR(30)  NOT NULL,   -- CONCEPT | MINI | REVIEW_WRONG | MCQ | REVIEW_WRONG2
  state        VARCHAR(20)  NOT NULL,   -- READY | PASS | FAIL
  score        INT          NULL,
  details_json JSON         NULL,
  created_at   TIMESTAMP    NOT NULL,
  CONSTRAINT fk_learn_step_session
    FOREIGN KEY (session_id) REFERENCES learn_session(id) ON DELETE CASCADE
) ENGINE=InnoDB;

CREATE INDEX ix_lstep_session ON learn_step (session_id);
