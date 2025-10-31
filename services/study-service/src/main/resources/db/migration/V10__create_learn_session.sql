CREATE TABLE IF NOT EXISTS learn_session (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id      VARCHAR(100) NOT NULL,
  topic_id     BIGINT       NOT NULL,
  mode         VARCHAR(20)  NOT NULL,   -- WRITTEN | PRACTICAL
  status       VARCHAR(20)  NOT NULL,   -- IN_PROGRESS | DONE
  progress_json JSON        NULL,
  started_at   TIMESTAMP    NOT NULL,
  updated_at   TIMESTAMP    NOT NULL
) ENGINE=InnoDB;

CREATE INDEX ix_lsession_user_topic ON learn_session (user_id, topic_id);
