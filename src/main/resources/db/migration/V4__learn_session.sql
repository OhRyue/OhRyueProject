CREATE TABLE IF NOT EXISTS learn_session (
  id         BIGINT PRIMARY KEY AUTO_INCREMENT,
  user_id    BIGINT NOT NULL,
  concept_id BIGINT NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  status     VARCHAR(20) DEFAULT 'OPEN',
  CONSTRAINT fk_ls_concept FOREIGN KEY (concept_id) REFERENCES concept(id)
);
CREATE INDEX idx_learn_session_user ON learn_session(user_id, created_at DESC);
