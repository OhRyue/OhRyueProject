CREATE TABLE IF NOT EXISTS concept (
  id           BIGINT PRIMARY KEY AUTO_INCREMENT,
  cert_id      BIGINT NOT NULL,
  category     VARCHAR(100) NOT NULL,
  title        VARCHAR(200) NOT NULL,
  summary      TEXT NOT NULL,
  pitfalls     TEXT,
  examples_json JSON,
  tags_json     JSON,
  created_at   TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_concept_cert FOREIGN KEY (cert_id) REFERENCES certificate(id)
);

CREATE TABLE IF NOT EXISTS concept_check (
  id            BIGINT PRIMARY KEY AUTO_INCREMENT,
  concept_id    BIGINT NOT NULL,
  stem          TEXT NOT NULL,
  choices_json  JSON NOT NULL,
  answer_idx    INT NOT NULL,
  explanation   TEXT,
  created_at    TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_cc_concept FOREIGN KEY (concept_id) REFERENCES concept(id)
);

CREATE INDEX idx_concept_cert_category ON concept(cert_id, category);
CREATE INDEX idx_concept_check_concept ON concept_check(concept_id);
