-- Cert Schedule table
CREATE TABLE IF NOT EXISTS cert_schedule (
  id BIGINT PRIMARY KEY AUTO_INCREMENT,
  cert_id BIGINT NOT NULL,
  year INT NOT NULL,
  term INT NOT NULL,
  reg_start DATE,
  reg_end DATE,
  exam_date DATE,
  INDEX idx_cert_year (cert_id, year)
);

