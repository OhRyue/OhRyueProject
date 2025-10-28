CREATE TABLE IF NOT EXISTS certificate (
  id BIGINT PRIMARY KEY AUTO_INCREMENT,
  name VARCHAR(255) NOT NULL,
  field VARCHAR(100) NOT NULL
);

CREATE TABLE IF NOT EXISTS cert_schedule (
  id BIGINT PRIMARY KEY AUTO_INCREMENT,
  cert_id BIGINT NOT NULL,
  year INT NOT NULL,
  term INT NOT NULL,
  reg_start DATE,
  reg_end DATE,
  exam_date DATE,
  CONSTRAINT fk_cert FOREIGN KEY (cert_id) REFERENCES certificate(id)
);
