-- Certificate table
CREATE TABLE IF NOT EXISTS certificate (
  id BIGINT PRIMARY KEY AUTO_INCREMENT,
  name VARCHAR(255) NOT NULL,
  field VARCHAR(100) NULL
);

-- Create index for better search performance
CREATE INDEX idx_certificate_field ON certificate(field);

