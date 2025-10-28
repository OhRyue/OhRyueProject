-- 페이징/필터 보조 인덱스
CREATE INDEX idx_certificate_field_name ON certificate(field, name);
CREATE INDEX idx_cert_schedule_cert_year ON cert_schedule(cert_id, year, exam_date);
