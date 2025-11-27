DROP TABLE IF EXISTS exam_schedule;

CREATE TABLE exam_schedule (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    source VARCHAR(20) NOT NULL,
    impl_yy VARCHAR(4),
    impl_seq VARCHAR(10),
    qualgb_cd VARCHAR(10),
    qualgb_nm VARCHAR(50),
    jm_cd VARCHAR(20),
    jm_nm VARCHAR(100),
    description TEXT,
    doc_reg_start_dt DATE,
    doc_reg_end_dt DATE,
    doc_exam_start_dt DATE,
    doc_exam_end_dt DATE,
    doc_pass_dt DATE,
    prac_reg_start_dt DATE,
    prac_reg_end_dt DATE,
    prac_exam_start_dt DATE,
    prac_exam_end_dt DATE,
    prac_pass_dt DATE,
    created_at TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    updated_at TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6)
);

CREATE INDEX idx_exam_schedule_jm
    ON exam_schedule (jm_cd);

CREATE INDEX idx_exam_schedule_impl
    ON exam_schedule (impl_yy, impl_seq);







