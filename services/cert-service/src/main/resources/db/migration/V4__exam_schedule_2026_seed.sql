-- 정보처리기사 2026년도 시험 일정
-- jmCd: 1320 (정보처리기사)

INSERT INTO exam_schedule (
  source,
  impl_yy,
  impl_seq,
  qualgb_cd,
  qualgb_nm,
  jm_cd,
  jm_nm,
  description,
  doc_reg_start_dt,
  doc_reg_end_dt,
  doc_exam_start_dt,
  doc_exam_end_dt,
  doc_pass_dt,
  prac_reg_start_dt,
  prac_reg_end_dt,
  prac_exam_start_dt,
  prac_exam_end_dt,
  prac_pass_dt
) VALUES
  -- 2026년도 제1회
  ('QNET', '2026', '1', 'T', '국가기술자격', '1320', '정보처리기사', 
   '국가기술자격 기사 (2026년도 제1회)',
   '2026-01-12', '2026-01-15', '2026-01-30', '2026-03-03', '2026-03-11',
   '2026-03-23', '2026-03-26', '2026-04-18', '2026-05-06', '2026-06-05'),
  
  -- 2026년도 제2회
  ('QNET', '2026', '2', 'T', '국가기술자격', '1320', '정보처리기사', 
   '국가기술자격 기사 (2026년도 제2회)',
   '2026-04-20', '2026-04-23', '2026-05-09', '2026-05-29', '2026-06-10',
   '2026-06-22', '2026-07-25', '2026-07-18', '2026-08-05', '2026-09-04'),
  
  -- 2026년도 제3회
  ('QNET', '2026', '3', 'T', '국가기술자격', '1320', '정보처리기사', 
   '국가기술자격 기사 (2026년도 제3회)',
   '2026-07-20', '2026-07-23', '2026-08-07', '2026-09-01', '2026-09-09',
   '2026-09-21', '2026-09-23', '2026-10-24', '2026-11-13', '2026-12-11')

ON DUPLICATE KEY UPDATE
  qualgb_cd = VALUES(qualgb_cd),
  qualgb_nm = VALUES(qualgb_nm),
  jm_nm = VALUES(jm_nm),
  description = VALUES(description),
  doc_reg_start_dt = VALUES(doc_reg_start_dt),
  doc_reg_end_dt = VALUES(doc_reg_end_dt),
  doc_exam_start_dt = VALUES(doc_exam_start_dt),
  doc_exam_end_dt = VALUES(doc_exam_end_dt),
  doc_pass_dt = VALUES(doc_pass_dt),
  prac_reg_start_dt = VALUES(prac_reg_start_dt),
  prac_reg_end_dt = VALUES(prac_reg_end_dt),
  prac_exam_start_dt = VALUES(prac_exam_start_dt),
  prac_exam_end_dt = VALUES(prac_exam_end_dt),
  prac_pass_dt = VALUES(prac_pass_dt);
