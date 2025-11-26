-- 정보처리기사 2026년도 시험 일정 수동 입력 (시연용)
-- jmCd: 1320 (정보처리기사)
-- 2025년도 데이터를 기반으로 2026년도 데이터 생성

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
  -- 2026년도 제1회 (2025년도 제1회 기준으로 1년 추가)
  ('QNET', '2026', '1', 'T', '국가기술자격', '1320', '정보처리기사', 
   '국가기술자격 기사 (2026년도 제1회)',
   '2026-01-13', '2026-01-16', '2026-02-07', '2026-03-04', '2026-03-12',
   '2026-03-24', '2026-04-14', '2026-04-19', '2026-05-09', '2026-06-05'),
  
  -- 2026년도 제2회 (2025년도 제2회 기준으로 1년 추가)
  ('QNET', '2026', '2', 'T', '국가기술자격', '1320', '정보처리기사', 
   '국가기술자격 기사 (2026년도 제2회)',
   '2026-04-14', '2026-04-17', '2026-05-10', '2026-05-30', '2026-06-11',
   '2026-06-23', '2026-07-14', '2026-07-19', '2026-08-06', '2026-09-05'),
  
  -- 2026년도 제3회 (2025년도 제3회 기준으로 1년 추가)
  ('QNET', '2026', '3', 'T', '국가기술자격', '1320', '정보처리기사', 
   '국가기술자격 기사 (2026년도 제3회)',
   '2026-07-21', '2026-07-24', '2026-08-09', '2026-09-01', '2026-09-10',
   '2026-09-22', '2026-10-27', '2026-11-01', '2026-11-21', '2026-12-05'),
  
  -- 2026년도 제3회 (추가 등록 기간 - 2025년도 데이터 참고)
  ('QNET', '2026', '3', 'T', '국가기술자격', '1320', '정보처리기사', 
   '국가기술자격 기사 (2026년도 제3회)',
   '2026-08-03', '2026-08-04', '2026-08-09', '2026-09-01', '2026-09-10',
   '2026-09-22', '2026-10-27', '2026-11-01', '2026-11-21', '2026-12-05')
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

