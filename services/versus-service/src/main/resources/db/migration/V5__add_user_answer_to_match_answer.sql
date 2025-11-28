-- match_answer 테이블에 user_answer 컬럼 추가
-- 단답식(SHORT)과 서술형(LONG) 답안을 저장하기 위함
ALTER TABLE match_answer
ADD COLUMN user_answer TEXT NULL COMMENT '사용자가 제출한 답안 내용 (OX/MCQ: label, SHORT/LONG: 텍스트)';

