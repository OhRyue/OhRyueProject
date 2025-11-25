-- LearningSession과 LearningStep 테이블 생성
-- 계층적 학습 세션 구조로 개선

-- 1. LearningSession 테이블 생성 (전체 학습 플로우 관리)
CREATE TABLE IF NOT EXISTS learning_session (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id          VARCHAR(100) NOT NULL,
  topic_id         BIGINT NOT NULL,
  mode             VARCHAR(20) NOT NULL COMMENT 'WRITTEN | PRACTICAL',
  status           VARCHAR(20) NOT NULL DEFAULT 'IN_PROGRESS' COMMENT 'IN_PROGRESS | DONE',
  started_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX ix_ls_user_topic (user_id, topic_id),
  INDEX ix_ls_user_updated (user_id, updated_at),
  INDEX ix_ls_status (status)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 2. LearningStep 테이블 생성 (각 학습 단계 관리)
CREATE TABLE IF NOT EXISTS learning_step (
  id                  BIGINT AUTO_INCREMENT PRIMARY KEY,
  learning_session_id BIGINT NOT NULL,
  step_code           VARCHAR(30) NOT NULL COMMENT 'CONCEPT | MINI | REVIEW_WRONG | MCQ | PRACTICAL | REVIEW_WRONG2 | SUMMARY',
  status              VARCHAR(20) NOT NULL DEFAULT 'READY' COMMENT 'READY | IN_PROGRESS | COMPLETE',
  score_pct           INT NULL COMMENT '정답률(%)',
  metadata_json       JSON NULL COMMENT '단계별 메타데이터',
  created_at          TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at          TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX ix_ls_learning_session (learning_session_id),
  INDEX ix_ls_step_code (learning_session_id, step_code),
  UNIQUE KEY uq_ls_session_step (learning_session_id, step_code),
  CONSTRAINT fk_ls_learning_session FOREIGN KEY (learning_session_id) REFERENCES learning_session(id)
    ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 3. StudySession에 learning_step_id 컬럼 추가
ALTER TABLE study_session
  ADD COLUMN learning_step_id BIGINT NULL COMMENT '어떤 학습 단계의 문제 풀이 세션인지',
  ADD INDEX ix_ss_learning_step (learning_step_id),
  ADD CONSTRAINT fk_ss_learning_step FOREIGN KEY (learning_step_id) REFERENCES learning_step(id)
    ON DELETE SET NULL ON UPDATE RESTRICT;

