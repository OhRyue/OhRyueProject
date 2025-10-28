-- 능력치(EMA)
CREATE TABLE IF NOT EXISTS ability_profile (
  user_id     BIGINT NOT NULL,
  tag         VARCHAR(100) NOT NULL,
  attempts    INT NOT NULL DEFAULT 0,
  corrects    INT NOT NULL DEFAULT 0,
  ema_correct DECIMAL(5,4) NOT NULL DEFAULT 0.5000,
  updated_at  TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (user_id, tag)
);

-- 오답노트
CREATE TABLE IF NOT EXISTS wrong_note (
  user_id      BIGINT NOT NULL,
  question_id  BIGINT NOT NULL,
  tag          VARCHAR(100) NOT NULL,
  wrong_count  INT NOT NULL DEFAULT 0,
  first_wrong_at TIMESTAMP NULL,
  last_wrong_at  TIMESTAMP NULL,
  status       ENUM('todo','reviewing','cleared') DEFAULT 'todo',
  PRIMARY KEY (user_id, question_id, tag)
);

CREATE INDEX idx_wrong_note_user_status ON wrong_note(user_id, status, last_wrong_at DESC);
CREATE INDEX idx_ability_profile_user_tag ON ability_profile(user_id, tag);
