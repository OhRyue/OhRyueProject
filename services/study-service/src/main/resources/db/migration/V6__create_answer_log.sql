-- 사용자 풀이 로그
CREATE TABLE IF NOT EXISTS answer_log (
    id           BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id      VARCHAR(100)  NOT NULL,
    question_id  BIGINT        NOT NULL,
    correct      BOOLEAN       NOT NULL,
    answered_at  TIMESTAMP     NOT NULL,
    user_answer  VARCHAR(50)   NULL,
    mode         VARCHAR(20)   NULL,   -- WRITTEN / PRACTICAL
    type         VARCHAR(20)   NULL    -- OX / MCQ / SHORT / LONG
) ENGINE=InnoDB;

CREATE INDEX ix_answerlog_user_time ON answer_log (user_id, answered_at);
CREATE INDEX ix_answerlog_question   ON answer_log (question_id);
