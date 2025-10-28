-- Micro/Review 결과 영구 저장 테이블

-- =========================
-- 1) Micro 결과
-- =========================
CREATE TABLE micro_result (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id      BIGINT NOT NULL,
  cert_id      BIGINT NULL,
  topic_id     BIGINT NOT NULL, -- micro topic(level=4)
  concept_id   BIGINT NULL,
  score        INT    NOT NULL,
  total        INT    NOT NULL,
  created_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE INDEX idx_micro_result_user_created ON micro_result(user_id, created_at DESC);

CREATE TABLE micro_result_item (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  result_id     BIGINT NOT NULL,
  item_type     VARCHAR(16) NOT NULL,     -- 'MINI' | 'QUIZ'
  ref_id        BIGINT NOT NULL,          -- concept_check.id or question.id
  chosen_idx    INT NULL,
  correct       TINYINT(1) NOT NULL DEFAULT 0,
  explanation   TEXT NULL,                -- MINI는 DB 해설, QUIZ는 AI→폴백
  ord_no        INT NOT NULL DEFAULT 0,
  created_at    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_micro_result_item_result
    FOREIGN KEY (result_id) REFERENCES micro_result(id)
      ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE INDEX idx_micro_result_item_result_ord ON micro_result_item(result_id, ord_no);


-- =========================
-- 2) Review 결과
-- =========================
CREATE TABLE review_result (
  id             BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id        BIGINT NOT NULL,
  cert_id        BIGINT NULL,
  detail_topic_id BIGINT NOT NULL,        -- level=3
  score          INT    NOT NULL,
  total          INT    NOT NULL,
  ai_summary     TEXT   NULL,             -- 최종 AI 요약
  created_at     TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE INDEX idx_review_result_user_created ON review_result(user_id, created_at DESC);

CREATE TABLE review_result_item (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  result_id     BIGINT NOT NULL,
  question_id   BIGINT NOT NULL,
  chosen_idx    INT NULL,
  correct       TINYINT(1) NOT NULL DEFAULT 0,
  ai_explanation TEXT NULL,               -- 문항별 AI 해설(폴백 포함)
  ord_no        INT NOT NULL DEFAULT 0,
  created_at    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_review_result_item_result
    FOREIGN KEY (result_id) REFERENCES review_result(id)
      ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE INDEX idx_review_result_item_result_ord ON review_result_item(result_id, ord_no);
