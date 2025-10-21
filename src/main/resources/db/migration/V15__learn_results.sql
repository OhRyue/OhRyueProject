-- V15__learn_results.sql
-- 제출 이력 저장(마이크로/리뷰)

-- Micro 회차 결과
CREATE TABLE IF NOT EXISTS micro_result (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id      BIGINT      NULL,
  concept_id   BIGINT      NOT NULL,
  topic_id     BIGINT      NOT NULL,
  score        INT         NOT NULL,
  total        INT         NOT NULL,
  created_at   TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP,
  KEY idx_micro_result_user (user_id),
  KEY idx_micro_result_topic (topic_id),
  KEY idx_micro_result_concept (concept_id),
  KEY idx_micro_result_created (created_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Micro 회차 문항 기록
CREATE TABLE IF NOT EXISTS micro_result_item (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  result_id     BIGINT      NOT NULL,
  item_type     VARCHAR(8)  NOT NULL,  -- 'MINI' | 'QUIZ'
  ref_id        BIGINT      NOT NULL,  -- concept_check.id 또는 question.id
  correct       TINYINT(1)  NOT NULL,
  chosen_idx    INT         NULL,      -- 사용자가 고른 보기
  answer_idx    INT         NULL,      -- 정답 인덱스(스냅샷)
  tag           VARCHAR(64) NULL,      -- 간이 태그(meta_json 기반)
  exp_snapshot  TEXT        NULL,      -- 해설 스냅샷(DB 해설 or AI 생성문)
  created_at    TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_micro_item_result FOREIGN KEY (result_id) REFERENCES micro_result(id) ON DELETE CASCADE,
  KEY idx_micro_item_result (result_id),
  KEY idx_micro_item_ref (ref_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Review 회차 결과
CREATE TABLE IF NOT EXISTS review_result (
  id               BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id          BIGINT      NULL,
  detail_topic_id  BIGINT      NOT NULL,
  score            INT         NOT NULL,
  total            INT         NOT NULL,
  created_at       TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP,
  KEY idx_review_result_user (user_id),
  KEY idx_review_result_detail (detail_topic_id),
  KEY idx_review_result_created (created_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Review 회차 문항 기록
CREATE TABLE IF NOT EXISTS review_result_item (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  result_id     BIGINT      NOT NULL,
  question_id   BIGINT      NOT NULL,
  correct       TINYINT(1)  NOT NULL,
  chosen_idx    INT         NULL,
  answer_idx    INT         NULL,
  tag           VARCHAR(64) NULL,
  exp_snapshot  TEXT        NULL,
  created_at    TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_review_item_result FOREIGN KEY (result_id) REFERENCES review_result(id) ON DELETE CASCADE,
  KEY idx_review_item_result (result_id),
  KEY idx_review_item_q (question_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
