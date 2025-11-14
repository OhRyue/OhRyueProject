SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- 카테고리(전체/후기/꿀팁/스터디/질문/자유)
CREATE TABLE IF NOT EXISTS post_category (
  id    TINYINT PRIMARY KEY,
  code  VARCHAR(32) NOT NULL UNIQUE,  -- 'ALL','REVIEW','TIP','STUDY','QNA','FREE'
  name  VARCHAR(50) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 게시글
CREATE TABLE IF NOT EXISTS post (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  category_id   TINYINT NOT NULL,
  author_id     VARCHAR(100) NOT NULL,         -- 실제 작성자 ID(account.user_id)
  is_anonymous  TINYINT(1) NOT NULL DEFAULT 0, -- 익명 노출 여부
  title         VARCHAR(200) NOT NULL,
  content       MEDIUMTEXT NOT NULL,
  like_count    INT NOT NULL DEFAULT 0,
  comment_count INT NOT NULL DEFAULT 0,
  view_count    INT NOT NULL DEFAULT 0,
  created_at    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at    TIMESTAMP NULL,
  deleted_at    TIMESTAMP NULL,
  INDEX ix_post_category_time (category_id, created_at),
  INDEX ix_post_author_time (author_id, created_at),
  INDEX ix_post_created (created_at),
  FULLTEXT KEY ft_post_title_content (title, content)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 댓글
CREATE TABLE IF NOT EXISTS comment (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  post_id      BIGINT NOT NULL,
  author_id    VARCHAR(100) NOT NULL,
  is_anonymous TINYINT(1) NOT NULL DEFAULT 0,
  content      TEXT NOT NULL,
  like_count   INT NOT NULL DEFAULT 0,
  created_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at   TIMESTAMP NULL,
  deleted_at   TIMESTAMP NULL,
  INDEX ix_comment_post_time (post_id, created_at),
  INDEX ix_comment_author_time (author_id, created_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 좋아요(게시글/댓글 공용)
CREATE TABLE IF NOT EXISTS reaction (
  id          BIGINT AUTO_INCREMENT PRIMARY KEY,
  target_type ENUM('POST','COMMENT') NOT NULL, -- ReactionTargetType과 매핑
  target_id   BIGINT NOT NULL,
  user_id     VARCHAR(100) NOT NULL,
  created_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE KEY uq_reaction_once (target_type, target_id, user_id),
  INDEX ix_reaction_target (target_type, target_id),
  INDEX ix_reaction_user (user_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 조회 로그(Unique View/오늘의 게시글 집계 용)
CREATE TABLE IF NOT EXISTS post_view_log (
  id         BIGINT AUTO_INCREMENT PRIMARY KEY,
  post_id    BIGINT NOT NULL,
  user_id    VARCHAR(100) NULL,   -- 비로그인 허용 시 NULL + 별도 식별(UA/IP) 고려
  viewed_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_view_post_time (post_id, viewed_at),
  INDEX ix_view_user_time (user_id, viewed_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 신고 테이블 (ModerationDtos.ReportRequest/Response 대응)
CREATE TABLE IF NOT EXISTS post_report (
  id BIGINT PRIMARY KEY AUTO_INCREMENT,
  target_type VARCHAR(20) NOT NULL,   -- 'POST' / 'COMMENT' (ReactionTargetType.name() 기준)
  target_id   BIGINT NOT NULL,
  reporter_id VARCHAR(100) NOT NULL,
  reason      VARCHAR(500),
  status      VARCHAR(20) NOT NULL DEFAULT 'PENDING',  -- PENDING / RESOLVED / REJECTED 등
  created_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  resolved_at TIMESTAMP NULL,
  UNIQUE KEY uq_post_report (target_type, target_id, reporter_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 차단 테이블 (ModerationDtos.BlockRequest/BlockEntry 대응)
CREATE TABLE IF NOT EXISTS user_block (
  user_id         VARCHAR(100) NOT NULL,
  blocked_user_id VARCHAR(100) NOT NULL,
  created_at      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (user_id, blocked_user_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;
