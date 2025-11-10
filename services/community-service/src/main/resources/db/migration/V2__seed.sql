SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ---------- 카테고리 ----------
CREATE TABLE IF NOT EXISTS post_category (
  id    TINYINT PRIMARY KEY,
  code  VARCHAR(32) NOT NULL UNIQUE,  -- 'ALL','REVIEW','TIP','STUDY','QNA','FREE'
  name  VARCHAR(50) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 기본 카테고리(ALL은 뷰/필터용 가상 개념이지만, 운영 편의상 0번으로 둡니다)
INSERT INTO post_category (id, code, name)
SELECT 0, 'ALL',   '전체(자동)'
WHERE NOT EXISTS (SELECT 1 FROM post_category WHERE id=0);
INSERT INTO post_category (id, code, name)
SELECT 1, 'REVIEW','후기'
WHERE NOT EXISTS (SELECT 1 FROM post_category WHERE id=1);
INSERT INTO post_category (id, code, name)
SELECT 2, 'TIP',   '꿀팁'
WHERE NOT EXISTS (SELECT 1 FROM post_category WHERE id=2);
INSERT INTO post_category (id, code, name)
SELECT 3, 'STUDY', '스터디'
WHERE NOT EXISTS (SELECT 1 FROM post_category WHERE id=3);
INSERT INTO post_category (id, code, name)
SELECT 4, 'QNA',   '질문'
WHERE NOT EXISTS (SELECT 1 FROM post_category WHERE id=4);
INSERT INTO post_category (id, code, name)
SELECT 5, 'FREE',  '자유'
WHERE NOT EXISTS (SELECT 1 FROM post_category WHERE id=5);

-- ---------- 게시글 ----------
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
  -- 멱등 시드 판별 위한 자연키(작성자+제목 조합) 보조 인덱스
  UNIQUE KEY uq_post_seed (author_id, title),
  INDEX ix_post_category_time (category_id, created_at),
  INDEX ix_post_author_time (author_id, created_at),
  INDEX ix_post_created (created_at),
  FULLTEXT KEY ft_post_title_content (title, content)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 샘플 사용자 변수
SET @u1 := 'ohryue';
SET @u2 := 'user2';
SET @u3 := 'guest';
SET @u4 := 'admin';

-- 샘플 게시글 (카테고리: 후기/꿀팁/스터디/질문/자유)
INSERT INTO post (category_id, author_id, is_anonymous, title, content)
SELECT 1, @u1, 0, '정보처리기사 1회 합격 후기',
'첫 도전에서 합격했습니다. 학습 루프(개념→OX→MCQ→총정리)가 큰 도움이 되었어요. D-Day 목표 잡고 매일 30분 이상 실습한 게 포인트였습니다.'
WHERE NOT EXISTS (SELECT 1 FROM post WHERE author_id=@u1 AND title='정보처리기사 1회 합격 후기');

INSERT INTO post (category_id, author_id, is_anonymous, title, content)
SELECT 2, @u2, 0, '필기 과목 암기 팁',
'요약 노트는 토픽 코드(1, 1.1, 1.1.1) 기준으로 정리하고, 헷갈리는 포인트는 태그로 묶어 재복습하세요.'
WHERE NOT EXISTS (SELECT 1 FROM post WHERE author_id=@u2 AND title='필기 과목 암기 팁');

INSERT INTO post (category_id, author_id, is_anonymous, title, content)
SELECT 3, @u3, 1, '주 2회 온라인 스터디 모집(초보 환영)',
'디스코드로 매주 화/목 밤 9시. 오늘 학습 루프 인증 + 어려웠던 문제 토론합니다. 익명 참여 가능.'
WHERE NOT EXISTS (SELECT 1 FROM post WHERE author_id=@u3 AND title='주 2회 온라인 스터디 모집(초보 환영)');

INSERT INTO post (category_id, author_id, is_anonymous, title, content)
SELECT 4, @u1, 0, '실기 준비는 언제 시작하는 게 좋을까요?',
'필기 합격 후 바로 실기 들어갈지, 쉬었다가 갈지 고민입니다. 선배님들 의견 부탁드려요.'
WHERE NOT EXISTS (SELECT 1 FROM post WHERE author_id=@u1 AND title='실기 준비는 언제 시작하는 게 좋을까요?');

INSERT INTO post (category_id, author_id, is_anonymous, title, content)
SELECT 5, @u2, 0, '오늘의 목표 달성!',
'마이크로 1세트 + MCQ 10문제 클리어했습니다. 모두 화이팅!'
WHERE NOT EXISTS (SELECT 1 FROM post WHERE author_id=@u2 AND title='오늘의 목표 달성!');

-- 방금 생성한 게시글 ID 변수화
SET @p_review := (SELECT id FROM post WHERE author_id=@u1 AND title='정보처리기사 1회 합격 후기' LIMIT 1);
SET @p_tip    := (SELECT id FROM post WHERE author_id=@u2 AND title='필기 과목 암기 팁' LIMIT 1);
SET @p_study  := (SELECT id FROM post WHERE author_id=@u3 AND title='주 2회 온라인 스터디 모집(초보 환영)' LIMIT 1);
SET @p_qna    := (SELECT id FROM post WHERE author_id=@u1 AND title='실기 준비는 언제 시작하는 게 좋을까요?' LIMIT 1);
SET @p_free   := (SELECT id FROM post WHERE author_id=@u2 AND title='오늘의 목표 달성!' LIMIT 1);

-- ---------- 댓글 ----------
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

-- 댓글 시드(멱등)
INSERT INTO comment (post_id, author_id, is_anonymous, content)
SELECT @p_review, @u2, 0, '축하드립니다! 비결 공유 감사해요 🙌'
WHERE @p_review IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM comment WHERE post_id=@p_review AND author_id=@u2 AND content='축하드립니다! 비결 공유 감사해요 🙌');

INSERT INTO comment (post_id, author_id, is_anonymous, content)
SELECT @p_qna, @u3, 1, '필기 끝나고 바로 실기 감 추천드립니다. 흐름 유지가 좋아요.'
WHERE @p_qna IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM comment WHERE post_id=@p_qna AND author_id=@u3 AND content='필기 끝나고 바로 실기 감 추천드립니다. 흐름 유지가 좋아요.');

-- ---------- 반응(좋아요) ----------
CREATE TABLE IF NOT EXISTS reaction (
  id          BIGINT AUTO_INCREMENT PRIMARY KEY,
  target_type ENUM('POST','COMMENT') NOT NULL,
  target_id   BIGINT NOT NULL,
  user_id     VARCHAR(100) NOT NULL,
  created_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE KEY uq_reaction_once (target_type, target_id, user_id),
  INDEX ix_reaction_target (target_type, target_id),
  INDEX ix_reaction_user (user_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 좋아요 시드(멱등)
-- post 좋아요
INSERT INTO reaction (target_type, target_id, user_id)
SELECT 'POST', @p_review, @u2
WHERE @p_review IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM reaction WHERE target_type='POST' AND target_id=@p_review AND user_id=@u2);

INSERT INTO reaction (target_type, target_id, user_id)
SELECT 'POST', @p_tip, @u1
WHERE @p_tip IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM reaction WHERE target_type='POST' AND target_id=@p_tip AND user_id=@u1);

-- comment 좋아요
SET @c1 := (SELECT id FROM comment WHERE post_id=@p_review AND author_id=@u2 LIMIT 1);
INSERT INTO reaction (target_type, target_id, user_id)
SELECT 'COMMENT', @c1, @u1
WHERE @c1 IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM reaction WHERE target_type='COMMENT' AND target_id=@c1 AND user_id=@u1);

-- ---------- 조회 로그(선택) ----------
CREATE TABLE IF NOT EXISTS post_view_log (
  id         BIGINT AUTO_INCREMENT PRIMARY KEY,
  post_id    BIGINT NOT NULL,
  user_id    VARCHAR(100) NULL,
  viewed_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_view_post_time (post_id, viewed_at),
  INDEX ix_view_user_time (user_id, viewed_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 오늘자 조회 샘플
INSERT INTO post_view_log (post_id, user_id)
SELECT @p_review, @u1
WHERE @p_review IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM post_view_log WHERE post_id=@p_review AND user_id=@u1 AND DATE(viewed_at)=CURRENT_DATE());

INSERT INTO post_view_log (post_id, user_id)
SELECT @p_review, @u2
WHERE @p_review IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM post_view_log WHERE post_id=@p_review AND user_id=@u2 AND DATE(viewed_at)=CURRENT_DATE());

INSERT INTO post_view_log (post_id, user_id)
SELECT @p_tip, @u3
WHERE @p_tip IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM post_view_log WHERE post_id=@p_tip AND user_id=@u3 AND DATE(viewed_at)=CURRENT_DATE());

-- ---------- 카운트 자동반영 트리거 ----------
-- 멱등: 먼저 제거 후 생성
DROP TRIGGER IF EXISTS trg_comment_after_ins;
DELIMITER $$
CREATE TRIGGER trg_comment_after_ins
AFTER INSERT ON comment
FOR EACH ROW
BEGIN
  UPDATE post SET comment_count = comment_count + 1 WHERE id = NEW.post_id;
END$$
DELIMITER ;

DROP TRIGGER IF EXISTS trg_comment_after_del;
DELIMITER $$
CREATE TRIGGER trg_comment_after_del
AFTER DELETE ON comment
FOR EACH ROW
BEGIN
  UPDATE post SET comment_count = GREATEST(comment_count - 1, 0) WHERE id = OLD.post_id;
END$$
DELIMITER ;

DROP TRIGGER IF EXISTS trg_reaction_after_ins;
DELIMITER $$
CREATE TRIGGER trg_reaction_after_ins
AFTER INSERT ON reaction
FOR EACH ROW
BEGIN
  IF NEW.target_type = 'POST' THEN
    UPDATE post SET like_count = like_count + 1 WHERE id = NEW.target_id;
  ELSEIF NEW.target_type = 'COMMENT' THEN
    UPDATE comment SET like_count = like_count + 1 WHERE id = NEW.target_id;
  END IF;
END$$
DELIMITER ;

DROP TRIGGER IF EXISTS trg_reaction_after_del;
DELIMITER $$
CREATE TRIGGER trg_reaction_after_del
AFTER DELETE ON reaction
FOR EACH ROW
BEGIN
  IF OLD.target_type = 'POST' THEN
    UPDATE post SET like_count = GREATEST(like_count - 1, 0) WHERE id = OLD.target_id;
  ELSEIF OLD.target_type = 'COMMENT' THEN
    UPDATE comment SET like_count = GREATEST(like_count - 1, 0) WHERE id = OLD.target_id;
  END IF;
END$$
DELIMITER ;

-- 조회수는 배치/애플리케이션에서 집계하는 것을 권장(중복/봇 필터 필요)
-- 필요 시, 매일 post_view_log를 합산해 반영하는 배치 작성

-- ---------- “오늘의 게시글” 뷰(예시) ----------
-- 좋아요*2 + 댓글*3 + 조회수 가중치로 오늘 점수 산정(예시)
DROP VIEW IF EXISTS v_post_hot_today;
CREATE VIEW v_post_hot_today AS
SELECT
  p.id,
  p.category_id,
  p.author_id,
  p.title,
  p.like_count,
  p.comment_count,
  p.view_count,
  (p.like_count*2 + p.comment_count*3 + p.view_count) AS hot_score,
  p.created_at
FROM post p
WHERE DATE(p.created_at) = CURRENT_DATE()
ORDER BY hot_score DESC, p.created_at DESC;

SET FOREIGN_KEY_CHECKS = 1;
