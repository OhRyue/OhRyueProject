SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ---------- 카테고리 시드 ----------
INSERT INTO post_category (id, code, name)
SELECT 0, 'ALL', '전체(자동)'
WHERE NOT EXISTS (SELECT 1 FROM post_category WHERE code = 'ALL');

INSERT INTO post_category (id, code, name)
SELECT 1, 'REVIEW', '후기'
WHERE NOT EXISTS (SELECT 1 FROM post_category WHERE code = 'REVIEW');

INSERT INTO post_category (id, code, name)
SELECT 2, 'TIP', '꿀팁'
WHERE NOT EXISTS (SELECT 1 FROM post_category WHERE code = 'TIP');

INSERT INTO post_category (id, code, name)
SELECT 3, 'STUDY', '스터디'
WHERE NOT EXISTS (SELECT 1 FROM post_category WHERE code = 'STUDY');

INSERT INTO post_category (id, code, name)
SELECT 4, 'QNA', '질문'
WHERE NOT EXISTS (SELECT 1 FROM post_category WHERE code = 'QNA');

INSERT INTO post_category (id, code, name)
SELECT 5, 'FREE', '자유'
WHERE NOT EXISTS (SELECT 1 FROM post_category WHERE code = 'FREE');

-- ---------- 게시글 시드 ----------
-- 후기
INSERT INTO post (category_id, author_id, is_anonymous, title, content)
SELECT 1, 'ohryue', 0,
       '정보처리기사 1회 합격 후기',
       '첫 도전에서 합격했습니다. 학습 루프(개념→OX→MCQ→총정리)가 큰 도움이 되었어요. D-Day 목표 잡고 매일 30분 이상 실습한 게 포인트였습니다.'
WHERE NOT EXISTS (
  SELECT 1 FROM post
  WHERE author_id = 'ohryue'
    AND title = '정보처리기사 1회 합격 후기'
);

-- 꿀팁
INSERT INTO post (category_id, author_id, is_anonymous, title, content)
SELECT 2, 'user2', 0,
       '필기 과목 암기 팁',
       '요약 노트는 토픽 코드(1, 1.1, 1.1.1) 기준으로 정리하고, 헷갈리는 포인트는 태그로 묶어 재복습하세요.'
WHERE NOT EXISTS (
  SELECT 1 FROM post
  WHERE author_id = 'user2'
    AND title = '필기 과목 암기 팁'
);

-- 스터디
INSERT INTO post (category_id, author_id, is_anonymous, title, content)
SELECT 3, 'guest', 1,
       '주 2회 온라인 스터디 모집(초보 환영)',
       '디스코드로 매주 화/목 밤 9시. 오늘 학습 루프 인증 + 어려웠던 문제 토론합니다. 익명 참여 가능.'
WHERE NOT EXISTS (
  SELECT 1 FROM post
  WHERE author_id = 'guest'
    AND title = '주 2회 온라인 스터디 모집(초보 환영)'
);

-- 질문
INSERT INTO post (category_id, author_id, is_anonymous, title, content)
SELECT 4, 'ohryue', 0,
       '실기 준비는 언제 시작하는 게 좋을까요?',
       '필기 합격 후 바로 실기 들어갈지, 쉬었다가 갈지 고민입니다. 선배님들 의견 부탁드려요.'
WHERE NOT EXISTS (
  SELECT 1 FROM post
  WHERE author_id = 'ohryue'
    AND title = '실기 준비는 언제 시작하는 게 좋을까요?'
);

-- 자유
INSERT INTO post (category_id, author_id, is_anonymous, title, content)
SELECT 5, 'user2', 0,
       '오늘의 목표 달성!',
       '마이크로 1세트 + MCQ 10문제 클리어했습니다. 모두 화이팅!'
WHERE NOT EXISTS (
  SELECT 1 FROM post
  WHERE author_id = 'user2'
    AND title = '오늘의 목표 달성!'
);

-- ---------- 댓글 시드 ----------
INSERT INTO comment (post_id, author_id, is_anonymous, content)
SELECT p.id, 'user2', 0,
       '축하드립니다! 비결 공유 감사해요 🙌'
FROM post p
WHERE p.author_id = 'ohryue'
  AND p.title = '정보처리기사 1회 합격 후기'
  AND NOT EXISTS (
    SELECT 1 FROM comment c
    WHERE c.post_id = p.id
      AND c.author_id = 'user2'
      AND c.content = '축하드립니다! 비결 공유 감사해요 🙌'
  );

INSERT INTO comment (post_id, author_id, is_anonymous, content)
SELECT p.id, 'guest', 1,
       '필기 끝나고 바로 실기 감 추천드립니다. 흐름 유지가 좋아요.'
FROM post p
WHERE p.author_id = 'ohryue'
  AND p.title = '실기 준비는 언제 시작하는 게 좋을까요?'
  AND NOT EXISTS (
    SELECT 1 FROM comment c
    WHERE c.post_id = p.id
      AND c.author_id = 'guest'
      AND c.content = '필기 끝나고 바로 실기 감 추천드립니다. 흐름 유지가 좋아요.'
  );

-- ---------- 좋아요 시드 ----------
INSERT INTO reaction (target_type, target_id, user_id)
SELECT 'POST', p.id, 'user2'
FROM post p
WHERE p.author_id = 'ohryue'
  AND p.title = '정보처리기사 1회 합격 후기'
  AND NOT EXISTS (
    SELECT 1 FROM reaction r
    WHERE r.target_type = 'POST'
      AND r.target_id = p.id
      AND r.user_id = 'user2'
  );

INSERT INTO reaction (target_type, target_id, user_id)
SELECT 'POST', p.id, 'ohryue'
FROM post p
WHERE p.author_id = 'user2'
  AND p.title = '필기 과목 암기 팁'
  AND NOT EXISTS (
    SELECT 1 FROM reaction r
    WHERE r.target_type = 'POST'
      AND r.target_id = p.id
      AND r.user_id = 'ohryue'
  );

INSERT INTO reaction (target_type, target_id, user_id)
SELECT 'COMMENT', c.id, 'ohryue'
FROM post p
JOIN comment c ON c.post_id = p.id
WHERE p.author_id = 'ohryue'
  AND p.title = '정보처리기사 1회 합격 후기'
  AND c.author_id = 'user2'
  AND c.content = '축하드립니다! 비결 공유 감사해요 🙌'
  AND NOT EXISTS (
    SELECT 1 FROM reaction r
    WHERE r.target_type = 'COMMENT'
      AND r.target_id = c.id
      AND r.user_id = 'ohryue'
  );

-- ---------- 조회 로그 시드 ----------
INSERT INTO post_view_log (post_id, user_id)
SELECT p.id, 'ohryue'
FROM post p
WHERE p.author_id = 'ohryue'
  AND p.title = '정보처리기사 1회 합격 후기'
  AND NOT EXISTS (
    SELECT 1 FROM post_view_log v
    WHERE v.post_id = p.id
      AND v.user_id = 'ohryue'
      AND DATE(v.viewed_at) = CURRENT_DATE()
  );

INSERT INTO post_view_log (post_id, user_id)
SELECT p.id, 'user2'
FROM post p
WHERE p.author_id = 'ohryue'
  AND p.title = '정보처리기사 1회 합격 후기'
  AND NOT EXISTS (
    SELECT 1 FROM post_view_log v
    WHERE v.post_id = p.id
      AND v.user_id = 'user2'
      AND DATE(v.viewed_at) = CURRENT_DATE()
  );

INSERT INTO post_view_log (post_id, user_id)
SELECT p.id, 'guest'
FROM post p
WHERE p.author_id = 'user2'
  AND p.title = '필기 과목 암기 팁'
  AND NOT EXISTS (
    SELECT 1 FROM post_view_log v
    WHERE v.post_id = p.id
      AND v.user_id = 'guest'
      AND DATE(v.viewed_at) = CURRENT_DATE()
  );

-- ---------- 신고/차단 샘플 ----------
INSERT INTO post_report (target_type, target_id, reporter_id, reason)
SELECT 'POST', p.id, 'admin', '스팸 여부 확인 요청'
FROM post p
WHERE p.author_id = 'guest'
  AND p.title = '주 2회 온라인 스터디 모집(초보 환영)'
  AND NOT EXISTS (
    SELECT 1 FROM post_report r
    WHERE r.target_type = 'POST'
      AND r.target_id = p.id
      AND r.reporter_id = 'admin'
  );

INSERT INTO user_block (user_id, blocked_user_id)
SELECT 'ohryue', 'guest'
WHERE NOT EXISTS (
  SELECT 1 FROM user_block b
  WHERE b.user_id = 'ohryue'
    AND b.blocked_user_id = 'guest'
);

SET FOREIGN_KEY_CHECKS = 1;
