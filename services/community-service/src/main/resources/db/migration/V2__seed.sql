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

SET FOREIGN_KEY_CHECKS = 1;
