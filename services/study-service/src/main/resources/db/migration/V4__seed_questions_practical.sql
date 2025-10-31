-- V4: 실기(주관식: SHORT/LONG) 샘플 문항
SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

SET @T_1_1_1 = (SELECT id FROM topic WHERE code='1.1.1');

-- SHORT
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url)
VALUES
(@T_1_1_1, 'SHORT','NORMAL',
 '응답시간(Response Time)의 정의를 1문장으로 서술하세요.',
 '응답시간은 요청~첫 응답까지의 시간이다.',
 NULL),
(@T_1_1_1, 'SHORT','NORMAL',
 '가용성(Availability)의 의미를 간단히 설명하세요.',
 '가용성은 서비스 가능한 시간 비율(업타임 비율)이다.',
 NULL);

-- LONG
INSERT INTO question (topic_id, type, difficulty, text, explanation, image_url)
VALUES
(@T_1_1_1, 'LONG','HARD',
 '플랫폼 성능 분석 절차를 단계별로 정리하고, 각 단계에서 수집할 핵심 지표를 예시와 함께 기술하세요.',
 '일반적 절차 예: 목표/범위 확정 → 기준선 수집 → 부하 설계/측정 → 병목 식별 → 개선/재측정. 지표 예: 응답시간, 처리량, 사용률, 가용성 등.',
 NULL),
(@T_1_1_1, 'LONG','NORMAL',
 'DBMS 인덱스 전략이 조회/조인 성능에 미치는 영향을 사례를 들어 설명하세요.',
 '인덱스는 조건절/조인키 선택도에 따라 검색 및 조인 비용을 크게 좌우한다.',
 NULL);

SET FOREIGN_KEY_CHECKS = 1;
