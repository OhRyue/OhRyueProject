-- V2: 토픽/개념 시드
SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- 최상위: 1 소프트웨어 설계 (필기)
INSERT INTO topic (parent_id, code, title, exam_mode) VALUES
  (NULL, '1', '소프트웨어 설계', 'WRITTEN')
ON DUPLICATE KEY UPDATE title=VALUES(title);

-- 하위: 1.1 요구사항 확인
INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT t1.id, '1.1', '요구사항 확인', 'WRITTEN' FROM topic t1 WHERE t1.code='1'
ON DUPLICATE KEY UPDATE title=VALUES(title);

-- 미시 단위: 1.1.1 현행 시스템 분석 / 1.1.2 요구사항 확인 / 1.1.3 분석 모델 확인
INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT t11.id, '1.1.1', '현행 시스템 분석', 'WRITTEN' FROM topic t11 WHERE t11.code='1.1'
ON DUPLICATE KEY UPDATE title=VALUES(title);

INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT t11.id, '1.1.2', '요구사항 확인', 'WRITTEN' FROM topic t11 WHERE t11.code='1.1'
ON DUPLICATE KEY UPDATE title=VALUES(title);

INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT t11.id, '1.1.3', '분석 모델 확인', 'WRITTEN' FROM topic t11 WHERE t11.code='1.1'
ON DUPLICATE KEY UPDATE title=VALUES(title);

-- ID 확보
SET @T_1_1_1 = (SELECT id FROM topic WHERE code='1.1.1');
SET @T_1_1_2 = (SELECT id FROM topic WHERE code='1.1.2');
SET @T_1_1_3 = (SELECT id FROM topic WHERE code='1.1.3');

-- 개념(리치) JSON: 1.1.1 예시(플랫폼 기능 분석 / OS / 네트워크 / DBMS)
INSERT INTO concept (topic_id, content, blocks_json)
VALUES
(
  @T_1_1_1,
  NULL,
  JSON_OBJECT(
    'sections', JSON_ARRAY(
      JSON_OBJECT(
        'orderNo', 1,
        'subCode', '1.1.1.1',
        'title', '플랫폼 기능 분석',
        'importance', 2,
        'blocks', JSON_ARRAY(
          JSON_OBJECT('type','heading','text','(1) 플랫폼의 개념'),
          JSON_OBJECT('type','list','items', JSON_ARRAY(
            '플랫폼은 애플리케이션을 구동시키는데 필요한 소프트웨어 환경이다.',
            '서비스 요구사항에 적합한 플랫폼 선택이 중요하다.'
          )),
          JSON_OBJECT('type','heading','text','(2) 플랫폼 성능 특성 분석'),
          JSON_OBJECT('type','paragraph','text','성능 분석을 통해 속도/자원 활용/가용성을 평가한다.'),
          JSON_OBJECT('type','heading','text','(3) 플랫폼 성능 특성 측정 항목'),
          JSON_OBJECT('type','table','caption','플랫폼 성능 특성 측정 항목',
            'headers', JSON_ARRAY('항목','설명'),
            'rows', JSON_ARRAY(
              JSON_ARRAY('경과시간','작업 시작~종료까지의 총 소요시간'),
              JSON_ARRAY('사용률','자원 사용 비율'),
              JSON_ARRAY('응답시간','요청~첫 응답까지의 시간'),
              JSON_ARRAY('가용성','서비스 가능한 시간 비율')
            )
          ),
          JSON_OBJECT('type','image','url','https://cdn.example.com/concepts/platform_metrics.png',
            'alt','플랫폼 성능 지표 개념도','caption','성능 지표 개념도')
        )
      ),
      JSON_OBJECT(
        'orderNo', 2,
        'subCode', '1.1.1.2',
        'title', '운영체제 분석',
        'importance', 1,
        'blocks', JSON_ARRAY(
          JSON_OBJECT('type','paragraph','text','운영체제는 자원 관리 및 스케줄링 정책이 중요하다.')
        )
      ),
      JSON_OBJECT(
        'orderNo', 3,
        'subCode', '1.1.1.3',
        'title', '네트워크 분석',
        'importance', 1,
        'blocks', JSON_ARRAY(
          JSON_OBJECT('type','paragraph','text','대역폭/지연/패킷손실 등 품질지표를 고려한다.')
        )
      ),
      JSON_OBJECT(
        'orderNo', 4,
        'subCode', '1.1.1.4',
        'title', 'DBMS 분석',
        'importance', 2,
        'blocks', JSON_ARRAY(
          JSON_OBJECT('type','paragraph','text','트랜잭션, 동시성 제어, 인덱스 전략을 검토한다.')
        )
      )
    )
  )
)
ON DUPLICATE KEY UPDATE blocks_json=VALUES(blocks_json);

-- 1.1.2/1.1.3은 일단 빈 섹션으로 둠(필요 시 채워나가면 됨)
INSERT INTO concept (topic_id, content, blocks_json)
VALUES
(@T_1_1_2, NULL, JSON_OBJECT('sections', JSON_ARRAY())),
(@T_1_1_3, NULL, JSON_OBJECT('sections', JSON_ARRAY()))
ON DUPLICATE KEY UPDATE blocks_json=VALUES(blocks_json);

SET FOREIGN_KEY_CHECKS = 1;
