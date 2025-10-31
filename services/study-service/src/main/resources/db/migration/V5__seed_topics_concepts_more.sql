-- V5: 추가 토픽(1.1.2, 1.1.3)
SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ============ 1.1.2 요구사항 확인 / 1.1.3 분석 모델 확인 ============

INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT NULL, '1', '소프트웨어 설계', 'WRITTEN'
WHERE NOT EXISTS (SELECT 1 FROM topic WHERE code='1');

INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT t1.id, '1.1', '요구사항 확인', 'WRITTEN'
FROM topic t1 WHERE t1.code='1'
ON DUPLICATE KEY UPDATE title=VALUES(title);

-- 1.1.2 / 1.1.3
INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT t11.id, '1.1.2', '요구사항 확인', 'WRITTEN'
FROM topic t11 WHERE t11.code='1.1'
ON DUPLICATE KEY UPDATE title=VALUES(title);

INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT t11.id, '1.1.3', '분석 모델 확인', 'WRITTEN'
FROM topic t11 WHERE t11.code='1.1'
ON DUPLICATE KEY UPDATE title=VALUES(title);

SET @T_1_1_2 = (SELECT id FROM topic WHERE code='1.1.2');
SET @T_1_1_3 = (SELECT id FROM topic WHERE code='1.1.3');

-- 1.1.2 개념(요구분석 기법 / UML / 애자일) — 표/이미지 다중 포함
INSERT INTO concept (topic_id, content, blocks_json)
VALUES
(
  @T_1_1_2,
  NULL,
  JSON_OBJECT(
    'sections', JSON_ARRAY(
      JSON_OBJECT(
        'orderNo', 1, 'subCode', '1.1.2.1',
        'title', '요구분석 기법', 'importance', 3,
        'blocks', JSON_ARRAY(
          JSON_OBJECT('type','heading','text','(1) 요구수집 기법'),
          JSON_OBJECT('type','list','items', JSON_ARRAY(
            '인터뷰(Stakeholder 중심 질적정보 수집)',
            '설문(다수의 정량적 의견 수렴)',
            '워크숍/브레인스토밍(아이디어 확장)'
          )),
          JSON_OBJECT('type','heading','text','(2) 분석/명세 기법'),
          JSON_OBJECT('type','table','caption','요구 명세 기법 비교',
            'headers', JSON_ARRAY('기법','설명','장점','주의점'),
            'rows', JSON_ARRAY(
              JSON_ARRAY('자연어 명세','문장 기반 명세','친숙/빠름','모호성 위험'),
              JSON_ARRAY('정형 명세','수학적/형식언어 기반','명확/검증 용이','난이도 높음'),
              JSON_ARRAY('유스케이스','행위 중심 시나리오','이해 용이','비기능 요구 반영 한계')
            )
          ),
          JSON_OBJECT('type','image','url','https://cdn.example.com/req/elicitation.png',
            'alt','요구수집 방법 개요','caption','요구수집 방법 개요'),
          JSON_OBJECT('type','image','url','https://cdn.example.com/req/spec_comparison.png',
            'alt','명세 기법 비교','caption','명세 기법 비교 다이어그램')
        )
      ),
      JSON_OBJECT(
        'orderNo', 2, 'subCode', '1.1.2.2',
        'title', 'UML', 'importance', 3,
        'blocks', JSON_ARRAY(
          JSON_OBJECT('type','paragraph','text','UML은 표준 시각적 모델링 언어로 구조/행위를 표현한다.'),
          JSON_OBJECT('type','table','caption','주요 UML 다이어그램',
            'headers', JSON_ARRAY('분류','다이어그램','핵심 목적'),
            'rows', JSON_ARRAY(
              JSON_ARRAY('구조','클래스','정적 구조/관계'),
              JSON_ARRAY('구조','컴포넌트','배치 단위/인터페이스'),
              JSON_ARRAY('행위','유스케이스','액터-시스템 상호작용'),
              JSON_ARRAY('행위','시퀀스','메시지 시간 순서'),
              JSON_ARRAY('행위','활동','절차 흐름')
            )
          ),
          JSON_OBJECT('type','image','url','https://cdn.example.com/uml/uc_sample.png',
            'alt','유스케이스 예시','caption','유스케이스 다이어그램 예시'),
          JSON_OBJECT('type','image','url','https://cdn.example.com/uml/seq_sample.png',
            'alt','시퀀스 예시','caption','시퀀스 다이어그램 예시')
        )
      ),
      JSON_OBJECT(
        'orderNo', 3, 'subCode', '1.1.2.3',
        'title', '애자일(Agile)', 'importance', 3,
        'blocks', JSON_ARRAY(
          JSON_OBJECT('type','heading','text','(1) 핵심 가치'),
          JSON_OBJECT('type','list','items', JSON_ARRAY(
            '개인/상호작용 > 프로세스/도구',
            '작동하는 소프트웨어 > 포괄적 문서',
            '고객 협력 > 계약 협상',
            '변경 대응 > 계획 준수'
          )),
          JSON_OBJECT('type','heading','text','(2) 스크럼 핵심'),
          JSON_OBJECT('type','table','caption','스크럼 이벤트/산출물',
            'headers', JSON_ARRAY('구분','내용','주기'),
            'rows', JSON_ARRAY(
              JSON_ARRAY('이벤트','스프린트 기획/데일리/리뷰/회고','1~4주'),
              JSON_ARRAY('산출물','백로그/증분','지속 갱신')
            )
          ),
          JSON_OBJECT('type','image','url','https://cdn.example.com/agile/scrum_cycle.png',
            'alt','스크럼 사이클','caption','스크럼 사이클')
        )
      )
    )
  )
)
ON DUPLICATE KEY UPDATE blocks_json=VALUES(blocks_json);

-- 1.1.3 개념(모델링 기법 / 분석 자동화 도구 / 요구사항 관리 도구)
INSERT INTO concept (topic_id, content, blocks_json)
VALUES
(
  @T_1_1_3,
  NULL,
  JSON_OBJECT(
    'sections', JSON_ARRAY(
      JSON_OBJECT(
        'orderNo', 1, 'subCode', '1.1.3.1',
        'title', '모델링 기법', 'importance', 2,
        'blocks', JSON_ARRAY(
          JSON_OBJECT('type','paragraph','text','문제 영역을 추상화해 의사소통/분석을 돕는 기법.'),
          JSON_OBJECT('type','table','caption','모델링 관점',
            'headers', JSON_ARRAY('관점','설명','예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY('기능','무엇을 하는가','DFD, 유스케이스'),
              JSON_ARRAY('정적 구조','구성/관계','ERD, 클래스'),
              JSON_ARRAY('동작','어떻게 동작하는가','상태/시퀀스/활동')
            )
          ),
          JSON_OBJECT('type','image','url','https://cdn.example.com/modeling/erd.png',
            'alt','ERD 샘플','caption','ERD 예시')
        )
      ),
      JSON_OBJECT(
        'orderNo', 2, 'subCode', '1.1.3.2',
        'title', '분석 자동화 도구', 'importance', 2,
        'blocks', JSON_ARRAY(
          JSON_OBJECT('type','list','items', JSON_ARRAY(
            '정적 분석(코드 규칙 위반 탐지)',
            '성능 분석(프로파일링/부하테스트 도구)',
            '모델 검증(일관성/제약 확인)'
          )),
          JSON_OBJECT('type','image','url','https://cdn.example.com/tools/static_analysis.png',
            'alt','정적 분석 개념도','caption','정적 분석 개념도')
        )
      ),
      JSON_OBJECT(
        'orderNo', 3, 'subCode', '1.1.3.3',
        'title', '요구사항 관리 도구', 'importance', 2,
        'blocks', JSON_ARRAY(
          JSON_OBJECT('type','table','caption','요구 관리 주요 기능',
            'headers', JSON_ARRAY('기능','설명'),
            'rows', JSON_ARRAY(
              JSON_ARRAY('추적성','요구~설계~테스트 간 링크'),
              JSON_ARRAY('버전관리','요구 변경 이력 관리'),
              JSON_ARRAY('우선순위','가치/노력 기반 정렬')
            )
          ),
          JSON_OBJECT('type','image','url','https://cdn.example.com/req/traceability.png',
            'alt','추적성','caption','요구 추적성 개요')
        )
      )
    )
  )
)
ON DUPLICATE KEY UPDATE blocks_json=VALUES(blocks_json);

-- ============ 2 소프트웨어 개발(루트/하위 골격) ============

-- 2 소프트웨어 개발 (루트)
INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT NULL, '2', '소프트웨어 개발', 'WRITTEN'
WHERE NOT EXISTS (SELECT 1 FROM topic WHERE code='2');

-- 2.1 개발 방법론 / 2.2 데이터베이스 / 2.3 프로그래밍 기초 (예시 골격)
INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT t2.id, '2.1', '개발 방법론', 'WRITTEN'
FROM topic t2 WHERE t2.code='2'
ON DUPLICATE KEY UPDATE title=VALUES(title);

INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT t2.id, '2.2', '데이터베이스', 'WRITTEN'
FROM topic t2 WHERE t2.code='2'
ON DUPLICATE KEY UPDATE title=VALUES(title);

INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT t2.id, '2.3', '프로그래밍 기초', 'WRITTEN'
FROM topic t2 WHERE t2.code='2'
ON DUPLICATE KEY UPDATE title=VALUES(title);

-- 2.2.1 정규화 (미시 토픽 예시)
INSERT INTO topic (parent_id, code, title, exam_mode)
SELECT t22.id, '2.2.1', '정규화', 'WRITTEN'
FROM topic t22 WHERE t22.code='2.2'
ON DUPLICATE KEY UPDATE title=VALUES(title);

SET @T_2_2_1 = (SELECT id FROM topic WHERE code='2.2.1');

-- 2.2.1 개념(간단 예시)
INSERT INTO concept (topic_id, content, blocks_json)
VALUES
(
  @T_2_2_1,
  NULL,
  JSON_OBJECT(
    'sections', JSON_ARRAY(
      JSON_OBJECT(
        'orderNo', 1, 'subCode', '2.2.1.1',
        'title', '정규화 개요', 'importance', 2,
        'blocks', JSON_ARRAY(
          JSON_OBJECT('type','paragraph','text','정규화는 이상현상을 줄이고 데이터 무결성을 높이기 위한 설계 절차.'),
          JSON_OBJECT('type','table','caption','정규형 요약',
            'headers', JSON_ARRAY('정규형','핵심 조건','설명'),
            'rows', JSON_ARRAY(
              JSON_ARRAY('1NF','원자성','반복/다중값 제거'),
              JSON_ARRAY('2NF','부분함수 종속 제거','기본키 전체 종속'),
              JSON_ARRAY('3NF','이행함수 종속 제거','키가 아닌 속성 간 종속 제거')
            )
          ),
          JSON_OBJECT('type','image','url','https://cdn.example.com/db/normalization.png',
            'alt','정규화 개념도','caption','정규화 개념도')
        )
      )
    )
  )
)
ON DUPLICATE KEY UPDATE blocks_json=VALUES(blocks_json);

SET FOREIGN_KEY_CHECKS = 1;
