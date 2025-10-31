UPDATE concept AS c
JOIN topic   AS t ON t.id = c.topic_id
SET c.blocks_json = JSON_OBJECT(
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
          '동일 플랫폼 내에서는 상호 호환이 가능하도록 만들어진 결합체를 의미한다.',
          '공급자와 수요자 등 복수 그룹이 참여하여 각 그룹이 얻고자 하는 가치를 공정한 거래를 통해 교환할 수 있도록 구축된 환경이다.'
        )),
        JSON_OBJECT('type','heading','text','(2) 플랫폼 성능 특성 분석'),
        JSON_OBJECT('type','paragraph','text','플랫폼 성능 분석을 통해 사용자의 서비스 이용 시 속도의 적정성을 알 수 있다.'),
        JSON_OBJECT('type','heading','text','(3) 플랫폼 성능 특성 측정 항목'),
        JSON_OBJECT('type','paragraph','text','측정 항목에는 경과시간, 사용률, 응답시간, 가용성이 있다.'),
        JSON_OBJECT(
          'type','table',
          'caption','플랫폼 성능 특성 측정 항목',
          'headers', JSON_ARRAY('항목','설명'),
          'rows', JSON_ARRAY(
            JSON_ARRAY('경과시간','작업 시작~종료까지의 총 소요시간'),
            JSON_ARRAY('사용률','자원 사용 비율'),
            JSON_ARRAY('응답시간','요청~첫 응답까지의 시간'),
            JSON_ARRAY('가용성','서비스 가능한 시간 비율')
          )
        ),
        JSON_OBJECT(
          'type','image',
          'url','https://cdn.example.com/concepts/platform_metrics.png',
          'alt','플랫폼 성능 지표 개념도',
          'caption','성능 지표 개념도'
        )
      )
    ),
    JSON_OBJECT(
      'orderNo', 2,
      'subCode', '1.1.1.2',
      'title', '운영체제 분석',
      'importance', 1,
      'blocks', JSON_ARRAY(
        JSON_OBJECT('type','paragraph','text','...운영체제 분석 내용...')
      )
    )
  )
)
WHERE t.code = '1.1.1'
  AND t.exam_mode = 'WRITTEN';
