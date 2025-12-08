-- =========================================
-- Topic ID 상수 (5과목 정보시스템 구축 관리 – 5.x.x)
-- =========================================
SET @tp_51101 := 15101; -- 5.1.1 정보시스템 구축 관리 개요
SET @tp_51102 := 15102; -- 5.1.2 IT 프로젝트 관리/범위·일정·비용
SET @tp_52101 := 15201; -- 5.2.1 인프라/시스템 아키텍처 설계
SET @tp_52102 := 15202; -- 5.2.2 네트워크 구축/설정
SET @tp_52103 := 15203; -- 5.2.3 하드웨어 구축 관리
SET @tp_52104 := 15204; -- 5.2.4 가상화/클라우드 인프라
SET @tp_53101 := 15301; -- 5.3.1 소프트웨어 설치·배포 및 형상 관리
SET @tp_53102 := 15302; -- 5.3.2 변경관리/배포 자동화·운영 전환
SET @tp_54101 := 15401; -- 5.4.1 시스템 보안 설계
SET @tp_54102 := 15402; -- 5.4.2 시스템 보안 구현


/***********************************************************
 * 5.1.1 정보시스템 구축 관리 개요  (MICRO용 OX 6문항)
 ***********************************************************/

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_51101, 'WRITTEN', 'OX', 'EASY',
       '정보시스템 구축 관리는 품질·비용·일정 제약 안에서 요구된 기능을 제공하기 위해 전체 생명주기를 계획하고 통제하는 활동이다.',
       'O',
       '구축 관리는 요구된 품질을 확보하면서 예산과 일정을 지키도록 전체 생명주기를 관리하는 것을 의미합니다.',
       'seed:5.1.1:intro-triple-constraint:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_51101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정보시스템 구축 관리는 품질·비용·일정 제약 안에서 요구된 기능을 제공하기 위해 전체 생명주기를 계획하고 통제하는 활동이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_51101, 'WRITTEN', 'OX', 'EASY',
       '정보시스템 구축 프로젝트는 요구사항 분석, 설계, 구현, 시험, 전환, 운영 등 단계별 산출물을 체계적으로 관리해야 한다.',
       'O',
       '각 단계별 산출물을 정의하고 관리해야 이후 품질 검토와 변경 관리가 수월해집니다.',
       'seed:5.1.1:lifecycle-artifacts:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_51101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정보시스템 구축 프로젝트는 요구사항 분석, 설계, 구현, 시험, 전환, 운영 등 단계별 산출물을 체계적으로 관리해야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_51101, 'WRITTEN', 'OX', 'NORMAL',
       '정보시스템 구축에서 품질은 기능만 정상 동작하면 되기 때문에, 성능·보안·장애 복구 같은 비기능 특성은 별도로 관리하지 않아도 된다.',
       'X',
       '성능·보안·가용성·복구 시간 등 비기능 요구도 품질의 중요한 요소이므로 초기부터 함께 관리해야 합니다.',
       'seed:5.1.1:nfr-quality:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_51101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정보시스템 구축에서 품질은 기능만 정상 동작하면 되기 때문에, 성능·보안·장애 복구 같은 비기능 특성은 별도로 관리하지 않아도 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_51101, 'WRITTEN', 'OX', 'NORMAL',
       '정보시스템 구축 관리에서 이해관계자 관리는 중요하지 않으며, 개발자와 운영자만 고려하면 충분하다.',
       'X',
       '경영진, 사용자, 운영자, 협력사 등 다양한 이해관계자를 고려해야 요구 충돌과 갈등을 줄일 수 있습니다.',
       'seed:5.1.1:stakeholder:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_51101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정보시스템 구축 관리에서 이해관계자 관리는 중요하지 않으며, 개발자와 운영자만 고려하면 충분하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_51101, 'WRITTEN', 'OX', 'NORMAL',
       '정보시스템 구축 관리에서는 위험을 사전에 식별하고 대응 계획을 수립하는 위험 관리 활동이 포함될 수 있다.',
       'O',
       '일정 지연·비용 초과·품질 저하 등 위험을 미리 파악하고 대응 전략을 세우는 것이 중요합니다.',
       'seed:5.1.1:risk-mgmt:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_51101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정보시스템 구축 관리에서는 위험을 사전에 식별하고 대응 계획을 수립하는 위험 관리 활동이 포함될 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_51101, 'WRITTEN', 'OX', 'EASY',
       '정보시스템 구축 관리에서도 변경 요청을 통제하고 영향 범위를 검토하는 변경 관리 절차가 필요하다.',
       'O',
       '무분별한 요구 변경은 품질과 일정을 해치므로, 변경 통제 절차를 통해 승인·추적을 해야 합니다.',
       'seed:5.1.1:change-control:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_51101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정보시스템 구축 관리에서도 변경 요청을 통제하고 영향 범위를 검토하는 변경 관리 절차가 필요하다.%'
);


/***********************************************************
 * 5.1.2 IT 프로젝트 관리/범위·일정·비용  (MICRO용 OX 6문항)
 ***********************************************************/

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_51102, 'WRITTEN', 'OX', 'EASY',
       '프로젝트 범위 관리는 해야 할 일과 하지 않을 일을 명확히 정의해, 범위 증가를 통제하는 것이 목적이다.',
       'O',
       '범위를 명확히 해야 무분별한 요구 추가를 막고, 일정과 예산을 안정적으로 관리할 수 있습니다.',
       'seed:5.1.2:scope-mgmt:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_51102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '프로젝트 범위 관리는 해야 할 일과 하지 않을 일을 명확히 정의해, 범위 증가를 통제하는 것이 목적이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_51102, 'WRITTEN', 'OX', 'EASY',
       'WBS는 프로젝트 작업을 계층 구조로 분해한 것으로, 일정 계획과 자원 배분의 기반이 된다.',
       'O',
       'WBS를 통해 작업 단위를 명확히 나누면 일정 산정과 책임 분담이 쉬워집니다.',
       'seed:5.1.2:wbs:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_51102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'WBS는 프로젝트 작업을 계층 구조로 분해한 것으로, 일정 계획과 자원 배분의 기반이 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_51102, 'WRITTEN', 'OX', 'NORMAL',
       '주 공정법(CPM)에서 여유 시간이 0인 경로를 크리티컬 패스라고 하며, 이 경로상의 작업 지연은 전체 일정에 영향을 미친다.',
       'O',
       '크리티컬 패스 상 작업은 지연 시 전체 완료 일정이 지연되므로, 우선 관리해야 합니다.',
       'seed:5.1.2:cpm-critical:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_51102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '주 공정법(CPM)에서 여유 시간이 0인 경로를 크리티컬 패스라고 하며, 이 경로상의 작업 지연은 전체 일정에 영향을 미친다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_51102, 'WRITTEN', 'OX', 'NORMAL',
       '프로젝트 비용 관리는 실제 비용만 기록하면 되며, 계획 대비 성과를 비교하는 일은 크게 중요하지 않다.',
       'X',
       '계획 대비 실제 비용과 진척을 비교해야 예산 초과를 조기에 발견하고 대응할 수 있습니다.',
       'seed:5.1.2:cost-control:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_51102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '프로젝트 비용 관리는 실제 비용만 기록하면 되며, 계획 대비 성과를 비교하는 일은 크게 중요하지 않다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_51102, 'WRITTEN', 'OX', 'NORMAL',
       '프로젝트 일정 단축을 위해 인력을 무조건 많이 투입하는 것은 항상 효과적이며, 비용과 품질에는 영향이 없다.',
       'X',
       '무리한 인력 투입은 커뮤니케이션 비용 증가와 품질 저하를 유발할 수 있어, 적정 인원과 공정 재조정이 필요합니다.',
       'seed:5.1.2:brooks-law:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_51102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '프로젝트 일정 단축을 위해 인력을 무조건 많이 투입하는 것은 항상 효과적이며, 비용과 품질에는 영향이 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_51102, 'WRITTEN', 'OX', 'EASY',
       '프로젝트 보고서는 진척 상황, 위험, 이슈, 변경 사항 등을 정리하여 이해관계자와 공유하는 문서이다.',
       'O',
       '정기적인 보고를 통해 프로젝트 투명성을 높이고, 의사결정을 지원합니다.',
       'seed:5.1.2:status-report:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_51102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '프로젝트 보고서는 진척 상황, 위험, 이슈, 변경 사항 등을 정리하여 이해관계자와 공유하는 문서이다.%'
);


/***********************************************************
 * 5.2.1 인프라/시스템 아키텍처 설계  (MICRO용 OX 6문항)
 ***********************************************************/

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52101, 'WRITTEN', 'OX', 'EASY',
       '시스템 아키텍처 설계에서는 서버, 스토리지, 네트워크, 미들웨어 등 주요 구성 요소와 이들 간 연결 구조를 정의한다.',
       'O',
       '전체 구조를 설계해 성능·확장성·가용성을 충족시키는 것이 중요합니다.',
       'seed:5.2.1:infra-arch:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '시스템 아키텍처 설계에서는 서버, 스토리지, 네트워크, 미들웨어 등 주요 구성 요소와 이들 간 연결 구조를 정의한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52101, 'WRITTEN', 'OX', 'EASY',
       '이중화 구성은 단일 장애 지점을 줄이기 위해 주요 구성 요소를 복수 개로 구성하는 방법이다.',
       'O',
       '서버·네트워크·스토리지 이중화를 통해 장애 시 서비스 중단 위험을 줄일 수 있습니다.',
       'seed:5.2.1:redundancy:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '이중화 구성은 단일 장애 지점을 줄이기 위해 주요 구성 요소를 복수 개로 구성하는 방법이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52101, 'WRITTEN', 'OX', 'NORMAL',
       '확장성이 좋은 아키텍처는 초기 용량을 크게 잡아 두는 것만으로 자동으로 확보된다.',
       'X',
       '수평 확장, 부하 분산 구조 등 확장 전략을 설계해야 사용량 증가 시 유연하게 대응할 수 있습니다.',
       'seed:5.2.1:scalability:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '확장성이 좋은 아키텍처는 초기 용량을 크게 잡아 두는 것만으로 자동으로 확보된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52101, 'WRITTEN', 'OX', 'NORMAL',
       '아키텍처 설계 시 성능 요구는 중요하지만, 보안 요구는 운영 단계에서 별도로 추가하면 되므로 설계 단계에서 고려하지 않아도 된다.',
       'X',
       '보안 요구는 설계 단계에서부터 반영해야 나중에 구조 변경 없이 안전한 시스템을 만들 수 있습니다.',
       'seed:5.2.1:security-in-design:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '아키텍처 설계 시 성능 요구는 중요하지만, 보안 요구는 운영 단계에서 별도로 추가하면 되므로 설계 단계에서 고려하지 않아도 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52101, 'WRITTEN', 'OX', 'NORMAL',
       '고가용성 구성을 위해 장애 감지와 자동 전환 기능이 포함된 클러스터링 소프트웨어를 사용할 수 있다.',
       'O',
       '클러스터링을 통해 장애 노드를 자동으로 감지하고 대기 노드로 서비스 전환이 가능합니다.',
       'seed:5.2.1:ha-cluster:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '고가용성 구성을 위해 장애 감지와 자동 전환 기능이 포함된 클러스터링 소프트웨어를 사용할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52101, 'WRITTEN', 'OX', 'EASY',
       '시스템 아키텍처 설계 시에는 향후 증설과 유지보수를 고려하여 모듈화된 구조를 설계하는 것이 바람직하다.',
       'O',
       '모듈화된 구조는 부분 교체·확장이 쉬워 장기적인 유지보수 비용을 줄여 줍니다.',
       'seed:5.2.1:modular-arch:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '시스템 아키텍처 설계 시에는 향후 증설과 유지보수를 고려하여 모듈화된 구조를 설계하는 것이 바람직하다.%'
);


/***********************************************************
 * 5.2.2 네트워크 구축/설정  (MICRO용 OX 6문항)
 ***********************************************************/

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52102, 'WRITTEN', 'OX', 'EASY',
       '네트워크 설계 시에는 대역폭, 지연 시간, 패킷 손실률 등 성능 요구를 고려하여 회선과 장비를 선정해야 한다.',
       'O',
       '업무 특성에 따라 필요한 네트워크 품질 수준을 충족하도록 설계해야 합니다.',
       'seed:5.2.2:network-qos:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '네트워크 설계 시에는 대역폭, 지연 시간, 패킷 손실률 등 성능 요구를 고려하여 회선과 장비를 선정해야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52102, 'WRITTEN', 'OX', 'EASY',
       'VLAN을 사용하면 하나의 물리 스위치를 여러 개의 논리적 네트워크로 분할해 브로드캐스트 범위를 나눌 수 있다.',
       'O',
       'VLAN은 네트워크 분리를 통해 보안과 트래픽 관리에 도움을 줍니다.',
       'seed:5.2.2:vlan:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'VLAN을 사용하면 하나의 물리 스위치를 여러 개의 논리적 네트워크로 분할해 브로드캐스트 범위를 나눌 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52102, 'WRITTEN', 'OX', 'NORMAL',
       '방화벽은 모든 트래픽을 무조건 허용한 뒤 이상 징후만 탐지하는 장비이므로, 별도의 정책 설정이 필요하지 않다.',
       'X',
       '방화벽은 허용·차단 정책을 설정해 특정 포트·IP·프로토콜을 제어하는 보안 장비입니다.',
       'seed:5.2.2:firewall-policy:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '방화벽은 모든 트래픽을 무조건 허용한 뒤 이상 징후만 탐지하는 장비이므로, 별도의 정책 설정이 필요하지 않다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52102, 'WRITTEN', 'OX', 'NORMAL',
       '로드 밸런서는 여러 서버에 트래픽을 분산해 성능과 가용성을 높이는 장비로, 헬스 체크 기능을 통해 장애 서버를 우회할 수 있다.',
       'O',
       '헬스 체크 결과에 따라 정상 서버로만 트래픽을 보내도록 설정할 수 있습니다.',
       'seed:5.2.2:load-balancer:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '로드 밸런서는 여러 서버에 트래픽을 분산해 성능과 가용성을 높이는 장비로, 헬스 체크 기능을 통해 장애 서버를 우회할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52102, 'WRITTEN', 'OX', 'NORMAL',
       '네트워크 구축 시 IP 주소 체계와 서브넷 마스크는 나중에 운영 단계에서 임의로 변경해도 다른 시스템에 영향이 없다.',
       'X',
       'IP 주소·서브넷 계획은 초기 설계에서 신중히 결정해야 하며, 변경 시 광범위한 영향을 줄 수 있습니다.',
       'seed:5.2.2:ip-plan:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '네트워크 구축 시 IP 주소 체계와 서브넷 마스크는 나중에 운영 단계에서 임의로 변경해도 다른 시스템에 영향이 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52102, 'WRITTEN', 'OX', 'EASY',
       '중요 시스템을 외부와 분리된 내부망에 두고, DMZ 영역을 통해 필요한 서비스만 개방하는 것은 보안을 강화하는 설계 방법이다.',
       'O',
       '내부망·DMZ·외부망을 분리하면 공격 표면을 줄이고, 보안 사고 확산을 방지할 수 있습니다.',
       'seed:5.2.2:dmz:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '중요 시스템을 외부와 분리된 내부망에 두고, DMZ 영역을 통해 필요한 서비스만 개방하는 것은 보안을 강화하는 설계 방법이다.%'
);


/***********************************************************
 * 5.2.3 하드웨어 구축 관리  (MICRO용 OX 6문항)
 ***********************************************************/

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52103, 'WRITTEN', 'OX', 'EASY',
       '하드웨어 구축 관리에서는 서버, 스토리지, 네트워크 장비 등 물리 자산의 도입·설치·검수·인수 절차를 관리한다.',
       'O',
       '장비 사양 검토부터 설치 검수, 인수인계까지 전 과정을 관리해야 합니다.',
       'seed:5.2.3:hw-lifecycle:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '하드웨어 구축 관리에서는 서버, 스토리지, 네트워크 장비 등 물리 자산의 도입·설치·검수·인수 절차를 관리한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52103, 'WRITTEN', 'OX', 'EASY',
       '하드웨어 설치 후에는 온도·전원·랙 장착 상태 등 환경을 점검해 안정적으로 운영될 수 있는지 확인해야 한다.',
       'O',
       '전원 이중화, 냉각, 랙 고정 상태 등 물리 환경 점검은 장애 예방의 기본입니다.',
       'seed:5.2.3:env-check:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '하드웨어 설치 후에는 온도·전원·랙 장착 상태 등 환경을 점검해 안정적으로 운영될 수 있는지 확인해야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52103, 'WRITTEN', 'OX', 'NORMAL',
       '용량 계획은 현재 사용량만 기준으로 장비를 선정하는 활동으로, 향후 증가량이나 피크 부하는 고려하지 않아도 된다.',
       'X',
       '현재 사용량, 성장률, 피크 부하를 함께 고려해 적정 용량을 산정해야 합니다.',
       'seed:5.2.3:capacity-plan:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '용량 계획은 현재 사용량만 기준으로 장비를 선정하는 활동으로, 향후 증가량이나 피크 부하는 고려하지 않아도 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52103, 'WRITTEN', 'OX', 'NORMAL',
       '정기적인 펌웨어 업데이트와 하드웨어 점검은 장애 예방보다는 성능 향상에만 도움이 되므로 필수는 아니다.',
       'X',
       '펌웨어·모듈 점검은 알려진 결함을 제거하고 장애를 예방하는 데 매우 중요합니다.',
       'seed:5.2.3:maintenance:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정기적인 펌웨어 업데이트와 하드웨어 점검은 장애 예방보다는 성능 향상에만 도움이 되므로 필수는 아니다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52103, 'WRITTEN', 'OX', 'NORMAL',
       '자산 관리는 장비의 일련번호, 위치, 담당자, 보증 기간, 유지보수 계약 정보 등을 체계적으로 관리하는 활동이다.',
       'O',
       '자산 정보를 관리해야 장애·교체·증설 시 적절한 의사결정을 할 수 있습니다.',
       'seed:5.2.3:asset-mgmt:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '자산 관리는 장비의 일련번호, 위치, 담당자, 보증 기간, 유지보수 계약 정보 등을 체계적으로 관리하는 활동이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52103, 'WRITTEN', 'OX', 'EASY',
       '중요 장비에 대해서는 장애 발생 시 대응 절차와 연락처가 포함된 운영 매뉴얼을 작성해 두는 것이 바람직하다.',
       'O',
       '운영 매뉴얼은 장애 대응 시간 단축과 인수인계에 큰 도움이 됩니다.',
       'seed:5.2.3:runbook:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '중요 장비에 대해서는 장애 발생 시 대응 절차와 연락처가 포함된 운영 매뉴얼을 작성해 두는 것이 바람직하다.%'
);


/***********************************************************
 * 5.2.4 가상화/클라우드 인프라  (MICRO용 OX 6문항)
 ***********************************************************/

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52104, 'WRITTEN', 'OX', 'EASY',
       '서버 가상화는 하나의 물리 서버 위에 여러 개의 가상 서버를 생성해 자원을 논리적으로 분리·할당하는 기술이다.',
       'O',
       '가상화를 통해 자원 활용률을 높이고, 환경을 빠르게 생성·삭제할 수 있습니다.',
       'seed:5.2.4:server-virtualization:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52104 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '서버 가상화는 하나의 물리 서버 위에 여러 개의 가상 서버를 생성해 자원을 논리적으로 분리·할당하는 기술이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52104, 'WRITTEN', 'OX', 'EASY',
       '클라우드 컴퓨팅에서는 필요할 때 자원을 할당하고 사용량 기반으로 과금하는 온디맨드 자원 사용 모델을 제공할 수 있다.',
       'O',
       '온디맨드·종량제 과금을 통해 초기 투자 비용을 줄이고 유연하게 확장할 수 있습니다.',
       'seed:5.2.4:cloud-on-demand:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52104 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '클라우드 컴퓨팅에서는 필요할 때 자원을 할당하고 사용량 기반으로 과금하는 온디맨드 자원 사용 모델을 제공할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52104, 'WRITTEN', 'OX', 'NORMAL',
       '가상화 환경에서는 물리 서버 장애가 발생해도 자동으로 다른 물리 서버로 이전되므로, 별도의 고가용성 설계가 필요 없다.',
       'X',
       '가상화만으로 모든 장애에 자동 대응되는 것은 아니며, 추가적인 고가용성·백업 설계가 필요합니다.',
       'seed:5.2.4:virtualization-ha:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52104 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '가상화 환경에서는 물리 서버 장애가 발생해도 자동으로 다른 물리 서버로 이전되므로, 별도의 고가용성 설계가 필요 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52104, 'WRITTEN', 'OX', 'NORMAL',
       '클라우드 환경에서는 데이터 위치와 접근 통제를 클라우드 사업자가 모두 책임지므로, 이용 기업은 별도의 보안 정책을 수립하지 않아도 된다.',
       'X',
       '클라우드는 공통 책임 모델을 따르며, 데이터 보안·접근 통제는 이용 기업의 책임 범위에 해당합니다.',
       'seed:5.2.4:cloud-shared-responsibility:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52104 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '클라우드 환경에서는 데이터 위치와 접근 통제를 클라우드 사업자가 모두 책임지므로, 이용 기업은 별도의 보안 정책을 수립하지 않아도 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52104, 'WRITTEN', 'OX', 'NORMAL',
       '컨테이너 기반 가상화는 운영체제 전체를 가상화하는 방식으로, 각 컨테이너마다 별도의 커널을 가진다.',
       'X',
       '컨테이너는 호스트 OS 커널을 공유하며, 프로세스 격리와 이미지 기반 배포를 특징으로 합니다.',
       'seed:5.2.4:container:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52104 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '컨테이너 기반 가상화는 운영체제 전체를 가상화하는 방식으로, 각 컨테이너마다 별도의 커널을 가진다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_52104, 'WRITTEN', 'OX', 'EASY',
       '가상화 및 클라우드 환경에서도 모니터링과 로그 수집을 통해 자원 사용량과 장애 징후를 지속적으로 관찰해야 한다.',
       'O',
       '가상 자원이라도 모니터링·알람 체계를 갖추어야 안정적인 운영이 가능합니다.',
       'seed:5.2.4:monitoring:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_52104 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '가상화 및 클라우드 환경에서도 모니터링과 로그 수집을 통해 자원 사용량과 장애 징후를 지속적으로 관찰해야 한다.%'
);


/***********************************************************
 * 5.3.1 소프트웨어 설치·배포 및 형상 관리  (MICRO용 OX 6문항)
 ***********************************************************/

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_53101, 'WRITTEN', 'OX', 'EASY',
       '형상 관리는 소프트웨어의 소스 코드, 문서, 설정 파일 등의 변경 이력을 관리하는 활동이다.',
       'O',
       '버전 관리 도구를 활용해 변경 사항을 추적하고, 복구와 협업을 지원합니다.',
       'seed:5.3.1:config-mgmt:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_53101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '형상 관리는 소프트웨어의 소스 코드, 문서, 설정 파일 등의 변경 이력을 관리하는 활동이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_53101, 'WRITTEN', 'OX', 'EASY',
       '소프트웨어 릴리스는 테스트가 완료된 버전을 운영 환경에 배포하기 위해 패키징하고 식별하는 활동이다.',
       'O',
       '릴리스 번호를 부여해 어떤 버전이 운영 중인지 명확히 관리해야 합니다.',
       'seed:5.3.1:release:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_53101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '소프트웨어 릴리스는 테스트가 완료된 버전을 운영 환경에 배포하기 위해 패키징하고 식별하는 활동이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_53101, 'WRITTEN', 'OX', 'NORMAL',
       '빌드 자동화 도구를 사용하면 소스 코드 컴파일, 테스트, 패키징 과정을 수동으로 반복해야 하므로, 오히려 효율이 떨어진다.',
       'X',
       '빌드 자동화는 반복 작업을 자동화해 효율과 일관성을 높이는 핵심 도구입니다.',
       'seed:5.3.1:build-automation:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_53101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '빌드 자동화 도구를 사용하면 소스 코드 컴파일, 테스트, 패키징 과정을 수동으로 반복해야 하므로, 오히려 효율이 떨어진다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_53101, 'WRITTEN', 'OX', 'NORMAL',
       '개발 환경과 운영 환경의 설정이 다르더라도, 애플리케이션 코드만 동일하면 문제 없이 동작하므로 설정 관리는 크게 중요하지 않다.',
       'X',
       '포트, DB 연결, 외부 연동 설정 등 환경 차이는 장애의 주요 원인이므로, 환경별 설정을 체계적으로 관리해야 합니다.',
       'seed:5.3.1:env-config:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_53101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '개발 환경과 운영 환경의 설정이 다르더라도, 애플리케이션 코드만 동일하면 문제 없이 동작하므로 설정 관리는 크게 중요하지 않다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_53101, 'WRITTEN', 'OX', 'NORMAL',
       '형상 식별은 관리 대상 항목에 고유한 식별자(버전 번호 등)를 부여해 어떤 상태인지 구분할 수 있게 하는 활동이다.',
       'O',
       '식별자가 있어야 변경 전후 상태를 구분하고, 특정 버전으로 롤백할 수 있습니다.',
       'seed:5.3.1:config-identification:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_53101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '형상 식별은 관리 대상 항목에 고유한 식별자(버전 번호 등)를 부여해 어떤 상태인지 구분할 수 있게 하는 활동이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_53101, 'WRITTEN', 'OX', 'EASY',
       '운영 환경에 배포할 때는 승인된 릴리스만 사용하도록 배포 절차를 표준화하는 것이 바람직하다.',
       'O',
       '임의의 개발 버전 배포를 막고, 승인된 버전만 운영에 반영되도록 절차를 정의해야 합니다.',
       'seed:5.3.1:deployment-process:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_53101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '운영 환경에 배포할 때는 승인된 릴리스만 사용하도록 배포 절차를 표준화하는 것이 바람직하다.%'
);


/***********************************************************
 * 5.3.2 변경관리/배포 자동화·운영 전환  (MICRO용 OX 6문항)
 ***********************************************************/

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_53102, 'WRITTEN', 'OX', 'EASY',
       '변경 관리는 시스템에 대한 변경 요청을 접수·분석·승인·기록하는 절차를 통해 무분별한 변경을 통제하는 활동이다.',
       'O',
       '변경 이력과 승인 내역을 남겨야 책임소재와 영향 분석이 가능합니다.',
       'seed:5.3.2:change-mgmt:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_53102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '변경 관리는 시스템에 대한 변경 요청을 접수·분석·승인·기록하는 절차를 통해 무분별한 변경을 통제하는 활동이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_53102, 'WRITTEN', 'OX', 'EASY',
       '운영 전환 시에는 사용자 교육, 매뉴얼 제공, 전환 일정 공지 등을 통해 서비스 영향과 혼란을 최소화해야 한다.',
       'O',
       '전환 계획과 커뮤니케이션을 잘 준비해야 업무 중단과 사용자 불편을 줄일 수 있습니다.',
       'seed:5.3.2:cutover:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_53102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '운영 전환 시에는 사용자 교육, 매뉴얼 제공, 전환 일정 공지 등을 통해 서비스 영향과 혼란을 최소화해야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_53102, 'WRITTEN', 'OX', 'NORMAL',
       '배포 자동화는 배포 절차를 사람이 직접 수행하도록 문서화하는 것으로, 도구를 사용한 자동 실행은 포함하지 않는다.',
       'X',
       '배포 자동화는 스크립트·도구를 사용해 빌드부터 배포까지를 자동으로 수행하도록 하는 것을 의미합니다.',
       'seed:5.3.2:deployment-automation:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_53102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '배포 자동화는 배포 절차를 사람이 직접 수행하도록 문서화하는 것으로, 도구를 사용한 자동 실행은 포함하지 않는다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_53102, 'WRITTEN', 'OX', 'NORMAL',
       '롤백 계획은 배포 실패 시 이전 정상 상태로 되돌리는 방법을 미리 정의한 것으로, 실제 운영에서는 거의 사용되지 않는다.',
       'X',
       '롤백 절차는 배포 실패 시 서비스 복구 시간을 줄이는 핵심 요소입니다.',
       'seed:5.3.2:rollback-plan:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_53102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '롤백 계획은 배포 실패 시 이전 정상 상태로 되돌리는 방법을 미리 정의한 것으로, 실제 운영에서는 거의 사용되지 않는다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_53102, 'WRITTEN', 'OX', 'NORMAL',
       '변경 작업은 업무 영향이 적은 시간대에 수행하고, 필요 시 장애 대응 인력을 대기시키는 등 사전 준비가 필요하다.',
       'O',
       '주요 시스템 변경은 심야나 비업무 시간대에 수행하고, 비상 연락 체계를 준비해야 안전합니다.',
       'seed:5.3.2:maintenance-window:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_53102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '변경 작업은 업무 영향이 적은 시간대에 수행하고, 필요 시 장애 대응 인력을 대기시키는 등 사전 준비가 필요하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_53102, 'WRITTEN', 'OX', 'EASY',
       '변경 이력에는 변경 일시, 변경 내용, 승인자, 수행자, 영향 시스템 등을 기록해 추적 가능성을 확보하는 것이 좋다.',
       'O',
       '추적 가능한 변경 이력은 장애 분석과 감사 대응에 필수입니다.',
       'seed:5.3.2:change-log:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_53102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '변경 이력에는 변경 일시, 변경 내용, 승인자, 수행자, 영향 시스템 등을 기록해 추적 가능성을 확보하는 것이 좋다.%'
);


/***********************************************************
 * 5.4.1 시스템 보안 설계  (MICRO용 OX 6문항)
 ***********************************************************/

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_54101, 'WRITTEN', 'OX', 'EASY',
       '시스템 보안 설계 단계에서는 네트워크 구간, 서버, 데이터베이스, 애플리케이션 등 각 계층에 대한 보안 요구를 정의해야 한다.',
       'O',
       '계층별 보안 요구를 정리해야 설계와 구현 단계에서 누락을 줄일 수 있습니다.',
       'seed:5.4.1:multi-layer-security:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_54101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '시스템 보안 설계 단계에서는 네트워크 구간, 서버, 데이터베이스, 애플리케이션 등 각 계층에 대한 보안 요구를 정의해야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_54101, 'WRITTEN', 'OX', 'EASY',
       '최소 권한 원칙은 사용자와 프로세스에 업무 수행에 필요한 최소한의 권한만 부여하는 것을 의미한다.',
       'O',
       '과도한 권한 부여는 내부·외부 공격 시 피해를 크게 만듭니다.',
       'seed:5.4.1:least-privilege:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_54101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '최소 권한 원칙은 사용자와 프로세스에 업무 수행에 필요한 최소한의 권한만 부여하는 것을 의미한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_54101, 'WRITTEN', 'OX', 'NORMAL',
       '보안 설계에서는 기능 요구를 우선 만족시키고, 보안 요구는 구현 단계에서 소스 코드 수준에서만 고려하면 충분하다.',
       'X',
       '보안은 설계 단계에서부터 구조·인터페이스·데이터 흐름에 반영해야 안전한 시스템을 만들 수 있습니다.',
       'seed:5.4.1:security-by-design:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_54101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '보안 설계에서는 기능 요구를 우선 만족시키고, 보안 요구는 구현 단계에서 소스 코드 수준에서만 고려하면 충분하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_54101, 'WRITTEN', 'OX', 'NORMAL',
       '망 분리는 중요 정보 자산을 물리적으로 완전히 분리된 네트워크에 배치해 외부 인터넷과 직접 통신하지 않도록 하는 개념이다.',
       'O',
       '업무망·인터넷망을 분리하면 외부 공격이 내부 중요 자산으로 바로 확산되는 것을 막을 수 있습니다.',
       'seed:5.4.1:network-segmentation:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_54101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '망 분리는 중요 정보 자산을 물리적으로 완전히 분리된 네트워크에 배치해 외부 인터넷과 직접 통신하지 않도록 하는 개념이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_54101, 'WRITTEN', 'OX', 'NORMAL',
       '데이터 분류와 등급은 모든 데이터를 동일한 보안 수준으로 보호하기 위해 수행하는 것으로, 등급에 따라 보호 수준을 달리 적용해서는 안 된다.',
       'X',
       '중요도에 따라 보호 수준을 차등 적용해야 자원을 효율적으로 사용하면서 위험을 줄일 수 있습니다.',
       'seed:5.4.1:data-classification:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_54101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '데이터 분류와 등급은 모든 데이터를 동일한 보안 수준으로 보호하기 위해 수행하는 것으로, 등급에 따라 보호 수준을 달리 적용해서는 안 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_54101, 'WRITTEN', 'OX', 'EASY',
       '보안 설계 시에는 감사 로그와 추적 가능성도 함께 고려해, 누가 무엇을 했는지 나중에 확인할 수 있도록 해야 한다.',
       'O',
       '접근·변경 행위를 남겨야 사고 분석과 감사 대응이 가능합니다.',
       'seed:5.4.1:audit-trail:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_54101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '보안 설계 시에는 감사 로그와 추적 가능성도 함께 고려해, 누가 무엇을 했는지 나중에 확인할 수 있도록 해야 한다.%'
);


/***********************************************************
 * 5.4.2 시스템 보안 구현  (MICRO용 OX 6문항)
 ***********************************************************/

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_54102, 'WRITTEN', 'OX', 'EASY',
       '보안 구현 단계에서는 계정·권한 설정, 패스워드 정책 적용, 불필요한 서비스 비활성화 등을 수행해야 한다.',
       'O',
       '기본 계정 삭제, 최소 권한 설정, 서비스 최소화는 보안 강화의 기본입니다.',
       'seed:5.4.2:hardening:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_54102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '보안 구현 단계에서는 계정·권한 설정, 패스워드 정책 적용, 불필요한 서비스 비활성화 등을 수행해야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_54102, 'WRITTEN', 'OX', 'EASY',
       '운영체제와 미들웨어, 애플리케이션에 대한 보안 패치를 정기적으로 적용하는 것은 알려진 취약점을 줄이는 데 중요하다.',
       'O',
       '패치 미적용은 알려진 취약점 공격에 그대로 노출되는 결과를 초래합니다.',
       'seed:5.4.2:patch-mgmt:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_54102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '운영체제와 미들웨어, 애플리케이션에 대한 보안 패치를 정기적으로 적용하는 것은 알려진 취약점을 줄이는 데 중요하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_54102, 'WRITTEN', 'OX', 'NORMAL',
       '중요 데이터는 저장 시에는 암호화할 수 있지만, 전송 시에는 네트워크 구간이 내부망이라면 암호화할 필요가 없다.',
       'X',
       '내부망에서도 스니핑 등 공격 가능성이 있으므로, 중요 데이터 전송은 암호화 프로토콜을 사용하는 것이 안전합니다.',
       'seed:5.4.2:data-encryption:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_54102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '중요 데이터는 저장 시에는 암호화할 수 있지만, 전송 시에는 네트워크 구간이 내부망이라면 암호화할 필요가 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_54102, 'WRITTEN', 'OX', 'NORMAL',
       '시스템 로그는 저장 공간을 차지하므로, 장애나 보안 사고와 무관하게 주기적으로 즉시 삭제하는 것이 바람직하다.',
       'X',
       '로그는 장애 및 보안 사고 분석의 핵심 자료이므로, 보존 기간과 보호 정책을 정해 관리해야 합니다.',
       'seed:5.4.2:log-retention:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_54102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '시스템 로그는 저장 공간을 차지하므로, 장애나 보안 사고와 무관하게 주기적으로 즉시 삭제하는 것이 바람직하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_54102, 'WRITTEN', 'OX', 'NORMAL',
       '계정 공유는 운영 효율을 위해 바람직한 방법이며, 여러 사람이 하나의 관리자 계정을 함께 사용하는 것이 추적성 측면에서도 좋다.',
       'X',
       '계정 공유는 누가 어떤 작업을 했는지 추적하기 어려워 보안 사고 시 책임 소재를 밝히기 어렵습니다.',
       'seed:5.4.2:account-sharing:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_54102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '계정 공유는 운영 효율을 위해 바람직한 방법이며, 여러 사람이 하나의 관리자 계정을 함께 사용하는 것이 추적성 측면에서도 좋다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_54102, 'WRITTEN', 'OX', 'EASY',
       '보안 장비와 시스템 설정 변경 시에는 변경 내용을 기록하고, 필요 시 원래 상태로 복구할 수 있도록 백업을 확보해야 한다.',
       'O',
       '설정 변경 전 백업과 변경 기록은 장애 발생 시 빠른 복구에 필수입니다.',
       'seed:5.4.2:config-backup:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_54102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '보안 장비와 시스템 설정 변경 시에는 변경 내용을 기록하고, 필요 시 원래 상태로 복구할 수 있도록 백업을 확보해야 한다.%'
);
