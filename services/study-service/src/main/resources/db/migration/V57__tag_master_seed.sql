SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

INSERT INTO tag_master (
  code, domain, label_ko, label_en, description, order_no
) VALUES
  -- 소프트웨어 설계/개발 (WRITTEN)
  ('SW_REQ_MODEL', 'WRITTEN',
   '요구분석·모델링·UI',
   'Requirements / Modeling / UI',
   'DFD, UI 유형, CASE, HIPO, 프로토타입 등 요구분석·모델링·UI 전반을 다루는 문제에 사용합니다.',
   1),

  ('SW_ARCH_DESIGN', 'WRITTEN',
   '소프트웨어 아키텍처·설계·모듈 구조',
   'Software Architecture & Design',
   '아키텍처 설계 과정, 플랫폼 성능 특성, 모듈 구조, fan-in/fan-out 등 설계/아키텍처 관련 문제에 사용합니다.',
   2),

  ('OOP_UML_PATTERN', 'WRITTEN',
   '객체지향·UML·디자인패턴',
   'OOP / UML / Design Pattern',
   '클래스/객체, 속성/오퍼레이션, UML 관계(Dependency, Generalization 등), 디자인 패턴의 구성요소·장단점을 묻는 문제에 사용합니다.',
   3),

  ('PROCESS_WATERFALL_AGILE', 'WRITTEN',
   '개발 프로세스·모형(폭포수/애자일)',
   'Dev Process (Waterfall / Agile)',
   '폭포수, 나선형, 애자일, XP, CASE·프로토타이핑, SDLC 전반의 개발 프로세스/모형 관련 문제에 사용합니다.',
   4),

  ('TEST_BASE_TECH', 'WRITTEN',
   '테스트 기본·기법',
   'Testing Basics & Techniques',
   '블랙박스/화이트박스, 동치/경계/원인결과, 테스트 케이스 구성, 성능 테스트 도구 등 소프트웨어 테스트 기본/기법 문제에 사용합니다.',
   5),

  ('CONFIG_CM', 'WRITTEN',
   '형상관리·형상통제',
   'Configuration Management',
   '형상 식별/통제/감사, 형상 관리 도구 기능 등 소프트웨어 형상관리/형상통제 관련 문제에 사용합니다.',
   6),

  ('DS_ALGO', 'WRITTEN',
   '자료구조·알고리즘',
   'Data Structures & Algorithms',
   '스택/큐/트리/그래프, DFS, 후위표기, 각종 정렬, 이진탐색, 분할정복 등 자료구조·알고리즘 문제에 사용합니다.',
   7),

  -- 데이터베이스 (WRITTEN)
  ('DB_MODEL_NORMAL', 'WRITTEN',
   '모델링·정규화·무결성·키',
   'Modeling / Normalization / Integrity',
   '개념/논리/물리 모델, 정규형(1NF~), 이상 제거, 개체/참조 무결성, 기본키/외래키 등 DB 모델링·정규화·무결성·키 관련 문제에 사용합니다.',
   8),

  ('DB_SQL_TX', 'WRITTEN',
   'SQL·트랜잭션·관계대수',
   'SQL / Transaction / Relational Algebra',
   'DDL/DML/DCL, VIEW, GRANT/REVOKE, SELECT/INSERT, 트랜잭션과 COMMIT/ROLLBACK, 관계대수(Select/Project/Join/Division 등) 문제에 사용합니다.',
   9),

  ('DB_META_CRUD', 'WRITTEN',
   '메타데이터·데이터사전·CRUD분석',
   'Metadata / Data Dictionary / CRUD Analysis',
   '데이터 사전, 시스템 카탈로그, CRUD 매트릭스, 트랜잭션 분석 등 메타데이터/CRUD 분석 관련 문제에 사용합니다.',
   10),

  -- 프로그래밍·OS·네트워크 (WRITTEN)
  ('LANG_BASIC_SCRIPT', 'WRITTEN',
   '프로그래밍 언어·라이브러리·스크립트',
   'Languages / Libraries / Scripts',
   'C/Java/Python 자료형 및 코드 결과, 표준 라이브러리, PHP/ASP/JSP 등 스크립트 언어 구분 문제에 사용합니다.',
   11),

  ('OS_PROC_MEM', 'WRITTEN',
   '운영체제·프로세스·메모리·교착',
   'OS / Process / Memory / Deadlock',
   'UNIX 특징, 스케줄링(FCFS/SJF/HRN/SRT), HRN 계산, 교착조건, 페이지 교체(FIFO 등) 등 운영체제 관련 문제에 사용합니다.',
   12),

  ('NET_PROTO_IP', 'WRITTEN',
   '네트워크·프로토콜·IP',
   'Network / Protocol / IP',
   'OSI 7계층, 네트워크 계층 기능, IPv4/IPv6, 주소/클래스, ARP, 무선LAN CSMA/CA 등 네트워크·프로토콜 관련 문제에 사용합니다.',
   13),

  -- 보안·관리·공학·신기술 (WRITTEN)
  ('SEC_BASE_DRM', 'WRITTEN',
   '보안 기초·DRM',
   'Security Basics / DRM',
   '기밀성/무결성/가용성, DRM 요소, 물리적 위협, 보안 개념 전반 등 보안 기초/DRM 문제에 사용합니다.',
   14),

  ('SEC_ATTACK_DEFENSE', 'WRITTEN',
   '공격·방어·시큐어코딩',
   'Attacks / Defense / Secure Coding',
   'SQL Injection, XSS, 명령어 삽입, 스택가드, DPI, SSH, SAN 등 공격·방어·시큐어코딩 관련 문제에 사용합니다.',
   15),

  ('SW_COST_PM', 'WRITTEN',
   '소프트웨어 공학·비용·프로젝트관리',
   'Software Engineering / Cost / Project Management',
   'COCOMO/LOC, 비용 산정, 일정 관리(간트/PERT/WBS 등)와 소프트웨어 공학·프로젝트 관리 문제에 사용합니다.',
   16),

  ('IT_NEW_DOCKER_SDN', 'WRITTEN',
   '신기술·인프라·컨테이너·SDN',
   'New Tech / Infra / Docker / SDN',
   'Docker, 컨테이너, SDN, 네트워크 가상화 등 최신 IT 인프라/신기술 관련 문제에 사용합니다.',
   17),

  -- 정보처리 실기 태그 (PRACTICAL)
  ('P_REQ_SCENARIO', 'PRACTICAL',
   '업무 시나리오 해석·요구 분석',
   'Req Scenario Analysis',
   '고객/주문/계좌 등 업무 시나리오 지문을 읽고 필요한 데이터·기능·업무 규칙을 해석하는 실기 문제에 사용합니다. (P.1.1 업무 시나리오 해석 중심)',
   101),

  ('P_REQ_DATA_ANALYSIS', 'PRACTICAL',
   '데이터 요구·업무/CRUD 분석',
   'Data Requirements / CRUD Analysis',
   '시나리오/표를 보고 CRUD 매트릭스, 트랜잭션, 필요한 컬럼/테이블 등을 도출하는 실기 문제에 사용합니다. (P.1.2 업무 및 데이터 요구 도출 중심)',
   102),

  ('P_MODEL_ER_PHYSICAL', 'PRACTICAL',
   '개념/논리/물리 모델링·ERD 설계',
   'Concept / Logical / Physical Modeling',
   '업무 설명이나 ERD를 보고 엔티티·관계·식별자·카디널리티를 해석하거나 논리→물리 모델(테이블/컬럼)로 변환하는 실기 문제에 사용합니다. (P.2.1)',
   103),

  ('P_MODEL_NORMAL_DENORMAL', 'PRACTICAL',
   '정규화·반정규화 적용',
   'Normalization / Denormalization',
   '이상(삽입/삭제/갱신), 함수적 종속, 정규형 판별 및 반정규화 여부 판단 등 정규화·반정규화 설계 문제에 사용합니다. (P.2.2)',
   104),

  ('P_SQL_SELECT_JOIN', 'PRACTICAL',
   'SELECT·집계·조인·서브쿼리 작성',
   'SELECT / JOIN / Aggregation',
   'SELECT, GROUP BY, HAVING, JOIN, 서브쿼리, EXISTS/IN/CASE WHEN 등을 이용해 정답 쿼리 블록을 작성하는 실기 문제에 사용합니다. (P.3.1)',
   105),

  ('P_SQL_INDEX_TUNING', 'PRACTICAL',
   '인덱스·실행계획·쿼리 튜닝',
   'Index / Execution Plan / Query Tuning',
   '실행계획(Full Scan vs Index Scan), 인덱스 컬럼 선택, 비효율 쿼리 개선 등 인덱스·쿼리 튜닝 실기 문제에 사용합니다. (P.3.2)',
   106),

  ('P_DB_TX_ISOLATION', 'PRACTICAL',
   '트랜잭션 특성·격리수준·무결성',
   'Transaction Properties / Isolation / Integrity',
   'ACID, 고립 수준(READ UNCOMMITTED~SERIALIZABLE), Dirty/Non-repeatable/Phantom Read, 무결성 제약 등 트랜잭션/격리수준 문제에 사용합니다. (P.4.1)',
   107),

  ('P_DB_LOCK_CONCURRENCY', 'PRACTICAL',
   '동시성 제어·락·교착상태',
   'Lock / Concurrency / Deadlock',
   'Shared/Exclusive Lock, Lock Granularity, Deadlock, 낙관/비관 잠금 전략 등 동시성 제어·락 문제에 사용합니다. (P.4.2)',
   108),

  ('P_DB_BACKUP_RECOVERY', 'PRACTICAL',
   '백업·복구·RPO/RTO 전략',
   'Backup / Recovery / RPO / RTO',
   'Full/Incremental/Differential 백업, 로그 백업, PITR, RPO/RTO 계산 등 백업·복구 전략 설계 실기 문제에 사용합니다. (P.5.1)',
   109),

  ('P_DB_OPERATION_MONITOR', 'PRACTICAL',
   'DB 운영·장애 분석·모니터링',
   'DB Operation / Incident Analysis / Monitoring',
   'APM/모니터링 지표를 보고 병목 원인 분석, 장애 리포트 기반 RCA 및 개선안 작성, 인덱스 재구성·통계 갱신·파티셔닝 등 DB 운영/장애 대응 문제에 사용합니다. (P.5.2)',
   110);

SET FOREIGN_KEY_CHECKS = 1;
