-- ===== WRITTEN: 소프트웨어 설계 트리 확장 =====
-- 상위(이미 존재 가정): id=1 '소프트웨어 설계'
insert into topic(parent_id, code, title, exam_mode)
select 1, '1.3', '애플리케이션 설계', 'WRITTEN'
where not exists (select 1 from topic where code='1.3');

insert into topic(parent_id, code, title, exam_mode)
select (select id from topic where code='1.3'), '1.3.1', '공통 모듈 설계', 'WRITTEN'
where not exists (select 1 from topic where code='1.3.1');

set @t_comm = (select id from topic where code='1.3.1' limit 1);

insert into concept(topic_id, content)
select @t_comm, '공통 모듈은 재사용/유지보수성 향상을 목표로 표준 인터페이스를 제공한다.'
where not exists (select 1 from concept where topic_id=@t_comm);

-- OX 5개
insert into question(topic_id,type,difficulty,text,ox_answer,explanation) values
(@t_comm,'OX','EASY','공통 모듈은 중복을 줄이는 목적이 있다.',true,'중복 제거/재사용'),
(@t_comm,'OX','EASY','공통 모듈은 서비스마다 인터페이스가 달라야 한다.',false,'표준화 필요'),
(@t_comm,'OX','NORMAL','공통 모듈은 테스트 용이성 향상에 기여한다.',true,'일관된 계약'),
(@t_comm,'OX','NORMAL','공통 모듈은 강결합을 권장한다.',false,'약결합 지향'),
(@t_comm,'OX','EASY','계약 기반 설계는 사전조건/사후조건을 포함한다.',true,'DbC');

-- MCQ 8개
insert into question(topic_id,type,difficulty,text,explanation) values
(@t_comm,'MCQ','EASY','공통 모듈 장점으로 거리가 먼 것은?', '강결합 유도는 장점 아님');
set @q1=last_insert_id();
insert into question_choice(question_id,label,text,correct) values
(@q1,'A','중복 코드 감소',false),(@q1,'B','유지보수성 향상',false),(@q1,'C','강결합 유도',true),(@q1,'D','일관된 인터페이스',false);

insert into question(topic_id,type,difficulty,text,explanation) values
(@t_comm,'MCQ','EASY','표준 인터페이스 장점?', '학습 곡선 감소/일관성');
set @q2=last_insert_id();
insert into question_choice(question_id,label,text,correct) values
(@q2,'A','일관성',true),(@q2,'B','무작위성',false),(@q2,'C','결합도 증가',false),(@q2,'D','가독성 하락',false);

insert into question(topic_id,type,difficulty,text,explanation) values
(@t_comm,'MCQ','NORMAL','아래 중 재사용성과 가장 연관 깊은 품질 특성은?', '재사용성/유지보수성');
set @q3=last_insert_id();
insert into question_choice(question_id,label,text,correct) values
(@q3,'A','가용성',false),(@q3,'B','유지보수성',true),(@q3,'C','보안성',false),(@q3,'D','감사 가능성',false);

insert into question(topic_id,type,difficulty,text,explanation) values
(@t_comm,'MCQ','NORMAL','계약 기반 설계 구성?', '사전조건/사후조건/불변식');
set @q4=last_insert_id();
insert into question_choice(question_id,label,text,correct) values
(@q4,'A','사전조건',true),(@q4,'B','사후조건',true),(@q4,'C','불변식',true),(@q4,'D','예외로그',false);

insert into question(topic_id,type,difficulty,text,explanation) values
(@t_comm,'MCQ','HARD','공통 모듈 인터페이스 버전 전략으로 적절한 것은?', 'SemVer/하위호환');
set @q5=last_insert_id();
insert into question_choice(question_id,label,text,correct) values
(@q5,'A','임의 문자열 버전',false),(@q5,'B','SemVer',true),(@q5,'C','매 배포시 대버전 증가',false),(@q5,'D','하위호환 무시',false);

insert into question(topic_id,type,difficulty,text,explanation) values
(@t_comm,'MCQ','EASY','약결합 촉진 기법?', 'DI/이벤트/메시징');
set @q6=last_insert_id();
insert into question_choice(question_id,label,text,correct) values
(@q6,'A','의존성 주입',true),(@q6,'B','하드코딩',false),(@q6,'C','강제 연결',false),(@q6,'D','글로벌 상태 남용',false);

insert into question(topic_id,type,difficulty,text,explanation) values
(@t_comm,'MCQ','NORMAL','테스트 용이성 향상 요소?', '계약/격리/모의');
set @q7=last_insert_id();
insert into question_choice(question_id,label,text,correct) values
(@q7,'A','격리 가능한 설계',true),(@q7,'B','전역 단일톤 남용',false),(@q7,'C','인터페이스 제거',false),(@q7,'D','하드 종속성',false);

insert into question(topic_id,type,difficulty,text,explanation) values
(@t_comm,'MCQ','HARD','공통 모듈 변경 시 위험 완화?', '호환성/디프리케이션');
set @q8=last_insert_id();
insert into question_choice(question_id,label,text,correct) values
(@q8,'A','하위호환 유지',true),(@q8,'B','사용처 강제 수정',false),(@q8,'C','디프리케이션 안내',true),(@q8,'D','불규칙 릴리스 노트',false);

-- 태그 부여
insert into question_tag(question_id, tag)
select @q1,'모듈' union all select @q2,'인터페이스' union all select @q3,'유지보수성'
union all select @q4,'설계원칙' union all select @q5,'버전전략' union all select @q6,'DI'
union all select @q7,'테스트' union all select @q8,'호환성'
where not exists (select 1 from question_tag where question_id=@q1 and tag='모듈');

-- ===== PRACTICAL: 네트워크 기초 확장 =====
set @t_np = (select id from topic where code='N' limit 1);

insert into question(topic_id,type,difficulty,text,explanation) values
(@t_np,'SHORT','NORMAL','ARP의 역할을 간단히 서술하라.',
 '정답: 주소 결정, MAC, IP-매핑, 브로드캐스트\nARP는 IP->MAC 매핑을 수행한다.'),
(@t_np,'SHORT','HARD','TLS 핸드셰이크 핵심 2가지 이상 적시하라.',
 '정답: 키교환, 인증서, 대칭키, 핸드셰이크\n인증서 검증 및 키교환 후 대칭키로 암호화.');

insert into question(topic_id,type,difficulty,text,explanation) values
(@t_np,'LONG','NORMAL','HTTP/2와 HTTP/3의 차이(전송/연결) 중심으로 기술하라.',
 '정답: 멀티플렉싱, HOLB, QUIC, UDP, 핸드셰이크 축소\nHTTP/3는 QUIC 기반으로 HOLB를 줄이고 0-RTT 등 연결 지연을 줄인다.'),
(@t_np,'LONG','HARD','BGP의 기본 동작과 장애 시 전파 과정을 설명하라.',
 '정답: AS, 경로속성, 경로전파, 컨버전스\nBGP는 AS 간 경로속성을 전파하고 장애 시 컨버전스에 시간이 소요된다.');
