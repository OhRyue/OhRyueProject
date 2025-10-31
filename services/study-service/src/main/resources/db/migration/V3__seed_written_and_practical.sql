-- 1) 설계 > 화면 설계 > UI 시나리오/스토리보드 (written)
insert into topic(parent_id, code, title, exam_mode)
select 2, '1.2.2', 'UI 시나리오/스토리보드', 'WRITTEN'
where not exists (select 1 from topic where code='1.2.2');

set @t_ui = (select id from topic where code='1.2.2' limit 1);

insert into concept(topic_id, content)
select @t_ui, 'UI 시나리오는 사용자 흐름 정의, 스토리보드는 화면 전환/요소 배치를 도식화합니다.'
where not exists (select 1 from concept where topic_id=@t_ui);

-- 미니체크 OX 5개
insert into question(topic_id,type,difficulty,text,ox_answer,explanation)
select @t_ui,'OX','EASY','스토리보드는 기능 요구사항의 흐름을 **시각화**한다.', true,  '스토리보드=시각화'
where not exists (select 1 from question where topic_id=@t_ui and type='OX' limit 1);
insert into question(topic_id,type,difficulty,text,ox_answer,explanation) values
 (@t_ui,'OX','EASY','UI 시나리오는 사용자 경로를 설명한다.', true,  '시나리오=사용자 경로'),
 (@t_ui,'OX','EASY','스토리보드는 데이터베이스 스키마를 설계한다.', false, 'DB 스키마 아님'),
 (@t_ui,'OX','EASY','와이어프레임은 색상/브랜딩 중심의 고해상 시안이다.', false, '와이어프레임=저해상 뼈대'),
 (@t_ui,'OX','EASY','페르소나는 대표 사용자 모델을 말한다.', true, '페르소나=대표 사용자');

/* MCQ 4개 */
insert into question(topic_id,type,difficulty,text,explanation)
values
 (@t_ui,'MCQ','EASY','와이어프레임의 핵심 목적은?', '구조/배치 파악(저해상)'); set @q1 = last_insert_id();
insert into question_choice(question_id,label,text,correct) values
 (@q1,'A','브랜딩/색상 최적화',false),(@q1,'B','저해상 구조 설계',true),(@q1,'C','CI 가이드 제작',false),(@q1,'D','배포 파이프라인',false);

insert into question(topic_id,type,difficulty,text,explanation)
values
 (@t_ui,'MCQ','NORMAL','스토리보드 구성요소로 거리가 먼 것은?', 'DB 인덱스는 아님'); set @q2 = last_insert_id();
insert into question_choice(question_id,label,text,correct) values
 (@q2,'A','화면 전환',false),(@q2,'B','화면 요소',false),(@q2,'C','DB 인덱스',true),(@q2,'D','사용자 이벤트',false);

insert into question(topic_id,type,difficulty,text,explanation)
values
 (@t_ui,'MCQ','EASY','UI 시나리오 산출물로 합당한 것은?', '시나리오 문서/플로우'); set @q3 = last_insert_id();
insert into question_choice(question_id,label,text,correct) values
 (@q3,'A','개발서버 로그',false),(@q3,'B','사용자 흐름 문서',true),(@q3,'C','서버 증설 계획',false),(@q3,'D','네트워크 토폴로지',false);

insert into question(topic_id,type,difficulty,text,explanation)
values
 (@t_ui,'MCQ','NORMAL','페르소나 정의 시 포함 요소로 적절한 것은?', '목표/행동/동기'); set @q4 = last_insert_id();
insert into question_choice(question_id,label,text,correct) values
 (@q4,'A','CPU 클럭',false),(@q4,'B','사용 목표',true),(@q4,'C','라우터 대역폭',false),(@q4,'D','디스크 IOPS',false);

-- 2) 네트워크 기초(실기 PRACTICAL 샘플)
insert into topic(parent_id, code, title, exam_mode)
select null, 'N', '네트워크 기초(실기)', 'PRACTICAL'
where not exists (select 1 from topic where code='N');

set @t_np = (select id from topic where code='N' limit 1);

-- SHORT(단답) : 정답 키워드를 해설 첫줄 "정답: ..." 포맷으로 저장
insert into question(topic_id,type,difficulty,text,explanation,image_url)
values
 (@t_np,'SHORT','EASY','DHCP의 주요 기능 1가지를 서술하세요.',
  '정답: DHCP, 동적, 호스트, 주소, 자동 할당\nDHCP는 동적 IP 주소 자동할당 프로토콜입니다.', null),
 (@t_np,'SHORT','NORMAL','TCP에서 3-way handshake 두 번째 단계의 플래그는?',
  '정답: SYN-ACK, synack\n연결 설정에서 두 번째는 SYN-ACK입니다.', null);

-- LONG(서술)
insert into question(topic_id,type,difficulty,text,explanation,image_url)
values
 (@t_np,'LONG','NORMAL','서버/클라이언트에서 발생 가능한 TCP 혼잡 제어 기법을 2가지 이상 설명하세요.',
  '정답: 슬로우스타트, 혼잡회피, 빠른재전송, 빠른회복\nTCP 혼잡 제어에는 슬로우 스타트, 혼잡 회피, 빠른 재전송/회복 등이 있으며 혼잡 윈도우(cwnd) 조정 전략을 포함한다.', null),
 (@t_np,'LONG','HARD','DNS 질의 과정(재귀/반복 포함)을 흐름 위주로 설명하세요.',
  '정답: 재귀, 반복, 루트, TLD, 권한, 리커서\n클라이언트는 리커시브 리졸버에 질의하고, 리졸버는 루트 → TLD → 권한 서버로 반복 질의를 통해 응답을 획득한다.', null);
