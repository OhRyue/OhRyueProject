-- 토픽(예: 소프트웨어 설계 > 화면 설계 > UI 표준/가이드)
insert into topic(parent_id, code, title, exam_mode) values
 (null, '1', '소프트웨어 설계', 'WRITTEN');                         -- id=1
insert into topic(parent_id, code, title, exam_mode) values
 (1, '1.2', '화면 설계', 'WRITTEN');                                -- id=2
insert into topic(parent_id, code, title, exam_mode) values
 (2, '1.2.1', 'UI 표준 및 가이드 정의', 'WRITTEN');                 -- id=3 (micro)

insert into concept(topic_id, content) values
 (3, 'OSI 7계층 핵심: **물리/데이터링크/네트워크/전송/세션/표현/응용** …');

-- 미니체크 OX (5개)
insert into question(topic_id,type,difficulty,text,ox_answer,explanation)
values
 (3,'OX','EASY','물리 계층은 **비트 전송**을 담당한다.', true,  '물리=비트.'),
 (3,'OX','EASY','IP 주소는 **전송 계층**에서 사용된다.',false, 'IP=네트워크(3계층).'),
 (3,'OX','EASY','TCP/UDP는 전송 계층 프로토콜이다.',      true, '전송=TCP/UDP.'),
 (3,'OX','EASY','데이터링크 계층은 프레임을 다룬다.',       true, '링크=프레임, MAC.'),
 (3,'OX','EASY','세션 계층은 패킷 라우팅을 담당한다.',      false, '라우팅=네트워크.');

-- 객관식 4+ (학습 MCQ)
insert into question(topic_id,type,difficulty,text,explanation)
values
 (3,'MCQ','EASY','OSI에서 **IP 주소**는 어디 계층?', 'IP=네트워크 계층(3계층).');

set @q1 = last_insert_id();
insert into question_choice(question_id,label,text,correct) values
 (@q1,'A','물리',false),(@q1,'B','데이터링크',false),(@q1,'C','네트워크',true),(@q1,'D','전송',false);

insert into question(topic_id,type,difficulty,text,explanation)
values
 (3,'MCQ','EASY','TCP와 가장 관련 있는 계층?', 'TCP=전송 계층');
set @q2 = last_insert_id();
insert into question_choice(question_id,label,text,correct) values
 (@q2,'A','응용',false),(@q2,'B','전송',true),(@q2,'C','네트워크',false),(@q2,'D','데이터링크',false);

insert into question(topic_id,type,difficulty,text,explanation)
values
 (3,'MCQ','EASY','MAC 주소는 어느 계층?', 'MAC=데이터링크');
set @q3 = last_insert_id();
insert into question_choice(question_id,label,text,correct) values
 (@q3,'A','데이터링크',true),(@q3,'B','응용',false),(@q3,'C','전송',false),(@q3,'D','표현',false);

insert into question(topic_id,type,difficulty,text,explanation)
values
 (3,'MCQ','NORMAL','다음 중 **라우팅**과 가장 관련이 깊은 계층?', '라우팅=네트워크');
set @q4 = last_insert_id();
insert into question_choice(question_id,label,text,correct) values
 (@q4,'A','세션',false),(@q4,'B','표현',false),(@q4,'C','네트워크',true),(@q4,'D','전송',false);
