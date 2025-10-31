-- 태그 테이블
create table if not exists question_tag (
  id bigint primary key auto_increment,
  question_id bigint not null,
  tag varchar(64) not null,
  constraint fk_qtag_question foreign key(question_id) references question(id),
  index idx_qtag_tag(tag),
  index idx_qtag_qid(question_id)
);

-- 사용자 시도 로그
create table if not exists user_answer (
  id bigint primary key auto_increment,
  user_id varchar(64) not null,
  question_id bigint not null,
  correct boolean not null,
  answer_text text null,        -- OX/서술형 입력, MCQ는 선택지 라벨
  created_at timestamp not null default current_timestamp,
  constraint fk_uans_q foreign key(question_id) references question(id),
  index idx_uans_user(user_id),
  index idx_uans_qid(question_id),
  index idx_uans_user_q(user_id, question_id)
);

-- 샘플 태그(기존 V2/V3 문항에 부여: 네트워크/OSI/UI 등)
insert into question_tag(question_id, tag)
select q.id, 'OSI' from question q
  where q.text like '%OSI%' and not exists(
    select 1 from question_tag t where t.question_id=q.id and t.tag='OSI');

insert into question_tag(question_id, tag)
select q.id, '네트워크' from question q
  where (q.text like '%IP%' or q.text like '%라우팅%' or q.text like '%TCP%' or q.text like '%MAC%')
    and not exists(
      select 1 from question_tag t where t.question_id=q.id and t.tag='네트워크');

insert into question_tag(question_id, tag)
select q.id, 'UI' from question q
  where (q.text like '%UI %' or q.text like '%와이어프레임%' or q.text like '%스토리보드%')
    and not exists(
      select 1 from question_tag t where t.question_id=q.id and t.tag='UI');
