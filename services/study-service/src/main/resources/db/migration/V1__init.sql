create table if not exists topic(
  id bigint primary key auto_increment,
  parent_id bigint null,
  code varchar(64) not null,
  title varchar(255) not null,
  exam_mode varchar(16) not null
);

create table if not exists concept(
  id bigint primary key auto_increment,
  topic_id bigint not null,
  content text,
  constraint fk_concept_topic foreign key(topic_id) references topic(id)
);

create table if not exists question(
  id bigint primary key auto_increment,
  topic_id bigint not null,
  type varchar(16) not null,
  difficulty varchar(16) not null,
  text text not null,
  image_url varchar(512),
  ox_answer bool,
  explanation text,
  constraint fk_question_topic foreign key(topic_id) references topic(id)
);

create table if not exists question_choice(
  id bigint primary key auto_increment,
  question_id bigint not null,
  label varchar(4) not null,
  text text not null,
  correct boolean not null,
  constraint fk_choice_question foreign key(question_id) references question(id)
);

create table if not exists user_progress(
  id bigint primary key auto_increment,
  user_id varchar(64) not null,
  topic_id bigint not null,
  exam_mode varchar(16) not null,
  mini_total int not null default 0,
  mini_correct int not null default 0,
  mini_passed boolean not null default false,
  mcq_total int not null default 0,
  mcq_correct int not null default 0,
  updated_at timestamp null,
  index idx_user_topic(user_id, topic_id),
  constraint fk_progress_topic foreign key(topic_id) references topic(id)
);
