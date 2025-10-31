-- V1: 베이스 스키마 (모든 핵심 테이블 생성)

-- 안전 설정
SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- 토픽 (3단계 계층: major/sub/micro or 자유 계층)
CREATE TABLE IF NOT EXISTS topic (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  parent_id    BIGINT       NULL,
  code         VARCHAR(50)  NOT NULL,           -- "1.1.1" 등
  title        VARCHAR(255) NOT NULL,           -- "현행 시스템 분석"
  exam_mode    ENUM('WRITTEN','PRACTICAL') NOT NULL DEFAULT 'WRITTEN',
  CONSTRAINT uq_topic_code UNIQUE (code),
  INDEX ix_topic_parent (parent_id),
  INDEX ix_topic_mode (exam_mode)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 개념 (레거시 content + 리치 JSON 블록)
CREATE TABLE IF NOT EXISTS concept (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  topic_id     BIGINT       NOT NULL,
  content      TEXT         NULL,               -- 레거시 단문
  blocks_json  JSON         NULL,               -- { sections:[{orderNo,subCode,title,importance,blocks:[...]}] }
  CONSTRAINT fk_concept_topic FOREIGN KEY (topic_id) REFERENCES topic(id) ON DELETE CASCADE,
  UNIQUE KEY uq_concept_topic (topic_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 문제
CREATE TABLE IF NOT EXISTS question (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  topic_id     BIGINT       NOT NULL,
  type         ENUM('OX','MCQ','SHORT','LONG') NOT NULL,
  difficulty   ENUM('EASY','NORMAL','HARD') NOT NULL DEFAULT 'NORMAL',
  text         TEXT         NOT NULL,
  explanation  TEXT         NULL,               -- DB 기본 해설(필기 채점용/실기 휴리스틱용)
  image_url    VARCHAR(500) NULL,
  -- OX 정답 (OX일 때 사용; TRUE=O, FALSE=X)
  ox_answer    TINYINT(1)   NULL,
  CONSTRAINT fk_question_topic FOREIGN KEY (topic_id) REFERENCES topic(id) ON DELETE CASCADE,
  INDEX ix_question_topic (topic_id),
  INDEX ix_question_type (type),
  INDEX ix_question_diff (difficulty)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 객관식 보기
CREATE TABLE IF NOT EXISTS question_choice (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  question_id  BIGINT       NOT NULL,
  label        CHAR(1)      NOT NULL,           -- 'A'~'D' 권장
  text         VARCHAR(1000) NOT NULL,
  is_correct   TINYINT(1)   NOT NULL DEFAULT 0,
  CONSTRAINT fk_choice_question FOREIGN KEY (question_id) REFERENCES question(id) ON DELETE CASCADE,
  UNIQUE KEY uq_choice_label (question_id, label),
  INDEX ix_choice_q (question_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 문제-태그 매핑 (약점 분석/추천)
CREATE TABLE IF NOT EXISTS question_tag (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  question_id  BIGINT       NOT NULL,
  tag          VARCHAR(100) NOT NULL,
  CONSTRAINT fk_qtag_question FOREIGN KEY (question_id) REFERENCES question(id) ON DELETE CASCADE,
  INDEX ix_qtag_q (question_id),
  INDEX ix_qtag_t (tag)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 사용자 진행(메인학습)
CREATE TABLE IF NOT EXISTS user_progress (
  id            BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id       VARCHAR(100) NOT NULL,
  topic_id      BIGINT       NOT NULL,
  exam_mode     ENUM('WRITTEN','PRACTICAL') NOT NULL DEFAULT 'WRITTEN',
  -- 미니체크
  mini_total    INT          NULL,
  mini_correct  INT          NULL,
  mini_passed   TINYINT(1)   NULL,
  -- MCQ
  mcq_total     INT          NULL,
  mcq_correct   INT          NULL,
  -- 타임스탬프
  updated_at    TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  CONSTRAINT fk_progress_topic FOREIGN KEY (topic_id) REFERENCES topic(id) ON DELETE CASCADE,
  UNIQUE KEY uq_progress (user_id, topic_id, exam_mode),
  INDEX ix_progress_user (user_id),
  INDEX ix_progress_topic (topic_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 사용자 답안 로그 (필기/실기 공용)
CREATE TABLE IF NOT EXISTS user_answer (
  id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id      VARCHAR(100)  NOT NULL,
  question_id  BIGINT        NOT NULL,
  correct      TINYINT(1)    NULL,        -- 필기: 정오 여부 / 실기: 60점 이상 PASS 기준 등으로 기록
  score        INT           NULL,        -- 실기 점수(0~100) / 필기는 NULL
  answer_text  VARCHAR(2000) NULL,        -- 사용자가 입력한 라벨/텍스트
  created_at   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_uans_question FOREIGN KEY (question_id) REFERENCES question(id) ON DELETE CASCADE,
  INDEX ix_uans_user_time (user_id, created_at),
  INDEX ix_uans_question (question_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;
