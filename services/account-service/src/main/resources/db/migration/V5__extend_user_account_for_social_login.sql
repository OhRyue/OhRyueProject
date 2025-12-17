-- user_account 테이블 확장 (소셜 로그인 대응)

ALTER TABLE user_account
    MODIFY email VARCHAR(255) NULL,
    MODIFY password_hash VARCHAR(255) NULL,
    ADD signup_type ENUM('EMAIL','SOCIAL') NOT NULL DEFAULT 'EMAIL';
