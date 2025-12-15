-- 소셜 로그인 계정 테이블 생성

CREATE TABLE IF NOT EXISTS social_account (
                                              id            BIGINT AUTO_INCREMENT PRIMARY KEY,
                                              user_id       VARCHAR(100) NOT NULL,
    provider      ENUM('GOOGLE','KAKAO','NAVER') NOT NULL,
    provider_id   VARCHAR(255) NOT NULL,
    created_at    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT fk_social_account_user
    FOREIGN KEY (user_id)
    REFERENCES user_account(id)
    ON DELETE CASCADE,

    UNIQUE KEY uk_provider_provider_id (provider, provider_id)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
