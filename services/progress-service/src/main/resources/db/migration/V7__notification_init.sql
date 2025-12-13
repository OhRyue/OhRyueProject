SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

CREATE TABLE user_notification (
    id           BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id      VARCHAR(100) NOT NULL,
    type         VARCHAR(50)  NOT NULL,
    title        VARCHAR(200) NOT NULL,
    message      TEXT         NOT NULL,
    payload_json JSON         NULL,
    is_read      TINYINT(1)   NOT NULL DEFAULT 0,
    created_at   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
    read_at      TIMESTAMP    NULL,
    INDEX idx_user_created (user_id, created_at),
    INDEX idx_user_read (user_id, is_read)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;











