CREATE TABLE notification_job (
    id BIGINT PRIMARY KEY AUTO_INCREMENT,
    user_id VARCHAR(100) NOT NULL,
    channel VARCHAR(20) NOT NULL,
    type VARCHAR(20) NOT NULL,
    payload_json MEDIUMTEXT,
    status VARCHAR(20) NOT NULL,
    scheduled_at TIMESTAMP NOT NULL,
    sent_at TIMESTAMP NULL,
    retry_count INT NOT NULL DEFAULT 0,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NULL,
    UNIQUE KEY uq_notification_job (user_id, channel, type, scheduled_at),
    INDEX ix_notification_status (status, scheduled_at)
);

