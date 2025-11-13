CREATE TABLE post_report (
    id BIGINT PRIMARY KEY AUTO_INCREMENT,
    target_type VARCHAR(20) NOT NULL,
    target_id BIGINT NOT NULL,
    reporter_id VARCHAR(100) NOT NULL,
    reason VARCHAR(500),
    status VARCHAR(20) NOT NULL DEFAULT 'PENDING',
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    resolved_at TIMESTAMP NULL,
    UNIQUE KEY uq_post_report (target_type, target_id, reporter_id)
);

CREATE TABLE user_block (
    user_id VARCHAR(100) NOT NULL,
    blocked_user_id VARCHAR(100) NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (user_id, blocked_user_id)
);

