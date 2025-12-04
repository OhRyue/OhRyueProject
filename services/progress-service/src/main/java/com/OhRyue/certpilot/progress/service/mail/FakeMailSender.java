package com.OhRyue.certpilot.progress.service.mail;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

/**
 * 개발/테스트용 가짜 메일 발송기
 * 실제 SMTP 서버 없이 로그만 출력합니다.
 * mail.enabled=false이거나 설정이 없을 때 활성화됩니다.
 */
@Slf4j
@Component
@ConditionalOnProperty(name = "mail.enabled", havingValue = "false", matchIfMissing = true)
public class FakeMailSender implements MailSender {

    @Override
    public void send(String to, String subject, String body) {
        log.info("=== FAKE EMAIL SENT ===");
        log.info("To: {}", to);
        log.info("Subject: {}", subject);
        log.info("Body:\n{}", body);
        log.info("======================");
    }

    @Override
    public void sendHtml(String to, String subject, String htmlBody) {
        log.info("=== FAKE HTML EMAIL SENT ===");
        log.info("To: {}", to);
        log.info("Subject: {}", subject);
        log.info("HTML Body:\n{}", htmlBody);
        log.info("============================");
    }
}

