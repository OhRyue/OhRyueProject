package com.OhRyue.certpilot.progress.service.mail;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.mail.MailAuthenticationException;
import org.springframework.mail.MailException;
import org.springframework.mail.MailSendException;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Component;

import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;

/**
 * 실제 SMTP를 통한 메일 발송기
 * mail.enabled=true일 때 활성화됩니다.
 */
@Slf4j
@Component
@RequiredArgsConstructor
@ConditionalOnProperty(name = "mail.enabled", havingValue = "true")
public class SmtpMailSender implements MailSender {

    private final JavaMailSender mailSender;

    @Override
    public void send(String to, String subject, String body) {
        try {
            log.info("📨 [Email] 메일 발송 시작 - to={}", to);

            // SMTP 연결 상태 확인을 위한 상세 로깅
            if (mailSender instanceof JavaMailSenderImpl javaMailSender) {
                log.debug("📧 [Email] SMTP 설정 확인 - host={}, port={}, username={}",
                        javaMailSender.getHost(), javaMailSender.getPort(), javaMailSender.getUsername());
            } else {
                log.debug("📧 [Email] JavaMailSender 구현체 = {}", mailSender.getClass().getName());
            }

            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(to);
            message.setSubject(subject);
            message.setText(body);
            mailSender.send(message);

            log.info("✅ [Email] 메일 발송 완료 - to={}, subject={}", to, subject);

        } catch (MailAuthenticationException e) {
            log.error("❌ [Email] 메일 전송 실패 - SMTP 인증 오류 - to={}, error={}",
                    to, e.getMessage(), e);
            log.error("   → SMTP 인증 정보를 확인해주세요. (username/password)");
            throw new RuntimeException("메일 전송 실패: SMTP 인증 오류", e);

        } catch (MailSendException e) {
            log.error("❌ [Email] 메일 전송 실패 - SMTP 전송 오류 - to={}, error={}",
                    to, e.getMessage(), e);

            // 실패한 주소별 상세 원인
            if (e.getFailedMessages() != null && !e.getFailedMessages().isEmpty()) {
                e.getFailedMessages().forEach((address, exception) ->
                        log.error("   → 실패한 주소: {}, 원인: {}", address, exception.getMessage()));
            }

            // root cause 확인
            Throwable root = e.getRootCause();
            if (root != null) {
                log.error("   → Root cause type={}, message={}",
                        root.getClass().getName(), root.getMessage());
            }

            log.error("   → SMTP 서버 연결 및 방화벽 설정을 확인해주세요. (예: smtp.gmail.com:587)");
            throw new RuntimeException("메일 전송 실패: SMTP 전송 오류", e);

        } catch (MailException e) {
            log.error("❌ [Email] 메일 전송 실패 - MailException - to={}, error={}, type={}",
                    to, e.getMessage(), e.getClass().getName(), e);
            throw new RuntimeException("메일 전송 실패: " + e.getMessage(), e);

        } catch (Exception e) {
            log.error("❌ [Email] 메일 전송 실패 - 예상치 못한 오류 - to={}, error={}, type={}",
                    to, e.getMessage(), e.getClass().getName(), e);
            throw new RuntimeException("메일 전송 실패: " + e.getMessage(), e);
        }
    }

    @Override
    public void sendHtml(String to, String subject, String htmlBody) {
        try {
            log.info("📨 [Email] HTML 메일 발송 시작 - to={}", to);

            MimeMessage mimeMessage = mailSender.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true, "UTF-8");
            
            helper.setTo(to);
            helper.setSubject(subject);
            helper.setText(htmlBody, true); // true = HTML 모드

            mailSender.send(mimeMessage);

            log.info("✅ [Email] HTML 메일 발송 완료 - to={}, subject={}", to, subject);

        } catch (MessagingException e) {
            log.error("❌ [Email] HTML 메일 전송 실패 - to={}, error={}", to, e.getMessage(), e);
            throw new RuntimeException("HTML 메일 전송 실패: " + e.getMessage(), e);
        } catch (Exception e) {
            log.error("❌ [Email] HTML 메일 전송 실패 - 예상치 못한 오류 - to={}, error={}", 
                    to, e.getMessage(), e);
            throw new RuntimeException("HTML 메일 전송 실패: " + e.getMessage(), e);
        }
    }
}

