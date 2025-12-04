package com.OhRyue.certpilot.progress.service.mail;

/**
 * 메일 발송 인터페이스
 */
public interface MailSender {
    void send(String to, String subject, String body);
    
    /**
     * HTML 메일 발송
     * @param to 수신자 이메일
     * @param subject 제목
     * @param htmlBody HTML 본문
     */
    void sendHtml(String to, String subject, String htmlBody);
}

