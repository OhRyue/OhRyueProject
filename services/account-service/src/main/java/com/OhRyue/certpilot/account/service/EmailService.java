package com.OhRyue.certpilot.account.service;

import lombok.RequiredArgsConstructor;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class EmailService {

    private final JavaMailSender mailSender;

    public void sendEmail(String to, String subject, String text) {
        SimpleMailMessage message = new SimpleMailMessage();
        message.setTo(to);                  // 받는 사람 이메일
        message.setSubject(subject);        // 제목
        message.setText(text);              // 내용
        mailSender.send(message);
    }
}
