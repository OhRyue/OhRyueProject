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

  public void sendVerificationCode(String to, String code) {
    String subject = "[CertPilot] 이메일 인증 코드";
    String text =
        "안녕하세요!\n\n" +
            "요청하신 인증 코드는 아래와 같습니다.\n\n" +
            "✅ 인증 코드: " + code + "\n\n" +
            "10분 이내에 입력하지 않으면 만료됩니다.";

    sendEmail(to, subject, text);
  }
}
