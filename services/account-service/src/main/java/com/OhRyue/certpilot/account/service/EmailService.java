package com.OhRyue.certpilot.account.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class EmailService {

  private final JavaMailSender mailSender;

  // 동기 메일 전송
  public void sendEmail(String to, String subject, String text) {
    SimpleMailMessage message = new SimpleMailMessage();
    message.setTo(to);           // 받는 사람 이메일
    message.setSubject(subject); // 제목
    message.setText(text);       // 내용
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

  /**
   * 회원가입 인증코드 메일을 비동기로 전송
   */
  @Async("mailTaskExecutor")
  public void sendVerificationCodeAsync(String to, String code) {
    try {
      log.info("📨 [Email] 인증코드 메일 비동기 전송 시작 - to={}", to);
      sendVerificationCode(to, code);
      log.info("✅ [Email] 인증코드 메일 비동기 전송 완료 - to={}", to);
    } catch (Exception e) {
      // 여기서 예외는 HTTP 응답으로는 안 나가고, 로그로만 남습니다.
      log.error("❌ [Email] 인증코드 메일 전송 실패 - to={}, message={}", to, e.getMessage(), e);
    }
  }
}
