package com.OhRyue.certpilot.account.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.mail.MailAuthenticationException;
import org.springframework.mail.MailException;
import org.springframework.mail.MailSendException;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
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
    String threadName = Thread.currentThread().getName();
    try {
      log.info("📨 [Email] 인증코드 메일 비동기 전송 시작 - to={}, thread={}", to, threadName);

      // SMTP 연결 상태 확인을 위한 상세 로깅
      if (mailSender instanceof JavaMailSenderImpl javaMailSender) {
        log.debug("📧 [Email] SMTP 설정 확인 - host={}, port={}",
            javaMailSender.getHost(), javaMailSender.getPort());
      } else {
        log.debug("📧 [Email] JavaMailSender 구현체 = {}", mailSender.getClass().getName());
      }

      sendVerificationCode(to, code);
      log.info("✅ [Email] 인증코드 메일 비동기 전송 완료 - to={}, thread={}", to, threadName);

    } catch (MailAuthenticationException e) {
      log.error("❌ [Email] 인증코드 메일 전송 실패 - SMTP 인증 오류 - to={}, thread={}, error={}",
          to, threadName, e.getMessage(), e);
      log.error("   → SMTP 인증 정보를 확인해주세요. (username/password)");

    } catch (MailSendException e) {
      log.error("❌ [Email] 인증코드 메일 전송 실패 - SMTP 전송 오류 - to={}, thread={}, error={}",
          to, threadName, e.getMessage(), e);

      // 실패한 주소별 상세 원인
      if (e.getFailedMessages() != null && !e.getFailedMessages().isEmpty()) {
        e.getFailedMessages().forEach((address, exception) ->
            log.error("   → 실패한 주소: {}, 원인: {}", address, exception.getMessage()));
      }

      // root cause 에 SocketTimeout / ConnectException 이 들어오는 경우를 로그로 확인
      Throwable root = e.getRootCause();
      if (root != null) {
        log.error("   → Root cause type={}, message={}",
            root.getClass().getName(), root.getMessage());
      }

      log.error("   → SMTP 서버 연결 및 방화벽 설정을 확인해주세요. (예: smtp.gmail.com:587)");

    } catch (MailException e) {
      // 그 외 Mail 관련 런타임 예외
      log.error("❌ [Email] 인증코드 메일 전송 실패 - MailException - to={}, thread={}, error={}, type={}",
          to, threadName, e.getMessage(), e.getClass().getName(), e);

    } catch (Exception e) {
      log.error("❌ [Email] 인증코드 메일 전송 실패 - 예상치 못한 오류 - to={}, thread={}, error={}, type={}",
          to, threadName, e.getMessage(), e.getClass().getName(), e);
      log.error("   → 전체 스택 트레이스:", e);
    }
  }
}
