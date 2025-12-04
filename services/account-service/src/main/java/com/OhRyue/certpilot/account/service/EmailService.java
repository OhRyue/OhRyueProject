package com.OhRyue.certpilot.account.service;

import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.mail.MailAuthenticationException;
import org.springframework.mail.MailException;
import org.springframework.mail.MailSendException;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;
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
    String htmlBody = buildVerificationCodeTemplate(code);
    
    try {
      log.info("📨 [Email] 인증코드 HTML 메일 발송 시작 - to={}", to);

      if (mailSender instanceof JavaMailSenderImpl javaMailSender) {
        log.debug("📧 [Email] SMTP 설정 확인 - host={}, port={}, username={}",
            javaMailSender.getHost(), javaMailSender.getPort(), javaMailSender.getUsername());
      } else {
        log.debug("📧 [Email] JavaMailSender 구현체 = {}", mailSender.getClass().getName());
      }

      MimeMessage message = mailSender.createMimeMessage();
      MimeMessageHelper helper = new MimeMessageHelper(message, true, "UTF-8");

      helper.setTo(to);
      helper.setSubject(subject);
      helper.setText(htmlBody, true); // true for HTML content

      mailSender.send(message);

      log.info("✅ [Email] 인증코드 HTML 메일 발송 완료 - to={}, subject={}", to, subject);

    } catch (MailAuthenticationException e) {
      log.error("❌ [Email] 인증코드 HTML 메일 전송 실패 - SMTP 인증 오류 - to={}, error={}",
          to, e.getMessage(), e);
      log.error("   → SMTP 인증 정보를 확인해주세요. (username/password)");
      throw new RuntimeException("메일 전송 실패: SMTP 인증 오류", e);

    } catch (MailSendException e) {
      log.error("❌ [Email] 인증코드 HTML 메일 전송 실패 - SMTP 전송 오류 - to={}, error={}",
          to, e.getMessage(), e);
      if (e.getFailedMessages() != null && !e.getFailedMessages().isEmpty()) {
        e.getFailedMessages().forEach((address, exception) ->
            log.error("   → 실패한 주소: {}, 원인: {}", address, exception.getMessage()));
      }
      Throwable root = e.getRootCause();
      if (root != null) {
        log.error("   → Root cause type={}, message={}",
            root.getClass().getName(), root.getMessage());
      }
      log.error("   → SMTP 서버 연결 및 방화벽 설정을 확인해주세요. (예: smtp.gmail.com:587)");
      throw new RuntimeException("메일 전송 실패: SMTP 전송 오류", e);

    } catch (MailException e) {
      log.error("❌ [Email] 인증코드 HTML 메일 전송 실패 - MailException - to={}, error={}, type={}",
          to, e.getMessage(), e.getClass().getName(), e);
      throw new RuntimeException("메일 전송 실패: " + e.getMessage(), e);

    } catch (MessagingException e) {
      log.error("❌ [Email] 인증코드 HTML 메일 전송 실패 - MessagingException - to={}, error={}",
          to, e.getMessage(), e);
      throw new RuntimeException("메일 전송 실패: 메시지 생성 오류", e);

    } catch (Exception e) {
      log.error("❌ [Email] 인증코드 HTML 메일 전송 실패 - 예상치 못한 오류 - to={}, error={}, type={}",
          to, e.getMessage(), e.getClass().getName(), e);
      throw new RuntimeException("메일 전송 실패: " + e.getMessage(), e);
    }
  }

  /**
   * 이메일 인증 코드 HTML 템플릿 생성
   */
  private String buildVerificationCodeTemplate(String code) {
    return """
        <!DOCTYPE html>
        <html lang="ko">
        <head>
          <meta charset="UTF-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1.0" />
          <title>CertPilot 이메일 인증 코드</title>
        </head>
        <body style="margin:0; padding:0; background-color:#f5f5f7; font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,'Helvetica Neue',Arial,sans-serif;">
          <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%%" style="background-color:#f5f5f7; padding:24px 0;">
            <tr>
              <td align="center">
                <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="480" style="max-width:480px; background-color:#ffffff; border-radius:16px; overflow:hidden; box-shadow:0 2px 8px rgba(0,0,0,0.08);">
                  <!-- Header -->
                  <tr>
                    <td style="background:linear-gradient(135deg, #6366f1 0%%, #8b5cf6 100%%); padding:24px 20px; text-align:center;">
                      <h1 style="margin:0; color:#ffffff; font-size:20px; font-weight:600;">CertPilot 이메일 인증</h1>
                      <p style="margin:8px 0 0; color:rgba(255,255,255,0.9); font-size:13px;">
                        계정을 안전하게 보호하기 위한 인증 코드입니다.
                      </p>
                    </td>
                  </tr>

                  <!-- Body -->
                  <tr>
                    <td style="padding:24px 20px 20px;">
                      <p style="margin:0 0 10px; color:#1f2933; font-size:15px; line-height:1.5;">
                        안녕하세요!<br/>
                        요청하신 이메일 인증 코드를 안내드립니다.
                      </p>
                      <p style="margin:0 0 16px; color:#6b7280; font-size:13px; line-height:1.5;">
                        아래 인증 코드를 CertPilot 화면에 입력해 주세요.
                      </p>

                      <!-- 코드 박스 -->
                      <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%%" style="margin:8px 0 4px;">
                        <tr>
                          <td style="background-color:#f9fafb; border-radius:12px; border:1px solid #e5e7eb; padding:16px 12px; text-align:center;">
                            <div style="font-size:26px; letter-spacing:0.28em; font-weight:700; color:#111827;">
                              %s
                            </div>
                          </td>
                        </tr>
                      </table>

                      <!-- 안내 문구 -->
                      <p style="margin:12px 0 0; color:#ef4444; font-size:12px;">
                        ⏰ 이 코드는 발송 시점 기준 <strong>10분 후 만료</strong>됩니다.
                      </p>
                      <p style="margin:6px 0 0; color:#9ca3af; font-size:12px; line-height:1.5;">
                        본인이 요청한 게 아니라면, 이 메일은 무시하셔도 됩니다.
                      </p>
                    </td>
                  </tr>

                  <!-- Footer -->
                  <tr>
                    <td style="padding:16px 20px 18px; border-top:1px solid #f3f4f6; text-align:left;">
                      <p style="margin:0 0 4px; color:#6b7280; font-size:12px;">
                        언제나 안전한 학습 환경을 위해 노력하겠습니다.
                      </p>
                      <p style="margin:0; color:#9ca3af; font-size:11px;">
                        &copy; CertPilot 팀 드림
                      </p>
                    </td>
                  </tr>

                </table>
              </td>
            </tr>
          </table>
        </body>
        </html>
        """.formatted(code);
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
