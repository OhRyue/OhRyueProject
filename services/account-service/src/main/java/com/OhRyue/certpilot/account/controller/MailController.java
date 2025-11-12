package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.service.EmailService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/mail")
@RequiredArgsConstructor
public class MailController {

    private final EmailService emailService;

    @PostMapping("/send")
    public ResponseEntity<?> sendTestMail() {
        emailService.sendEmail(
                "test@test.com",
                "CertPilot SMTP Test",
                "테스트 메일입니다."
        );
        return ResponseEntity.ok("메일 발송 완료!");
    }
}
