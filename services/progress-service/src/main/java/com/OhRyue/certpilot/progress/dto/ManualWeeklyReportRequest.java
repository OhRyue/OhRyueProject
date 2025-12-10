package com.OhRyue.certpilot.progress.dto;

import lombok.Getter;
import lombok.Setter;

/**
 * 테스트용 주간 리포트 메일 수동 발송 요청 DTO
 */
@Getter
@Setter
public class ManualWeeklyReportRequest {
    /**
     * 수신 받을 이메일 주소
     */
    private String email;
}

