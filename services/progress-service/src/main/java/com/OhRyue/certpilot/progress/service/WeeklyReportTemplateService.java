package com.OhRyue.certpilot.progress.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Random;

/**
 * 주간 학습 리포트 HTML 템플릿 생성 서비스
 */
@Slf4j
@Service
public class WeeklyReportTemplateService {

    private static final Random RANDOM = new Random();

    // 문제수 50개 이상일 경우 메시지
    private static final List<String> HIGH_ACHIEVEMENT_MESSAGES = List.of(
            "이번 주 정말 강해지고 있어요! 🔥",
            "계속 이렇게만 가봅시다! ⚡",
            "실력이 눈에 띄게 올라가고 있어요! 📈",
            "이번 주 정말 많이 성장했어요 💖",
            "스스로에게 박수를 보내주세요! 정말 훌륭해요 👏"
    );

    // 기본 메시지
    private static final List<String> DEFAULT_MESSAGES = List.of(
            "이번 주도 화이팅하세요! 🚀",
            "오늘도 한 걸음씩 나아가 봅시다! 🌈",
            "항상 응원하고 있어요! 함께 가요 🤗",
            "꾸준함은 누구도 이길 수 없습니다! ✨",
            "작은 루틴이 큰 변화를 만들어요! 💫",
            "지속하는 힘이 실력을 만듭니다. 계속 함께 가요! 💪"
    );

    // 0문제일 경우 메시지
    private static final List<String> ZERO_PROBLEM_MESSAGES = List.of(
            "매일의 한 걸음이 가장 큰 힘이 됩니다 🌱",
            "작은 노력들이 모여 큰 결과가 됩니다 💡",
            "다시 시작해볼까요? 시작이 반이에요 🏁"
    );

    // 배지 획득시 메시지
    private static final List<String> BADGE_MESSAGES = List.of(
            "새 배지를 획득했어요! 축하드려요 🎉✨",
            "당신은 배지수집가! 멋져요 🏅✨"
    );

    /**
     * 학습 통계에 따라 동기부여 메시지를 선택합니다.
     * 우선순위:
     * 1) solvedCount == 0 → 0문제 메시지
     * 2) newBadges > 0 → 배지 획득 메시지
     * 3) solvedCount >= 50 → 성장형 메시지
     * 4) 그 외 → 기본 메시지
     */
    public String pickMotivationMessage(int totalSolved, int newBadgesCount) {
        List<String> messages;
        
        if (totalSolved == 0) {
            messages = ZERO_PROBLEM_MESSAGES;
        } else if (newBadgesCount > 0) {
            messages = BADGE_MESSAGES;
        } else if (totalSolved >= 50) {
            messages = HIGH_ACHIEVEMENT_MESSAGES;
        } else {
            messages = DEFAULT_MESSAGES;
        }
        
        return messages.get(RANDOM.nextInt(messages.size()));
    }

    /**
     * 카드형 HTML 템플릿 생성
     */
    public String buildCardTemplate(String nickname, String weekIso, 
                                    int totalSolved, int totalCorrect, double accuracy,
                                    int totalStudyMinutes, int newBadgesCount, int streakDays) {
        String motivationMessage = pickMotivationMessage(totalSolved, newBadgesCount);
        
        return """
            <!DOCTYPE html>
            <html lang="ko">
            <head>
              <meta charset="UTF-8" />
              <meta name="viewport" content="width=device-width, initial-scale=1.0" />
              <title>CertPilot 주간 학습 리포트</title>
            </head>
            <body style="margin:0; padding:0; background-color:#f5f5f7; font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,'Helvetica Neue',Arial,sans-serif;">
              <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%%" style="background-color:#f5f5f7; padding:24px 0;">
                <tr>
                  <td align="center">
                    <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="600" style="max-width:600px; background-color:#ffffff; border-radius:16px; overflow:hidden; box-shadow:0 2px 8px rgba(0,0,0,0.1);">
                      <!-- Header -->
                      <tr>
                        <td style="background:linear-gradient(135deg, #667eea 0%%, #764ba2 100%%); padding:32px 24px; text-align:center;">
                          <h1 style="margin:0; color:#ffffff; font-size:24px; font-weight:600;">CertPilot 주간 학습 리포트</h1>
                          <p style="margin:8px 0 0; color:rgba(255,255,255,0.9); font-size:14px;">%s</p>
                        </td>
                      </tr>
                      
                      <!-- Greeting -->
                      <tr>
                        <td style="padding:32px 24px 24px;">
                          <p style="margin:0 0 16px; color:#1d1d1f; font-size:16px; line-height:1.5;">
                            안녕하세요, <strong style="color:#667eea;">%s</strong>님!
                          </p>
                          <p style="margin:0; color:#6e6e73; font-size:14px; line-height:1.5;">
                            지난 주 학습 통계를 확인해보세요.
                          </p>
                        </td>
                      </tr>
                      
                      <!-- Stats Cards -->
                      <tr>
                        <td style="padding:0 24px 24px;">
                          <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%%">
                            <tr>
                              <td width="50%%" style="padding-right:8px;">
                                <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%%" style="background:#f5f5f7; border-radius:12px;">
                                  <tr>
                                    <td style="padding:20px; text-align:center;">
                                      <div style="font-size:32px; font-weight:700; color:#1d1d1f; margin-bottom:4px;">%d</div>
                                      <div style="font-size:12px; color:#6e6e73;">해결한 문제</div>
                                    </td>
                                  </tr>
                                </table>
                              </td>
                              <td width="50%%" style="padding-left:8px;">
                                <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%%" style="background:#f5f5f7; border-radius:12px;">
                                  <tr>
                                    <td style="padding:20px; text-align:center;">
                                      <div style="font-size:32px; font-weight:700; color:#1d1d1f; margin-bottom:4px;">%.1f%%</div>
                                      <div style="font-size:12px; color:#6e6e73;">평균 정답률</div>
                                    </td>
                                  </tr>
                                </table>
                              </td>
                            </tr>
                            <tr>
                              <td colspan="2" style="padding-top:8px;">
                                <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%%" style="background:#f5f5f7; border-radius:12px;">
                                  <tr>
                                    <td style="padding:20px; text-align:center;">
                                      <div style="font-size:32px; font-weight:700; color:#1d1d1f; margin-bottom:4px;">%d분</div>
                                      <div style="font-size:12px; color:#6e6e73;">총 학습 시간</div>
                                    </td>
                                  </tr>
                                </table>
                              </td>
                            </tr>
                          </table>
                        </td>
                      </tr>
                      
                      <!-- Badges & Streak -->
                      <tr>
                        <td style="padding:0 24px 24px;">
                          <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%%">
                            <tr>
                              <td width="50%%" style="padding-right:8px;">
                                <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%%" style="background:#fff4e6; border-radius:12px; border:1px solid #ffe0b2;">
                                  <tr>
                                    <td style="padding:16px; text-align:center;">
                                      <div style="font-size:24px; font-weight:700; color:#e65100; margin-bottom:4px;">%d개</div>
                                      <div style="font-size:12px; color:#bf360c;">새로 획득한 배지</div>
                                    </td>
                                  </tr>
                                </table>
                              </td>
                              <td width="50%%" style="padding-left:8px;">
                                <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%%" style="background:#e8f5e9; border-radius:12px; border:1px solid #c8e6c9;">
                                  <tr>
                                    <td style="padding:16px; text-align:center;">
                                      <div style="font-size:24px; font-weight:700; color:#2e7d32; margin-bottom:4px;">%d일</div>
                                      <div style="font-size:12px; color:#1b5e20;">연속 학습 일수</div>
                                    </td>
                                  </tr>
                                </table>
                              </td>
                            </tr>
                          </table>
                        </td>
                      </tr>
                      
                      <!-- Footer -->
                      <tr>
                        <td style="padding:24px; text-align:center; border-top:1px solid #f5f5f7;">
                          <p style="margin:0 0 8px; color:#1d1d1f; font-size:16px; font-weight:500;">
                            %s
                          </p>
                          <p style="margin:0 0 12px; color:#9ca3af; font-size:12px;">
                            &copy; CertPilot 팀 드림
                          </p>
                          <p style="margin:0; color:#9ca3af; font-size:11px; line-height:1.5;">
                            알림 설정은 [설정 &gt; 알림]에서 변경하실 수 있어요.
                          </p>
                        </td>
                      </tr>
                    </table>
                  </td>
                </tr>
              </table>
            </body>
            </html>
            """.formatted(weekIso, nickname, totalSolved, accuracy, totalStudyMinutes, newBadgesCount, streakDays, motivationMessage);
    }
}

