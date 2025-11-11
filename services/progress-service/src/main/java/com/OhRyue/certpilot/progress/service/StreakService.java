package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.UserStreak;
import com.OhRyue.certpilot.progress.repository.UserStreakRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.ZoneId;

@Service @RequiredArgsConstructor
public class StreakService {
  private static final ZoneId KST = ZoneId.of("Asia/Seoul");
  private final UserStreakRepository streakRepo;

  /** 오늘 활동 처리: 연속일수 갱신 */
  @Transactional
  public UserStreak tickToday(String userId) {
    LocalDate today = LocalDate.now(KST);
    UserStreak s = streakRepo.findById(userId).orElse(
        UserStreak.builder().userId(userId).currentDays(0).bestDays(0).lastActiveDate(null).build()
    );

    if (s.getLastActiveDate() == null) {
      s.setCurrentDays(1);
      s.setBestDays(Math.max(s.getBestDays(), 1));
    } else {
      LocalDate prev = s.getLastActiveDate();
      if (prev.equals(today)) {
        // 같은 날 중복 호출 → 변화 없음
        return s;
      } else if (prev.plusDays(1).equals(today)) {
        s.setCurrentDays(s.getCurrentDays() + 1);
        s.setBestDays(Math.max(s.getBestDays(), s.getCurrentDays()));
      } else {
        // 끊김
        s.setCurrentDays(1);
      }
    }
    s.setLastActiveDate(today);
    return streakRepo.save(s);
  }

  @Transactional(readOnly = true)
  public UserStreak get(String userId){
    return streakRepo.findById(userId).orElse(
        UserStreak.builder().userId(userId).currentDays(0).bestDays(0).build()
    );
  }
}
