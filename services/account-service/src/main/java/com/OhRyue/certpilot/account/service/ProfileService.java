package com.OhRyue.certpilot.account.service;

import com.OhRyue.certpilot.account.domain.UserProfile;
import com.OhRyue.certpilot.account.repo.UserProfileRepository;
import org.springframework.stereotype.Service;

@Service
public class ProfileService {

  private final UserProfileRepository profileRepository;

  public ProfileService(UserProfileRepository profileRepository) {
    this.profileRepository = profileRepository;
  }

  public UserProfile get(String userId) {
    return profileRepository.findById(userId)
        .orElseGet(() -> profileRepository.save(UserProfile.builder()
            .userId(userId)
            .nickname(defaultNickname(userId))
            .skinId(1L)
            .timezone("Asia/Seoul")
            .lang("ko-KR")
            .onboardingCompleted(false)
            .build()));
  }

  public UserProfile upsert(UserProfile profile) {
    return profileRepository.save(profile);
  }

  public java.util.List<UserProfile> getAll(java.util.Collection<String> userIds) {
    if (userIds == null || userIds.isEmpty()) {
      return java.util.List.of();
    }
    return profileRepository.findAllById(userIds);
  }

  /**
   * 닉네임 중복 확인
   * @param nickname 확인할 닉네임
   * @param excludeUserId 중복 확인에서 제외할 사용자 ID (자기 자신의 닉네임은 제외하기 위해)
   * @return 중복이면 true, 사용 가능하면 false
   */
  public boolean isNicknameDuplicate(String nickname, String excludeUserId) {
    if (nickname == null || nickname.isBlank()) {
      return false;
    }
    return profileRepository.findByNickname(nickname.trim())
        .map(profile -> !profile.getUserId().equals(excludeUserId))
        .orElse(false);
  }

  /**
   * 닉네임 중복 확인 (제외 사용자 없이)
   * @param nickname 확인할 닉네임
   * @return 중복이면 true, 사용 가능하면 false
   */
  public boolean isNicknameDuplicate(String nickname) {
    if (nickname == null || nickname.isBlank()) {
      return false;
    }
    return profileRepository.existsByNickname(nickname.trim());
  }

  private String defaultNickname(String userId) {
    if (userId == null || !userId.contains("@")) {
      return userId;
    }
    return userId.substring(0, userId.indexOf('@'));
  }
}
