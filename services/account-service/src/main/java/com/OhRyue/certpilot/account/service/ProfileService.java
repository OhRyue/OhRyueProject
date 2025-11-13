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
            .avatarUrl(null)
            .timezone("Asia/Seoul")
            .lang("ko-KR")
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

  private String defaultNickname(String userId) {
    if (userId == null || !userId.contains("@")) {
      return userId;
    }
    return userId.substring(0, userId.indexOf('@'));
  }
}
