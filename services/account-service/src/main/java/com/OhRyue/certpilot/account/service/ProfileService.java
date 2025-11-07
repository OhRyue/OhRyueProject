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
        .orElseGet(() -> UserProfile.builder()
            .userId(userId)
            .nickname(userId) // 기본값
            .avatarUrl(null)
            .timezone("Asia/Seoul")
            .lang("ko-KR")
            .build());
  }

  public UserProfile upsert(UserProfile profile) {
    return profileRepository.save(profile);
  }
}
