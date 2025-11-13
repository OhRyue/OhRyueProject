package com.OhRyue.certpilot.community.service;

import com.OhRyue.certpilot.community.domain.PostViewLog;
import com.OhRyue.certpilot.community.repository.PostViewLogRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Clock;
import java.time.Instant;

@Service
@RequiredArgsConstructor
public class ViewLogService {

  private final PostViewLogRepository viewLogRepository;
  private final Clock clock = Clock.systemUTC();

  @Transactional
  public void record(Long postId, String userId) {
    PostViewLog log = new PostViewLog();
    log.setPostId(postId);
    log.setUserId(userId);
    log.setViewedAt(Instant.now(clock));
    viewLogRepository.save(log);
  }
}


