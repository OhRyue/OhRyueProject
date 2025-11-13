package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.MatchPhase;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

@Service
@RequiredArgsConstructor
@Slf4j
public class VersusRealtimeService {

  private static final String CACHE_NAME = "versus-scoreboard";

  private final CacheManager cacheManager;

  private final ConcurrentMap<Long, Snapshot> localSnapshots = new ConcurrentHashMap<>();

  public void pushSnapshot(Long roomId,
                           VersusDtos.ScoreBoardResp scoreboard,
                           Integer roundNo,
                           MatchPhase phase) {
    Snapshot snapshot = new Snapshot(scoreboard, roundNo, phase, Instant.now());
    Cache cache = cacheManager.getCache(CACHE_NAME);
    if (cache != null) {
      try {
        cache.put(roomId, snapshot);
        return;
      } catch (Exception e) {
        log.warn("Failed to cache scoreboard snapshot for room {} via cache manager: {}", roomId, e.getMessage());
      }
    }
    localSnapshots.put(roomId, snapshot);
  }

  public Optional<Snapshot> loadSnapshot(Long roomId) {
    Cache cache = cacheManager.getCache(CACHE_NAME);
    if (cache != null) {
      try {
        Snapshot snapshot = cache.get(roomId, Snapshot.class);
        if (snapshot != null) {
          return Optional.of(snapshot);
        }
      } catch (Exception e) {
        log.warn("Failed to read scoreboard snapshot for room {} via cache manager: {}", roomId, e.getMessage());
      }
    }
    return Optional.ofNullable(localSnapshots.get(roomId));
  }

  public record Snapshot(
      VersusDtos.ScoreBoardResp scoreboard,
      Integer roundNo,
      MatchPhase phase,
      Instant updatedAt
  ) {}
}

