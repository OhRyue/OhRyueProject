package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.UserRankScore;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.repository.UserRankScoreRepository;
import com.OhRyue.certpilot.progress.repository.UserXpWalletRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.List;

@Service @RequiredArgsConstructor
public class RankService {
  private final UserRankScoreRepository rankRepo;
  private final UserXpWalletRepository walletRepo;

  /** 간이 랭킹: score = xp_total (필요시 가중치 적용) */
  @Transactional
  public UserRankScore recomputeForUser(String userId){
    UserXpWallet w = walletRepo.findById(userId).orElse(null);
    long score = (w==null?0:w.getXpTotal());
    UserRankScore r = rankRepo.findById(userId).orElse(
        UserRankScore.builder().userId(userId).score(0).lastUpdatedAt(Instant.now()).build()
    );
    r.setScore(score);
    r.setLastUpdatedAt(Instant.now());
    return rankRepo.save(r);
  }

  @Transactional(readOnly = true)
  public List<UserRankScore> topN(int n){
    List<UserRankScore> top10 = rankRepo.findTop10ByOrderByScoreDesc();
    return top10.size()<=n? top10 : top10.subList(0,n);
  }
}
