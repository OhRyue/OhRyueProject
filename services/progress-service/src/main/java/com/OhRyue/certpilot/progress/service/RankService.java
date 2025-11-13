package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.UserRankScore;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.repository.UserRankScoreRepository;
import com.OhRyue.certpilot.progress.repository.UserXpLedgerRepository;
import com.OhRyue.certpilot.progress.repository.UserXpWalletRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RankService {
  private final UserRankScoreRepository rankRepo;
  private final UserXpWalletRepository walletRepo;
  private final UserXpLedgerRepository ledgerRepo;

  @Transactional
  public UserRankScore recomputeForUser(String userId){
    return recomputeInternal(userId);
  }

  @Transactional
  public long recomputeAll() {
    List<UserXpWallet> wallets = walletRepo.findAll();
    wallets.forEach(wallet -> recomputeInternal(wallet.getUserId()));

    Set<String> walletIds = wallets.stream()
        .map(UserXpWallet::getUserId)
        .collect(Collectors.toSet());

    rankRepo.findAll().stream()
        .map(UserRankScore::getUserId)
        .filter(userId -> !walletIds.contains(userId))
        .forEach(this::recomputeInternal);

    return rankRepo.count();
  }

  @Transactional
  public List<UserRankScore> topN(int n){
    List<UserRankScore> initial = rankRepo.findTop10ByOrderByScoreDesc();
    initial.forEach(score -> recomputeInternal(score.getUserId()));
    List<UserRankScore> refreshed = rankRepo.findTop10ByOrderByScoreDesc();
    return refreshed.size() <= n ? refreshed : refreshed.subList(0, n);
  }

  @Transactional(readOnly = true)
  public UserRankScore getScore(String userId) {
    recomputeInternal(userId);
    return rankRepo.findById(userId).orElse(null);
  }

  @Transactional(readOnly = true)
  public int resolveRank(UserRankScore score) {
    if (score == null) {
      return -1;
    }
    long higher = rankRepo.countByScoreGreaterThan(score.getScore());
    return (int) higher + 1;
  }

  private UserRankScore recomputeInternal(String userId) {
    Instant now = Instant.now();
    UserXpWallet wallet = walletRepo.findById(userId).orElse(null);
    long totalXp = wallet == null ? 0 : wallet.getXpTotal();
    long recent30 = ledgerRepo.sumDeltaSince(userId, now.minusSeconds(30L * 24 * 3600));
    long recent7 = ledgerRepo.sumDeltaSince(userId, now.minusSeconds(7L * 24 * 3600));

    double score = totalXp * 0.7 + recent30 * 1.3 + recent7 * 0.5;

    UserRankScore rank = rankRepo.findById(userId).orElse(
        UserRankScore.builder().userId(userId).score(0).lastUpdatedAt(now).build()
    );
    rank.setScore(Math.round(score));
    rank.setLastUpdatedAt(now);
    return rankRepo.save(rank);
  }
}
