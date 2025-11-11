// src/main/java/com/OhRyue/certpilot/progress/service/XpService.java
package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.UserXpLedger;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.repository.UserXpLedgerRepository;
import com.OhRyue.certpilot.progress.repository.UserXpWalletRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;

@Service @RequiredArgsConstructor
public class XpService {
  private final UserXpWalletRepository walletRepo;
  private final UserXpLedgerRepository ledgerRepo;

  /** 간이 레벨 계산 규칙: 500xp 당 1레벨 */
  private int calcLevel(long xpTotal) {
    return Math.max(1, (int)(xpTotal / 500) + 1);
  }

  @Transactional
  public UserXpWallet addXp(String userId, int delta, XpReason reason, String refId) {
    if (delta == 0) {
      return walletRepo.findById(userId).orElseGet(() ->
          walletRepo.save(UserXpWallet.builder()
              .userId(userId).xpTotal(0).level(1).build()));
    }
    UserXpWallet w = walletRepo.findById(userId).orElseGet(() ->
        walletRepo.save(UserXpWallet.builder()
            .userId(userId).xpTotal(0).level(1).build()));

    long before = w.getXpTotal();
    w.setXpTotal(before + delta);

    int beforeLv = w.getLevel();
    int newLv = calcLevel(w.getXpTotal());
    if (newLv > beforeLv) {
      w.setLevel(newLv);
      w.setLastLevelupAt(Instant.now());
    }
    walletRepo.save(w);

    ledgerRepo.save(UserXpLedger.builder()
        .userId(userId)
        .delta(delta)
        .reason(reason)
        .refId(refId)
        .createdAt(Instant.now())
        .build());

    return w;
  }

  @Transactional(readOnly = true)
  public UserXpWallet getWallet(String userId){
    return walletRepo.findById(userId).orElse(
        UserXpWallet.builder().userId(userId).xpTotal(0).level(1).build()
    );
  }
}
