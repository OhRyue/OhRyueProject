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

@Service
@RequiredArgsConstructor
public class XpService {

    private final UserXpWalletRepository walletRepo;
    private final UserXpLedgerRepository ledgerRepo;

    /** 간이 레벨 계산 규칙: 500xp 당 1레벨 */
    private int calcLevel(long xpTotal) {
        return Math.max(1, (int) (xpTotal / 500) + 1);
    }

    /**
     * XP 지급 (idempotent 버전)
     *
     * - delta == 0 이면 단순히 지갑 조회/생성만 합니다.
     * - refId 가 주어졌을 때,
     *   같은 (userId, reason, refId) 조합의 기록이 이미 있으면 XP를 추가로 주지 않습니다.
     *
     */
    @Transactional
    public UserXpWallet addXp(String userId, int delta, XpReason reason, String refId) {
        // delta 0이면 XP는 안 오르되, 지갑이 없으면 생성
        if (delta == 0) {
            return walletRepo.findById(userId).orElseGet(() ->
                    walletRepo.save(UserXpWallet.builder()
                            .userId(userId)
                            .xpTotal(0)
                            .level(1)
                            .build()));
        }

        // idempotency: refId 가 있고, 동일 (userId, reason, refId) 가 이미 있으면 XP 스킵
        if (refId != null && !refId.isBlank()) {
            boolean exists = ledgerRepo.existsByUserIdAndReasonAndRefId(userId, reason, refId);
            if (exists) {
                // 이미 이 refId로 XP를 준 적이 있으므로, 현재 지갑 상태만 반환
                return walletRepo.findById(userId).orElseGet(() ->
                        walletRepo.save(UserXpWallet.builder()
                                .userId(userId)
                                .xpTotal(0)
                                .level(1)
                                .build()));
            }
        }

        // 실제 지갑 조회/생성
        UserXpWallet w = walletRepo.findById(userId).orElseGet(() ->
                walletRepo.save(UserXpWallet.builder()
                        .userId(userId)
                        .xpTotal(0)
                        .level(1)
                        .build()));

        long before = w.getXpTotal();
        w.setXpTotal(before + delta);

        int beforeLv = w.getLevel();
        int newLv = calcLevel(w.getXpTotal());
        if (newLv > beforeLv) {
            w.setLevel(newLv);
            w.setLastLevelupAt(Instant.now());
        }
        walletRepo.save(w);

        // ledger 기록 (idempotency는 위에서 체크)
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
    public UserXpWallet getWallet(String userId) {
        return walletRepo.findById(userId).orElse(
                UserXpWallet.builder()
                        .userId(userId)
                        .xpTotal(0)
                        .level(1)
                        .build()
        );
    }
}
