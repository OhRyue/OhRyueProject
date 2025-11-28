// src/main/java/com/OhRyue/certpilot/progress/service/XpService.java
package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.UserPointLedger;
import com.OhRyue.certpilot.progress.domain.UserPointWallet;
import com.OhRyue.certpilot.progress.domain.UserXpLedger;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.PointReason;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.repository.UserPointLedgerRepository;
import com.OhRyue.certpilot.progress.repository.UserPointWalletRepository;
import com.OhRyue.certpilot.progress.repository.UserXpLedgerRepository;
import com.OhRyue.certpilot.progress.repository.UserXpWalletRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;

@Slf4j
@Service
@RequiredArgsConstructor
public class XpService {

    private final UserXpWalletRepository walletRepo;
    private final UserXpLedgerRepository ledgerRepo;
    private final UserPointWalletRepository pointWalletRepo;
    private final UserPointLedgerRepository pointLedgerRepo;
    
    // 레벨업 시 지급할 포인트
    private static final int POINTS_PER_LEVEL = 100;

    /**
     * 레벨 계산 규칙: 300 + 레벨 × 50
     * 
     * 레벨별 요구 XP:
     * Lv1 → 2: 350 XP (300 + 1 × 50)
     * Lv2 → 3: 400 XP (300 + 2 × 50)
     * Lv3 → 4: 450 XP (300 + 3 × 50)
     * ...
     * 
     * 누적 XP:
     * Lv1: 0 XP
     * Lv2: 350 XP
     * Lv3: 750 XP (350 + 400)
     * Lv4: 1200 XP (750 + 450)
     * ...
     */
    private int calcLevel(long xpTotal) {
        if (xpTotal < 350) {
            return 1; // Lv1: 0 ~ 349 XP
        }
        
        // 누적 XP를 계산하여 레벨 결정
        long cumulative = 0;
        int level = 1;
        while (true) {
            int requiredXp = 300 + level * 50; // 현재 레벨에서 다음 레벨까지 필요한 XP
            if (cumulative + requiredXp > xpTotal) {
                break; // 다음 레벨에 도달하지 못함
            }
            cumulative += requiredXp;
            level++;
            
            // 무한 루프 방지 (최대 레벨 제한)
            if (level > 1000) {
                break;
            }
        }
        return level;
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
        boolean leveledUp = newLv > beforeLv;
        if (leveledUp) {
            w.setLevel(newLv);
            w.setLastLevelupAt(Instant.now());
            
            // 레벨업 시 포인트 지급
            int levelsGained = newLv - beforeLv;
            int pointsToAward = levelsGained * POINTS_PER_LEVEL;
            awardPointsOnLevelUp(userId, pointsToAward, newLv);
            
            log.info("User {} leveled up from {} to {}! Awarded {} points", userId, beforeLv, newLv, pointsToAward);
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

    /**
     * 레벨업 시 포인트 지급
     */
    private void awardPointsOnLevelUp(String userId, int points, int newLevel) {
        try {
            UserPointWallet pointWallet = pointWalletRepo.findById(userId)
                    .orElseGet(() -> {
                        UserPointWallet newWallet = UserPointWallet.builder()
                                .userId(userId)
                                .pointTotal(0L)
                                .updatedAt(Instant.now())
                                .build();
                        return pointWalletRepo.save(newWallet);
                    });
            
            long beforePoints = pointWallet.getPointTotal();
            pointWallet.setPointTotal(beforePoints + points);
            pointWalletRepo.save(pointWallet);
            
            // 포인트 지급 기록
            pointLedgerRepo.save(UserPointLedger.builder()
                    .userId(userId)
                    .delta(points)
                    .reason(PointReason.REWARD)
                    .refId("levelup:" + newLevel)
                    .createdAt(Instant.now())
                    .build());
            
            log.info("Awarded {} points to user {} for leveling up to level {}", points, userId, newLevel);
        } catch (Exception e) {
            log.error("Failed to award points on level up for user {}: {}", userId, e.getMessage(), e);
            // 포인트 지급 실패는 치명적이지 않으므로 예외를 던지지 않음
        }
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
