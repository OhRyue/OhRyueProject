// src/main/java/com/OhRyue/certpilot/progress/service/XpService.java
package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.UserPointLedger;
import com.OhRyue.certpilot.progress.domain.UserPointWallet;
import com.OhRyue.certpilot.progress.domain.UserXpLedger;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.PointReason;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.dto.XpDtos;
import com.OhRyue.certpilot.progress.repository.UserPointLedgerRepository;
import com.OhRyue.certpilot.progress.repository.UserPointWalletRepository;
import com.OhRyue.certpilot.progress.repository.UserXpLedgerRepository;
import com.OhRyue.certpilot.progress.repository.UserXpWalletRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.Map;
import static java.util.Map.entry;

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
    
    // XP 규칙 테이블: activityType → 기본 XP (100% 완벽 클리어 시)
    private static final Map<String, Integer> BASE_XP_RULES = Map.ofEntries(
            entry("WRITTEN_MICRO", 150),
            entry("PRACTICAL_MICRO", 200),
            entry("WRITTEN_REVIEW", 200),
            entry("PRACTICAL_REVIEW", 250),
            entry("ASSIST_CORRECT", 5),
            entry("ASSIST_WRONG", 0),
            entry("DUEL_WIN", 30),
            entry("DUEL_JOIN", 5),
            entry("TOURNAMENT_WIN", 100),
            entry("TOURNAMENT_JOIN", 10),
            entry("GOLDENBELL_WIN", 200),
            entry("GOLDENBELL_JOIN", 20)
    );
    
    /**
     * 정답률 기반 XP 계산
     * - 100%: 활동별 기본 XP
     * - 80% ~ 99%: 50 XP
     * - 30% ~ 79%: 30 XP
     * - 0% ~ 29%: 10 XP
     * 
     * @param activityType 활동 타입
     * @param scorePct 정답률 (0.0 ~ 100.0)
     * @return 지급할 XP
     */
    private int calculateXpByScore(String activityType, Double scorePct) {
        log.info("[calculateXpByScore] Called with: activityType={}, scorePct={}", activityType, scorePct);
        
        // 메인학습 활동인지 확인
        boolean isMainLearning = activityType != null && (
            activityType.equals("WRITTEN_MICRO") || 
            activityType.equals("PRACTICAL_MICRO") ||
            activityType.equals("WRITTEN_REVIEW") || 
            activityType.equals("PRACTICAL_REVIEW")
        );
        
        log.info("[calculateXpByScore] isMainLearning={}", isMainLearning);
        
        // 메인학습이 아니면 기존 로직 (scorePct 무시)
        if (!isMainLearning) {
            int baseXp = BASE_XP_RULES.getOrDefault(activityType, 0);
            log.info("[calculateXpByScore] Not main learning, returning base XP: {}", baseXp);
            return baseXp;
        }
        
        // 메인학습: scorePct가 null이거나 유효하지 않으면 0 XP 반환 (기본 XP 지급 안 함)
        if (scorePct == null) {
            log.warn("[calculateXpByScore] scorePct is NULL for main learning activity: activityType={}, returning 0 XP", 
                    activityType);
            return 0;
        }
        
        if (scorePct.isNaN() || scorePct < 0.0 || scorePct > 100.0) {
            log.warn("[calculateXpByScore] Invalid scorePct for main learning activity: activityType={}, scorePct={}, returning 0 XP", 
                    activityType, scorePct);
            return 0;
        }
        
        // 메인학습: 정답률 기반 XP 계산
        int xp;
        if (scorePct == 100.0) {
            // 100%: 기본 XP
            xp = BASE_XP_RULES.getOrDefault(activityType, 0);
            log.info("[calculateXpByScore] Perfect score! activityType={}, scorePct={}, baseXp={}", activityType, scorePct, xp);
        } else if (scorePct >= 80.0) {
            // 80% ~ 99%: 50 XP
            xp = 50;
            log.info("[calculateXpByScore] High score: activityType={}, scorePct={}, xp=50", activityType, scorePct);
        } else if (scorePct >= 30.0) {
            // 30% ~ 79%: 30 XP
            xp = 30;
            log.info("[calculateXpByScore] Medium score: activityType={}, scorePct={}, xp=30", activityType, scorePct);
        } else {
            // 0% ~ 29%: 10 XP
            xp = 10;
            log.info("[calculateXpByScore] Low score: activityType={}, scorePct={}, xp=10", activityType, scorePct);
        }
        
        log.info("[calculateXpByScore] Final result: activityType={}, scorePct={}, xp={}", activityType, scorePct, xp);
        return xp;
    }

    /**
     * 다음 레벨까지 필요한 XP 계산
     * 공식: 300 + 현재 레벨 × 50
     */
    @Transactional(readOnly = true)
    public int calculateXpToNextLevel(int currentLevel, long currentXpTotal) {
        int requiredXp = 300 + currentLevel * 50;
        
        // 현재 레벨에서 다음 레벨까지 필요한 누적 XP 계산
        long cumulative = 0;
        int level = 1;
        while (level < currentLevel) {
            cumulative += (300 + level * 50);
            level++;
        }
        long nextLevelThreshold = cumulative + requiredXp;
        
        return (int) Math.max(0, nextLevelThreshold - currentXpTotal);
    }
    
    /**
     * 레벨 계산 규칙: 300 + 레벨 × 50
     * 
     * 레벨별 요구 XP 및 누적 XP:
     * Lv1 → 2: 350 XP (누적 350)
     * Lv2 → 3: 400 XP (누적 750)
     * Lv3 → 4: 450 XP (누적 1200)
     * Lv4 → 5: 500 XP (누적 1700)
     * Lv5 → 6: 550 XP (누적 2250)
     * Lv6 → 7: 600 XP (누적 2850)
     * Lv7 → 8: 650 XP (누적 3500)
     * Lv8 → 9: 700 XP (누적 4200)
     * Lv9 → 10: 750 XP (누적 4950)
     * ...
     * 
     * 공식: 각 레벨에서 다음 레벨까지 필요한 XP = 300 + 현재 레벨 × 50
     * 예: Lv1 → 2 = 300 + 1 × 50 = 350 XP
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

        // 경험치 지급 로그
        log.info("XP granted: userId={}, delta={}, reason={}, refId={}, beforeXp={}, afterXp={}, level={}", 
                userId, delta, reason, refId, before, w.getXpTotal(), w.getLevel());

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

    @Transactional(readOnly = true)
    public Page<UserXpLedger> getLedger(String userId, Pageable pageable) {
        return ledgerRepo.findByUserIdOrderByCreatedAtDesc(userId, pageable);
    }
    
    /**
     * XP 지급 API
     * 
     * 중복 지급 방지는 study-service의 xp_granted 플래그로 처리합니다.
     * progress-service는 요청이 오면 항상 XP를 지급합니다.
     * 
     * @param userId 사용자 ID
     * @param req XP 지급 요청
     * @return XP 지급 응답
     */
    @Transactional
    public XpDtos.XpEarnResponse earnXp(String userId, XpDtos.XpEarnRequest req) {
        // 1. 요청 파라미터 검증 및 로깅
        log.info("[XP_EARN] ========== XP EARN REQUEST ==========");
        log.info("[XP_EARN] userId={}", userId);
        log.info("[XP_EARN] activityType={}", req.activityType());
        log.info("[XP_EARN] sessionId={}", req.sessionId());
        log.info("[XP_EARN] topicId={}", req.topicId());
        log.info("[XP_EARN] scorePct={} (type: {})", req.scorePct(), req.scorePct() != null ? req.scorePct().getClass().getName() : "null");
        
        // 2. 정답률 기반 XP 계산
        int xpAmount = calculateXpByScore(req.activityType(), req.scorePct());
        log.info("[XP_EARN] ========== XP CALCULATION RESULT ==========");
        log.info("[XP_EARN] activityType={}, scorePct={}, xpAmount={}", 
                req.activityType(), req.scorePct(), xpAmount);
        
        if (xpAmount == 0 && req.scorePct() == null) {
            log.warn("[XP_EARN] Unknown activity type or null scorePct: activityType={}, scorePct={}", 
                    req.activityType(), req.scorePct());
        }
        
        // 2. XP 지급 (중복 지급 방지는 study-service의 xp_granted 플래그로 처리)
        // refId를 null로 전달하여 addXp의 중복 지급 방지 로직을 우회
        // (study-service에서 이미 xp_granted 플래그로 중복 지급을 방지함)
        XpReason reason = mapActivityTypeToReason(req.activityType());
        
        UserXpWallet walletBefore = getWallet(userId);
        long xpBefore = walletBefore.getXpTotal();
        int levelBefore = walletBefore.getLevel();
        
        // refId를 null로 전달하여 항상 XP 지급 (중복 지급 방지 우회)
        UserXpWallet walletAfter = addXp(userId, xpAmount, reason, null);
        long xpAfter = walletAfter.getXpTotal();
        int levelAfter = walletAfter.getLevel();
        
        int earnedXp = (int) (xpAfter - xpBefore);
        boolean leveledUp = levelAfter > levelBefore;
        int levelUpRewardPoints = leveledUp ? (levelAfter - levelBefore) * POINTS_PER_LEVEL : 0;
        
        log.info("XP earned: userId={}, activityType={}, sessionId={}, scorePct={}, earnedXp={}, totalXp={}, level={}->{}, leveledUp={}", 
                userId, req.activityType(), req.sessionId(), req.scorePct(), earnedXp, xpAfter, levelBefore, levelAfter, leveledUp);
        
        return new XpDtos.XpEarnResponse(
                earnedXp,
                xpAfter,
                levelAfter,
                calculateXpToNextLevel(levelAfter, xpAfter),
                leveledUp,
                levelUpRewardPoints
        );
    }
    
    /**
     * activityType을 XpReason으로 매핑
     */
    private XpReason mapActivityTypeToReason(String activityType) {
        if (activityType == null) {
            return XpReason.ETC;
        }
        
        return switch (activityType) {
            case "WRITTEN_MICRO", "PRACTICAL_MICRO" -> XpReason.MICRO;
            case "WRITTEN_REVIEW", "PRACTICAL_REVIEW" -> XpReason.REVIEW;
            case "ASSIST_CORRECT", "ASSIST_WRONG" -> XpReason.ASSIST;
            case "DUEL_WIN", "DUEL_JOIN", "TOURNAMENT_WIN", "TOURNAMENT_JOIN", 
                 "GOLDENBELL_WIN", "GOLDENBELL_JOIN" -> XpReason.BATTLE;
            default -> XpReason.ETC;
        };
    }
}
