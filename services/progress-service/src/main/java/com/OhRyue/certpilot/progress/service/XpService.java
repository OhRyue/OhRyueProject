// src/main/java/com/OhRyue/certpilot/progress/service/XpService.java
package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.UserXpLedger;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.dto.XpDtos;
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
    private final BadgeService badgeService;
    
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
     * 정답률에 비례해서 XP 계산
     * 공식: earnedXp = round(maxXp * accuracy)
     * 
     * - accuracy는 correctCount/totalCount 또는 scorePct/100.0으로 계산
     * - 0% 정답 → 0 XP
     * - 50% 정답 → maxXp * 0.5 XP
     * - 100% 정답 → maxXp XP
     * 
     * @param activityType 활동 타입
     * @param scorePct 정답률 (0.0 ~ 100.0)
     * @param correctCount 정답 수 (accuracy 계산용, 선택)
     * @param totalCount 총 문제 수 (accuracy 계산용, 선택)
     * @return 지급할 XP (정답률에 비례)
     */
    private int calculateXpByScore(String activityType, Double scorePct, Integer correctCount, Integer totalCount) {
        log.info("[calculateXpByScore] Called with: activityType={}, scorePct={}, correctCount={}, totalCount={}", 
                activityType, scorePct, correctCount, totalCount);
        
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
        
        // MICRO의 경우 totalCount가 9인지 확인 (OX 4 + MCQ/SHORT 5 = 9)
        if ((activityType.equals("WRITTEN_MICRO") || activityType.equals("PRACTICAL_MICRO")) 
                && totalCount != null) {
            if (!totalCount.equals(9)) {
                log.warn("[calculateXpByScore] MICRO totalCount is not 9: activityType={}, totalCount={}, returning 0 XP", 
                        activityType, totalCount);
                return 0;
            }
        }
        
        // REVIEW의 경우 totalCount가 0이면 XP 지급하지 않음
        if ((activityType.equals("WRITTEN_REVIEW") || activityType.equals("PRACTICAL_REVIEW")) 
                && totalCount != null) {
            if (totalCount.equals(0)) {
                log.warn("[calculateXpByScore] REVIEW totalCount is 0 (문제 부족): activityType={}, returning 0 XP", 
                        activityType);
                return 0;
            }
        }
        
        // 메인학습: 정답률에 비례해서 XP 계산
        // earnedXp = round(maxXp * accuracy)
        // accuracy는 scorePct / 100.0 또는 correctCount / totalCount로 계산
        int maxXp = BASE_XP_RULES.getOrDefault(activityType, 0);
        
        // accuracy 계산: correctCount/totalCount가 있으면 그것을 사용 (더 정확)
        // 없으면 scorePct를 사용
        double accuracy;
        if (correctCount != null && totalCount != null && totalCount > 0) {
            accuracy = (double) correctCount / totalCount;
            log.info("[calculateXpByScore] accuracy 계산 (정수 기반): correctCount={}, totalCount={}, accuracy={}", 
                    correctCount, totalCount, accuracy);
        } else {
            // scorePct를 사용 (0.0 ~ 100.0 범위를 0.0 ~ 1.0으로 변환)
            accuracy = scorePct / 100.0;
            log.warn("[calculateXpByScore] accuracy 계산 (scorePct 기반, 하위 호환성): activityType={}, scorePct={}, accuracy={}. " +
                    "correctCount/totalCount를 제공하는 것을 권장합니다.", 
                    activityType, scorePct, accuracy);
        }
        
        // accuracy는 0.0 ~ 1.0 범위로 제한
        if (accuracy < 0.0) {
            accuracy = 0.0;
        } else if (accuracy > 1.0) {
            accuracy = 1.0;
        }
        
        // 정답률에 비례해서 XP 계산: earnedXp = round(maxXp * accuracy)
        int xp = (int) Math.round(maxXp * accuracy);
        
        log.info("[calculateXpByScore] XP 계산 결과: activityType={}, maxXp={}, scorePct={}, correctCount={}, totalCount={}, accuracy={}, earnedXp={}", 
                activityType, maxXp, scorePct, correctCount, totalCount, accuracy, xp);
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
            
            // 레벨업 시 포인트 지급 제거 (레벨 × 500 방식으로 변경)
            // 포인트는 조회 시 실시간 계산: 현재 레벨 × 500 - 구매한 아이템 총액
            
            log.info("User {} leveled up from {} to {}!", userId, beforeLv, newLv);
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

    // 레벨업 시 포인트 지급 메서드 제거 (레벨 × 500 방식으로 변경)
    // 포인트는 조회 시 실시간 계산: 현재 레벨 × 500 - 구매한 아이템 총액

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
        log.info("[XP_EARN] earnedXp={} (단일 소스, 우선 사용)", req.earnedXp());
        log.info("[XP_EARN] scorePct={} (earnedXp가 없을 때만 사용)", req.scorePct());
        log.info("[XP_EARN] correctCount={}, totalCount={}", req.correctCount(), req.totalCount());
        
        // 2. XP 계산: earnedXp가 있으면 그대로 사용 (단일 소스), 없으면 기존 계산 로직 사용
        int xpAmount;
        if (req.earnedXp() != null) {
            // earnedXp를 단일 소스로 사용 (SUMMARY에서 계산된 값)
            xpAmount = req.earnedXp();
            log.info("[XP_EARN] earnedXp를 단일 소스로 사용: earnedXp={}", xpAmount);
        } else {
            // earnedXp가 없으면 기존 계산 로직 사용 (하위 호환성)
            xpAmount = calculateXpByScore(req.activityType(), req.scorePct(), req.correctCount(), req.totalCount());
            log.info("[XP_EARN] earnedXp가 없어 기존 계산 로직 사용: activityType={}, scorePct={}, correctCount={}, totalCount={}, xpAmount={}", 
                    req.activityType(), req.scorePct(), req.correctCount(), req.totalCount(), xpAmount);
        }
        
        // 3. XP 지급 (중복 지급 방지: sessionId를 refId로 사용)
        XpReason reason = mapActivityTypeToReason(req.activityType());
        
        // refId 생성: sessionId를 사용하여 중복 적립 방지
        // 형식: "session:{sessionId}" 또는 null (sessionId가 없을 때)
        String refId = req.sessionId() != null ? "session:" + req.sessionId() : null;
        
        UserXpWallet walletBefore = getWallet(userId);
        long xpBefore = walletBefore.getXpTotal();
        int levelBefore = walletBefore.getLevel();
        
        log.info("[XP_EARN] XP 적립 직전: userId={}, xpAmount={}, reason={}, refId={}, walletBefore={}", 
                userId, xpAmount, reason, refId, xpBefore);
        
        // addXp 호출 (refId로 중복 적립 방지)
        UserXpWallet walletAfter = addXp(userId, xpAmount, reason, refId);
        long xpAfter = walletAfter.getXpTotal();
        int levelAfter = walletAfter.getLevel();
        
        int actualEarnedXp = (int) (xpAfter - xpBefore);
        boolean leveledUp = levelAfter > levelBefore;
        // 레벨업 보상 포인트는 더 이상 지급하지 않음 (레벨 × 500 방식으로 변경)
        int levelUpRewardPoints = 0;
        
        log.info("[XP_EARN] ========== XP EARN RESULT ==========");
        log.info("[XP_EARN] userId={}, activityType={}, sessionId={}, earnedXp={}, actualEarnedXp={}, totalXp={}, level={}->{}, leveledUp={}", 
                userId, req.activityType(), req.sessionId(), req.earnedXp(), actualEarnedXp, xpAfter, levelBefore, levelAfter, leveledUp);
        
        // 중복 적립 방지 확인 (idempotency hit)
        if (actualEarnedXp == 0 && xpAmount > 0) {
            log.warn("[XP_EARN] 중복 적립 방지됨 (idempotency hit): userId={}, sessionId={}, refId={}, requestedXp={}", 
                    userId, req.sessionId(), refId, xpAmount);
        }
        
        // XP 10000 배지 체크 (비동기로 처리하여 성능 영향 최소화)
        if (xpAfter >= 10_000 && xpBefore < 10_000) {
            try {
                badgeService.evaluate(userId);
            } catch (Exception e) {
                log.warn("Failed to check XP_10000 badge for user {}: {}", userId, e.getMessage());
            }
        }
        
        return new XpDtos.XpEarnResponse(
                actualEarnedXp,  // 실제 적립된 XP (중복 방지로 0일 수 있음)
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
