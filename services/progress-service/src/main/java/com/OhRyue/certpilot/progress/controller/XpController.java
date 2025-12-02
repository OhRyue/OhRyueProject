package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.dto.XpDtos;
import com.OhRyue.certpilot.progress.service.XpService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.*;

import static com.OhRyue.common.auth.AuthUserUtil.getCurrentUserId;

@Tag(name = "Progress - XP", description = "경험치 관리 APIs")
@RestController
@RequestMapping("/api/progress/xp")
@RequiredArgsConstructor
public class XpController {

  private final XpService xp;

  @Operation(summary = "XP 지갑 조회")
  @GetMapping("/wallet")
  public XpDtos.XpWalletResponse wallet() {
    String userId = getCurrentUserId();
    UserXpWallet wallet = xp.getWallet(userId);
    int xpToNextLevel = xp.calculateXpToNextLevel(wallet.getLevel(), wallet.getXpTotal());
    return new XpDtos.XpWalletResponse(
        wallet.getXpTotal(),
        wallet.getLevel(),
        xpToNextLevel
    );
  }

  @Operation(summary = "XP 적립")
  @PostMapping("/gain")
  public UserXpWallet gain(@RequestParam int delta,
                           @RequestParam(defaultValue = "ETC") XpReason reason,
                           @RequestParam(required = false) String refId) {
    String userId = getCurrentUserId();
    return xp.addXp(userId, delta, reason, refId);
  }

  @Operation(summary = "XP 지급 (활동 타입 기반, sessionId로 중복 방지)")
  @PostMapping("/earn")
  public XpDtos.XpEarnResponse earn(@RequestBody XpDtos.XpEarnRequest req) {
    String userId = getCurrentUserId();
    return xp.earnXp(userId, req);
  }

  @Operation(summary = "XP 기록 조회 (최근 N개)")
  @GetMapping("/ledger")
  public Page<com.OhRyue.certpilot.progress.domain.UserXpLedger> ledger(
      @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size) {
    String userId = getCurrentUserId();
    Pageable pageable = PageRequest.of(page, size);
    return xp.getLedger(userId, pageable);
  }
}
