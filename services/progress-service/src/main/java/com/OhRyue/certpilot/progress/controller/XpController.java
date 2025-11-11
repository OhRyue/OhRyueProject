package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.service.XpService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/progress/xp")
@RequiredArgsConstructor
public class XpController {
  private final XpService xp;

  @Operation(summary="XP 지갑 조회")
  @GetMapping("/wallet")
  public UserXpWallet wallet(@RequestParam String userId){
    return xp.getWallet(userId);
  }

  @Operation(summary="XP 적립")
  @PostMapping("/gain")
  public UserXpWallet gain(@RequestParam String userId,
                           @RequestParam int delta,
                           @RequestParam(defaultValue = "ETC") XpReason reason,
                           @RequestParam(required = false) String refId){
    return xp.addXp(userId, delta, reason, refId);
  }
}
