package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.domain.StoreItem;
import com.OhRyue.certpilot.progress.domain.UserInventory;
import com.OhRyue.certpilot.progress.domain.UserLoadout;
import com.OhRyue.certpilot.progress.dto.StoreDtos;
import com.OhRyue.certpilot.progress.service.StoreService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

import static com.OhRyue.common.auth.AuthUserUtil.getCurrentUserId;

@Tag(name = "Progress - Store", description = "상점 및 인벤토리 APIs")
@RestController
@RequestMapping("/api/progress/store")
@RequiredArgsConstructor
public class StoreController {

  private final StoreService store;

  @Operation(summary = "상점 카탈로그 조회", description = "사용자 포인트/인벤토리 및 아이템 목록 제공")
  @GetMapping("/catalog")
  public StoreDtos.StoreCatalog catalog(@RequestParam(required = false, name = "q") String keyword) {
    String userId = getCurrentUserId();
    return store.catalog(userId, keyword);
  }

  @Operation(summary = "상점 아이템(활성) 목록")
  @GetMapping("/items")
  public List<StoreItem> items(@RequestParam(required = false, name = "q") String keyword) {
    return store.listActive(keyword);
  }

  @Operation(summary = "아이템 구매")
  @PostMapping("/purchase")
  public ResponseEntity<String> purchase(@RequestParam Long itemId) {
    try {
      String userId = getCurrentUserId();
      store.purchase(userId, itemId);
      return ResponseEntity.ok("구매가 완료되었습니다.");
    } catch (IllegalArgumentException e) {
      return ResponseEntity.badRequest().body(e.getMessage());
    } catch (IllegalStateException e) {
      return ResponseEntity.badRequest().body(e.getMessage());
    } catch (Exception e) {
      return ResponseEntity.status(500).body("구매 중 오류가 발생했습니다: " + e.getMessage());
    }
  }

  @Operation(summary = "포인트 잔액 조회")
  @GetMapping("/points")
  public ResponseEntity<Map<String, Object>> getPoints() {
    String userId = getCurrentUserId();
    long balance = store.getPointBalance(userId);
    return ResponseEntity.ok(Map.of(
        "userId", userId,
        "pointBalance", balance
    ));
  }

  @Operation(summary = "인벤토리 조회")
  @GetMapping("/inventory")
  public List<UserInventory> inventory() {
    String userId = getCurrentUserId();
    return store.listInventory(userId);
  }

  @Operation(summary = "로드아웃 설정")
  @PostMapping("/loadout")
  public UserLoadout setLoadout(@RequestParam(required = false) Long hatId,
                                @RequestParam(required = false) Long clothesId,
                                @RequestParam(required = false) Long accId,
                                @RequestParam(required = false) Long bgId,
                                @RequestParam(required = false) Long specialId) {
    String userId = getCurrentUserId();
    return store.setLoadout(userId, hatId, clothesId, accId, bgId, specialId);
  }

  @Operation(summary = "로드아웃 조회")
  @GetMapping("/loadout")
  public UserLoadout getLoadout() {
    String userId = getCurrentUserId();
    return store.getLoadout(userId);
  }
}
