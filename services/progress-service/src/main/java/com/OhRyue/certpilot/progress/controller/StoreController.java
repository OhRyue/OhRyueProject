package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.domain.StoreItem;
import com.OhRyue.certpilot.progress.domain.UserInventory;
import com.OhRyue.certpilot.progress.domain.UserLoadout;
import com.OhRyue.certpilot.progress.domain.enums.ItemCategory;
import com.OhRyue.certpilot.progress.dto.StoreDtos;
import com.OhRyue.certpilot.progress.service.StoreService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Progress - Store", description = "상점 및 인벤토리 APIs")
@RestController
@RequestMapping("/api/progress/store")
@RequiredArgsConstructor
public class StoreController {

  private final StoreService store;

  @Operation(summary = "상점 카탈로그 조회", description = "사용자 포인트/인벤토리 및 카테고리별 아이템 제공")
  @GetMapping("/catalog")
  public StoreDtos.StoreCatalog catalog(@RequestParam String userId,
                                        @RequestParam(required = false) ItemCategory category,
                                        @RequestParam(required = false, name = "q") String keyword) {
    return store.catalog(userId, category, keyword);
  }

  @Operation(summary = "상점 아이템(활성) 목록")
  @GetMapping("/items")
  public List<StoreItem> items(@RequestParam(required = false) ItemCategory category,
                               @RequestParam(required = false, name = "q") String keyword) {
    return store.listActive(category, keyword);
  }

  @Operation(summary = "아이템 구매")
  @PostMapping("/purchase")
  public String purchase(@RequestParam String userId, @RequestParam Long itemId) {
    store.purchase(userId, itemId);
    return "OK";
  }

  @Operation(summary = "인벤토리 조회")
  @GetMapping("/inventory")
  public List<UserInventory> inventory(@RequestParam String userId) {
    return store.listInventory(userId);
  }

  @Operation(summary = "로드아웃 설정")
  @PostMapping("/loadout")
  public UserLoadout setLoadout(@RequestParam String userId,
                                @RequestParam(required = false) Long hatId,
                                @RequestParam(required = false) Long clothesId,
                                @RequestParam(required = false) Long accId,
                                @RequestParam(required = false) Long bgId,
                                @RequestParam(required = false) Long specialId) {
    return store.setLoadout(userId, hatId, clothesId, accId, bgId, specialId);
  }

  @Operation(summary = "로드아웃 조회")
  @GetMapping("/loadout")
  public UserLoadout getLoadout(@RequestParam String userId) {
    return store.getLoadout(userId);
  }
}
