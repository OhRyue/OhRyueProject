package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.domain.StoreItem;
import com.OhRyue.certpilot.progress.domain.UserInventory;
import com.OhRyue.certpilot.progress.domain.UserLoadout;
import com.OhRyue.certpilot.progress.service.StoreService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/progress/store")
@RequiredArgsConstructor
public class StoreController {
  private final StoreService store;

  @Operation(summary="상점 아이템(활성) 목록")
  @GetMapping("/items")
  public List<StoreItem> items(){
    return store.listActive();
  }

  @Operation(summary="아이템 구매")
  @PostMapping("/purchase")
  public String purchase(@RequestParam String userId, @RequestParam Long itemId){
    store.purchase(userId, itemId);
    return "OK";
  }

  @Operation(summary="인벤토리 조회")
  @GetMapping("/inventory")
  public List<UserInventory> inventory(@RequestParam String userId){
    return store.listInventory(userId);
  }

  @Operation(summary="로드아웃 설정")
  @PostMapping("/loadout")
  public UserLoadout setLoadout(@RequestParam String userId,
                                @RequestParam(required=false) Long hatId,
                                @RequestParam(required=false) Long clothesId,
                                @RequestParam(required=false) Long accId,
                                @RequestParam(required=false) Long bgId,
                                @RequestParam(required=false) Long specialId){
    return store.setLoadout(userId, hatId, clothesId, accId, bgId, specialId);
  }

  @Operation(summary="로드아웃 조회")
  @GetMapping("/loadout")
  public UserLoadout getLoadout(@RequestParam String userId){
    return store.getLoadout(userId);
  }
}
