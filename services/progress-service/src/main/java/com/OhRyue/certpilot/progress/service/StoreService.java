package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.*;
import com.OhRyue.certpilot.progress.domain.enums.PointReason;
import com.OhRyue.certpilot.progress.dto.StoreDtos;
import com.OhRyue.certpilot.progress.dto.StoreDtos.StoreItemView;
import com.OhRyue.certpilot.progress.dto.StoreDtos.StoreUserSummary;
import com.OhRyue.certpilot.progress.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class StoreService {

  private static final int POINTS_PER_LEVEL = 500;

  private final StoreItemRepository itemRepo;
  private final UserPointLedgerRepository ledgerRepo;
  private final UserInventoryRepository invRepo;
  private final UserLoadoutRepository loadoutRepo;
  private final XpService xpService;

  @Transactional(readOnly = true)
  public List<StoreItem> listActive() {
    return itemRepo.findByIsActiveTrue();
  }

  @Transactional(readOnly = true)
  public List<StoreItem> listActive(String keyword) {
    List<StoreItem> items = itemRepo.findByIsActiveTrue();

    if (keyword == null || keyword.isBlank()) {
      return items.stream()
          .sorted(Comparator.comparing(StoreItem::getName))
          .toList();
    }

    String lowered = keyword.toLowerCase();
    return items.stream()
        .filter(item -> item.getName() != null && item.getName().toLowerCase().contains(lowered))
        .sorted(Comparator.comparing(StoreItem::getName))
        .toList();
  }

  @Transactional(readOnly = true)
  public StoreDtos.StoreCatalog catalog(String userId, String keyword) {
    Objects.requireNonNull(userId, "userId is required");

    List<UserInventory> inventory = invRepo.findByUserId(userId);
    Set<Long> ownedIds = inventory.stream()
        .map(UserInventory::getItemId)
        .collect(Collectors.toSet());

    List<StoreItem> items = listActive(keyword);
    List<StoreItemView> itemViews = items.stream()
        .map(item -> toView(item, ownedIds.contains(item.getId())))
        .sorted(Comparator.comparingInt(StoreItemView::price))
        .toList();

    // 포인트 잔액 계산 (레벨 × 500 - 구매 총액)
    long pointBalance = calculatePointBalance(userId);
    StoreUserSummary userSummary = new StoreUserSummary(userId, pointBalance, ownedIds.size());

    return new StoreDtos.StoreCatalog(userSummary, itemViews, Instant.now());
  }

  /** 포인트 차감 → 인벤토리 지급 → (선택) 자동 로드아웃은 하지 않음 */
  @Transactional
  public void purchase(String userId, Long itemId) {
    StoreItem item = itemRepo.findById(itemId)
        .orElseThrow(() -> new IllegalArgumentException("아이템을 찾을 수 없습니다. itemId: " + itemId));
    
    if (!item.isActive()) {
      throw new IllegalStateException("비활성화된 아이템입니다. itemId: " + itemId);
    }

    // 이미 구매한 아이템인지 확인
    if (invRepo.existsByUserIdAndItemId(userId, itemId)) {
      throw new IllegalStateException("이미 구매한 아이템입니다. itemId: " + itemId);
    }

    if (item.getLimitPerUser() != null && item.getLimitPerUser() > 0) {
      long owned = invRepo.countByUserIdAndItemId(userId, itemId);
      if (owned >= item.getLimitPerUser()) {
        throw new IllegalStateException("구매 제한 수량에 도달했습니다. itemId: " + itemId);
      }
    }

    // 포인트 잔액 확인 (레벨 × 500 - 구매 총액)
    long currentBalance = calculatePointBalance(userId);
    if (currentBalance < item.getPrice()) {
      throw new IllegalStateException(
          String.format("포인트가 부족합니다. 필요: %d, 보유: %d", item.getPrice(), currentBalance)
      );
    }

    // 포인트 차감하지 않고 구매 기록만 저장 (레벨 × 500 방식)
    ledgerRepo.save(UserPointLedger.builder()
        .userId(userId)
        .delta(-item.getPrice())
        .reason(PointReason.PURCHASE)
        .refId("shop:" + itemId)
        .build());

    // 인벤토리에 추가
    invRepo.save(UserInventory.builder().userId(userId).itemId(itemId).build());
  }

  @Transactional(readOnly = true)
  public List<UserInventory> listInventory(String userId) {
    return invRepo.findByUserId(userId);
  }

  @Transactional(readOnly = true)
  public boolean hasItem(String userId, Long itemId) {
    return invRepo.existsByUserIdAndItemId(userId, itemId);
  }

  /**
   * 포인트 잔액 계산: 현재 레벨 × 500 - 구매한 아이템 총액
   */
  @Transactional(readOnly = true)
  public long getPointBalance(String userId) {
    return calculatePointBalance(userId);
  }
  
  /**
   * 포인트 잔액 계산: 현재 레벨 × 500 - 구매한 아이템 총액
   */
  private long calculatePointBalance(String userId) {
    // 현재 레벨 조회
    int currentLevel = xpService.getWallet(userId).getLevel();
    
    // 구매한 아이템 총액 조회
    long purchaseTotal = ledgerRepo.sumPurchaseTotal(userId, PointReason.PURCHASE);
    
    // 포인트 = 레벨 × 500 - 구매 총액
    long balance = (long) currentLevel * POINTS_PER_LEVEL - purchaseTotal;
    
    return Math.max(0, balance); // 음수 방지
  }

  /** 회원가입 시 기본 인벤토리 아이템 추가 */
  @Transactional
  public void initializeDefaultInventory(String userId) {
    // 기본 스킨 아이템 ID: 1, 16
    List<Long> defaultItemIds = List.of(1L, 16L);
    
    for (Long itemId : defaultItemIds) {
      if (!invRepo.existsByUserIdAndItemId(userId, itemId)) {
        UserInventory inventory = UserInventory.builder()
            .userId(userId)
            .itemId(itemId)
            .build();
        invRepo.save(inventory);
      }
    }
  }

  @Transactional
  public UserLoadout setLoadout(String userId,
                                Long hat,
                                Long clothes,
                                Long acc,
                                Long bg,
                                Long sp) {
    UserLoadout loadout = loadoutRepo.findById(userId)
        .orElse(UserLoadout.builder().userId(userId).build());
    loadout.setHatId(hat);
    loadout.setClothesId(clothes);
    loadout.setAccId(acc);
    loadout.setBgId(bg);
    loadout.setSpecialId(sp);
    return loadoutRepo.save(loadout);
  }

  @Transactional(readOnly = true)
  public UserLoadout getLoadout(String userId) {
    return loadoutRepo.findById(userId)
        .orElse(UserLoadout.builder().userId(userId).build());
  }

  private StoreItemView toView(StoreItem item, boolean owned) {
    return new StoreItemView(
        item.getId(),
        item.getName(),
        null,
        item.getPrice(),
        owned,
        item.getLimitPerUser(),
        item.isActive()
    );
  }

}
