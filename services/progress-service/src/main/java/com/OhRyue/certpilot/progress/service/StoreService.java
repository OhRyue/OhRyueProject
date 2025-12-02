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

  private final StoreItemRepository itemRepo;
  private final UserPointWalletRepository walletRepo;
  private final UserPointLedgerRepository ledgerRepo;
  private final UserInventoryRepository invRepo;
  private final UserLoadoutRepository loadoutRepo;

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

    UserPointWallet wallet = walletRepo.findById(userId)
        .orElseGet(() -> defaultWallet(userId));
    StoreUserSummary userSummary = new StoreUserSummary(userId, wallet.getPointTotal(), ownedIds.size());

    return new StoreDtos.StoreCatalog(userSummary, itemViews, Instant.now());
  }

  /** 포인트 차감 → 인벤토리 지급 → (선택) 자동 로드아웃은 하지 않음 */
  @Transactional
  public void purchase(String userId, Long itemId) {
    StoreItem item = itemRepo.findById(itemId)
        .orElseThrow(() -> new IllegalArgumentException("아이템 없음"));
    if (!item.isActive()) {
      throw new IllegalStateException("비활성화 아이템");
    }

    if (item.getLimitPerUser() != null && item.getLimitPerUser() > 0) {
      long owned = invRepo.countByUserIdAndItemId(userId, itemId);
      if (owned >= item.getLimitPerUser()) {
        throw new IllegalStateException("구매 제한 수량 도달");
      }
    }

    UserPointWallet wallet = walletRepo.findById(userId)
        .orElseGet(() -> defaultWallet(userId));
    if (wallet.getPointTotal() < item.getPrice()) {
      throw new IllegalStateException("포인트 부족");
    }

    wallet.setPointTotal(wallet.getPointTotal() - item.getPrice());
    walletRepo.save(wallet);

    ledgerRepo.save(UserPointLedger.builder()
        .userId(userId)
        .delta(-item.getPrice())
        .reason(PointReason.PURCHASE)
        .refId("shop:" + itemId)
        .build());

    if (!invRepo.existsByUserIdAndItemId(userId, itemId)) {
      invRepo.save(UserInventory.builder().userId(userId).itemId(itemId).build());
    }
  }

  @Transactional(readOnly = true)
  public List<UserInventory> listInventory(String userId) {
    return invRepo.findByUserId(userId);
  }

  @Transactional(readOnly = true)
  public boolean hasItem(String userId, Long itemId) {
    return invRepo.existsByUserIdAndItemId(userId, itemId);
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

  private UserPointWallet defaultWallet(String userId) {
    return new UserPointWallet(userId, 0L, Instant.now());
  }
}
