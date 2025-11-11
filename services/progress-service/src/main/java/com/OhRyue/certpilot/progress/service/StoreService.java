package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.*;
import com.OhRyue.certpilot.progress.domain.enums.PointReason;
import com.OhRyue.certpilot.progress.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service @RequiredArgsConstructor
public class StoreService {
  private final StoreItemRepository itemRepo;
  private final UserPointWalletRepository walletRepo;
  private final UserPointLedgerRepository ledgerRepo;
  private final UserInventoryRepository invRepo;
  private final UserLoadoutRepository loadoutRepo;

  @Transactional(readOnly = true)
  public List<StoreItem> listActive(){
    return itemRepo.findByIsActiveTrue();
  }

  /** 포인트 차감 → 인벤토리 지급 → (선택) 자동 로드아웃은 하지 않음 */
  @Transactional
  public void purchase(String userId, Long itemId){
    StoreItem item = itemRepo.findById(itemId).orElseThrow(() -> new IllegalArgumentException("아이템 없음"));
    if (!item.isActive()) throw new IllegalStateException("비활성화 아이템");

    // 제한 수량 체크
    if (item.getLimitPerUser()!=null && item.getLimitPerUser()>0) {
      long owned = invRepo.countByUserIdAndItemId(userId, itemId);
      if (owned >= item.getLimitPerUser())
        throw new IllegalStateException("구매 제한 수량 도달");
    }

    UserPointWallet w = walletRepo.findById(userId).orElse(
        UserPointWallet.builder().userId(userId).pointTotal(0).build()
    );
    if (w.getPointTotal() < item.getPrice())
      throw new IllegalStateException("포인트 부족");

    // 차감
    w.setPointTotal(w.getPointTotal() - item.getPrice());
    walletRepo.save(w);

    ledgerRepo.save(UserPointLedger.builder()
        .userId(userId).delta(-item.getPrice())
        .reason(PointReason.PURCHASE).refId("shop:"+itemId).build());

    // 지급
    if (!invRepo.existsByUserIdAndItemId(userId, itemId)) {
      invRepo.save(UserInventory.builder().userId(userId).itemId(itemId).build());
    }
  }

  @Transactional(readOnly = true)
  public List<UserInventory> listInventory(String userId){
    return invRepo.findByUserId(userId);
  }

  @Transactional
  public UserLoadout setLoadout(String userId, Long hat, Long clothes, Long acc, Long bg, Long sp){
    // 소유 검증은 간단히 “있으면 통과, null이면 무시” 정도로 처리
    UserLoadout lo = loadoutRepo.findById(userId).orElse(
        UserLoadout.builder().userId(userId).build()
    );
    lo.setHatId(hat);
    lo.setClothesId(clothes);
    lo.setAccId(acc);
    lo.setBgId(bg);
    lo.setSpecialId(sp);
    return loadoutRepo.save(lo);
  }

  @Transactional(readOnly = true)
  public UserLoadout getLoadout(String userId){
    return loadoutRepo.findById(userId).orElse(
        UserLoadout.builder().userId(userId).build()
    );
  }
}
