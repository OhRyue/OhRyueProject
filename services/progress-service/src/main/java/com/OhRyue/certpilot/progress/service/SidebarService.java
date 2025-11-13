package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.UserStreak;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.dto.SidebarDtos.QuickLink;
import com.OhRyue.certpilot.progress.dto.SidebarDtos.SidebarSummary;
import com.OhRyue.certpilot.progress.dto.SidebarDtos.SidebarUserCard;
import com.OhRyue.certpilot.progress.dto.SidebarDtos.StorePreview;
import com.OhRyue.certpilot.progress.dto.StoreDtos;
import com.OhRyue.certpilot.progress.feign.AccountClient;
import com.OhRyue.certpilot.progress.feign.dto.AccountMeResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class SidebarService {

  private final AccountClient accountClient;
  private final XpService xpService;
  private final StreakService streakService;
  private final StoreService storeService;

  public SidebarSummary sidebar(String authorization, String userId) {
    AccountMeResponse me = accountClient.me(authorization);
    AccountMeResponse.Profile profile = me.profile();

    UserXpWallet wallet = xpService.getWallet(userId);
    UserStreak streak = streakService.get(userId);
    StoreDtos.StoreCatalog catalog = storeService.catalog(userId, null, null);

    SidebarUserCard userCard = new SidebarUserCard(
        userId,
        profile == null ? null : profile.nickname(),
        profile == null ? null : profile.avatarUrl(),
        wallet.getLevel(),
        wallet.getXpTotal(),
        streak.getCurrentDays(),
        catalog.user().pointBalance()
    );

    int itemCount = catalog.categories().stream()
        .mapToInt(section -> section.items().size())
        .sum();
    StorePreview storePreview = new StorePreview(
        catalog.user().pointBalance(),
        catalog.user().ownedItemCount(),
        itemCount
    );

    return new SidebarSummary(userCard, storePreview, defaultQuickLinks());
  }

  private List<QuickLink> defaultQuickLinks() {
    return List.of(
        new QuickLink("메인 학습", "/study/main", "book"),
        new QuickLink("보조 학습", "/study/assist", "sparkles"),
        new QuickLink("대전/이벤트", "/versus", "trophy"),
        new QuickLink("커뮤니티", "/community", "chat")
    );
  }
}

