package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.UserPointLedger;
import com.OhRyue.certpilot.progress.domain.UserPointWallet;
import com.OhRyue.certpilot.progress.domain.UserXpLedger;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.PointReason;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.repository.UserPointLedgerRepository;
import com.OhRyue.certpilot.progress.repository.UserPointWalletRepository;
import com.OhRyue.certpilot.progress.repository.UserXpLedgerRepository;
import com.OhRyue.certpilot.progress.repository.UserXpWalletRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@DisplayName("XpService 테스트")
class XpServiceTest {

    @Mock
    private UserXpWalletRepository walletRepo;

    @Mock
    private UserXpLedgerRepository ledgerRepo;

    @Mock
    private UserPointWalletRepository pointWalletRepo;

    @Mock
    private UserPointLedgerRepository pointLedgerRepo;

    @InjectMocks
    private XpService xpService;

    private String testUserId;

    @BeforeEach
    void setUp() {
        testUserId = "test-user-123";
    }

    @Test
    @DisplayName("레벨 1: 0 XP")
    void testLevel1_ZeroXp() {
        // Given
        UserXpWallet wallet = UserXpWallet.builder()
                .userId(testUserId)
                .xpTotal(0)
                .level(1)
                .build();
        when(walletRepo.findById(testUserId)).thenReturn(Optional.of(wallet));
        when(ledgerRepo.existsByUserIdAndReasonAndRefId(anyString(), any(), anyString())).thenReturn(false);

        // When
        UserXpWallet result = xpService.addXp(testUserId, 0, XpReason.ETC, "test-ref");

        // Then
        assertEquals(0, result.getXpTotal());
        assertEquals(1, result.getLevel());
    }

    @Test
    @DisplayName("레벨 1 → 2: 350 XP 누적 (요구: 350 XP)")
    void testLevelUp_1To2() {
        // Given
        UserXpWallet wallet = UserXpWallet.builder()
                .userId(testUserId)
                .xpTotal(0)
                .level(1)
                .build();
        when(walletRepo.findById(testUserId)).thenReturn(Optional.of(wallet));
        when(ledgerRepo.existsByUserIdAndReasonAndRefId(anyString(), any(), anyString())).thenReturn(false);
        when(pointWalletRepo.findById(testUserId)).thenReturn(Optional.empty());

        // When
        UserXpWallet result = xpService.addXp(testUserId, 350, XpReason.MICRO, "test-ref");

        // Then
        assertEquals(350, result.getXpTotal());
        assertEquals(2, result.getLevel());

        // 레벨업 포인트 지급 확인
        ArgumentCaptor<UserPointWallet> pointWalletCaptor = ArgumentCaptor.forClass(UserPointWallet.class);
        verify(pointWalletRepo, atLeastOnce()).save(pointWalletCaptor.capture());
        UserPointWallet savedPointWallet = pointWalletCaptor.getValue();
        assertEquals(100, savedPointWallet.getPointTotal()); // 레벨 2 달성 = 100 포인트
    }

    @Test
    @DisplayName("레벨 2 → 3: 750 XP 누적 (요구: 400 XP)")
    void testLevelUp_2To3() {
        // Given
        UserXpWallet wallet = UserXpWallet.builder()
                .userId(testUserId)
                .xpTotal(350)
                .level(2)
                .build();
        when(walletRepo.findById(testUserId)).thenReturn(Optional.of(wallet));
        when(ledgerRepo.existsByUserIdAndReasonAndRefId(anyString(), any(), anyString())).thenReturn(false);
        when(pointWalletRepo.findById(testUserId)).thenReturn(Optional.of(
                UserPointWallet.builder().userId(testUserId).pointTotal(100L).build()));

        // When
        UserXpWallet result = xpService.addXp(testUserId, 400, XpReason.MICRO, "test-ref");

        // Then
        assertEquals(750, result.getXpTotal());
        assertEquals(3, result.getLevel());
    }

    @Test
    @DisplayName("레벨 3 → 4: 1200 XP 누적 (요구: 450 XP)")
    void testLevelUp_3To4() {
        // Given
        UserXpWallet wallet = UserXpWallet.builder()
                .userId(testUserId)
                .xpTotal(750)
                .level(3)
                .build();
        when(walletRepo.findById(testUserId)).thenReturn(Optional.of(wallet));
        when(ledgerRepo.existsByUserIdAndReasonAndRefId(anyString(), any(), anyString())).thenReturn(false);
        when(pointWalletRepo.findById(testUserId)).thenReturn(Optional.of(
                UserPointWallet.builder().userId(testUserId).pointTotal(200L).build()));

        // When
        UserXpWallet result = xpService.addXp(testUserId, 450, XpReason.REVIEW, "test-ref");

        // Then
        assertEquals(1200, result.getXpTotal());
        assertEquals(4, result.getLevel());
    }

    @Test
    @DisplayName("동일 refId로 중복 XP 지급 방지 (idempotency)")
    void testDuplicateXpPrevention() {
        // Given
        UserXpWallet wallet = UserXpWallet.builder()
                .userId(testUserId)
                .xpTotal(100)
                .level(1)
                .build();
        when(walletRepo.findById(testUserId)).thenReturn(Optional.of(wallet));
        when(ledgerRepo.existsByUserIdAndReasonAndRefId(testUserId, XpReason.MICRO, "duplicate-ref"))
                .thenReturn(true); // 이미 지급된 기록 존재

        // When - 같은 refId로 다시 시도
        UserXpWallet result = xpService.addXp(testUserId, 150, XpReason.MICRO, "duplicate-ref");

        // Then - XP가 증가하지 않음
        assertEquals(100, result.getXpTotal());
        verify(ledgerRepo, never()).save(any(UserXpLedger.class)); // ledger에 기록 안됨
    }

    @Test
    @DisplayName("refId가 null이면 매번 지급 (ASSIST, BATTLE 등)")
    void testNullRefId_AlwaysGrants() {
        // Given
        UserXpWallet wallet = UserXpWallet.builder()
                .userId(testUserId)
                .xpTotal(0)
                .level(1)
                .build();
        when(walletRepo.findById(testUserId)).thenReturn(Optional.of(wallet));

        // When - refId null로 첫 번째 지급
        UserXpWallet result1 = xpService.addXp(testUserId, 5, XpReason.ASSIST, null);

        // Then
        assertEquals(5, result1.getXpTotal());

        // 두 번째 지급 (같은 refId null)
        when(walletRepo.findById(testUserId)).thenReturn(Optional.of(result1));
        UserXpWallet result2 = xpService.addXp(testUserId, 5, XpReason.ASSIST, null);

        // Then - 두 번째도 지급됨
        assertEquals(10, result2.getXpTotal());
    }

    @Test
    @DisplayName("지갑이 없으면 자동 생성")
    void testAutoCreateWallet() {
        // Given
        when(walletRepo.findById(testUserId)).thenReturn(Optional.empty());
        when(ledgerRepo.existsByUserIdAndReasonAndRefId(anyString(), any(), anyString())).thenReturn(false);

        // When
        UserXpWallet result = xpService.addXp(testUserId, 150, XpReason.MICRO, "new-ref");

        // Then
        assertNotNull(result);
        assertEquals(150, result.getXpTotal());
        verify(walletRepo, atLeastOnce()).save(any(UserXpWallet.class));
    }
}










