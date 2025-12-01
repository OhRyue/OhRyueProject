package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import com.OhRyue.certpilot.progress.dto.VersusDtos;
import com.OhRyue.certpilot.progress.repository.BattleAnswerRepository;
import com.OhRyue.certpilot.progress.repository.BattleRecordRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@DisplayName("VersusResultService 테스트 - 대전/이벤트 XP 지급")
class VersusResultServiceTest {

    @Mock
    private XpService xpService;

    @Mock
    private BadgeService badgeService;

    @Mock
    private RankService rankService;

    @Mock
    private BattleRecordRepository battleRecordRepository;

    @Mock
    private BattleAnswerRepository battleAnswerRepository;

    @InjectMocks
    private VersusResultService versusResultService;

    private Long testRoomId;

    @BeforeEach
    void setUp() {
        testRoomId = 100L;
    }

    @Test
    @DisplayName("1:1 배틀 승리 - 30 XP 지급")
    void testDuelWin_30Xp() {
        // Given
        VersusDtos.VersusResultRequest request = new VersusDtos.VersusResultRequest(
                "DUEL",
                testRoomId,
                "winner-user",
                List.of(
                        new VersusDtos.ParticipantResult(
                                "winner-user", 8500, 1, 8, 10, 45000L,
                                null
                        ),
                        new VersusDtos.ParticipantResult(
                                "loser-user", 7200, 2, 7, 10, 50000L,
                                null
                        )
                ),
                10,
                120000L,
                "WRITTEN"
        );

        when(xpService.addXp(anyString(), anyInt(), any(), isNull())).thenReturn(
                UserXpWallet.builder().userId("winner-user").xpTotal(30).level(1).build()
        );
        when(battleRecordRepository.save(any())).thenAnswer(invocation -> invocation.getArgument(0));

        // When
        versusResultService.recordVersusResult(request);

        // Then
        ArgumentCaptor<Integer> xpCaptor = ArgumentCaptor.forClass(Integer.class);
        verify(xpService, times(1)).addXp(
                eq("winner-user"),
                xpCaptor.capture(),
                eq(XpReason.BATTLE),
                isNull() // refId는 null (매번 지급)
        );
        assertEquals(30, xpCaptor.getValue());
    }

    @Test
    @DisplayName("1:1 배틀 참가 - 5 XP 지급")
    void testDuelParticipate_5Xp() {
        // Given
        VersusDtos.VersusResultRequest request = new VersusDtos.VersusResultRequest(
                "DUEL",
                testRoomId,
                "winner-user",
                List.of(
                        new VersusDtos.ParticipantResult(
                                "winner-user", 8500, 1, 8, 10, 45000L,
                                null
                        ),
                        new VersusDtos.ParticipantResult(
                                "loser-user", 7200, 2, 7, 10, 50000L,
                                null
                        )
                ),
                10,
                120000L,
                "WRITTEN"
        );

        when(xpService.addXp(eq("winner-user"), anyInt(), any(), isNull())).thenReturn(
                UserXpWallet.builder().userId("winner-user").xpTotal(30).level(1).build()
        );
        when(xpService.addXp(eq("loser-user"), anyInt(), any(), isNull())).thenReturn(
                UserXpWallet.builder().userId("loser-user").xpTotal(5).level(1).build()
        );
        when(battleRecordRepository.save(any())).thenAnswer(invocation -> invocation.getArgument(0));

        // When
        versusResultService.recordVersusResult(request);

        // Then
        ArgumentCaptor<Integer> loserXpCaptor = ArgumentCaptor.forClass(Integer.class);
        verify(xpService, times(1)).addXp(
                eq("loser-user"),
                loserXpCaptor.capture(),
                eq(XpReason.BATTLE),
                isNull()
        );
        assertEquals(5, loserXpCaptor.getValue());
    }

    @Test
    @DisplayName("토너먼트 우승 - 100 XP 지급")
    void testTournamentWin_100Xp() {
        // Given
        VersusDtos.VersusResultRequest request = new VersusDtos.VersusResultRequest(
                "TOURNAMENT",
                testRoomId,
                "winner-user",
                List.of(
                        new VersusDtos.ParticipantResult(
                                "winner-user", 15000, 1, 9, 9, 60000L, null
                        )
                ),
                9,
                180000L,
                "WRITTEN"
        );

        when(xpService.addXp(anyString(), anyInt(), any(), isNull())).thenReturn(
                UserXpWallet.builder().userId("winner-user").xpTotal(100).level(1).build()
        );
        when(battleRecordRepository.save(any())).thenAnswer(invocation -> invocation.getArgument(0));

        // When
        versusResultService.recordVersusResult(request);

        // Then
        ArgumentCaptor<Integer> xpCaptor = ArgumentCaptor.forClass(Integer.class);
        verify(xpService, times(1)).addXp(
                eq("winner-user"),
                xpCaptor.capture(),
                eq(XpReason.BATTLE),
                isNull()
        );
        assertEquals(100, xpCaptor.getValue());
    }

    @Test
    @DisplayName("골든벨 우승 - 200 XP 지급")
    void testGoldenbellWin_200Xp() {
        // Given
        VersusDtos.VersusResultRequest request = new VersusDtos.VersusResultRequest(
                "GOLDENBELL",
                testRoomId,
                "winner-user",
                List.of(
                        new VersusDtos.ParticipantResult(
                                "winner-user", 20000, 1, 6, 6, 55000L,
                                null
                        )
                ),
                6,
                60000L,
                "WRITTEN"
        );

        when(xpService.addXp(anyString(), anyInt(), any(), isNull())).thenReturn(
                UserXpWallet.builder().userId("winner-user").xpTotal(200).level(1).build()
        );
        when(battleRecordRepository.save(any())).thenAnswer(invocation -> invocation.getArgument(0));

        // When
        versusResultService.recordVersusResult(request);

        // Then
        ArgumentCaptor<Integer> xpCaptor = ArgumentCaptor.forClass(Integer.class);
        verify(xpService, times(1)).addXp(
                eq("winner-user"),
                xpCaptor.capture(),
                eq(XpReason.BATTLE),
                isNull()
        );
        assertEquals(200, xpCaptor.getValue());
    }

    @Test
    @DisplayName("골든벨 참가 - 20 XP 지급")
    void testGoldenbellParticipate_20Xp() {
        // Given
        VersusDtos.VersusResultRequest request = new VersusDtos.VersusResultRequest(
                "GOLDENBELL",
                testRoomId,
                "winner-user",
                List.of(
                        new VersusDtos.ParticipantResult(
                                "winner-user", 20000, 1, 6, 6, 55000L, null
                        ),
                        new VersusDtos.ParticipantResult(
                                "participant-user", 5000, 5, 3, 6, 40000L, null
                        )
                ),
                6,
                60000L,
                "WRITTEN"
        );

        when(xpService.addXp(eq("winner-user"), anyInt(), any(), isNull())).thenReturn(
                UserXpWallet.builder().userId("winner-user").xpTotal(200).level(1).build()
        );
        when(xpService.addXp(eq("participant-user"), anyInt(), any(), isNull())).thenReturn(
                UserXpWallet.builder().userId("participant-user").xpTotal(20).level(1).build()
        );
        when(battleRecordRepository.save(any())).thenAnswer(invocation -> invocation.getArgument(0));

        // When
        versusResultService.recordVersusResult(request);

        // Then
        ArgumentCaptor<Integer> participantXpCaptor = ArgumentCaptor.forClass(Integer.class);
        verify(xpService, times(1)).addXp(
                eq("participant-user"),
                participantXpCaptor.capture(),
                eq(XpReason.BATTLE),
                isNull()
        );
        assertEquals(20, participantXpCaptor.getValue());
    }
}

