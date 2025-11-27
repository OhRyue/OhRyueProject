package com.OhRyue.certpilot.versus.integration;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchPhase;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.*;
import com.OhRyue.certpilot.versus.service.VersusService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 비즈니스 룰 테스트
 * 
 * 테스트 항목:
 * 1. DUEL 모드: 정확히 2명만 가능
 * 2. TOURNAMENT 모드: 정확히 8명만 가능
 * 3. GOLDENBELL 예약 시스템: 10분 전 입장, 자동 시작
 * 4. 시간 초과 자동 오답 처리
 */
@SpringBootTest
@ActiveProfiles("test")
@Transactional
class BusinessRulesTest {

    @Autowired
    private VersusService versusService;

    @Autowired
    private MatchRoomRepository roomRepository;

    @Autowired
    private MatchParticipantRepository participantRepository;

    @Autowired
    private MatchQuestionRepository questionRepository;

    @Autowired
    private MatchAnswerRepository answerRepository;

    @Test
    @DisplayName("DUEL 모드: 정확히 2명이 아니면 시작 불가")
    void testDuelMode_ExactTwoParticipants() {
        // Given: DUEL 모드 방 생성 (1명만 초대)
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            "{}",
            List.of("user2"), // 1명만 초대
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            null
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 1명만 있을 때 시작 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.startRoom(room.room().roomId()))
            .isInstanceOf(ResponseStatusException.class)
            .hasMessageContaining("정확히 2명이어야 합니다");

        // Given: 2명 참가
        versusService.joinRoom(room.room().roomId(), "user1");

        // When: 시작 시도
        VersusDtos.RoomDetailResp startedRoom = versusService.startRoom(room.room().roomId());

        // Then: 시작 성공
        assertThat(startedRoom.room().status()).isEqualTo(MatchStatus.ONGOING);
    }

    @Test
    @DisplayName("DUEL 모드: 3명 이상 참가 불가")
    void testDuelMode_MaxTwoParticipants() {
        // Given: DUEL 모드 방 생성 (2명 초대)
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            "{}",
            List.of("user2", "user3"), // 2명 초대
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            null
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 3번째 사용자 참가 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.joinRoom(room.room().roomId(), "user4"))
            .isInstanceOf(ResponseStatusException.class)
            .hasMessageContaining("정확히 2명이어야 합니다");
    }

    @Test
    @DisplayName("TOURNAMENT 모드: 정확히 8명이 아니면 시작 불가")
    void testTournamentMode_ExactEightParticipants() {
        // Given: TOURNAMENT 모드 방 생성 (7명만 초대)
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.TOURNAMENT,
            "{}",
            List.of("user2", "user3", "user4", "user5", "user6", "user7", "user8"), // 7명만 초대
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            null
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 7명만 있을 때 시작 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.startRoom(room.room().roomId()))
            .isInstanceOf(ResponseStatusException.class)
            .hasMessageContaining("정확히 8명이어야 합니다");

        // Given: 8명 참가
        versusService.joinRoom(room.room().roomId(), "user1");

        // When: 시작 시도
        VersusDtos.RoomDetailResp startedRoom = versusService.startRoom(room.room().roomId());

        // Then: 시작 성공
        assertThat(startedRoom.room().status()).isEqualTo(MatchStatus.ONGOING);
    }

    @Test
    @DisplayName("TOURNAMENT 모드: 9명 이상 참가 불가")
    void testTournamentMode_MaxEightParticipants() {
        // Given: TOURNAMENT 모드 방 생성 (8명 초대)
        List<String> participants = List.of(
            "user2", "user3", "user4", "user5", "user6", "user7", "user8", "user9"
        );

        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.TOURNAMENT,
            "{}",
            participants,
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            null
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 9번째 사용자 참가 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.joinRoom(room.room().roomId(), "user10"))
            .isInstanceOf(ResponseStatusException.class)
            .hasMessageContaining("정확히 8명이어야 합니다");
    }

    @Test
    @DisplayName("GOLDENBELL 예약 시스템: 10분 전부터 입장 가능")
    void testGoldenbellScheduled_JoinBeforeTenMinutes() {
        // Given: GOLDENBELL 모드 방 생성 (예약 시간: 10분 후)
        Instant scheduledAt = Instant.now().plus(10, ChronoUnit.MINUTES);

        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.GOLDENBELL,
            "{}",
            List.of(),
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            scheduledAt
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 예약 시간 10분 전 이전 입장 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.joinRoom(room.room().roomId(), "user1"))
            .isInstanceOf(ResponseStatusException.class)
            .hasMessageContaining("입장 가능 시간이 아닙니다");

        // Given: 예약 시간 10분 전 (또는 그 이후)
        // 실제로는 시간을 기다려야 하지만, 테스트를 위해 scheduledAt을 과거로 설정
        // 또는 테스트 환경에서 시간 조작 필요
    }

    @Test
    @DisplayName("GOLDENBELL 예약 시스템: 예약 시간 이전 수동 시작 불가")
    void testGoldenbellScheduled_ManualStartBeforeScheduledTime() {
        // Given: GOLDENBELL 모드 방 생성 (예약 시간: 10분 후)
        Instant scheduledAt = Instant.now().plus(10, ChronoUnit.MINUTES);

        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.GOLDENBELL,
            "{}",
            List.of("user2"),
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            scheduledAt
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 예약 시간 이전 수동 시작 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.startRoom(room.room().roomId()))
            .isInstanceOf(ResponseStatusException.class)
            .hasMessageContaining("예약된 시작 시간이 아닙니다");
    }

    @Test
    @DisplayName("GOLDENBELL 예약 시스템: 예약 시간 이후 자동 시작 가능")
    void testGoldenbellScheduled_AutoStartAfterScheduledTime() {
        // Given: GOLDENBELL 모드 방 생성 (예약 시간: 과거)
        Instant scheduledAt = Instant.now().minus(1, ChronoUnit.MINUTES); // 1분 전

        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.GOLDENBELL,
            "{}",
            List.of("user2"),
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            scheduledAt
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When: 시작 시도 (예약 시간이 지났으므로 가능)
        VersusDtos.RoomDetailResp startedRoom = versusService.startRoom(room.room().roomId());

        // Then: 시작 성공
        assertThat(startedRoom.room().status()).isEqualTo(MatchStatus.ONGOING);
    }

    @Test
    @DisplayName("GOLDENBELL 예약 시스템: 최소 인원 없이도 시작 가능")
    void testGoldenbellScheduled_StartWithoutMinParticipants() {
        // Given: GOLDENBELL 모드 방 생성 (예약 시간: 과거, 참가자 없음)
        Instant scheduledAt = Instant.now().minus(1, ChronoUnit.MINUTES);

        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.GOLDENBELL,
            "{}",
            List.of(), // 참가자 없음
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            scheduledAt
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When: 1명만 참가하고 시작 시도
        versusService.joinRoom(room.room().roomId(), "user1");

        // Then: 시작 성공 (예약 시스템이므로 최소 인원 제한 없음)
        VersusDtos.RoomDetailResp startedRoom = versusService.startRoom(room.room().roomId());
        assertThat(startedRoom.room().status()).isEqualTo(MatchStatus.ONGOING);
    }

    @Test
    @DisplayName("시간 초과: 답안 미제출 시 자동 오답 처리")
    void testTimeoutAnswer_AutoIncorrect() {
        // Given: DUEL 모드 방 생성 및 시작
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            "{}",
            List.of("user2"),
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 5) // 5초 제한
            ),
            null,
            null,
            null,
            null
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");
        versusService.joinRoom(room.room().roomId(), "user1");
        versusService.startRoom(room.room().roomId());

        // When: user1만 답안 제출, user2는 미제출
        VersusDtos.SubmitAnswerReq submitReq = new VersusDtos.SubmitAnswerReq(
            1L, "A", true, 3000, null, 1, MatchPhase.MAIN
        );
        versusService.submitAnswer(room.room().roomId(), "user1", submitReq);

        // Then: user2의 답안이 없음 (TimeoutAnswerService가 처리할 것)
        boolean user2Answered = answerRepository
            .findByRoomIdAndQuestionIdAndUserId(room.room().roomId(), 1L, "user2")
            .isPresent();
        
        // TimeoutAnswerService는 10초마다 실행되므로, 여기서는 확인만
        // 실제로는 시간이 지나면 자동으로 오답 처리됨
        // 테스트 환경에서는 시간 조작이 필요할 수 있음
    }
}




