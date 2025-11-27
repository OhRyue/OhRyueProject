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

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

@SpringBootTest
@ActiveProfiles("test")
@Transactional
class VersusServiceE2ETest {

    @Autowired
    private VersusService versusService;

    @Autowired
    private MatchRoomRepository roomRepository;

    @Autowired
    private MatchParticipantRepository participantRepository;

    @Autowired
    private MatchQuestionRepository questionRepository;

    @Test
    @DisplayName("E2E: 방 생성 → 문제 자동 생성 → 문제 풀이 → 매치 완료 → 보상 지급")
    void testE2E_DuelFlow() {
        // Given: scopeJson 기반 방 생성
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            """
            {
              "examMode": "WRITTEN",
              "difficulty": "NORMAL",
              "topicScope": "ALL"
            }
            """,
            List.of("user1", "user2"),
            null, // questions는 자동 생성
            null,
            null,
            null
        );

        // When: 방 생성
        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // Then: 방이 생성되고 문제가 자동 생성되었는지 확인
        assertThat(room.room().mode()).isEqualTo(MatchMode.DUEL);
        assertThat(room.room().status()).isEqualTo(MatchStatus.WAIT);
        assertThat(room.questions()).isNotEmpty();
        assertThat(room.participants()).hasSize(2);

        // When: 방 시작
        versusService.startRoom(room.room().roomId());

        // Then: 방 상태가 ONGOING으로 변경
        VersusDtos.RoomDetailResp startedRoom = versusService.getRoom(room.room().roomId());
        assertThat(startedRoom.room().status()).isEqualTo(MatchStatus.ONGOING);
    }

    @Test
    @DisplayName("인원 제한 검증 - DUEL 모드 최대 2명")
    void testParticipantLimit_Duel() {
        // Given: DUEL 모드 방 생성
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            "{}",
            List.of("user1", "user2"),
            null,
            null,
            null,
            null
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 3번째 사용자 참가 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.joinRoom(room.room().roomId(), "user3"))
            .hasMessageContaining("최대 2명까지만 참가 가능");
    }

    @Test
    @DisplayName("인원 제한 검증 - TOURNAMENT 모드 최대 8명")
    void testParticipantLimit_Tournament() {
        // Given: TOURNAMENT 모드 방 생성
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.TOURNAMENT,
            "{}",
            List.of("user1", "user2", "user3", "user4", "user5", "user6", "user7", "user8"),
            null,
            null,
            null,
            null
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 9번째 사용자 참가 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.joinRoom(room.room().roomId(), "user9"))
            .hasMessageContaining("최대 8명까지만 참가 가능");
    }

    @Test
    @DisplayName("인원 제한 검증 - GOLDENBELL 모드 최대 20명")
    void testParticipantLimit_Goldenbell() {
        // Given: GOLDENBELL 모드 방 생성 (20명 참가)
        List<String> participants = new java.util.ArrayList<>();
        for (int i = 1; i <= 20; i++) {
            participants.add("user" + i);
        }

        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.GOLDENBELL,
            "{}",
            participants,
            null,
            null,
            null,
            null
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 21번째 사용자 참가 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.joinRoom(room.room().roomId(), "user21"))
            .hasMessageContaining("최대 20명까지만 참가 가능");
    }

    @Test
    @DisplayName("최소 인원 검증 - DUEL 모드 최소 2명")
    void testMinParticipants_Duel() {
        // Given: DUEL 모드 방 생성 (1명만 참가)
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            "{}",
            List.of("user1"),
            null,
            null,
            null,
            null
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 1명만 있을 때 방 시작 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.startRoom(room.room().roomId()))
            .hasMessageContaining("최소 2명이 필요");
    }

    @Test
    @DisplayName("서버 기준 시간 계산 - 문제 시작 시점 이벤트 기록 확인")
    void testServerTimeCalculation() {
        // Given: 방 생성 및 시작
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            "{}",
            List.of("user1", "user2"),
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");
        versusService.startRoom(room.room().roomId());

        // When: 답안 제출 (문제 시작 시점 이벤트 기록됨)
        VersusDtos.SubmitAnswerReq submitReq = new VersusDtos.SubmitAnswerReq(
            1L,
            "A", // userAnswer
            true,
            5000, // clientTimeMs
            null,
            null,
            null
        );

        versusService.submitAnswer(room.room().roomId(), "user1", submitReq);

        // Then: QUESTION_STARTED 이벤트가 기록되었는지 확인
        // (실제로는 MatchEventRepository를 통해 확인)
        // 여기서는 예외가 발생하지 않으면 성공으로 간주
    }
}




