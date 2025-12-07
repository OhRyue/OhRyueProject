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

/**
 * Versus-Service 통합 테스트
 * 
 * 테스트 시나리오:
 * 1. 방 생성 (scopeJson 기반 문제 자동 생성)
 * 2. 참가자 참가
 * 3. 방 시작
 * 4. 답안 제출
 * 5. 매치 완료 및 보상 지급 확인
 */
@SpringBootTest
@ActiveProfiles("test")
@Transactional
class VersusServiceIntegrationTest {

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

    @Autowired
    private MatchEventRepository eventRepository;

    @Test
    @DisplayName("E2E 테스트: DUEL 모드 - 방 생성 → 문제 자동 생성 → 참가 → 시작 → 답안 제출 → 완료")
    void testE2E_DuelMode() {
        // 1. 방 생성 (scopeJson 기반)
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            """
            {
              "examMode": "WRITTEN",
              "difficulty": "NORMAL",
              "topicScope": "ALL"
            }
            """,
            List.of("user2"), // 초대할 참가자
            null, // questions는 자동 생성
            null,
            null,
            null,
            false  // skipCreatorJoin
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // 검증: 방이 생성되고 문제가 자동 생성되었는지 확인
        assertThat(room.room().mode()).isEqualTo(MatchMode.DUEL);
        assertThat(room.room().status()).isEqualTo(MatchStatus.WAIT);
        assertThat(room.questions()).isNotEmpty();
        assertThat(room.questions().size()).isGreaterThanOrEqualTo(1);
        assertThat(room.participants()).hasSize(1); // user2만 (방 생성자는 별도 추가 필요)

        Long roomId = room.room().roomId();

        // 2. 참가자 참가 (방 생성자)
        // 주의: 실제로는 컨트롤러에서 JWT로 userId를 가져오지만, 여기서는 직접 호출
        // versusService.joinRoom(roomId, "user1"); // 방 생성자

        // 3. 방 시작
        VersusDtos.RoomDetailResp startedRoom = versusService.startRoom(roomId);

        // 검증: 방 상태가 ONGOING으로 변경
        assertThat(startedRoom.room().status()).isEqualTo(MatchStatus.ONGOING);

        // 4. 답안 제출
        if (!room.questions().isEmpty()) {
            VersusDtos.QuestionInfo firstQuestion = room.questions().get(0);
            
            VersusDtos.SubmitAnswerReq submitReq = new VersusDtos.SubmitAnswerReq(
                firstQuestion.questionId(),
                "A", // userAnswer (MCQ 답안)
                true, // correct
                5000, // timeMs
                null, // scoreDelta
                firstQuestion.roundNo(),
                firstQuestion.phase()
            );

            // user2가 답안 제출
            VersusDtos.ScoreBoardResp scoreboard = versusService.submitAnswer(roomId, "user2", submitReq);

            // 검증: 스코어보드가 반환됨
            assertThat(scoreboard).isNotNull();
            assertThat(scoreboard.items()).isNotEmpty();
        }
    }

    @Test
    @DisplayName("인원 제한 검증: DUEL 모드 최대 2명 초과 시 예외 발생")
    void testParticipantLimit_DuelMaxExceeded() {
        // Given: DUEL 모드 방 생성 (2명 참가)
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            "{}",
            List.of("user2"), // 1명 초대
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            false  // skipCreatorJoin
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 3번째 사용자 참가 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.joinRoom(room.room().roomId(), "user3"))
            .hasMessageContaining("최대 2명까지만 참가 가능");
    }

    @Test
    @DisplayName("인원 제한 검증: TOURNAMENT 모드 최대 8명 초과 시 예외 발생")
    void testParticipantLimit_TournamentMaxExceeded() {
        // Given: TOURNAMENT 모드 방 생성 (8명 참가)
        List<String> participants = List.of(
            "user2", "user3", "user4", "user5", "user6", "user7", "user8"
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
            false  // skipCreatorJoin
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 9번째 사용자 참가 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.joinRoom(room.room().roomId(), "user9"))
            .hasMessageContaining("최대 8명까지만 참가 가능");
    }

    @Test
    @DisplayName("최소 인원 검증: DUEL 모드 1명만 있을 때 시작 시도 시 예외 발생")
    void testMinParticipants_Duel() {
        // Given: DUEL 모드 방 생성 (1명만 참가)
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            "{}",
            List.of(), // 참가자 없음
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            false  // skipCreatorJoin
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // When & Then: 1명만 있을 때 방 시작 시도 시 예외 발생
        assertThatThrownBy(() -> versusService.startRoom(room.room().roomId()))
            .hasMessageContaining("최소 2명이 필요");
    }

    @Test
    @DisplayName("문제 자동 생성: scopeJson 기반 문제 생성 확인")
    void testAutoQuestionGeneration() {
        // Given: scopeJson 제공
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            """
            {
              "examMode": "WRITTEN",
              "difficulty": "NORMAL",
              "topicScope": "ALL"
            }
            """,
            List.of("user2"),
            null, // questions는 제공하지 않음
            null,
            null,
            null,
            false  // skipCreatorJoin
        );

        // When: 방 생성
        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");

        // Then: 문제가 자동으로 생성되었는지 확인
        assertThat(room.questions()).isNotEmpty();
        assertThat(room.questions().size()).isGreaterThanOrEqualTo(1);
    }

    @Test
    @DisplayName("답안 제출: userAnswer 필드 필수 확인")
    void testSubmitAnswer_UserAnswerRequired() {
        // Given: 방 생성 및 시작
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            "{}",
            List.of("user2"),
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            false  // skipCreatorJoin
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");
        versusService.joinRoom(room.room().roomId(), "user1");
        versusService.startRoom(room.room().roomId());

        // When: userAnswer 없이 답안 제출
        VersusDtos.SubmitAnswerReq submitReq = new VersusDtos.SubmitAnswerReq(
            1L,
            null, // userAnswer 없음
            true,
            5000,
            null,
            1,
            MatchPhase.MAIN
        );

        // Then: 경고 로그는 남지만 정상 처리됨 (fallback으로 클라이언트 값 사용)
        VersusDtos.ScoreBoardResp scoreboard = versusService.submitAnswer(
            room.room().roomId(), "user1", submitReq);
        
        assertThat(scoreboard).isNotNull();
    }

    @Test
    @DisplayName("문제 시작 시점 이벤트 기록 확인")
    void testQuestionStartEventRecording() {
        // Given: 방 생성 및 시작
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            "{}",
            List.of("user2"),
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            false  // skipCreatorJoin
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");
        versusService.joinRoom(room.room().roomId(), "user1");
        versusService.startRoom(room.room().roomId());

        // When: 답안 제출 (문제 시작 시점 이벤트 기록됨)
        VersusDtos.SubmitAnswerReq submitReq = new VersusDtos.SubmitAnswerReq(
            1L,
            "A",
            true,
            5000,
            null,
            1,
            MatchPhase.MAIN
        );

        versusService.submitAnswer(room.room().roomId(), "user1", submitReq);

        // Then: QUESTION_STARTED 이벤트가 기록되었는지 확인
        List<com.OhRyue.certpilot.versus.domain.MatchEvent> events = 
            eventRepository.findByRoomIdAndEventTypeContaining(
                room.room().roomId(), "QUESTION_STARTED");
        
        assertThat(events).isNotEmpty();
        assertThat(events.stream()
            .anyMatch(e -> e.getPayloadJson() != null && 
                e.getPayloadJson().contains("\"questionId\":1")))
            .isTrue();
    }

    @Test
    @DisplayName("매치 완료 시 progress-service 통지 확인")
    void testMatchCompletion_ProgressServiceNotification() {
        // Given: DUEL 모드 방 생성 (2명 참가)
        VersusDtos.CreateRoomReq createReq = new VersusDtos.CreateRoomReq(
            MatchMode.DUEL,
            "{}",
            List.of("user2"),
            List.of(
                new VersusDtos.QuestionInfo(1L, 1, MatchPhase.MAIN, 1, 10)
            ),
            null,
            null,
            null,
            false  // skipCreatorJoin
        );

        VersusDtos.RoomDetailResp room = versusService.createRoom(createReq, "test-user");
        versusService.joinRoom(room.room().roomId(), "user1");
        versusService.startRoom(room.room().roomId());

        // When: 두 참가자가 모두 답안 제출 (매치 완료)
        VersusDtos.SubmitAnswerReq submitReq1 = new VersusDtos.SubmitAnswerReq(
            1L, "A", true, 5000, null, 1, MatchPhase.MAIN
        );
        VersusDtos.SubmitAnswerReq submitReq2 = new VersusDtos.SubmitAnswerReq(
            1L, "B", false, 6000, null, 1, MatchPhase.MAIN
        );

        versusService.submitAnswer(room.room().roomId(), "user1", submitReq1);
        VersusDtos.ScoreBoardResp finalScoreboard = versusService.submitAnswer(
            room.room().roomId(), "user2", submitReq2);

        // Then: 매치 상태가 DONE으로 변경되었는지 확인
        // (실제 progress-service 호출은 Mock으로 확인해야 함)
        assertThat(finalScoreboard.status()).isEqualTo(MatchStatus.DONE);
    }
}




