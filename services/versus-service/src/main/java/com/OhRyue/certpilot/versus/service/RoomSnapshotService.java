package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.client.AccountServiceClient;
import com.OhRyue.certpilot.versus.domain.*;
import com.OhRyue.certpilot.versus.dto.RoomSnapshotDto;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.*;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 방 상태 스냅샷 서비스
 * 
 * WebSocket JOIN_ROOM 시 현재 방의 상태를 스냅샷으로 구성하여 반환
 * 재접속(새로고침 포함) 시에도 동일한 스냅샷을 받아 현재 대전 상태를 복구할 수 있음
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RoomSnapshotService {

    private final MatchRoomRepository roomRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchQuestionRepository questionRepository;
    private final MatchEventRepository eventRepository;
    private final GoldenbellStateRepository goldenbellStateRepository;
    private final AccountServiceClient accountServiceClient;
    private final VersusService versusService;
    private final ObjectMapper objectMapper;

    /**
     * 방 상태 스냅샷 생성
     * 
     * @param roomId 방 ID
     * @return 방 상태 스냅샷
     */
    @Transactional(readOnly = true)
    public RoomSnapshotDto createSnapshot(Long roomId) {
        MatchRoom room = roomRepository.findById(roomId)
                .orElseThrow(() -> new IllegalArgumentException("Room not found: " + roomId));

        // 방 기본 정보
        RoomSnapshotDto.RoomInfo roomInfo = buildRoomInfo(room);

        // 참가자 목록
        List<RoomSnapshotDto.ParticipantInfo> participants = buildParticipants(roomId);

        // 현재 문제 정보
        RoomSnapshotDto.CurrentQuestionSnapshot currentQuestion = buildCurrentQuestion(roomId, room.getStatus());

        // 쉬는 시간 정보
        RoomSnapshotDto.IntermissionSnapshot intermission = buildIntermission(roomId, room.getStatus());

        // 현재 라운드/페이즈
        Integer currentRoundNo = determineCurrentRoundNo(currentQuestion, intermission);
        MatchPhase currentPhase = determineCurrentPhase(currentQuestion, intermission);

        // 스코어보드
        RoomSnapshotDto.ScoreboardSnapshot scoreboard = buildScoreboard(room);

        return new RoomSnapshotDto(
                roomInfo,
                participants,
                currentQuestion,
                intermission,
                currentRoundNo,
                currentPhase,
                scoreboard,
                Instant.now()
        );
    }

    /**
     * 방 기본 정보 구성
     */
    private RoomSnapshotDto.RoomInfo buildRoomInfo(MatchRoom room) {
        String examMode = extractExamMode(room);
        
        return new RoomSnapshotDto.RoomInfo(
                room.getId(),
                room.getMode(),
                room.getStatus(),
                examMode,
                room.getCreatedAt(),
                room.getScheduledAt(),
                room.getIsBotMatch()
        );
    }

    /**
     * 참가자 목록 구성
     */
    private List<RoomSnapshotDto.ParticipantInfo> buildParticipants(Long roomId) {
        List<MatchParticipant> participants = participantRepository.findByRoomId(roomId);
        Map<String, GoldenbellState> goldenState = goldenbellStateRepository.findByRoomId(roomId).stream()
                .collect(Collectors.toMap(GoldenbellState::getUserId, g -> g));

        // 사용자 프로필 정보 조회
        List<String> userIds = participants.stream()
                .map(MatchParticipant::getUserId)
                .toList();
        Map<String, AccountServiceClient.ProfileSummary> profileMap = new HashMap<>();
        try {
            List<AccountServiceClient.ProfileSummary> profiles = accountServiceClient.getUserProfiles(userIds);
            profileMap = profiles.stream()
                    .collect(Collectors.toMap(
                            AccountServiceClient.ProfileSummary::userId,
                            profile -> profile,
                            (a, b) -> a
                    ));
        } catch (Exception e) {
            log.warn("사용자 프로필 조회 실패: roomId={}, error={}", roomId, e.getMessage());
        }

        Map<String, AccountServiceClient.ProfileSummary> finalProfileMap = profileMap;
        return participants.stream()
                .map(p -> {
                    AccountServiceClient.ProfileSummary profile = finalProfileMap.get(p.getUserId());
                    GoldenbellState gs = goldenState.get(p.getUserId());
                    
                    return new RoomSnapshotDto.ParticipantInfo(
                            p.getUserId(),
                            profile != null ? profile.nickname() : null,
                            profile != null ? profile.skinId() : null,
                            p.isEliminated(),
                            gs != null && gs.isRevived(),
                            p.getFinalScore(),
                            p.getPlayerRank(),
                            p.getJoinedAt()
                    );
                })
                .collect(Collectors.toList());
    }

    /**
     * 현재 문제 정보 구성
     */
    private RoomSnapshotDto.CurrentQuestionSnapshot buildCurrentQuestion(Long roomId, MatchStatus status) {
        if (status != MatchStatus.ONGOING) {
            return null;
        }

        try {
            // 가장 최근 QUESTION_STARTED 이벤트 찾기
            List<MatchEvent> startEvents = eventRepository.findByRoomIdAndEventTypeContaining(
                    roomId, "QUESTION_STARTED");

            Optional<MatchEvent> latestEvent = startEvents.stream()
                    .max(Comparator.comparing(MatchEvent::getCreatedAt));

            if (latestEvent.isPresent()) {
                MatchEvent event = latestEvent.get();
                if (event.getPayloadJson() == null) {
                    return null;
                }

                Map<String, Object> payload = objectMapper.readValue(
                        event.getPayloadJson(), new TypeReference<Map<String, Object>>() {});

                Object questionIdObj = payload.get("questionId");
                if (questionIdObj == null) {
                    return null;
                }

                Long questionId = Long.valueOf(questionIdObj.toString());
                Integer roundNo = payload.get("roundNo") != null
                        ? Integer.valueOf(payload.get("roundNo").toString())
                        : null;
                String phaseStr = payload.get("phase") != null
                        ? payload.get("phase").toString()
                        : null;
                MatchPhase phase = phaseStr != null
                        ? MatchPhase.valueOf(phaseStr)
                        : null;

                // startedAt 시간 가져오기
                String startedAtStr = (String) payload.get("startedAt");
                Instant startTime = startedAtStr != null
                        ? Instant.parse(startedAtStr)
                        : event.getCreatedAt();

                // startedAt 시간이 아직 지나지 않았으면 null 반환
                Instant now = Instant.now();
                if (now.isBefore(startTime)) {
                    return null;
                }

                // MatchQuestion에서 정보 가져오기
                Optional<MatchQuestion> matchQuestion = questionRepository.findByRoomIdAndQuestionId(roomId, questionId);
                if (matchQuestion.isPresent()) {
                    MatchQuestion q = matchQuestion.get();
                    Instant endTime = startTime.plusSeconds(q.getTimeLimitSec());
                    long remainingSeconds = Math.max(0, java.time.Duration.between(now, endTime).getSeconds());

                    return new RoomSnapshotDto.CurrentQuestionSnapshot(
                            questionId,
                            roundNo != null ? roundNo : q.getRoundNo(),
                            phase != null ? phase : q.getPhase(),
                            q.getOrderNo(),
                            q.getTimeLimitSec(),
                            endTime,
                            remainingSeconds
                    );
                }
            }
        } catch (Exception e) {
            log.debug("Failed to build current question snapshot: {}", e.getMessage());
        }

        return null;
    }

    /**
     * 쉬는 시간 정보 구성
     */
    private RoomSnapshotDto.IntermissionSnapshot buildIntermission(Long roomId, MatchStatus status) {
        if (status != MatchStatus.ONGOING) {
            return null;
        }

        try {
            // 가장 최근 INTERMISSION_STARTED 이벤트 찾기
            List<MatchEvent> intermissionEvents = eventRepository.findByRoomIdAndEventTypeContaining(
                    roomId, "INTERMISSION_STARTED");

            Optional<MatchEvent> latestIntermission = intermissionEvents.stream()
                    .max(Comparator.comparing(MatchEvent::getCreatedAt));

            if (latestIntermission.isPresent()) {
                MatchEvent event = latestIntermission.get();
                if (event.getPayloadJson() == null) {
                    return null;
                }

                Map<String, Object> payload = objectMapper.readValue(
                        event.getPayloadJson(), new TypeReference<Map<String, Object>>() {});

                String questionStartAtStr = (String) payload.get("questionStartAt");
                if (questionStartAtStr == null) {
                    return null;
                }

                Instant questionStartAt = Instant.parse(questionStartAtStr);
                Instant now = Instant.now();

                // questionStartAt 시간이 지나지 않았으면 쉬는 시간 중
                if (now.isBefore(questionStartAt)) {
                    Object nextQuestionIdObj = payload.get("nextQuestionId");
                    Object nextRoundNoObj = payload.get("nextRoundNo");
                    Object nextPhaseStr = payload.get("nextPhase");
                    Object durationSecObj = payload.get("durationSec");

                    Long nextQuestionId = nextQuestionIdObj != null
                            ? Long.valueOf(nextQuestionIdObj.toString())
                            : null;
                    Integer nextRoundNo = nextRoundNoObj != null
                            ? Integer.valueOf(nextRoundNoObj.toString())
                            : null;
                    MatchPhase nextPhase = nextPhaseStr != null
                            ? MatchPhase.valueOf(nextPhaseStr.toString())
                            : null;
                    Integer durationSec = durationSecObj != null
                            ? Integer.valueOf(durationSecObj.toString())
                            : null;

                    long remainingSeconds = Math.max(0, java.time.Duration.between(now, questionStartAt).getSeconds());

                    return new RoomSnapshotDto.IntermissionSnapshot(
                            nextQuestionId,
                            nextRoundNo,
                            nextPhase,
                            durationSec,
                            questionStartAt,
                            remainingSeconds
                    );
                }
            }
        } catch (Exception e) {
            log.debug("Failed to build intermission snapshot: {}", e.getMessage());
        }

        return null;
    }

    /**
     * 현재 라운드 번호 결정
     */
    private Integer determineCurrentRoundNo(
            RoomSnapshotDto.CurrentQuestionSnapshot currentQuestion,
            RoomSnapshotDto.IntermissionSnapshot intermission) {
        if (currentQuestion != null) {
            return currentQuestion.roundNo();
        }
        if (intermission != null) {
            return intermission.nextRoundNo();
        }
        return null;
    }

    /**
     * 현재 페이즈 결정
     */
    private MatchPhase determineCurrentPhase(
            RoomSnapshotDto.CurrentQuestionSnapshot currentQuestion,
            RoomSnapshotDto.IntermissionSnapshot intermission) {
        if (currentQuestion != null) {
            return currentQuestion.phase();
        }
        if (intermission != null) {
            return intermission.nextPhase();
        }
        return null;
    }

    /**
     * 스코어보드 구성
     */
    private RoomSnapshotDto.ScoreboardSnapshot buildScoreboard(MatchRoom room) {
        VersusDtos.ScoreBoardResp scoreboard = versusService.computeScoreboard(room);
        
        List<RoomSnapshotDto.ScoreboardItem> items = scoreboard.items().stream()
                .map(item -> new RoomSnapshotDto.ScoreboardItem(
                        item.userId(),
                        item.nickname(),
                        item.skinId(),
                        item.correctCount(),
                        item.totalCount(),
                        item.score(),
                        item.totalTimeMs(),
                        item.rank(),
                        item.alive(),
                        item.revived()
                ))
                .collect(Collectors.toList());

        return new RoomSnapshotDto.ScoreboardSnapshot(items);
    }

    /**
     * 방의 examMode 추출 (기존 VersusService 로직 재사용)
     */
    private String extractExamMode(MatchRoom room) {
        if (room.getScopeJson() == null || room.getScopeJson().isBlank()) {
            return null;
        }
        try {
            Map<String, Object> scope = objectMapper.readValue(
                    room.getScopeJson(), new TypeReference<Map<String, Object>>() {});
            Object examMode = scope.get("examMode");
            return examMode != null ? examMode.toString() : null;
        } catch (Exception e) {
            log.debug("Failed to extract examMode: {}", e.getMessage());
            return null;
        }
    }
}

