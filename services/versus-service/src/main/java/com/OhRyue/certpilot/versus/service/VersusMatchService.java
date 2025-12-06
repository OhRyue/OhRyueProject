package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.*;
import com.OhRyue.certpilot.versus.repository.*;
import com.OhRyue.certpilot.versus.support.VersusBotConst;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Versus 매칭 서비스
 * - 연습 봇과의 매칭 기능 제공
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class VersusMatchService {

    private final MatchRoomRepository matchRoomRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchEventRepository eventRepository;
    private final GoldenbellStateRepository goldenbellStateRepository;
    private final GoldenbellRuleRepository goldenbellRuleRepository;
    private final BotPlayService botPlayService;
    private final VersusService versusService;

    /**
     * 연습 봇과 1:1 배틀 시작
     * @param userId 사용자 ID
     * @param scopeType 카테고리 모드("CATEGORY") 또는 난이도 모드("DIFFICULTY")
     * @param topicId 카테고리 모드일 때 2레벨 토픽 ID (null 가능)
     * @param difficulty 난이도 모드일 때 난이도 ("EASY", "NORMAL", "HARD") (null 가능)
     */
    @Transactional
    public DuelWithBotResult startDuelWithBot(String userId, String examMode, String scopeType, Long topicId, String difficulty) {
        // examMode 기본값 처리
        if (examMode == null || examMode.isBlank()) {
            examMode = "WRITTEN";
        }
        if (!examMode.equalsIgnoreCase("WRITTEN") && !examMode.equalsIgnoreCase("PRACTICAL")) {
            throw new IllegalArgumentException("examMode는 WRITTEN 또는 PRACTICAL이어야 합니다.");
        }
        
        // scopeJson 생성
        String scopeJson = buildScopeJson(examMode, scopeType, topicId, difficulty);
        
        // 1) DUEL 방 생성 (status = ONGOING, isBotMatch = true)
        MatchRoom room = MatchRoom.builder()
                .mode(MatchMode.DUEL)
                .status(MatchStatus.ONGOING)
                .scopeJson(scopeJson)
                .isBotMatch(true)  // 봇전으로 표시
                .build();
        matchRoomRepository.save(room);

        // 2) 참가자 등록: 나 + 봇
        MatchParticipant me = MatchParticipant.builder()
                .roomId(room.getId())
                .userId(userId)
                .joinedAt(Instant.now())
                .eliminated(false)
                .build();

        MatchParticipant bot = MatchParticipant.builder()
                .roomId(room.getId())
                .userId(VersusBotConst.DUEL_BOT_USER_ID)
                .joinedAt(Instant.now())
                .eliminated(false)
                .build();

        participantRepository.save(me);
        participantRepository.save(bot);

        // 3) 타임라인 이벤트
        saveEvent(room.getId(), "ROOM_CREATED", Map.of(
                "mode", "DUEL",
                "type", "BOT_MATCH"
        ));

        saveEvent(room.getId(), "PLAYER_JOINED", Map.of(
                "userId", userId,
                "role", "PLAYER"
        ));

        saveEvent(room.getId(), "PLAYER_JOINED", Map.of(
                "userId", VersusBotConst.DUEL_BOT_USER_ID,
                "nickname", VersusBotConst.DUEL_BOT_NICKNAME,
                "role", "BOT"
        ));

        saveEvent(room.getId(), "MATCH_STARTED", Map.of(
                "mode", "DUEL",
                "opponent", VersusBotConst.DUEL_BOT_USER_ID,
                "opponentNickname", VersusBotConst.DUEL_BOT_NICKNAME,
                "type", "BOT_MATCH",
                "startedAt", Instant.now().toString()
        ));

        // 4) 문제 자동 생성은 BotPlayService에서 처리
        // (봇 플레이 시작 시 문제가 없으면 자동 생성)
        // 하지만 첫 번째 문제 시작 이벤트를 기록하기 위해 문제가 생성될 때까지 기다려야 함
        // BotPlayService.simulateDuelBotPlayAsync에서 문제 생성 후 첫 번째 문제 시작 이벤트를 기록하도록 수정 필요
        
        // 임시 해결책: 문제가 생성될 때까지 대기하지 않고, 
        // BotPlayService에서 문제 생성 후 첫 번째 문제 시작 이벤트를 기록하도록 함
        // 또는 여기서 문제를 미리 생성하고 첫 번째 문제 시작 이벤트를 기록
        
        // 6) 응답 DTO
        DuelWithBotResult result = new DuelWithBotResult(
                room.getId(),
                userId,
                VersusBotConst.DUEL_BOT_USER_ID,
                VersusBotConst.DUEL_BOT_NICKNAME,
                scopeJson
        );

        // 7) 봇 자동 플레이 비동기 시작 (트랜잭션 커밋 이후 실행)
        // JWT 토큰 추출 (study-service 호출용)
        String jwtToken = extractJwtToken();
        botPlayService.simulateDuelBotPlayAsync(room.getId(), VersusBotConst.DUEL_BOT_USER_ID, jwtToken);

        log.info("연습 봇 매칭 시작: roomId={}, userId={}, botUserId={}, scopeType={}, topicId={}, difficulty={}", 
                room.getId(), userId, VersusBotConst.DUEL_BOT_USER_ID, scopeType, topicId, difficulty);

        return result;
    }

    private void saveEvent(Long roomId, String eventType, Map<String, Object> payload) {
        try {
            String payloadJson = payload == null || payload.isEmpty() 
                    ? null 
                    : new com.fasterxml.jackson.databind.ObjectMapper().writeValueAsString(payload);
            
            MatchEvent event = MatchEvent.builder()
                    .roomId(roomId)
                    .eventType(eventType)
                    .payloadJson(payloadJson)
                    .build();
            
            eventRepository.save(event);
        } catch (Exception e) {
            log.warn("이벤트 저장 실패: roomId={}, eventType={}, error={}", 
                    roomId, eventType, e.getMessage());
        }
    }

    /**
     * TOURNAMENT 봇 자동 매칭 시작
     * @param userId 사용자 ID
     * @param examMode "WRITTEN" 또는 "PRACTICAL" (기본값: "WRITTEN")
     */
    @Transactional
    public TournamentWithBotResult startTournamentWithBot(String userId, String examMode) {
        // examMode 기본값 처리
        if (examMode == null || examMode.isBlank()) {
            examMode = "WRITTEN";
        }
        if (!examMode.equalsIgnoreCase("WRITTEN") && !examMode.equalsIgnoreCase("PRACTICAL")) {
            throw new IllegalArgumentException("examMode는 WRITTEN 또는 PRACTICAL이어야 합니다.");
        }
        
        // 1) TOURNAMENT 방 생성
        String scopeJson = String.format("{\"examMode\":\"%s\",\"topicScope\":\"ALL\",\"difficulty\":\"NORMAL\"}", examMode);
        MatchRoom room = MatchRoom.builder()
                .mode(MatchMode.TOURNAMENT)
                .status(MatchStatus.ONGOING)
                .scopeJson(scopeJson)
                .isBotMatch(true)  // 봇전으로 표시
                .build();
        matchRoomRepository.save(room);

        // 2) 사용자 참가
        MatchParticipant me = MatchParticipant.builder()
                .roomId(room.getId())
                .userId(userId)
                .joinedAt(Instant.now())
                .eliminated(false)
                .build();
        participantRepository.save(me);

        // 3) 봇 7명 자동 참가 (총 8명)
        List<String> botUserIds = new ArrayList<>();
        for (int i = 1; i <= 7; i++) {
            VersusBotConst.BotDifficulty difficulty = i <= 2 ? VersusBotConst.BotDifficulty.EASY
                    : i <= 5 ? VersusBotConst.BotDifficulty.NORMAL
                    : VersusBotConst.BotDifficulty.HARD;
            
            String botUserId = VersusBotConst.generateBotUserId(difficulty, i);
            botUserIds.add(botUserId);
            
            MatchParticipant bot = MatchParticipant.builder()
                    .roomId(room.getId())
                    .userId(botUserId)
                    .joinedAt(Instant.now())
                    .eliminated(false)
                    .build();
            participantRepository.save(bot);
            
            saveEvent(room.getId(), "PLAYER_JOINED", Map.of(
                    "userId", botUserId,
                    "nickname", VersusBotConst.getBotNickname(difficulty),
                    "role", "BOT"
            ));
        }

        saveEvent(room.getId(), "ROOM_CREATED", Map.of(
                "mode", "TOURNAMENT",
                "type", "BOT_MATCH"
        ));

        saveEvent(room.getId(), "PLAYER_JOINED", Map.of(
                "userId", userId,
                "role", "PLAYER"
        ));

        saveEvent(room.getId(), "MATCH_STARTED", Map.of(
                "mode", "TOURNAMENT",
                "type", "BOT_MATCH",
                "totalParticipants", 8
        ));

        // 4) 봇 자동 플레이 비동기 시작
        String jwtToken = extractJwtToken();
        botPlayService.simulateTournamentBotPlayAsync(room.getId(), jwtToken);

        log.info("TOURNAMENT 봇 매칭 시작: roomId={}, userId={}, bots={}", 
                room.getId(), userId, botUserIds.size());

        return new TournamentWithBotResult(
                room.getId(),
                userId,
                botUserIds
        );
    }

    /**
     * GOLDENBELL 봇 자동 매칭 시작
     * @param userId 사용자 ID
     * @param examMode "WRITTEN" 또는 "PRACTICAL" (기본값: "WRITTEN")
     */
    @Transactional
    public GoldenbellWithBotResult startGoldenbellWithBot(String userId, String examMode) {
        // examMode 기본값 처리
        if (examMode == null || examMode.isBlank()) {
            examMode = "WRITTEN";
        }
        if (!examMode.equalsIgnoreCase("WRITTEN") && !examMode.equalsIgnoreCase("PRACTICAL")) {
            throw new IllegalArgumentException("examMode는 WRITTEN 또는 PRACTICAL이어야 합니다.");
        }
        boolean isPractical = "PRACTICAL".equalsIgnoreCase(examMode);
        
        // 1) GOLDENBELL 방 생성
        String scopeJson = String.format("{\"examMode\":\"%s\",\"topicScope\":\"ALL\",\"difficulty\":\"NORMAL\"}", examMode);
        MatchRoom room = MatchRoom.builder()
                .mode(MatchMode.GOLDENBELL)
                .status(MatchStatus.ONGOING)
                .scopeJson(scopeJson)
                .isBotMatch(true)  // 봇전으로 표시
                .build();
        matchRoomRepository.save(room);

        // 2) 사용자 참가
        MatchParticipant me = MatchParticipant.builder()
                .roomId(room.getId())
                .userId(userId)
                .joinedAt(Instant.now())
                .eliminated(false)
                .build();
        participantRepository.save(me);

        // 3) GoldenbellState 초기화
        goldenbellStateRepository.save(GoldenbellState.alive(room.getId(), userId));

        // 4) 봇 19명 자동 참가 (총 20명)
        List<String> botUserIds = new ArrayList<>();
        for (int i = 1; i <= 19; i++) {
            VersusBotConst.BotDifficulty difficulty = i <= 6 ? VersusBotConst.BotDifficulty.EASY
                    : i <= 13 ? VersusBotConst.BotDifficulty.NORMAL
                    : VersusBotConst.BotDifficulty.HARD;
            
            String botUserId = VersusBotConst.generateBotUserId(difficulty, i);
            botUserIds.add(botUserId);
            
            MatchParticipant bot = MatchParticipant.builder()
                    .roomId(room.getId())
                    .userId(botUserId)
                    .joinedAt(Instant.now())
                    .eliminated(false)
                    .build();
            participantRepository.save(bot);
            
            goldenbellStateRepository.save(GoldenbellState.alive(room.getId(), botUserId));
            
            saveEvent(room.getId(), "PLAYER_JOINED", Map.of(
                    "userId", botUserId,
                    "nickname", VersusBotConst.getBotNickname(difficulty),
                    "role", "BOT"
            ));
        }

        // 5) GoldenbellRule 설정 (새로운 규칙)
        String ruleJson;
        String revivalRuleJson;
        
        if (isPractical) {
            // 실기 골든벨: SHORT만 사용
            ruleJson = """
                    [
                      {"round":1,"type":"SHORT","count":2,"limitSec":25,"phase":"MAIN"},
                      {"round":2,"type":"SHORT","count":2,"limitSec":25,"phase":"MAIN"},
                      {"round":3,"type":"SHORT","count":1,"limitSec":25,"phase":"REVIVAL"},
                      {"round":4,"type":"SHORT","count":2,"limitSec":25,"phase":"FINAL"}
                    ]
                    """;
        } else {
            // 필기 골든벨: OX, MCQ 사용
            ruleJson = """
                    [
                      {"round":1,"type":"OX","count":2,"limitSec":8,"phase":"MAIN"},
                      {"round":2,"type":"MCQ","count":2,"limitSec":12,"phase":"MAIN"},
                      {"round":3,"type":"MCQ","count":1,"limitSec":15,"phase":"REVIVAL"},
                      {"round":4,"type":"MCQ","count":2,"limitSec":12,"phase":"FINAL"}
                    ]
                    """;
        }
        
        // 패자부활 규칙 (필기/실기 공통)
        revivalRuleJson = """
                {
                  "enabled": true,
                  "triggerMode": "AFTER_QUESTION_INDEX",
                  "triggerAfterIndex": 4,
                  "minAlive": 5,
                  "revivalPhase": "REVIVAL",
                  "mode": "ONE_QUESTION_FASTEST",
                  "revivalQuestion": {"type":"%s","limitSec":%d},
                  "slots": 1,
                  "spectatorExistingAlive": true,
                  "candidates": "ELIMINATED_ONLY",
                  "earlyZeroAlive": {
                    "enabled": true,
                    "topN": 5,
                    "criteria": "best_correct_then_fastest",
                    "fillIfAllWrong": true,
                    "phase": "REVIVAL"
                  }
                }
                """.formatted(isPractical ? "SHORT" : "MCQ", isPractical ? 25 : 15);
        
        goldenbellRuleRepository.save(GoldenbellRule.builder()
                .roomId(room.getId())
                .roundFlowJson(ruleJson)
                .elimination(GoldenbellElimination.IMMEDIATE)
                .revivalRuleJson(revivalRuleJson)
                .build());

        saveEvent(room.getId(), "ROOM_CREATED", Map.of(
                "mode", "GOLDENBELL",
                "type", "BOT_MATCH"
        ));

        saveEvent(room.getId(), "PLAYER_JOINED", Map.of(
                "userId", userId,
                "role", "PLAYER"
        ));

        saveEvent(room.getId(), "MATCH_STARTED", Map.of(
                "mode", "GOLDENBELL",
                "type", "BOT_MATCH",
                "totalParticipants", 20
        ));

        // 6) 봇 자동 플레이 비동기 시작
        String jwtToken = extractJwtToken();
        botPlayService.simulateGoldenbellBotPlayAsync(room.getId(), jwtToken);

        log.info("GOLDENBELL 봇 매칭 시작: roomId={}, userId={}, bots={}", 
                room.getId(), userId, botUserIds.size());

        return new GoldenbellWithBotResult(
                room.getId(),
                userId,
                botUserIds
        );
    }

    /**
     * scopeJson 생성 (카테고리 모드 또는 난이도 모드)
     */
    private String buildScopeJson(String examMode, String scopeType, Long topicId, String difficulty) {
        try {
            Map<String, Object> scope = new HashMap<>();
            scope.put("examMode", examMode != null ? examMode.toUpperCase() : "WRITTEN");
            
            if ("CATEGORY".equalsIgnoreCase(scopeType)) {
                // 카테고리 모드: 2레벨 토픽 한 가지
                if (topicId == null) {
                    throw new IllegalArgumentException("카테고리 모드일 때 topicId는 필수입니다.");
                }
                scope.put("topicScope", "CATEGORY");
                scope.put("topicId", topicId);
                scope.put("difficulty", "ALL"); // 카테고리 모드는 난이도 무관
            } else if ("DIFFICULTY".equalsIgnoreCase(scopeType)) {
                // 난이도 모드: 쉬움, 보통, 어려움
                if (difficulty == null || difficulty.isBlank()) {
                    difficulty = "NORMAL"; // 기본값
                }
                if (!difficulty.equalsIgnoreCase("EASY") && 
                    !difficulty.equalsIgnoreCase("NORMAL") && 
                    !difficulty.equalsIgnoreCase("HARD")) {
                    throw new IllegalArgumentException("난이도는 EASY, NORMAL, HARD 중 하나여야 합니다.");
                }
                scope.put("topicScope", "ALL");
                scope.put("difficulty", difficulty.toUpperCase());
            } else {
                // 기본값: 전체 범위, 보통 난이도
                scope.put("topicScope", "ALL");
                scope.put("difficulty", "NORMAL");
            }
            
            return new com.fasterxml.jackson.databind.ObjectMapper().writeValueAsString(scope);
        } catch (Exception e) {
            log.warn("scopeJson 생성 실패, 기본값 사용: {}", e.getMessage());
            return "{\"examMode\":\"WRITTEN\",\"topicScope\":\"ALL\",\"difficulty\":\"NORMAL\"}";
        }
    }

    public record DuelWithBotResult(
            Long roomId,
            String myUserId,
            String botUserId,
            String botNickname,
            String scopeJson
    ) {}

    public record TournamentWithBotResult(
            Long roomId,
            String myUserId,
            List<String> botUserIds
    ) {}

    public record GoldenbellWithBotResult(
            Long roomId,
            String myUserId,
            List<String> botUserIds
    ) {}

    /**
     * 현재 요청에서 JWT 토큰 추출
     */
    private String extractJwtToken() {
        try {
            ServletRequestAttributes attributes = 
                (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
            if (attributes != null) {
                HttpServletRequest request = attributes.getRequest();
                String authHeader = request.getHeader("Authorization");
                if (authHeader != null && !authHeader.isBlank()) {
                    return authHeader;
                }
            }
        } catch (Exception e) {
            log.warn("JWT 토큰 추출 실패: {}", e.getMessage());
        }
        return null;
    }
}


