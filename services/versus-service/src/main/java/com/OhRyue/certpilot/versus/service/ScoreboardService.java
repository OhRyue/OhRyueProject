package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.client.AccountServiceClient;
import com.OhRyue.certpilot.versus.domain.*;
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
import java.util.Comparator;
import java.util.stream.Collectors;

/**
 * 스코어보드 계산 서비스
 * 
 * 매치의 점수, 순위, 통계를 계산하는 전용 서비스입니다.
 * VersusService와의 순환 참조를 방지하기 위해 독립적으로 분리되었습니다.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class ScoreboardService {

    private static final int GOLDENBELL_TIME_LIMIT_SEC = 10;

    private final MatchParticipantRepository participantRepository;
    private final MatchQuestionRepository questionRepository;
    private final MatchAnswerRepository answerRepository;
    private final GoldenbellStateRepository goldenbellStateRepository;
    private final MatchEventRepository eventRepository;
    private final AccountServiceClient accountServiceClient;
    private final ObjectMapper objectMapper;

    /**
     * 스코어보드 계산 (기본)
     */
    @Transactional(readOnly = true)
    public VersusDtos.ScoreBoardResp computeScoreboard(MatchRoom room) {
        return computeScoreboard(room, null, null);
    }

    /**
     * 스코어보드 계산 (토너먼트 모드에서 현재 라운드의 활성 참가자만 표시)
     * 
     * @param room 방 정보
     * @param currentQuestion 현재 문제 (토너먼트 모드에서 라운드 필터링용, null이면 전체 표시)
     * @param currentUserId 현재 사용자 ID (토너먼트 모드에서 상대방 필터링용, null이면 전체 표시)
     */
    @Transactional(readOnly = true)
    public VersusDtos.ScoreBoardResp computeScoreboard(MatchRoom room, MatchQuestion currentQuestion, String currentUserId) {
        Long roomId = room.getId();
        List<MatchParticipant> participants = participantRepository.findByRoomId(roomId);
        Map<String, GoldenbellState> goldenState = goldenbellStateRepository.findByRoomId(roomId).stream()
                .collect(Collectors.toMap(GoldenbellState::getUserId, g -> g));

        // 사용자 프로필 정보 조회 (닉네임, 스킨 ID)
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

        Map<Long, MatchQuestion> questionMap = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId).stream()
                .collect(Collectors.toMap(
                        MatchQuestion::getQuestionId,
                        q -> q,
                        (existing, replacement) -> existing  // 중복 키 발생 시 기존 값 유지
                ));

        // 답안 조회
        List<MatchAnswer> answers = answerRepository.findByRoomId(roomId);
        Map<String, Score> stats = new HashMap<>();
        Map<String, FinalRoundScore> finalScores = new HashMap<>(); // FINAL 라운드 점수 별도 관리

        // GOLDENBELL 모드인 경우 FINAL 라운드 점수 별도 계산
        boolean isGoldenbell = room.getMode() == MatchMode.GOLDENBELL;

        for (MatchAnswer answer : answers) {
            Score score = stats.computeIfAbsent(answer.getUserId(), u -> new Score());
            MatchQuestion q = questionMap.get(answer.getQuestionId());

            // 디버깅: 답안 정보 로그
            log.debug("Processing answer: roomId={}, userId={}, questionId={}, correct={}, userAnswer=[{}], scoreDelta={}",
                    roomId, answer.getUserId(), answer.getQuestionId(), answer.isCorrect(),
                    answer.getUserAnswer(), answer.getScoreDelta());

            // FINAL 라운드인지 확인
            boolean isFinalRound = q != null && q.getPhase() == MatchPhase.FINAL;

            if (isFinalRound && isGoldenbell) {
                // FINAL 라운드 점수 별도 관리
                FinalRoundScore finalScore = finalScores.computeIfAbsent(answer.getUserId(), u -> new FinalRoundScore());
                finalScore.total++;
                if (answer.isCorrect()) {
                    finalScore.correct++;
                }
                finalScore.score += Optional.ofNullable(answer.getScoreDelta()).orElse(0);
                int limitMs = Optional.ofNullable(q)
                        .map(MatchQuestion::getTimeLimitSec)
                        .orElse(GOLDENBELL_TIME_LIMIT_SEC) * 1000;
                int time = Optional.ofNullable(answer.getTimeMs()).orElse(limitMs);
                if (time <= 0) {
                    time = limitMs;
                }
                finalScore.totalTimeMs += Math.min(time, limitMs);
            } else {
                // 일반 라운드 점수
                score.total++;
                if (answer.isCorrect()) {
                    score.correct++;
                }
                score.score += Optional.ofNullable(answer.getScoreDelta()).orElse(0);
                int limitMs = Optional.ofNullable(q)
                        .map(MatchQuestion::getTimeLimitSec)
                        .orElse(GOLDENBELL_TIME_LIMIT_SEC) * 1000;
                int time = Optional.ofNullable(answer.getTimeMs()).orElse(limitMs);
                if (time <= 0) {
                    time = limitMs;
                }
                score.totalTimeMs += Math.min(time, limitMs);
            }
        }

        // 디버깅: 최종 통계 로그
        log.debug("Scoreboard stats for room {}: {}", roomId,
                stats.entrySet().stream()
                        .map(e -> String.format("%s: correct=%d, total=%d, score=%d",
                                e.getKey(), e.getValue().correct, e.getValue().total, e.getValue().score))
                        .collect(Collectors.joining(", ")));

        for (MatchParticipant participant : participants) {
            stats.computeIfAbsent(participant.getUserId(), u -> new Score());
        }

        // GOLDENBELL 모드이고 FINAL 라운드가 있으면 FINAL 라운드 점수로 정렬
        List<ScoreboardIntermediate> intermediates;
        if (isGoldenbell && !finalScores.isEmpty()) {
            // FINAL 라운드 점수로 정렬 (FINAL 라운드 점수가 우선)
            intermediates = stats.entrySet().stream()
                    .map(entry -> {
                        String userId = entry.getKey();
                        Score score = entry.getValue();
                        FinalRoundScore finalScore = finalScores.getOrDefault(userId, new FinalRoundScore());

                        // 전체 점수 합산: 일반 라운드 점수 + FINAL 라운드 점수
                        int totalScoreValue = score.score + finalScore.score;
                        // correctCount와 totalCount는 모든 라운드 합계
                        int totalCorrect = score.correct + finalScore.correct;
                        int totalCount = score.total + finalScore.total;
                        // 전체 시간 합산: 일반 라운드 시간 + FINAL 라운드 시간
                        long totalTime = score.totalTimeMs + finalScore.totalTimeMs;

                        GoldenbellState state = goldenState.get(userId);
                        boolean alive = state != null ? state.isAlive() : participants.stream()
                                .filter(p -> p.getUserId().equals(userId))
                                .findFirst()
                                .map(p -> !p.isEliminated())
                                .orElse(true);
                        boolean revived = state != null && state.isRevived();
                        return new ScoreboardIntermediate(userId, totalCorrect, totalCount, totalScoreValue, totalTime, alive, revived);
                    })
                    .sorted((a, b) -> {
                        // alive 상태 우선: alive=true가 항상 alive=false보다 높은 순위
                        int aliveCompare = Boolean.compare(b.alive(), a.alive());
                        if (aliveCompare != 0) return aliveCompare;
                        // FINAL 라운드 점수 내림차순
                        int scoreCompare = Integer.compare(b.score(), a.score());
                        if (scoreCompare != 0) return scoreCompare;
                        // 점수가 같을 경우 전체 제출속도(합산) 빠른 사람이 우선
                        int timeCompare = Long.compare(a.totalTimeMs(), b.totalTimeMs());
                        if (timeCompare != 0) return timeCompare;
                        // userId 오름차순
                        return a.userId().compareTo(b.userId());
                    })
                    .toList();
        } else {
            // 일반 정렬 (점수 내림차순)
            intermediates = stats.entrySet().stream()
                    .map(entry -> {
                        String userId = entry.getKey();
                        Score score = entry.getValue();
                        GoldenbellState state = goldenState.get(userId);
                        boolean alive = state != null ? state.isAlive() : participants.stream()
                                .filter(p -> p.getUserId().equals(userId))
                                .findFirst()
                                .map(p -> !p.isEliminated())
                                .orElse(true);
                        boolean revived = state != null && state.isRevived();
                        return new ScoreboardIntermediate(userId, score.correct, score.total, score.score, score.totalTimeMs, alive, revived);
                    })
                    .sorted((a, b) -> {
                        // GOLDENBELL, TOURNAMENT 모드인 경우 alive 상태 우선
                        if (isGoldenbell || room.getMode() == MatchMode.TOURNAMENT) {
                            int aliveCompare = Boolean.compare(b.alive(), a.alive());
                            if (aliveCompare != 0) return aliveCompare;
                        }
                        // 점수 내림차순
                        int scoreCompare = Integer.compare(b.score(), a.score());
                        if (scoreCompare != 0) return scoreCompare;
                        // 점수가 같을 경우 전체 제출속도(합산) 빠른 사람이 우선
                        int timeCompare = Long.compare(a.totalTimeMs(), b.totalTimeMs());
                        if (timeCompare != 0) return timeCompare;
                        // userId 오름차순
                        return a.userId().compareTo(b.userId());
                    })
                    .toList();
        }

        // 정렬 결과 확인 (디버깅용)
        log.debug("정렬된 intermediates: {}", intermediates.stream()
                .map(i -> String.format("%s: score=%d, correct=%d", i.userId(), i.score(), i.correct()))
                .collect(Collectors.joining(", ")));

        // 토너먼트 모드: 현재 라운드의 활성 참가자만 필터링
        Set<String> activeUserIds = null;
        if (room.getMode() == MatchMode.TOURNAMENT && currentQuestion != null) {
            List<MatchParticipant> activeParticipants = participantRepository.findByRoomIdAndEliminatedFalse(roomId);
            activeUserIds = activeParticipants.stream()
                    .map(MatchParticipant::getUserId)
                    .collect(Collectors.toSet());

            log.debug("토너먼트 라운드 {} 활성 참가자: {}", currentQuestion.getRoundNo(), activeUserIds);
        }

        int rank = 1;
        int previousScore = Integer.MIN_VALUE;
        int previousCorrect = Integer.MIN_VALUE;
        Long previousTime = null;

        List<VersusDtos.ScoreBoardItem> finalItems = new ArrayList<>();
        Map<String, MatchParticipant> participantMap = participants.stream()
                .collect(Collectors.toMap(MatchParticipant::getUserId, p -> p));

        Map<String, AccountServiceClient.ProfileSummary> finalProfileMap = profileMap;
        for (ScoreboardIntermediate intermediate : intermediates) {
            // 토너먼트 모드: 현재 라운드의 활성 참가자만 포함
            if (activeUserIds != null && !activeUserIds.contains(intermediate.userId())) {
                continue;
            }
            Long totalTime = intermediate.totalTimeMs() == 0 && intermediate.total() == 0
                    ? null
                    : intermediate.totalTimeMs();
            if (intermediate.score() != previousScore
                    || intermediate.correct() != previousCorrect
                    || !Objects.equals(totalTime, previousTime)) {
                rank = finalItems.size() + 1;
            }
            previousScore = intermediate.score();
            previousCorrect = intermediate.correct();
            previousTime = totalTime;

            AccountServiceClient.ProfileSummary profile = finalProfileMap.get(intermediate.userId());
            finalItems.add(new VersusDtos.ScoreBoardItem(
                    intermediate.userId(),
                    profile != null ? profile.nickname() : null,
                    determineSkinId(intermediate.userId(), profile),
                    intermediate.correct(),
                    intermediate.total(),
                    intermediate.score(),
                    totalTime,
                    rank,
                    intermediate.alive(),
                    intermediate.revived()
            ));

            MatchParticipant participant = participantMap.get(intermediate.userId());
            if (participant != null) {
                participant.setFinalScore(intermediate.score());
                participant.setPlayerRank(rank);
            }
        }

        participantRepository.saveAll(participantMap.values());

        // 현재 문제 정보 계산
        VersusDtos.CurrentQuestionInfo currentQuestionInfo = getCurrentQuestionInfo(roomId, room.getStatus());

        // 쉬는 시간 정보 계산 (현재 문제가 없고 진행 중일 때만)
        VersusDtos.IntermissionInfo intermissionInfo = null;
        if (room.getStatus() == MatchStatus.ONGOING && currentQuestionInfo == null) {
            intermissionInfo = getCurrentIntermissionInfo(roomId);
        }

        List<VersusDtos.XpResult> xpResults = fetchXpResults(roomId);
        return new VersusDtos.ScoreBoardResp(roomId, room.getStatus(), finalItems, currentQuestionInfo, intermissionInfo, xpResults);
    }

    // ========== Private Helper Methods ==========

    private VersusDtos.CurrentQuestionInfo getCurrentQuestionInfo(Long roomId, MatchStatus status) {
        if (status != MatchStatus.ONGOING) {
            return null;
        }

        List<MatchEvent> startEvents = eventRepository.findByRoomIdAndEventTypeContaining(roomId, "QUESTION_STARTED");
        Optional<MatchEvent> latestQuestionStarted = startEvents.stream()
                .max(Comparator.comparing(MatchEvent::getCreatedAt));

        if (latestQuestionStarted.isEmpty()) {
            return null;
        }

        try {
            MatchEvent event = latestQuestionStarted.get();
            if (event.getPayloadJson() == null) {
                return null;
            }

            Map<String, Object> payload = objectMapper.readValue(event.getPayloadJson(), 
                    new com.fasterxml.jackson.core.type.TypeReference<Map<String, Object>>() {});
            
            Long questionId = payload.get("questionId") instanceof Number n ? n.longValue() : null;
            Integer roundNo = payload.get("roundNo") instanceof Number n ? n.intValue() : null;
            String phaseStr = payload.get("phase") instanceof String s ? s : null;
            String startedAtStr = payload.get("startedAt") instanceof String s ? s : null;

            if (questionId == null || roundNo == null || phaseStr == null) {
                return null;
            }

            MatchPhase phase = MatchPhase.valueOf(phaseStr);
            Instant startedAt = startedAtStr != null ? Instant.parse(startedAtStr) : event.getCreatedAt();

            MatchQuestion question = questionRepository.findByRoomIdAndQuestionId(roomId, questionId).orElse(null);
            if (question == null) {
                return null;
            }

            return new VersusDtos.CurrentQuestionInfo(
                    questionId,
                    roundNo,
                    phase,
                    question.getOrderNo(),
                    question.getTimeLimitSec(),
                    startedAt
            );
        } catch (Exception e) {
            log.warn("Failed to parse QUESTION_STARTED event payload: roomId={}, error={}", roomId, e.getMessage());
            return null;
        }
    }

    private VersusDtos.IntermissionInfo getCurrentIntermissionInfo(Long roomId) {
        List<MatchEvent> intermissionEvents = eventRepository.findByRoomIdAndEventTypeContaining(roomId, "INTERMISSION_STARTED");
        Optional<MatchEvent> latestIntermission = intermissionEvents.stream()
                .max(Comparator.comparing(MatchEvent::getCreatedAt));

        if (latestIntermission.isEmpty()) {
            return null;
        }

        try {
            MatchEvent event = latestIntermission.get();
            if (event.getPayloadJson() == null) {
                return null;
            }

            Map<String, Object> payload = objectMapper.readValue(event.getPayloadJson(), 
                    new com.fasterxml.jackson.core.type.TypeReference<Map<String, Object>>() {});
            
            String questionStartAtStr = payload.get("questionStartAt") instanceof String s ? s : null;
            if (questionStartAtStr == null) {
                return null;
            }

            Instant questionStartAt = Instant.parse(questionStartAtStr);
            Instant now = Instant.now();

            // 쉬는 시간이 아직 진행 중인지 확인
            if (now.isAfter(questionStartAt)) {
                return null; // 이미 쉬는 시간이 끝남
            }

            Long nextQuestionId = payload.get("nextQuestionId") instanceof Number n ? n.longValue() : null;
            Integer nextRoundNo = payload.get("nextRoundNo") instanceof Number n ? n.intValue() : null;
            String nextPhaseStr = payload.get("nextPhase") instanceof String s ? s : null;
            MatchPhase nextPhase = nextPhaseStr != null ? MatchPhase.valueOf(nextPhaseStr) : null;
            Integer durationSec = payload.get("durationSec") instanceof Number n ? n.intValue() : 5; // 기본값 5초

            return new VersusDtos.IntermissionInfo(
                    nextQuestionId,
                    nextRoundNo,
                    nextPhase,
                    durationSec,
                    event.getCreatedAt(),
                    questionStartAt
            );
        } catch (Exception e) {
            log.warn("Failed to parse INTERMISSION_STARTED event payload: roomId={}, error={}", roomId, e.getMessage());
            return null;
        }
    }

    private List<VersusDtos.XpResult> fetchXpResults(Long roomId) {
        try {
            return eventRepository.findByRoomIdAndEventType(roomId, "XP_REWARDED").stream()
                    .max(Comparator.comparing(MatchEvent::getCreatedAt))
                    .map(MatchEvent::getPayloadJson)
                    .map(json -> {
                        try {
                            Map<String, Object> payload = objectMapper.readValue(json, new TypeReference<Map<String, Object>>() {});
                            Object xpResultsObj = payload.get("xpResults");
                            if (xpResultsObj == null) {
                                return null;
                            }
                            List<com.OhRyue.certpilot.versus.client.ProgressServiceClient.XpResult> xpList = objectMapper.convertValue(
                                    xpResultsObj, new TypeReference<List<com.OhRyue.certpilot.versus.client.ProgressServiceClient.XpResult>>() {});
                            if (xpList == null) return null;
                            return xpList.stream()
                                    .map(xp -> new VersusDtos.XpResult(
                                            xp.userId(),
                                            xp.xpDelta(),
                                            xp.reason(),
                                            xp.totalXp(),
                                            xp.leveledUp()
                                    ))
                                    .toList();
                        } catch (Exception e) {
                            log.warn("Failed to parse XP_REWARDED payload for room {}: {}", roomId, e.getMessage());
                            return null;
                        }
                    })
                    .orElse(null);
        } catch (Exception e) {
            log.warn("Failed to fetch XP results for room {}: {}", roomId, e.getMessage());
            return List.of();
        }
    }

    private Long determineSkinId(String userId, AccountServiceClient.ProfileSummary profile) {
        if (profile != null && profile.skinId() != null) {
            return profile.skinId();
        }
        // 봇인 경우: userId 기반 고정 스킨 ID (1~17)
        if (userId.startsWith("BOT_")) {
            int hash = userId.hashCode();
            return (long) (Math.abs(hash) % 17 + 1);
        }
        return 1L; // 기본 스킨 ID
    }

    // ========== Inner Classes ==========

    private static class Score {
        int correct;
        int total;
        int score;
        long totalTimeMs;
    }

    private static class FinalRoundScore {
        int correct;
        int total;
        int score;
        long totalTimeMs;
    }

    private record ScoreboardIntermediate(
            String userId,
            int correct,
            int total,
            int score,
            long totalTimeMs,
            boolean alive,
            boolean revived
    ) {
    }
}

