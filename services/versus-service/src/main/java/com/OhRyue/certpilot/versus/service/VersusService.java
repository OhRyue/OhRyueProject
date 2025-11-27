package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.client.ProgressServiceClient;
import com.OhRyue.certpilot.versus.client.StudyServiceClient;
import com.OhRyue.certpilot.versus.config.MonitoringConfig;
import com.OhRyue.certpilot.versus.domain.*;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;
import io.micrometer.core.instrument.Timer;

import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class VersusService {

    private static final int DUEL_TOTAL_QUESTIONS = 10;
    private static final int DUEL_WRITTEN_OX_LIMIT_SEC = 5;
    private static final int DUEL_WRITTEN_MCQ_LIMIT_SEC = 10;
    private static final int DUEL_PRACTICAL_SHORT_LIMIT_SEC = 15;
    private static final int DUEL_PRACTICAL_LONG_LIMIT_SEC = 25;

    private static final int TOURNAMENT_ROUNDS = 3;
    private static final int TOURNAMENT_QUESTIONS_PER_ROUND = 3;
    private static final int GOLDENBELL_REVIVE_CHECK_ROUND = 4;
    private static final int GOLDENBELL_REVIVE_TARGET = 5;
    private static final int GOLDENBELL_TIME_LIMIT_SEC = 10;

    private static final int BASE_SCORE = 1000;
    private static final int SPEED_BONUS_MAX = 200;
    private static final int MAX_TIMELINE_FETCH = 200;

    private final MatchRoomRepository roomRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchQuestionRepository questionRepository;
    private final MatchAnswerRepository answerRepository;
    private final TournamentBracketRepository bracketRepository;
    private final GoldenbellRuleRepository goldenbellRuleRepository;
    private final GoldenbellStateRepository goldenbellStateRepository;
    private final MatchEventRepository eventRepository;
    private final VersusRealtimeService realtimeService;
    private final ObjectMapper objectMapper;
    private final StudyServiceClient studyServiceClient;
    private final ProgressServiceClient progressServiceClient;
    private final MonitoringConfig monitoringConfig;
    private final RewardRetryService rewardRetryService;

    // 모드별 인원 제한
    private static final int DUEL_MAX_PARTICIPANTS = 2;
    private static final int TOURNAMENT_MAX_PARTICIPANTS = 8;
    private static final int GOLDENBELL_MAX_PARTICIPANTS = 20;

    @Transactional
    public VersusDtos.RoomDetailResp createRoom(VersusDtos.CreateRoomReq req, String creatorUserId) {
        MatchRoom room = roomRepository.save(MatchRoom.builder()
                .mode(req.mode())
                .status(MatchStatus.WAIT)
                .scopeJson(Optional.ofNullable(req.scopeJson()).orElse("{}"))
                .scheduledAt(req.scheduledAt())  // GOLDENBELL 예약 시간
                .build());

        Map<String, Object> eventPayload = new HashMap<>();
        eventPayload.put("mode", room.getMode().name());
        eventPayload.put("scope", room.getScopeJson());
        if (creatorUserId != null) {
            eventPayload.put("creator", creatorUserId);
        }
        recordEvent(room.getId(), "ROOM_CREATED", eventPayload);

        // 방 생성자는 자동으로 참가
        if (creatorUserId != null && !creatorUserId.isBlank()) {
            try {
                int currentCount = (int) participantRepository.countByRoomId(room.getId());
                validateParticipantLimit(room, currentCount);
                MatchParticipant creator = registerParticipant(room.getId(), creatorUserId);
                recordEvent(room.getId(), "PLAYER_JOINED", Map.of(
                        "userId", creator.getUserId(),
                        "joinedAt", creator.getJoinedAt().toString(),
                        "role", "CREATOR"
                ));
            } catch (ResponseStatusException e) {
                // 이미 참가한 경우 무시 (중복 참가 방지)
                log.debug("Creator {} already joined room {}", creatorUserId, room.getId());
            }
        }

        // 초대된 참가자들 참가
        if (req.participants() != null) {
            req.participants().forEach(userId -> {
                // 방 생성자와 중복 체크
                if (creatorUserId != null && creatorUserId.equals(userId)) {
                    log.debug("Skipping creator {} from participants list", userId);
                    return;
                }
                try {
                    int currentCount = (int) participantRepository.countByRoomId(room.getId());
                    validateParticipantLimit(room, currentCount);
                    MatchParticipant participant = registerParticipant(room.getId(), userId);
                    recordEvent(room.getId(), "PLAYER_JOINED", Map.of(
                            "userId", participant.getUserId(),
                            "joinedAt", participant.getJoinedAt().toString()
                    ));
                } catch (ResponseStatusException e) {
                    // 이미 참가한 경우 무시
                    log.debug("Participant {} already joined room {}", userId, room.getId());
                }
            });
        }

        // 문제 자동 생성 또는 수동 제공
        List<VersusDtos.QuestionInfo> questionInfos;
        if (req.questions() != null && !req.questions().isEmpty()) {
            questionInfos = req.questions();
        } else if (req.scopeJson() != null && !req.scopeJson().isBlank()) {
            // scopeJson 기반으로 문제 자동 생성
            questionInfos = generateQuestionsFromScope(room, req.scopeJson());
        } else {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
                    "Questions or scopeJson is required to start a match");
        }

        List<MatchQuestion> questions = questionInfos.stream()
                .map(q -> MatchQuestion.builder()
                        .roomId(room.getId())
                        .roundNo(Optional.ofNullable(q.roundNo()).orElse(1))
                        .phase(Optional.ofNullable(q.phase()).orElse(MatchPhase.MAIN))
                        .orderNo(Optional.ofNullable(q.order()).orElse(1))
                        .questionId(q.questionId())
                        .timeLimitSec(Optional.ofNullable(q.timeLimitSec()).orElse(10))
                        .build())
                .toList();

        validateQuestionLayout(room, questions);

        // GOLDENBELL 모드인 경우 round_flow_json과 문제 구성 검증
        if (room.getMode() == MatchMode.GOLDENBELL) {
            validateGoldenbellQuestionsWithRule(room.getId(), questions);
        }

        questionRepository.saveAll(questions);
        recordEvent(room.getId(), "QUESTIONS_REGISTERED", Map.of(
                "count", questions.size()
        ));

        if (req.tournamentBracketJson() != null && !req.tournamentBracketJson().isBlank()) {
            TournamentBracket bracket = TournamentBracket.builder()
                    .roomId(room.getId())
                    .roundNo(Optional.ofNullable(req.tournamentBracketRound()).orElse(1))
                    .pairingJson(req.tournamentBracketJson())
                    .build();
            bracketRepository.save(bracket);
            recordEvent(room.getId(), "TOURNAMENT_BRACKET", Map.of(
                    "round", bracket.getRoundNo(),
                    "payload", bracket.getPairingJson()
            ));
        }

        if (req.mode() == MatchMode.GOLDENBELL) {
            String ruleJson = Optional.ofNullable(req.goldenbellRuleJson())
                    .orElseGet(this::defaultGoldenbellRule);
            goldenbellRuleRepository.save(GoldenbellRule.builder()
                    .roomId(room.getId())
                    .roundFlowJson(ruleJson)
                    .elimination(GoldenbellElimination.IMMEDIATE)
                    .revivalRuleJson(null)
                    .build());

            List<String> initialPlayers = Optional.ofNullable(req.participants()).orElse(List.of());
            if (!initialPlayers.isEmpty()) {
                List<GoldenbellState> states = initialPlayers.stream()
                        .map(userId -> GoldenbellState.alive(room.getId(), userId))
                        .toList();
                goldenbellStateRepository.saveAll(states);
                recordEvent(room.getId(), "GOLDENBELL_STATE_INIT", Map.of(
                        "players", initialPlayers
                ));
            }
        }

        return buildRoomDetail(room.getId());
    }

    @Transactional(readOnly = true)
    public List<VersusDtos.RoomSummary> listRooms(MatchMode mode, MatchStatus status) {
        List<MatchRoom> rooms;
        if (mode != null && status != null) {
            rooms = roomRepository.findByModeAndStatus(mode, status);
        } else if (mode != null) {
            rooms = roomRepository.findByMode(mode);
        } else if (status != null) {
            rooms = roomRepository.findByStatus(status);
        } else {
            rooms = roomRepository.findAll();
        }

        Set<Long> roomIds = rooms.stream().map(MatchRoom::getId).collect(Collectors.toSet());
        Map<Long, Long> participantCount = participantRepository.findByRoomIdIn(roomIds).stream()
                .collect(Collectors.groupingBy(MatchParticipant::getRoomId, Collectors.counting()));

        return rooms.stream()
                .sorted(Comparator.comparing(MatchRoom::getCreatedAt).reversed())
                .map(room -> new VersusDtos.RoomSummary(
                        room.getId(),
                        room.getMode(),
                        room.getStatus(),
                        participantCount.getOrDefault(room.getId(), 0L).intValue(),
                        room.getCreatedAt()))
                .toList();
    }

    @Transactional(readOnly = true)
    public VersusDtos.RoomDetailResp getRoom(Long roomId) {
        return buildRoomDetail(roomId);
    }

    /**
     * 방의 문제 목록 조회 (questionId 확인용)
     */
    @Transactional(readOnly = true)
    public List<VersusDtos.QuestionInfo> getRoomQuestions(Long roomId) {
        findRoomOrThrow(roomId);
        List<MatchQuestion> questions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId);
        return questions.stream()
                .map(q -> new VersusDtos.QuestionInfo(
                        q.getQuestionId(),
                        q.getRoundNo(),
                        q.getPhase(),
                        q.getOrderNo(),
                        q.getTimeLimitSec()
                ))
                .toList();
    }

    @Transactional(readOnly = true)
    public List<VersusDtos.TimelineEvent> timeline(Long roomId, int limit) {
        findRoomOrThrow(roomId);
        int size = Math.max(1, Math.min(limit, MAX_TIMELINE_FETCH));
        Pageable pageable = PageRequest.of(0, size);
        List<MatchEvent> events = eventRepository.findByRoomIdOrderByCreatedAtDesc(roomId, pageable);
        Collections.reverse(events);
        return events.stream()
                .map(this::toTimelineEvent)
                .toList();
    }

    @Transactional(readOnly = true)
    public VersusDtos.RoomStateResp roomState(Long roomId, int timelineLimit) {
        VersusDtos.RoomDetailResp detail = buildRoomDetail(roomId);
        List<VersusDtos.TimelineEvent> timeline = timeline(roomId, timelineLimit);
        VersusDtos.RealtimeSnapshot realtime = realtimeService.loadSnapshot(roomId)
                .map(snapshot -> new VersusDtos.RealtimeSnapshot(
                        snapshot.scoreboard(), snapshot.roundNo(), snapshot.phase(), snapshot.updatedAt()))
                .orElseGet(() -> new VersusDtos.RealtimeSnapshot(
                        detail.scoreboard(), null, null, Instant.now()));
        return new VersusDtos.RoomStateResp(detail, timeline, realtime);
    }

    @Transactional
    public VersusDtos.RoomDetailResp joinRoom(Long roomId, String userId) {
        MatchRoom room = findRoomOrThrow(roomId);
        if (room.getStatus() != MatchStatus.WAIT) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Room already started");
        }

        // GOLDENBELL 예약 시스템: 10분 전부터 입장 가능
        if (room.getMode() == MatchMode.GOLDENBELL && room.getScheduledAt() != null) {
            Instant now = Instant.now();
            Instant tenMinutesBefore = room.getScheduledAt().minus(10, ChronoUnit.MINUTES);
            if (now.isBefore(tenMinutesBefore)) {
                throw new ResponseStatusException(
                        HttpStatus.BAD_REQUEST,
                        String.format("입장 가능 시간이 아닙니다. %s부터 입장 가능합니다.",
                                tenMinutesBefore.toString())
                );
            }
        }

        // 인원 제한 검증
        int currentCount = (int) participantRepository.countByRoomId(roomId);
        validateParticipantLimit(room, currentCount);

        MatchParticipant participant = registerParticipant(room.getId(), userId);
        recordEvent(room.getId(), "PLAYER_JOINED", Map.of(
                "userId", participant.getUserId(),
                "joinedAt", participant.getJoinedAt().toString()
        ));

        if (room.getMode() == MatchMode.GOLDENBELL) {
            goldenbellStateRepository.findByRoomIdAndUserId(room.getId(), userId)
                    .orElseGet(() -> goldenbellStateRepository.save(GoldenbellState.alive(room.getId(), userId)));
            recordEvent(room.getId(), "GOLDENBELL_STATE_INIT", Map.of(
                    "userId", userId
            ));
        }
        return buildRoomDetail(room.getId());
    }

    @Transactional
    public VersusDtos.RoomDetailResp startRoom(Long roomId) {
        MatchRoom room = findRoomOrThrow(roomId);
        if (room.getStatus() != MatchStatus.WAIT) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Room already started");
        }

        // 최소 인원 검증
        validateMinParticipants(room);

        room.setStatus(MatchStatus.ONGOING);
        roomRepository.save(room);
        recordEvent(roomId, "MATCH_STARTED", Map.of(
                "startedAt", Instant.now().toString()
        ));
        return buildRoomDetail(room.getId());
    }

    @Transactional
    public VersusDtos.ScoreBoardResp submitAnswer(Long roomId,
                                                  String userId,
                                                  VersusDtos.SubmitAnswerReq req) {
        MatchRoom room = findRoomOrThrow(roomId);
        MatchParticipant participant = participantRepository.findByRoomIdAndUserId(roomId, userId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.BAD_REQUEST, "Participant not joined"));

        if (room.getMode() == MatchMode.TOURNAMENT && participant.isEliminated()) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Eliminated participant cannot submit answers");
        }

        if (room.getMode() == MatchMode.GOLDENBELL) {
            GoldenbellState state = goldenbellStateRepository.findByRoomIdAndUserId(roomId, userId)
                    .orElse(null);
            if (state != null && !state.isAlive()) {
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Eliminated participant cannot submit answers");
            }
        }

        MatchQuestion question = resolveQuestion(roomId, req.questionId());

        // 문제 시작 시점 기록 (처음 답안 제출 시)
        recordQuestionStartIfNeeded(roomId, question.getQuestionId(), userId);

        MatchAnswer answer = answerRepository.findByRoomIdAndQuestionIdAndUserId(
                        roomId, req.questionId(), userId)
                .orElse(MatchAnswer.builder()
                        .roomId(roomId)
                        .questionId(req.questionId())
                        .userId(userId)
                        .build());

        // 서버 사이드 정답 검증
        boolean serverValidatedCorrect = validateAnswerOnServer(question.getQuestionId(), req);
        // 서버 검증 결과를 우선 사용 (클라이언트 값은 참고용)
        boolean finalCorrect = serverValidatedCorrect;

        // 서버 기준 시간 계산 (문제 시작 시점이 있으면 사용)
        Integer serverTimeMs = calculateServerTimeMs(roomId, question.getQuestionId(), userId, req.timeMs());

        ScoreOutcome outcome = evaluateScore(question, finalCorrect, serverTimeMs);

        answer.setCorrect(outcome.correct());
        answer.setScoreDelta(outcome.scoreDelta());
        answer.setTimeMs(outcome.timeMs());
        answer.setRoundNo(Optional.ofNullable(req.roundNo()).orElse(question.getRoundNo()));
        answer.setPhase(Optional.ofNullable(req.phase()).orElse(question.getPhase()));
        answerRepository.save(answer);

        recordEvent(roomId, "ANSWER_SUBMITTED", Map.of(
                "userId", userId,
                "questionId", req.questionId(),
                "round", question.getRoundNo(),
                "phase", question.getPhase().name(),
                "correct", outcome.correct(),
                "scoreDelta", outcome.scoreDelta(),
                "timeMs", outcome.timeMs()
        ));

        boolean stateChanged = false;
        if (room.getStatus() == MatchStatus.WAIT) {
            room.setStatus(MatchStatus.ONGOING);
            roomRepository.save(room);
            stateChanged = true;
        }

        if (room.getMode() == MatchMode.GOLDENBELL) {
            stateChanged |= processGoldenbellState(room, participant, outcome.correct(), question);
        }

        VersusDtos.ScoreBoardResp scoreboard = computeScoreboard(room);
        ModeResolution resolution = handleModeAfterAnswer(room, question, scoreboard);
        if (resolution.matchCompleted()) {
            room.setStatus(MatchStatus.DONE);
            roomRepository.save(room);
            stateChanged = true;

            // progress-service에 결과 통지 및 보상 지급
            notifyProgressService(room, scoreboard);
        }
        if (resolution.stateChanged()) {
            stateChanged = true;
        }

        if (stateChanged) {
            scoreboard = computeScoreboard(room);
        }

        realtimeService.pushSnapshot(roomId, scoreboard, question.getRoundNo(), question.getPhase());
        return scoreboard;
    }

    @Transactional
    public VersusDtos.ScoreBoardResp scoreboard(Long roomId) {
        MatchRoom room = findRoomOrThrow(roomId);
        VersusDtos.ScoreBoardResp resp = computeScoreboard(room);
        realtimeService.pushSnapshot(roomId, resp, null, null);
        return resp;
    }

    public ModeResolution handleModeAfterAnswer(MatchRoom room,
                                                MatchQuestion question,
                                                VersusDtos.ScoreBoardResp scoreboard) {
        return switch (room.getMode()) {
            case DUEL -> handleDuelProgress(room, question, scoreboard);
            case TOURNAMENT -> handleTournamentProgress(room, question, scoreboard);
            case GOLDENBELL -> handleGoldenbellProgress(room, question, scoreboard);
        };
    }

    private ModeResolution handleDuelProgress(MatchRoom room,
                                              MatchQuestion question,
                                              VersusDtos.ScoreBoardResp scoreboard) {
        long participants = participantRepository.countByRoomId(room.getId());
        if (participants <= 1) {
            String winner = scoreboard.items().isEmpty() ? null : scoreboard.items().get(0).userId();
            recordEvent(room.getId(), "MATCH_FINISHED", Map.of(
                    "mode", "DUEL",
                    "winner", winner,
                    "reason", "OPPONENT_LEFT"
            ));
            return new ModeResolution(true, true, true);
        }
        long answeredForQuestion = answerRepository.countByRoomIdAndQuestionId(room.getId(), question.getQuestionId());
        boolean roundCompleted = participants > 0 && answeredForQuestion >= participants;

        if (roundCompleted) {
            recordEvent(room.getId(), "ROUND_COMPLETED", Map.of(
                    "mode", "DUEL",
                    "round", question.getRoundNo(),
                    "phase", question.getPhase().name()
            ));
        }

        boolean matchCompleted = checkAllQuestionsAnswered(room.getId(), participants);
        if (matchCompleted) {
            String winner = scoreboard.items().isEmpty() ? null : scoreboard.items().get(0).userId();
            recordEvent(room.getId(), "MATCH_FINISHED", Map.of(
                    "mode", "DUEL",
                    "winner", winner
            ));
        }

        return new ModeResolution(roundCompleted, matchCompleted, matchCompleted);
    }

    private ModeResolution handleTournamentProgress(MatchRoom room,
                                                    MatchQuestion question,
                                                    VersusDtos.ScoreBoardResp scoreboard) {
        Long roomId = room.getId();
        List<MatchParticipant> activeParticipants = participantRepository.findByRoomIdAndEliminatedFalse(roomId);
        if (activeParticipants.isEmpty()) {
            return ModeResolution.none();
        }

        int questionsInRound = questionRepository.findByRoomIdAndRoundNo(roomId, question.getRoundNo()).size();
        long answersInRound = answerRepository.countByRoomIdAndRoundNo(roomId, question.getRoundNo());
        long expectedAnswers = (long) activeParticipants.size() * questionsInRound;
        boolean roundCompleted = expectedAnswers > 0 && answersInRound >= expectedAnswers;

        if (!roundCompleted) {
            return ModeResolution.none();
        }

        Set<String> activeIds = activeParticipants.stream()
                .map(MatchParticipant::getUserId)
                .collect(Collectors.toSet());

        List<VersusDtos.ScoreBoardItem> ordered = scoreboard.items().stream()
                .filter(item -> activeIds.contains(item.userId()))
                .toList();

        int survivorCount = Math.max(1, (int) Math.ceil(activeParticipants.size() / 2.0));
        List<String> survivors = ordered.stream()
                .limit(survivorCount)
                .map(VersusDtos.ScoreBoardItem::userId)
                .toList();

        List<String> eliminatedIds = activeParticipants.stream()
                .map(MatchParticipant::getUserId)
                .filter(id -> !survivors.contains(id))
                .toList();

        if (!eliminatedIds.isEmpty()) {
            List<MatchParticipant> toUpdate = activeParticipants.stream()
                    .filter(p -> eliminatedIds.contains(p.getUserId()))
                    .peek(p -> p.setEliminated(true))
                    .toList();
            participantRepository.saveAll(toUpdate);
            eliminatedIds.forEach(id -> recordEvent(roomId, "PLAYER_ELIMINATED", Map.of(
                    "userId", id,
                    "mode", "TOURNAMENT",
                    "round", question.getRoundNo()
            )));
        }

        recordEvent(roomId, "ROUND_COMPLETED", Map.of(
                "mode", "TOURNAMENT",
                "round", question.getRoundNo(),
                "survivors", survivors,
                "eliminated", eliminatedIds
        ));

        persistBracket(roomId, question.getRoundNo(), survivors, eliminatedIds);

        boolean matchCompleted = survivors.size() <= 1 || !hasNextRound(roomId, question.getRoundNo());
        if (matchCompleted) {
            String winner = survivors.isEmpty() ? null : survivors.get(0);
            recordEvent(roomId, "MATCH_FINISHED", Map.of(
                    "mode", "TOURNAMENT",
                    "winner", winner
            ));
        }

        boolean stateChanged = !eliminatedIds.isEmpty() || matchCompleted;
        return new ModeResolution(true, matchCompleted, stateChanged);
    }

    private ModeResolution handleGoldenbellProgress(MatchRoom room,
                                                    MatchQuestion question,
                                                    VersusDtos.ScoreBoardResp scoreboard) {
        Long roomId = room.getId();
        List<GoldenbellState> states = goldenbellStateRepository.findByRoomId(roomId);
        long aliveCount = states.stream().filter(GoldenbellState::isAlive).count();

        long answeredForQuestion = answerRepository.countByRoomIdAndQuestionId(roomId, question.getQuestionId());
        long expectedAnswers = Math.max(aliveCount, 1);
        boolean roundCompleted = answeredForQuestion >= expectedAnswers;
        boolean stateChanged = false;

        if (roundCompleted) {
            recordEvent(roomId, "ROUND_COMPLETED", Map.of(
                    "mode", "GOLDENBELL",
                    "round", question.getRoundNo(),
                    "phase", question.getPhase().name(),
                    "aliveCount", aliveCount
            ));

            if (question.getRoundNo() == GOLDENBELL_REVIVE_CHECK_ROUND) {
                if (aliveCount <= GOLDENBELL_REVIVE_TARGET) {
                    if (reviveFastestEliminated(roomId, GOLDENBELL_REVIVE_CHECK_ROUND)) {
                        stateChanged = true;
                        states = goldenbellStateRepository.findByRoomId(roomId);
                        aliveCount = states.stream().filter(GoldenbellState::isAlive).count();
                    }
                }
                if (aliveCount == 0) {
                    if (reviveTopN(room, GOLDENBELL_REVIVE_TARGET)) {
                        stateChanged = true;
                        states = goldenbellStateRepository.findByRoomId(roomId);
                        aliveCount = states.stream().filter(GoldenbellState::isAlive).count();
                    }
                }
            }
        }

        boolean hasNext = hasNextRound(roomId, question.getRoundNo());
        boolean matchCompleted = false;
        if (aliveCount <= 1) {
            matchCompleted = true;
        } else if (!hasNext) {
            matchCompleted = true;
        }

        if (matchCompleted) {
            String winner = scoreboard.items().isEmpty() ? null : scoreboard.items().get(0).userId();
            recordEvent(roomId, "MATCH_FINISHED", Map.of(
                    "mode", "GOLDENBELL",
                    "winner", winner,
                    "aliveCount", aliveCount
            ));
            stateChanged = true;
        }

        return new ModeResolution(roundCompleted, matchCompleted, stateChanged);
    }

    private boolean processGoldenbellState(MatchRoom room,
                                           MatchParticipant participant,
                                           boolean correct,
                                           MatchQuestion question) {
        GoldenbellState state = goldenbellStateRepository.findByRoomIdAndUserId(room.getId(), participant.getUserId())
                .orElseGet(() -> goldenbellStateRepository.save(GoldenbellState.alive(room.getId(), participant.getUserId())));

        if (!correct && state.isAlive()) {
            state.setAlive(false);
            goldenbellStateRepository.save(state);
            recordEvent(room.getId(), "PLAYER_ELIMINATED", Map.of(
                    "userId", participant.getUserId(),
                    "mode", "GOLDENBELL",
                    "round", question.getRoundNo()
            ));
            return true;
        }
        return false;
    }

    private MatchRoom findRoomOrThrow(Long roomId) {
        return roomRepository.findById(roomId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Room not found"));
    }

    private MatchParticipant registerParticipant(Long roomId, String userId) {
        participantRepository.findByRoomIdAndUserId(roomId, userId)
                .ifPresent(p -> {
                    throw new ResponseStatusException(HttpStatus.CONFLICT, "Already joined");
                });
        MatchParticipant participant = MatchParticipant.builder()
                .roomId(roomId)
                .userId(userId)
                .joinedAt(Instant.now())
                .eliminated(false)
                .build();
        return participantRepository.save(participant);
    }

    private MatchQuestion resolveQuestion(Long roomId, Long questionId) {
        return questionRepository.findByRoomIdAndQuestionId(roomId, questionId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.BAD_REQUEST, "Question not registered for room"));
    }

    private VersusDtos.RoomDetailResp buildRoomDetail(Long roomId) {
        MatchRoom room = findRoomOrThrow(roomId);
        List<MatchParticipant> participants = participantRepository.findByRoomId(roomId);
        Map<String, GoldenbellState> goldenState = goldenbellStateRepository.findByRoomId(roomId).stream()
                .collect(Collectors.toMap(GoldenbellState::getUserId, g -> g));

        List<VersusDtos.ParticipantSummary> participantSummaries = participants.stream()
                .map(p -> {
                    GoldenbellState state = goldenState.get(p.getUserId());
                    boolean alive = state != null ? state.isAlive() : !p.isEliminated();
                    boolean revived = state != null && state.isRevived();
                    return new VersusDtos.ParticipantSummary(
                            p.getUserId(),
                            p.getFinalScore(),
                            p.getPlayerRank(),
                            alive,
                            revived,
                            p.getJoinedAt()
                    );
                })
                .sorted(Comparator.comparing(VersusDtos.ParticipantSummary::joinedAt))
                .toList();

        List<VersusDtos.QuestionInfo> questions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId).stream()
                .map(q -> new VersusDtos.QuestionInfo(
                        q.getQuestionId(),
                        q.getRoundNo(),
                        q.getPhase(),
                        q.getOrderNo(),
                        q.getTimeLimitSec()))
                .toList();

        String bracketJson = bracketRepository.findByRoomIdOrderByRoundNoAsc(roomId).stream()
                .map(TournamentBracket::getPairingJson)
                .reduce((first, second) -> second)
                .orElse(null);

        String goldenbellRuleJson = goldenbellRuleRepository.findById(roomId)
                .map(GoldenbellRule::getRoundFlowJson)
                .orElse(null);

        VersusDtos.ScoreBoardResp scoreboard = computeScoreboard(room);

        VersusDtos.RoomSummary summary = new VersusDtos.RoomSummary(
                room.getId(),
                room.getMode(),
                room.getStatus(),
                participantSummaries.size(),
                room.getCreatedAt()
        );

        return new VersusDtos.RoomDetailResp(
                summary,
                participantSummaries,
                questions,
                bracketJson,
                goldenbellRuleJson,
                scoreboard
        );
    }

    public VersusDtos.ScoreBoardResp computeScoreboard(MatchRoom room) {
        Long roomId = room.getId();
        List<MatchParticipant> participants = participantRepository.findByRoomId(roomId);
        Map<String, GoldenbellState> goldenState = goldenbellStateRepository.findByRoomId(roomId).stream()
                .collect(Collectors.toMap(GoldenbellState::getUserId, g -> g));

        Map<Long, MatchQuestion> questionMap = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId).stream()
                .collect(Collectors.toMap(MatchQuestion::getQuestionId, q -> q));

        List<MatchAnswer> answers = answerRepository.findByRoomId(roomId);
        Map<String, Score> stats = new HashMap<>();
        Map<String, FinalRoundScore> finalScores = new HashMap<>(); // FINAL 라운드 점수 별도 관리

        // GOLDENBELL 모드인 경우 FINAL 라운드 점수 별도 계산
        boolean isGoldenbell = room.getMode() == MatchMode.GOLDENBELL;

        for (MatchAnswer answer : answers) {
            Score score = stats.computeIfAbsent(answer.getUserId(), u -> new Score());
            MatchQuestion q = questionMap.get(answer.getQuestionId());

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

                        // FINAL 라운드 점수를 메인 점수에 합산 (또는 별도 처리)
                        // 여기서는 FINAL 라운드 점수를 우선으로 사용
                        int finalScoreValue = finalScore.score > 0 ? finalScore.score : score.score;
                        int finalCorrect = finalScore.correct > 0 ? finalScore.correct : score.correct;
                        long finalTime = finalScore.totalTimeMs > 0 ? finalScore.totalTimeMs : score.totalTimeMs;

                        GoldenbellState state = goldenState.get(userId);
                        boolean alive = state != null ? state.isAlive() : participants.stream()
                                .filter(p -> p.getUserId().equals(userId))
                                .findFirst()
                                .map(p -> !p.isEliminated())
                                .orElse(true);
                        boolean revived = state != null && state.isRevived();
                        return new ScoreboardIntermediate(userId, finalCorrect, score.total, finalScoreValue, finalTime, alive, revived);
                    })
                    .sorted(Comparator
                            .comparing(ScoreboardIntermediate::score).reversed()
                            .thenComparing(ScoreboardIntermediate::correct).reversed()
                            .thenComparing(ScoreboardIntermediate::totalTimeMs)
                            .thenComparing(ScoreboardIntermediate::userId))
                    .toList();
        } else {
            // 일반 정렬
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
                    .sorted(Comparator
                            .comparing(ScoreboardIntermediate::score).reversed()
                            .thenComparing(ScoreboardIntermediate::correct).reversed()
                            .thenComparing(ScoreboardIntermediate::totalTimeMs)
                            .thenComparing(ScoreboardIntermediate::userId))
                    .toList();
        }

        int rank = 1;
        int previousScore = Integer.MIN_VALUE;
        int previousCorrect = Integer.MIN_VALUE;
        Long previousTime = null;

        List<VersusDtos.ScoreBoardItem> finalItems = new ArrayList<>();
        Map<String, MatchParticipant> participantMap = participants.stream()
                .collect(Collectors.toMap(MatchParticipant::getUserId, p -> p));

        for (ScoreboardIntermediate intermediate : intermediates) {
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

            finalItems.add(new VersusDtos.ScoreBoardItem(
                    intermediate.userId(),
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

        return new VersusDtos.ScoreBoardResp(roomId, room.getStatus(), finalItems);
    }

    private boolean hasNextRound(Long roomId, int currentRound) {
        return questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId).stream()
                .anyMatch(q -> q.getRoundNo() > currentRound);
    }

    private boolean checkAllQuestionsAnswered(Long roomId, long participants) {
        long totalAnswers = answerRepository.countByRoomId(roomId);
        long questionCount = questionRepository.countByRoomId(roomId);
        return participants > 0 && questionCount > 0 && totalAnswers >= questionCount * participants;
    }

    private void persistBracket(Long roomId,
                                Integer roundNo,
                                List<String> survivors,
                                List<String> eliminated) {
        try {
            String payload = objectMapper.writeValueAsString(Map.of(
                    "round", roundNo,
                    "timestamp", Instant.now().toString(),
                    "survivors", survivors,
                    "eliminated", eliminated
            ));
            TournamentBracket bracket = bracketRepository.findByRoomIdAndRoundNo(roomId, roundNo)
                    .orElseGet(() -> TournamentBracket.builder()
                            .roomId(roomId)
                            .roundNo(roundNo)
                            .pairingJson(payload)
                            .build());
            bracket.setPairingJson(payload);
            bracketRepository.save(bracket);
        } catch (JsonProcessingException e) {
            log.warn("Failed to persist tournament bracket state for room {}: {}", roomId, e.getMessage());
        }
    }

    private void validateQuestionLayout(MatchRoom room, List<MatchQuestion> questions) {
        if (questions.isEmpty()) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Question set cannot be empty");
        }
        switch (room.getMode()) {
            case DUEL -> validateDuelQuestions(room, questions);
            case TOURNAMENT -> validateTournamentQuestions(room, questions);
            case GOLDENBELL -> validateGoldenbellQuestions(questions);
        }
    }

    private void validateDuelQuestions(MatchRoom room, List<MatchQuestion> questions) {
        if (questions.size() != DUEL_TOTAL_QUESTIONS) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "1:1 배틀은 정확히 10문제가 필요합니다.");
        }
        Map<String, Object> scope = readScope(room.getScopeJson());
        boolean practical = isPracticalExam(scope);
        long shortLimit = practical ? DUEL_PRACTICAL_SHORT_LIMIT_SEC : DUEL_WRITTEN_OX_LIMIT_SEC;
        long longLimit = practical ? DUEL_PRACTICAL_LONG_LIMIT_SEC : DUEL_WRITTEN_MCQ_LIMIT_SEC;

        long shortCount = questions.stream()
                .filter(q -> Objects.equals(q.getTimeLimitSec(), (int) shortLimit))
                .count();
        long longCount = questions.stream()
                .filter(q -> Objects.equals(q.getTimeLimitSec(), (int) longLimit))
                .count();

        if (shortCount != 5 || longCount != 5) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
                    "1:1 배틀은 시간 제한 " + shortLimit + "초 문제 5개와 " + longLimit + "초 문제 5개로 구성되어야 합니다.");
        }
    }

    private void validateTournamentQuestions(MatchRoom room, List<MatchQuestion> questions) {
        Map<Integer, List<MatchQuestion>> byRound = questions.stream()
                .collect(Collectors.groupingBy(MatchQuestion::getRoundNo));
        if (byRound.size() != TOURNAMENT_ROUNDS) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "토너먼트는 3개 라운드가 필요합니다.");
        }
        Map<String, Object> scope = readScope(room.getScopeJson());
        boolean practical = isPracticalExam(scope);

        for (int round = 1; round <= TOURNAMENT_ROUNDS; round++) {
            List<MatchQuestion> roundQuestions = byRound.getOrDefault(round, List.of());
            if (roundQuestions.size() != TOURNAMENT_QUESTIONS_PER_ROUND) {
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
                        "토너먼트 라운드 " + round + "는 3문제를 가져야 합니다.");
            }
            int expectedLimitSec = switch (round) {
                case 1 -> practical ? DUEL_PRACTICAL_SHORT_LIMIT_SEC : DUEL_WRITTEN_OX_LIMIT_SEC;
                case 2 -> practical ? DUEL_PRACTICAL_LONG_LIMIT_SEC : DUEL_WRITTEN_MCQ_LIMIT_SEC;
                default -> DUEL_WRITTEN_MCQ_LIMIT_SEC;
            };
            boolean invalidLimit = roundQuestions.stream()
                    .anyMatch(q -> !Objects.equals(q.getTimeLimitSec(), expectedLimitSec));
            if (invalidLimit) {
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
                        "토너먼트 라운드 " + round + "의 시간 제한은 " + expectedLimitSec + "초로 통일되어야 합니다.");
            }
        }
    }

    private void validateGoldenbellQuestions(List<MatchQuestion> questions) {
        if (questions.size() < GOLDENBELL_REVIVE_CHECK_ROUND + 4) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "골든벨은 최소 8문제가 필요합니다.");
        }
        boolean invalidLimit = questions.stream()
                .anyMatch(q -> !Objects.equals(q.getTimeLimitSec(), GOLDENBELL_TIME_LIMIT_SEC));
        if (invalidLimit) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "골든벨 모든 문제의 제한시간은 10초여야 합니다.");
        }
    }

    /**
     * GoldenbellRule의 round_flow_json과 실제 문제 구성을 비교 검증
     */
    private void validateGoldenbellQuestionsWithRule(Long roomId, List<MatchQuestion> questions) {
        Optional<GoldenbellRule> ruleOpt = goldenbellRuleRepository.findById(roomId);
        if (ruleOpt.isEmpty()) {
            log.warn("GoldenbellRule not found for room {}, skipping rule validation", roomId);
            return;
        }

        GoldenbellRule rule = ruleOpt.get();
        List<RoundFlowRule> flowRules = parseRoundFlowJson(rule.getRoundFlowJson());

        // 문제를 라운드별로 그룹화
        Map<Integer, List<MatchQuestion>> questionsByRound = questions.stream()
                .collect(Collectors.groupingBy(MatchQuestion::getRoundNo));

        // 각 라운드 룰과 실제 문제 구성 비교
        for (RoundFlowRule flowRule : flowRules) {
            int roundNo = flowRule.round();
            List<MatchQuestion> roundQuestions = questionsByRound.getOrDefault(roundNo, List.of());

            // 라운드별 문제 개수 검증
            if (roundQuestions.size() != flowRule.count()) {
                throw new ResponseStatusException(
                        HttpStatus.BAD_REQUEST,
                        String.format("Goldenbell round %d: expected %d questions, but found %d",
                                roundNo, flowRule.count(), roundQuestions.size()));
            }

            // 라운드별 시간 제한 검증
            for (MatchQuestion q : roundQuestions) {
                if (!Objects.equals(q.getTimeLimitSec(), flowRule.limitSec())) {
                    throw new ResponseStatusException(
                            HttpStatus.BAD_REQUEST,
                            String.format("Goldenbell round %d: expected timeLimit %d sec, but found %d sec",
                                    roundNo, flowRule.limitSec(), q.getTimeLimitSec()));
                }

                // Phase 검증
                if (q.getPhase() != MatchPhase.valueOf(flowRule.phase())) {
                    throw new ResponseStatusException(
                            HttpStatus.BAD_REQUEST,
                            String.format("Goldenbell round %d: expected phase %s, but found %s",
                                    roundNo, flowRule.phase(), q.getPhase()));
                }
            }
        }

        log.debug("GoldenbellRule validation passed for room {}: {} rounds validated",
                roomId, flowRules.size());
    }

    private Map<String, Object> readScope(String scopeJson) {
        if (scopeJson == null || scopeJson.isBlank()) {
            return Map.of();
        }
        try {
            return objectMapper.readValue(scopeJson, new TypeReference<Map<String, Object>>() {
            });
        } catch (Exception e) {
            log.warn("Failed to parse scope json: {}", e.getMessage());
            return Map.of();
        }
    }

    private boolean isPracticalExam(Map<String, Object> scope) {
        Object exam = scope.get("examMode");
        if (exam instanceof String str) {
            return "PRACTICAL".equalsIgnoreCase(str);
        }
        return false;
    }

    private boolean reviveFastestEliminated(Long roomId, int upToRound) {
        List<GoldenbellState> states = goldenbellStateRepository.findByRoomId(roomId);
        Map<String, GoldenbellState> stateMap = states.stream()
                .collect(Collectors.toMap(GoldenbellState::getUserId, s -> s));
        List<GoldenbellState> eliminated = states.stream()
                .filter(state -> !state.isAlive())
                .toList();
        if (eliminated.isEmpty()) {
            return false;
        }

        List<MatchAnswer> answers = answerRepository.findByRoomId(roomId);
        String revivedUser = null;
        int bestTime = Integer.MAX_VALUE;

        for (GoldenbellState state : eliminated) {
            int candidateTime = answers.stream()
                    .filter(a -> state.getUserId().equals(a.getUserId()))
                    .filter(MatchAnswer::isCorrect)
                    .filter(a -> a.getRoundNo() != null && a.getRoundNo() <= upToRound)
                    .map(a -> Optional.ofNullable(a.getTimeMs()).orElse(GOLDENBELL_TIME_LIMIT_SEC * 1000))
                    .min(Integer::compareTo)
                    .orElse(Integer.MAX_VALUE);
            if (candidateTime < bestTime) {
                bestTime = candidateTime;
                revivedUser = state.getUserId();
            }
        }

        if (revivedUser == null || bestTime == Integer.MAX_VALUE) {
            return false;
        }

        GoldenbellState target = stateMap.get(revivedUser);
        if (target == null) {
            target = GoldenbellState.alive(roomId, revivedUser);
        }
        if (target.isAlive()) {
            return false;
        }
        target.setAlive(true);
        target.setRevived(true);
        goldenbellStateRepository.save(target);
        recordEvent(roomId, "GOLDENBELL_REVIVE_FASTEST", Map.of(
                "userId", revivedUser,
                "timeMs", bestTime
        ));
        return true;
    }

    private boolean reviveTopN(MatchRoom room, int count) {
        VersusDtos.ScoreBoardResp scoreboard = computeScoreboard(room);
        if (scoreboard.items().isEmpty()) {
            return false;
        }

        List<String> topUsers = scoreboard.items().stream()
                .map(VersusDtos.ScoreBoardItem::userId)
                .limit(count)
                .toList();

        List<GoldenbellState> states = goldenbellStateRepository.findByRoomId(room.getId());
        Map<String, GoldenbellState> stateMap = states.stream()
                .collect(Collectors.toMap(GoldenbellState::getUserId, s -> s));

        List<GoldenbellState> toSave = new ArrayList<>();
        List<String> revived = new ArrayList<>();

        for (String userId : topUsers) {
            GoldenbellState state = stateMap.get(userId);
            if (state == null) {
                state = GoldenbellState.alive(room.getId(), userId);
                state.setRevived(true);
                stateMap.put(userId, state);
                toSave.add(state);
                revived.add(userId);
                continue;
            }
            if (!state.isAlive()) {
                state.setAlive(true);
                state.setRevived(true);
                toSave.add(state);
                revived.add(userId);
            }
        }

        if (toSave.isEmpty()) {
            return false;
        }

        goldenbellStateRepository.saveAll(toSave);
        recordEvent(room.getId(), "GOLDENBELL_REVIVE_TOP", Map.of(
                "users", revived
        ));
        return true;
    }

    private void recordEvent(Long roomId, String type, Map<String, Object> payload) {
        try {
            String payloadJson = payload == null || payload.isEmpty()
                    ? null
                    : objectMapper.writeValueAsString(payload);
            MatchEvent event = MatchEvent.builder()
                    .roomId(roomId)
                    .eventType(type)
                    .payloadJson(payloadJson)
                    .build();
            eventRepository.save(event);
        } catch (JsonProcessingException e) {
            log.warn("Failed to serialize payload for match event {} in room {}: {}", type, roomId, e.getMessage());
            MatchEvent event = MatchEvent.builder()
                    .roomId(roomId)
                    .eventType(type)
                    .payloadJson(null)
                    .build();
            eventRepository.save(event);
        }
    }

    private VersusDtos.TimelineEvent toTimelineEvent(MatchEvent event) {
        Map<String, Object> payload = Map.of();
        if (event.getPayloadJson() != null) {
            try {
                payload = objectMapper.readValue(event.getPayloadJson(), new TypeReference<>() {
                });
            } catch (Exception e) {
                payload = Map.of("raw", event.getPayloadJson());
            }
        }
        return new VersusDtos.TimelineEvent(event.getEventType(), event.getCreatedAt(), payload);
    }

    private String defaultGoldenbellRule() {
        return """
                [
                  {"round":1,"type":"OX","limitSec":10,"phase":"MAIN"},
                  {"round":2,"type":"OX","limitSec":10,"phase":"MAIN"},
                  {"round":3,"type":"MCQ","limitSec":10,"phase":"MAIN"},
                  {"round":4,"type":"MCQ","limitSec":10,"phase":"MAIN"},
                  {"round":5,"type":"SHORT","limitSec":10,"phase":"MAIN"},
                  {"round":6,"type":"LONG","limitSec":10,"phase":"MAIN"},
                  {"round":7,"type":"SHORT","limitSec":10,"phase":"FINAL"},
                  {"round":8,"type":"LONG","limitSec":10,"phase":"FINAL"}
                ]
                """;
    }

    private ScoreOutcome evaluateScore(MatchQuestion question,
                                       boolean requestedCorrect,
                                       Integer requestedTimeMs) {
        int limitMs = Math.max(1, Optional.ofNullable(question.getTimeLimitSec()).orElse(GOLDENBELL_TIME_LIMIT_SEC)) * 1000;
        int timeMs = requestedTimeMs == null ? limitMs : Math.max(0, requestedTimeMs);
        boolean correct = requestedCorrect && timeMs <= limitMs;
        int cappedTime = Math.min(timeMs, limitMs);
        int scoreDelta = 0;
        if (correct) {
            double speedRatio = (double) (limitMs - cappedTime) / limitMs;
            int bonus = (int) Math.round(speedRatio * SPEED_BONUS_MAX);
            scoreDelta = BASE_SCORE + bonus;
        }
        return new ScoreOutcome(correct, cappedTime, scoreDelta);
    }

    private record ScoreOutcome(boolean correct, int timeMs, int scoreDelta) {
    }

    private static class Score {
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

    private record ModeResolution(
            boolean roundCompleted,
            boolean matchCompleted,
            boolean stateChanged
    ) {
        static ModeResolution none() {
            return new ModeResolution(false, false, false);
        }
    }

    // ========== 개선 사항 1: 모드별 인원 제한 ==========

    private void validateParticipantLimit(MatchRoom room, int currentCount) {
        MatchMode mode = room.getMode();

        if (mode == MatchMode.DUEL) {
            // DUEL: 정확히 2명이어야 함
            if (currentCount >= DUEL_MAX_PARTICIPANTS) {
                throw new ResponseStatusException(
                        HttpStatus.BAD_REQUEST,
                        "DUEL 모드는 정확히 2명이어야 합니다. 현재: " + currentCount + "명"
                );
            }
        } else if (mode == MatchMode.TOURNAMENT) {
            // TOURNAMENT: 정확히 8명이어야 함
            if (currentCount >= TOURNAMENT_MAX_PARTICIPANTS) {
                throw new ResponseStatusException(
                        HttpStatus.BAD_REQUEST,
                        "TOURNAMENT 모드는 정확히 8명이어야 합니다. 현재: " + currentCount + "명"
                );
            }
        } else if (mode == MatchMode.GOLDENBELL) {
            // GOLDENBELL: 최대 20명 (예약 시스템이므로 최소 인원 제한 없음)
            if (currentCount >= GOLDENBELL_MAX_PARTICIPANTS) {
                throw new ResponseStatusException(
                        HttpStatus.BAD_REQUEST,
                        "GOLDENBELL 모드는 최대 20명까지만 참가 가능합니다. 현재: " + currentCount + "명"
                );
            }
        }
    }

    private void validateMinParticipants(MatchRoom room) {
        int count = (int) participantRepository.countByRoomId(room.getId());
        MatchMode mode = room.getMode();

        if (mode == MatchMode.DUEL) {
            // DUEL: 정확히 2명이어야 함
            if (count != 2) {
                throw new ResponseStatusException(
                        HttpStatus.BAD_REQUEST,
                        "DUEL 모드는 정확히 2명이어야 합니다. 현재: " + count + "명"
                );
            }
        } else if (mode == MatchMode.TOURNAMENT) {
            // TOURNAMENT: 정확히 8명이어야 함
            if (count != 8) {
                throw new ResponseStatusException(
                        HttpStatus.BAD_REQUEST,
                        "TOURNAMENT 모드는 정확히 8명이어야 합니다. 현재: " + count + "명"
                );
            }
        } else if (mode == MatchMode.GOLDENBELL) {
            // GOLDENBELL: 예약 시스템이므로 최소 인원 제한 없음 (자동 시작)
            // scheduledAt이 있으면 시간이 되면 자동 시작
            if (room.getScheduledAt() == null) {
                // 예약이 아닌 경우에만 최소 2명 체크
                if (count < 2) {
                    throw new ResponseStatusException(
                            HttpStatus.BAD_REQUEST,
                            "GOLDENBELL 모드는 최소 2명이 필요합니다. 현재: " + count + "명"
                    );
                }
            }
            // scheduledAt이 있으면 자동 시작되므로 최소 인원 체크 안 함
        }
    }

    // ========== 개선 사항 2: study-service 연동 - 문제 출제 자동화 ==========

    /**
     * scopeJson 기반으로 문제 생성 (public으로 변경하여 다른 서비스에서도 사용 가능)
     */
    public List<VersusDtos.QuestionInfo> generateQuestionsFromScope(MatchRoom room, String scopeJson) {
        Timer.Sample timer = monitoringConfig.startTimer("generateQuestionsFromScope");

        try {
            Map<String, Object> scope = readScope(scopeJson);
            String examMode = (String) scope.getOrDefault("examMode", "WRITTEN");
            String difficulty = (String) scope.getOrDefault("difficulty", "NORMAL");
            Long topicId = scope.get("topicId") != null ? Long.valueOf(scope.get("topicId").toString()) : null;

            // 모드별 문제 구성 결정
            List<StudyServiceClient.QuestionTypeSpec> questionTypes = determineQuestionTypes(room.getMode(), examMode);

            int totalCount = getTotalQuestionCount(room.getMode());
            StudyServiceClient.VersusQuestionRequest request = new StudyServiceClient.VersusQuestionRequest(
                    examMode,
                    topicId != null ? "SPECIFIC" : "ALL",
                    topicId,
                    difficulty,
                    totalCount,
                    questionTypes
            );

            log.info("Requesting {} questions from study-service for room {}: mode={}, examMode={}, difficulty={}",
                    totalCount, room.getId(), room.getMode(), examMode, difficulty);

            List<StudyServiceClient.QuestionDto> questions = studyServiceClient.generateVersusQuestions(request);

            if (questions.isEmpty()) {
                monitoringConfig.recordTimer(timer, "generateQuestionsFromScope", "status", "not_found");
                throw new ResponseStatusException(
                        HttpStatus.NOT_FOUND,
                        "No questions available for the given scope. Please check examMode, difficulty, and topicId."
                );
            }

            monitoringConfig.recordTimer(timer, "generateQuestionsFromScope", "status", "success");
            log.info("Generated {} questions from study-service for room {}", questions.size(), room.getId());

            // MatchQuestion 형태로 변환
            return convertToQuestionInfos(questions, room.getMode());
        } catch (ResponseStatusException e) {
            monitoringConfig.recordTimer(timer, "generateQuestionsFromScope", "status", "error");
            throw e; // 이미 적절한 HTTP 상태 코드가 설정된 예외는 그대로 전파
        } catch (Exception e) {
            monitoringConfig.recordTimer(timer, "generateQuestionsFromScope", "status", "failure");
            monitoringConfig.recordFeignFailure("study-service", "generateVersusQuestions", e);
            log.error("Failed to generate questions from scope for room {}: {}", room.getId(), e.getMessage(), e);
            throw new ResponseStatusException(
                    HttpStatus.INTERNAL_SERVER_ERROR,
                    "Failed to generate questions: " + e.getMessage()
            );
        }
    }

    private List<StudyServiceClient.QuestionTypeSpec> determineQuestionTypes(MatchMode matchMode, String examMode) {
        if (matchMode == MatchMode.DUEL) {
            if ("WRITTEN".equals(examMode)) {
                return List.of(
                        new StudyServiceClient.QuestionTypeSpec("OX", 2),
                        new StudyServiceClient.QuestionTypeSpec("MCQ", 8)
                );
            } else {
                return List.of(
                        new StudyServiceClient.QuestionTypeSpec("SHORT", 8),
                        new StudyServiceClient.QuestionTypeSpec("LONG", 2)
                );
            }
        } else if (matchMode == MatchMode.TOURNAMENT) {
            // 토너먼트는 라운드별로 구성이 다름 (현재는 간단히 처리)
            return List.of(
                    new StudyServiceClient.QuestionTypeSpec("OX", 3),
                    new StudyServiceClient.QuestionTypeSpec("MCQ", 6)
            );
        } else { // GOLDENBELL
            return List.of(
                    new StudyServiceClient.QuestionTypeSpec("OX", 2),
                    new StudyServiceClient.QuestionTypeSpec("MCQ", 2),
                    new StudyServiceClient.QuestionTypeSpec("SHORT", 1),
                    new StudyServiceClient.QuestionTypeSpec("LONG", 1)
            );
        }
    }

    private int getTotalQuestionCount(MatchMode mode) {
        return switch (mode) {
            case DUEL -> DUEL_TOTAL_QUESTIONS;
            case TOURNAMENT -> TOURNAMENT_ROUNDS * TOURNAMENT_QUESTIONS_PER_ROUND;
            case GOLDENBELL -> 6; // OX2 + MCQ2 + SHORT1 + LONG1 (FINAL은 별도)
        };
    }

    private List<VersusDtos.QuestionInfo> convertToQuestionInfos(
            List<StudyServiceClient.QuestionDto> questions, MatchMode mode) {
        List<VersusDtos.QuestionInfo> result = new ArrayList<>();
        int roundNo = 1;
        int orderNo = 1;

        for (StudyServiceClient.QuestionDto q : questions) {
            MatchPhase phase = determinePhase(q.type(), mode);
            int timeLimit = determineTimeLimit(q.type(), q.mode());

            result.add(new VersusDtos.QuestionInfo(
                    q.id(),
                    roundNo,
                    phase,
                    orderNo,
                    timeLimit
            ));

            orderNo++;
            // 라운드 구분 (간단한 로직)
            if (orderNo > getQuestionsPerRound(mode)) {
                roundNo++;
                orderNo = 1;
            }
        }

        return result;
    }

    private MatchPhase determinePhase(String questionType, MatchMode mode) {
        // GOLDENBELL의 경우 round_flow_json에서 결정해야 하지만, 일단 기본값
        if (mode == MatchMode.GOLDENBELL && ("SHORT".equals(questionType) || "LONG".equals(questionType))) {
            // 마지막 문제들은 FINAL로 처리
            return MatchPhase.FINAL;
        }
        return MatchPhase.MAIN;
    }

    private int determineTimeLimit(String questionType, String examMode) {
        if ("WRITTEN".equals(examMode)) {
            return switch (questionType) {
                case "OX" -> 5;
                case "MCQ" -> 10;
                default -> 10;
            };
        } else {
            return switch (questionType) {
                case "SHORT" -> 15;
                case "LONG" -> 25;
                default -> 15;
            };
        }
    }

    private int getQuestionsPerRound(MatchMode mode) {
        return switch (mode) {
            case DUEL -> DUEL_TOTAL_QUESTIONS;
            case TOURNAMENT -> TOURNAMENT_QUESTIONS_PER_ROUND;
            case GOLDENBELL -> 1; // 골든벨은 문제당 1라운드
        };
    }

    // ========== 개선 사항 3: 서버 사이드 정답 검증 ==========

    private boolean validateAnswerOnServer(Long questionId, VersusDtos.SubmitAnswerReq req) {
        Timer.Sample timer = monitoringConfig.startTimer("validateAnswerOnServer");

        try {
            // 사용자 답안이 없으면 클라이언트 값 사용 (하위 호환성)
            if (req.userAnswer() == null || req.userAnswer().isBlank()) {
                log.warn("User answer not provided for question {}, using client-provided correct value", questionId);
                monitoringConfig.recordTimer(timer, "validateAnswerOnServer", "status", "fallback");
                return req.correct();
            }

            // study-service의 validateAnswer API 사용
            StudyServiceClient.UserAnswerDto userAnswerDto = new StudyServiceClient.UserAnswerDto(
                    req.userAnswer(),
                    null  // answerType은 자동 판단
            );

            StudyServiceClient.AnswerValidationResult result = studyServiceClient.validateAnswer(
                    questionId, userAnswerDto);

            monitoringConfig.recordTimer(timer, "validateAnswerOnServer", "status", "success");
            log.debug("Answer validation for question {}: userAnswer={}, correct={}, serverCorrect={}",
                    questionId, req.userAnswer(), req.correct(), result.correct());

            return result.correct();
        } catch (Exception e) {
            monitoringConfig.recordTimer(timer, "validateAnswerOnServer", "status", "failure");
            monitoringConfig.recordFeignFailure("study-service", "validateAnswer", e);
            log.error("Failed to validate answer for question {}: {}", questionId, e.getMessage(), e);
            // 검증 실패 시 클라이언트 값 사용 (fallback)
            return req.correct();
        }
    }

    // ========== 개선 사항 4: 서버 기준 시간 계산 ==========

    /**
     * 문제 시작 시점 이벤트 기록 (처음 답안 제출 시)
     */
    private void recordQuestionStartIfNeeded(Long roomId, Long questionId, String userId) {
        // 이미 기록된 이벤트가 있으면 스킵
        Optional<MatchEvent> existing = eventRepository
                .findFirstByRoomIdAndEventTypeAndPayloadJsonContainingOrderByCreatedAtDesc(
                        roomId, "QUESTION_STARTED",
                        "\"questionId\":" + questionId + ",\"userId\":\"" + userId + "\""
                );

        if (existing.isPresent()) {
            return; // 이미 기록됨
        }

        // 문제 시작 시점 이벤트 기록
        recordEvent(roomId, "QUESTION_STARTED", Map.of(
                "questionId", questionId,
                "userId", userId,
                "startedAt", Instant.now().toString()
        ));

        log.debug("Recorded QUESTION_STARTED event for room={}, question={}, user={}",
                roomId, questionId, userId);
    }

    private Integer calculateServerTimeMs(Long roomId, Long questionId, String userId, Integer clientTimeMs) {
        try {
            // 문제 시작 시점 이벤트 찾기
            List<MatchEvent> startEvents = eventRepository.findByRoomIdAndEventTypeContaining(
                    roomId, "QUESTION_STARTED");
            Optional<MatchEvent> startEvent = startEvents.stream()
                    .filter(e -> {
                        try {
                            if (e.getPayloadJson() == null) return false;
                            Map<String, Object> payload = objectMapper.readValue(
                                    e.getPayloadJson(), new TypeReference<Map<String, Object>>() {
                                    });
                            Object qId = payload.get("questionId");
                            Object uId = payload.get("userId");
                            return qId != null && questionId.equals(Long.valueOf(qId.toString())) &&
                                    uId != null && userId.equals(uId.toString());
                        } catch (Exception ex) {
                            return false;
                        }
                    })
                    .findFirst();

            if (startEvent.isPresent()) {
                try {
                    Map<String, Object> payload = objectMapper.readValue(
                            startEvent.get().getPayloadJson(), new TypeReference<Map<String, Object>>() {
                            });
                    String startedAtStr = (String) payload.get("startedAt");
                    if (startedAtStr != null) {
                        Instant startTime = Instant.parse(startedAtStr);
                        long serverTimeMs = Duration.between(startTime, Instant.now()).toMillis();
                        return (int) Math.max(0, serverTimeMs);
                    }
                } catch (Exception e) {
                    log.debug("Failed to parse question start time: {}", e.getMessage());
                }
            }

            // 문제 시작 이벤트가 없으면 클라이언트 시간 사용
            return clientTimeMs;
        } catch (Exception e) {
            log.debug("Failed to calculate server time: {}", e.getMessage());
            return clientTimeMs;
        }
    }

    // ========== 개선 사항 6: progress-service 연동 ==========

    private void notifyProgressService(MatchRoom room, VersusDtos.ScoreBoardResp scoreboard) {
        if (scoreboard.items().isEmpty()) {
            log.warn("No participants in scoreboard for room {}, skipping progress-service notification", room.getId());
            return;
        }

        Timer.Sample timer = monitoringConfig.startTimer("notifyProgressService");

        try {
            List<ProgressServiceClient.ParticipantResult> participants = scoreboard.items().stream()
                    .map(item -> new ProgressServiceClient.ParticipantResult(
                            item.userId(),
                            item.score(),
                            item.rank(),
                            item.correctCount(),
                            item.totalCount(),
                            item.totalTimeMs()
                    ))
                    .toList();

            String winner = scoreboard.items().get(0).userId(); // 1등이 우승자
            int questionCount = (int) questionRepository.countByRoomId(room.getId());
            long durationMs = calculateMatchDuration(room);

            ProgressServiceClient.VersusResultRequest request = new ProgressServiceClient.VersusResultRequest(
                    room.getMode().name(),
                    room.getId(),
                    winner,
                    participants,
                    questionCount,
                    durationMs
            );

            log.info("Notifying progress-service for room {} completion: mode={}, winner={}, participants={}, questions={}, duration={}ms",
                    room.getId(), room.getMode(), winner, participants.size(), questionCount, durationMs);

            progressServiceClient.recordVersusResult(request);
            monitoringConfig.recordTimer(timer, "notifyProgressService", "status", "success");
            log.info("Successfully notified progress-service for room {} completion", room.getId());
        } catch (Exception e) {
            monitoringConfig.recordTimer(timer, "notifyProgressService", "status", "failure");
            monitoringConfig.recordRewardFailure(room.getId().toString(), "all", e);
            log.error("Failed to notify progress-service for room {}: {}", room.getId(), e.getMessage(), e);

            // 비동기 재시도 큐에 추가
            try {
                List<ProgressServiceClient.ParticipantResult> participants = scoreboard.items().stream()
                        .map(item -> new ProgressServiceClient.ParticipantResult(
                                item.userId(),
                                item.score(),
                                item.rank(),
                                item.correctCount(),
                                item.totalCount(),
                                item.totalTimeMs()
                        ))
                        .toList();

                String winner = scoreboard.items().get(0).userId();
                int questionCount = (int) questionRepository.countByRoomId(room.getId());
                long durationMs = calculateMatchDuration(room);

                ProgressServiceClient.VersusResultRequest request = new ProgressServiceClient.VersusResultRequest(
                        room.getMode().name(),
                        room.getId(),
                        winner,
                        participants,
                        questionCount,
                        durationMs
                );

                rewardRetryService.retryRewardPayment(room.getId(), request);
                log.info("보상 지급 재시도 큐에 추가: roomId={}", room.getId());
            } catch (Exception retryException) {
                log.error("보상 지급 재시도 큐 추가 실패: roomId={}", room.getId(), retryException);
            }
        }
    }

    private long calculateMatchDuration(MatchRoom room) {
        Optional<MatchEvent> startEvent = eventRepository.findByRoomIdAndEventType(room.getId(), "MATCH_STARTED")
                .stream()
                .findFirst();

        if (startEvent.isPresent()) {
            return Duration.between(startEvent.get().getCreatedAt(), Instant.now()).toMillis();
        }
        return 0;
    }

    // ========== 개선 사항 6: GoldenbellRule 활용 강화 ==========

    private List<RoundFlowRule> parseRoundFlowJson(String json) {
        try {
            List<Map<String, Object>> rounds = objectMapper.readValue(
                    json, new TypeReference<List<Map<String, Object>>>() {
                    });
            return rounds.stream()
                    .map(this::toRoundFlowRule)
                    .toList();
        } catch (Exception e) {
            log.error("Failed to parse round_flow_json: {}", e.getMessage());
            throw new ResponseStatusException(
                    HttpStatus.BAD_REQUEST,
                    "Invalid round_flow_json: " + e.getMessage()
            );
        }
    }

    private RoundFlowRule toRoundFlowRule(Map<String, Object> map) {
        return new RoundFlowRule(
                ((Number) map.getOrDefault("round", 1)).intValue(),
                (String) map.getOrDefault("type", "MCQ"),
                ((Number) map.getOrDefault("count", 1)).intValue(),
                ((Number) map.getOrDefault("limitSec", 10)).intValue(),
                (String) map.getOrDefault("phase", "MAIN"),
                map.get("finalParts") != null ?
                        (List<String>) map.get("finalParts") : List.of(),
                (String) map.getOrDefault("scoring", "default")
        );
    }

    private record RoundFlowRule(
            int round,
            String type,
            int count,
            int limitSec,
            String phase,
            List<String> finalParts,
            String scoring
    ) {
    }

    private static class FinalRoundScore {
        int correct;
        int total;
        int score;
        long totalTimeMs;
    }
}
