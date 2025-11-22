package com.OhRyue.certpilot.versus.service;

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

import java.time.Instant;
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

  @Transactional
  public VersusDtos.RoomDetailResp createRoom(VersusDtos.CreateRoomReq req) {
    MatchRoom room = roomRepository.save(MatchRoom.builder()
        .mode(req.mode())
        .status(MatchStatus.WAIT)
        .scopeJson(Optional.ofNullable(req.scopeJson()).orElse("{}"))
        .build());

    recordEvent(room.getId(), "ROOM_CREATED", Map.of(
        "mode", room.getMode().name(),
        "scope", room.getScopeJson()
    ));

    if (req.participants() != null) {
      req.participants().forEach(userId -> {
        MatchParticipant participant = registerParticipant(room.getId(), userId);
        recordEvent(room.getId(), "PLAYER_JOINED", Map.of(
            "userId", participant.getUserId(),
            "joinedAt", participant.getJoinedAt().toString()
        ));
      });
    }

    if (req.questions() == null || req.questions().isEmpty()) {
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Questions are required to start a match");
    }

    List<MatchQuestion> questions = req.questions().stream()
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

    MatchAnswer answer = answerRepository.findByRoomIdAndQuestionIdAndUserId(
            roomId, req.questionId(), userId)
        .orElse(MatchAnswer.builder()
            .roomId(roomId)
            .questionId(req.questionId())
            .userId(userId)
            .build());

    ScoreOutcome outcome = evaluateScore(question, req.correct(), req.timeMs());

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

  private ModeResolution handleModeAfterAnswer(MatchRoom room,
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
        .ifPresent(p -> { throw new ResponseStatusException(HttpStatus.CONFLICT, "Already joined"); });
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
              p.getRank(),
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

  private VersusDtos.ScoreBoardResp computeScoreboard(MatchRoom room) {
    Long roomId = room.getId();
    List<MatchParticipant> participants = participantRepository.findByRoomId(roomId);
    Map<String, GoldenbellState> goldenState = goldenbellStateRepository.findByRoomId(roomId).stream()
        .collect(Collectors.toMap(GoldenbellState::getUserId, g -> g));

    Map<Long, MatchQuestion> questionMap = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId).stream()
        .collect(Collectors.toMap(MatchQuestion::getQuestionId, q -> q));

    List<MatchAnswer> answers = answerRepository.findByRoomId(roomId);
    Map<String, Score> stats = new HashMap<>();

    for (MatchAnswer answer : answers) {
      Score score = stats.computeIfAbsent(answer.getUserId(), u -> new Score());
      score.total++;
      if (answer.isCorrect()) {
        score.correct++;
      }
      score.score += Optional.ofNullable(answer.getScoreDelta()).orElse(0);
      MatchQuestion q = questionMap.get(answer.getQuestionId());
      int limitMs = Optional.ofNullable(q)
          .map(MatchQuestion::getTimeLimitSec)
          .orElse(GOLDENBELL_TIME_LIMIT_SEC) * 1000;
      int time = Optional.ofNullable(answer.getTimeMs()).orElse(limitMs);
      if (time <= 0) {
        time = limitMs;
      }
      score.totalTimeMs += Math.min(time, limitMs);
    }

    for (MatchParticipant participant : participants) {
      stats.computeIfAbsent(participant.getUserId(), u -> new Score());
    }

    List<ScoreboardIntermediate> intermediates = stats.entrySet().stream()
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
        participant.setRank(rank);
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

  private Map<String, Object> readScope(String scopeJson) {
    if (scopeJson == null || scopeJson.isBlank()) {
      return Map.of();
    }
    try {
      return objectMapper.readValue(scopeJson, new TypeReference<Map<String, Object>>() {});
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
        payload = objectMapper.readValue(event.getPayloadJson(), new TypeReference<>() {});
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

  private record ScoreOutcome(boolean correct, int timeMs, int scoreDelta) {}

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
  ) {}

  private record ModeResolution(
      boolean roundCompleted,
      boolean matchCompleted,
      boolean stateChanged
  ) {
    static ModeResolution none() {
      return new ModeResolution(false, false, false);
    }
  }
}
