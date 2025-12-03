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
import feign.FeignException;

import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.Comparator;
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
    private static final int GOLDENBELL_REVIVE_CHECK_ROUND = 2; // 라운드 2 종료 후 (OX+MCQ 총 4문제) 체크
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
        
        // 모든 모드에서 첫 번째 문제 시작 이벤트 기록 (모든 참가자가 동시에 시작)
        List<MatchQuestion> questions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId);
        if (!questions.isEmpty()) {
            MatchQuestion firstQuestion = questions.get(0);
            // 첫 번째 문제의 시작 시점을 기록 (모든 참가자 공통)
            recordEvent(roomId, "QUESTION_STARTED", Map.of(
                    "questionId", firstQuestion.getQuestionId(),
                    "roundNo", firstQuestion.getRoundNo(),
                    "phase", firstQuestion.getPhase().name(),
                    "startedAt", Instant.now().toString(),
                    "allParticipants", true // 모든 참가자 공통 시작
            ));
            log.info("첫 번째 문제 시작 이벤트 기록: mode={}, roomId={}, questionId={}, roundNo={}",
                    room.getMode(), roomId, firstQuestion.getQuestionId(), firstQuestion.getRoundNo());
        }
        
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
        // 답안 제출 전에 기록해야 정확한 시간 계산 가능
        Instant answerSubmitTime = Instant.now();
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
        
        // 디버깅: 정답 검증 결과 로그
        log.info("Answer validation result: roomId={}, userId={}, questionId={}, userAnswer={}, clientCorrect={}, serverCorrect={}, finalCorrect={}",
                roomId, userId, question.getQuestionId(), req.userAnswer(), req.correct(), serverValidatedCorrect, finalCorrect);

        // 서버 기준 시간 계산 (문제 시작 시점이 있으면 사용)
        // answerSubmitTime을 전달하여 정확한 시간 계산
        Integer serverTimeMs = calculateServerTimeMs(roomId, question.getQuestionId(), userId, req.timeMs(), answerSubmitTime);
        
        // 디버깅: 시간 계산 결과 로그
        log.info("Time calculation: roomId={}, userId={}, questionId={}, clientTimeMs={}, serverTimeMs={}, timeLimitSec={}",
                roomId, userId, question.getQuestionId(), req.timeMs(), serverTimeMs, question.getTimeLimitSec());

        ScoreOutcome outcome = evaluateScore(question, finalCorrect, serverTimeMs);
        
        // 디버깅: 점수 계산 결과 로그
        log.info("Score evaluation: roomId={}, userId={}, questionId={}, correct={}, scoreDelta={}, timeMs={}, timeLimitMs={}",
                roomId, userId, question.getQuestionId(), outcome.correct(), outcome.scoreDelta(), outcome.timeMs(), 
                question.getTimeLimitSec() * 1000);

        answer.setCorrect(outcome.correct());
        answer.setScoreDelta(outcome.scoreDelta());
        answer.setTimeMs(outcome.timeMs());
        answer.setRoundNo(Optional.ofNullable(req.roundNo()).orElse(question.getRoundNo()));
        answer.setPhase(Optional.ofNullable(req.phase()).orElse(question.getPhase()));
        // 사용자가 제출한 답안 내용 저장 (단답식/서술형 표시용)
        answer.setUserAnswer(req.userAnswer());
        answerRepository.save(answer);
        answerRepository.flush(); // 즉시 DB에 반영
        
        // 저장 후 검증: 실제 저장된 값 확인
        MatchAnswer savedAnswer = answerRepository.findByRoomIdAndQuestionIdAndUserId(roomId, req.questionId(), userId)
                .orElseThrow(() -> new IllegalStateException("Answer not found after save"));
        
        log.info("Answer saved: roomId={}, userId={}, questionId={}, correct={}, scoreDelta={}, timeMs={}, userAnswer={}, savedCorrect={}",
                roomId, userId, question.getQuestionId(), outcome.correct(), outcome.scoreDelta(), outcome.timeMs(), req.userAnswer(), savedAnswer.isCorrect());
        
        // 저장된 값이 예상과 다른 경우 경고
        if (savedAnswer.isCorrect() != outcome.correct()) {
            log.error("CRITICAL: Saved answer correct value mismatch! Expected={}, Actual={}, roomId={}, userId={}, questionId={}",
                    outcome.correct(), savedAnswer.isCorrect(), roomId, userId, question.getQuestionId());
        }

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

        // 스코어보드 계산: DUEL 모드에서는 모든 참가자 표시, TOURNAMENT 모드에서는 현재 라운드의 활성 참가자만 표시
        // 답안이 저장된 후(flush 후) 계산하므로 최신 답안이 반영됨
        // flush() 후에도 영속성 컨텍스트에 이전 답안이 캐시되어 있을 수 있으므로,
        // computeScoreboard 내에서 명시적으로 DB에서 최신 답안을 조회하도록 함
        VersusDtos.ScoreBoardResp scoreboard;
        if (room.getMode() == MatchMode.TOURNAMENT) {
            // 토너먼트 모드: 현재 라운드의 활성 참가자만 표시
            scoreboard = computeScoreboard(room, question, userId);
        } else {
            // DUEL, GOLDENBELL 모드: 모든 참가자 표시
            scoreboard = computeScoreboard(room);
        }
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

    /**
     * 문제 시작 이벤트 기록 (테스트용)
     * 골든벨 모드에서 문제 시작 이벤트를 직접 기록합니다.
     */
    @Transactional
    public Map<String, Object> recordQuestionStartEvent(Long roomId, Long questionId) {
        MatchRoom room = findRoomOrThrow(roomId);
        
        // 모든 모드에서 사용 가능 (DUEL, TOURNAMENT, GOLDENBELL)
        
        // 문제 존재 확인
        MatchQuestion question = questionRepository.findByRoomIdAndQuestionId(roomId, questionId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.BAD_REQUEST, 
                        "Question not found: " + questionId));
        
        // 이미 QUESTION_STARTED 이벤트가 있는지 확인
        List<MatchEvent> existingEvents = eventRepository.findByRoomIdAndEventTypeContaining(
                roomId, "QUESTION_STARTED");
        boolean alreadyRecorded = existingEvents.stream()
                .anyMatch(e -> {
                    try {
                        if (e.getPayloadJson() == null) return false;
                        Map<String, Object> payload = objectMapper.readValue(
                                e.getPayloadJson(), new TypeReference<Map<String, Object>>() {
                                });
                        Object qId = payload.get("questionId");
                        Boolean allParticipants = (Boolean) payload.get("allParticipants");
                        return qId != null && questionId.equals(Long.valueOf(qId.toString())) &&
                                Boolean.TRUE.equals(allParticipants);
                    } catch (Exception ex) {
                        return false;
                    }
                });
        
        if (alreadyRecorded) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, 
                    "QUESTION_STARTED event already recorded for this question");
        }
        
        // 문제 시작 이벤트 기록 (모든 참가자 공통)
        recordEvent(roomId, "QUESTION_STARTED", Map.of(
                "questionId", questionId,
                "roundNo", question.getRoundNo(),
                "phase", question.getPhase().name(),
                "startedAt", Instant.now().toString(),
                "allParticipants", true // 모든 참가자 공통 시작
        ));
        
        log.info("문제 시작 이벤트 기록 (테스트용): roomId={}, questionId={}, roundNo={}",
                roomId, questionId, question.getRoundNo());
        
        return Map.of(
                "success", true,
                "roomId", roomId,
                "questionId", questionId,
                "roundNo", question.getRoundNo(),
                "phase", question.getPhase().name(),
                "message", "QUESTION_STARTED event recorded successfully"
        );
    }

    /**
     * 문제별 모든 답안 조회 (골든벨 프론트엔드용)
     * 골든벨 모드에서만 모든 사용자의 답안을 반환
     */
    @Transactional(readOnly = true)
    public VersusDtos.QuestionAnswersResp getQuestionAnswers(Long roomId, Long questionId) {
        MatchRoom room = findRoomOrThrow(roomId);
        
        // 골든벨 모드가 아니면 빈 리스트 반환 (1:1 배틀, 토너먼트는 상대방 답 안 띄움)
        if (room.getMode() != MatchMode.GOLDENBELL) {
            return new VersusDtos.QuestionAnswersResp(questionId, List.of());
        }
        
        List<MatchAnswer> answers = answerRepository.findByRoomIdAndQuestionId(roomId, questionId);
        
        List<VersusDtos.AnswerInfo> answerInfos = answers.stream()
                .map(answer -> new VersusDtos.AnswerInfo(
                        answer.getUserId(),
                        answer.getUserAnswer(), // 단답식/서술형 답안 텍스트
                        answer.isCorrect(),
                        answer.getTimeMs(),
                        answer.getScoreDelta(),
                        answer.getSubmittedAt()
                ))
                .toList();
        
        return new VersusDtos.QuestionAnswersResp(questionId, answerInfos);
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
        log.debug("handleDuelProgress 호출: roomId={}, questionId={}, participants={}", 
                room.getId(), question.getQuestionId(), participants);
        
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

        log.info("DUEL 진행 체크: roomId={}, questionId={}, participants={}, answeredForQuestion={}, roundCompleted={}", 
                room.getId(), question.getQuestionId(), participants, answeredForQuestion, roundCompleted);

        if (roundCompleted) {
            log.info("DUEL 라운드 완료: roomId={}, questionId={}, round={}, phase={}", 
                    room.getId(), question.getQuestionId(), question.getRoundNo(), question.getPhase().name());
            
            recordEvent(room.getId(), "ROUND_COMPLETED", Map.of(
                    "mode", "DUEL",
                    "round", question.getRoundNo(),
                    "phase", question.getPhase().name()
            ));
            
            // 다음 문제가 있으면 시작 이벤트 기록 (모든 참가자 공통)
            Optional<MatchQuestion> nextQuestion = findNextQuestion(room.getId(), question);
            if (nextQuestion.isPresent()) {
                MatchQuestion next = nextQuestion.get();
                // 다음 문제 시작 이벤트 기록 (모든 참가자 공통)
                Instant startedAt = Instant.now();
                recordEvent(room.getId(), "QUESTION_STARTED", Map.of(
                        "questionId", next.getQuestionId(),
                        "roundNo", next.getRoundNo(),
                        "phase", next.getPhase().name(),
                        "startedAt", startedAt.toString(),
                        "allParticipants", true // 모든 참가자 공통 시작
                ));
                log.info("1:1 배틀 다음 문제 시작 이벤트 기록: roomId={}, questionId={}, roundNo={}, orderNo={}, startedAt={}",
                        room.getId(), next.getQuestionId(), next.getRoundNo(), next.getOrderNo(), startedAt);
            } else {
                log.info("DUEL 다음 문제 없음: roomId={}, currentQuestionId={}, roundNo={}, orderNo={}", 
                        room.getId(), question.getQuestionId(), question.getRoundNo(), question.getOrderNo());
            }
        } else {
            log.debug("DUEL 라운드 미완료: roomId={}, questionId={}, participants={}, answeredForQuestion={}", 
                    room.getId(), question.getQuestionId(), participants, answeredForQuestion);
        }

        boolean matchCompleted = checkAllQuestionsAnswered(room.getId(), participants);
        if (matchCompleted) {
            String winner = scoreboard.items().isEmpty() ? null : scoreboard.items().get(0).userId();
            recordEvent(room.getId(), "MATCH_FINISHED", Map.of(
                    "mode", "DUEL",
                    "winner", winner
            ));
            log.info("DUEL 매치 완료: roomId={}, winner={}", room.getId(), winner);
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

        // 동점 처리 규칙: 모두 0점인지 체크
        boolean allZeroScore = ordered.stream().allMatch(item -> item.score() == 0);
        int currentRound = question.getRoundNo();
        
        if (allZeroScore) {
            if (currentRound == 1) {
                // 1라운드: 모두 0점이면 재경기
                log.info("토너먼트 1라운드 모두 0점 감지: roomId={}, 재경기 진행", roomId);
                if (retryTournamentRound(room, currentRound)) {
                    recordEvent(roomId, "TOURNAMENT_ROUND_RETRY", Map.of(
                            "mode", "TOURNAMENT",
                            "round", currentRound,
                            "reason", "ALL_ZERO_SCORE"
                    ));
                    return new ModeResolution(false, false, true); // 재경기 진행, 매치 미완료, 상태 변경됨
                } else {
                    log.error("토너먼트 1라운드 재경기 실패: roomId={}, round={}", roomId, currentRound);
                    // 재경기 실패 시 기존 로직대로 진행 (시간 기준으로 순위 결정)
                }
            } else {
                // 2라운드, 3라운드: 모두 0점이면 누적 점수로 순위 결정
                // computeScoreboard에서 이미 누적 점수로 계산하고 있으므로, 그대로 사용
                log.info("토너먼트 {}라운드 모두 0점 감지: roomId={}, 누적 점수로 순위 결정", currentRound, roomId);
                recordEvent(roomId, "TOURNAMENT_ROUND_ALL_ZERO", Map.of(
                        "mode", "TOURNAMENT",
                        "round", currentRound,
                        "reason", "ALL_ZERO_SCORE_USE_CUMULATIVE"
                ));
            }
        }

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
        
        // 다음 문제가 있으면 시작 이벤트 기록 (모든 참가자 공통)
        Optional<MatchQuestion> nextQuestion = findNextQuestion(roomId, question);
        if (nextQuestion.isPresent()) {
            MatchQuestion next = nextQuestion.get();
            // 다음 문제 시작 이벤트 기록 (모든 참가자 공통)
            recordEvent(roomId, "QUESTION_STARTED", Map.of(
                    "questionId", next.getQuestionId(),
                    "roundNo", next.getRoundNo(),
                    "phase", next.getPhase().name(),
                    "startedAt", Instant.now().toString(),
                    "allParticipants", true // 모든 참가자 공통 시작
            ));
            log.info("토너먼트 다음 문제 시작 이벤트 기록: roomId={}, questionId={}, roundNo={}",
                    roomId, next.getQuestionId(), next.getRoundNo());
        }

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
            // 전원 탈락 방지 규칙: aliveCount == 0이면 문제 무효 처리 및 재출제
            if (aliveCount == 0) {
                log.warn("골든벨 전원 탈락 감지: roomId={}, round={}, order={}, questionId={}. 문제 무효 처리 및 재출제",
                        roomId, question.getRoundNo(), question.getOrderNo(), question.getQuestionId());
                
                if (retryQuestionOnFullElimination(room, question)) {
                    stateChanged = true;
                    // 재출제 후 상태 재계산
                    states = goldenbellStateRepository.findByRoomId(roomId);
                    aliveCount = states.stream().filter(GoldenbellState::isAlive).count();
                    // 재출제했으므로 roundCompleted는 false로 유지 (같은 라운드에서 재시도)
                    return new ModeResolution(false, false, true);
                }
            }
            
            recordEvent(roomId, "ROUND_COMPLETED", Map.of(
                    "mode", "GOLDENBELL",
                    "round", question.getRoundNo(),
                    "phase", question.getPhase().name(),
                    "aliveCount", aliveCount
            ));
            
            // 다음 문제가 있으면 시작 이벤트 기록 (모든 참가자 공통)
            Optional<MatchQuestion> nextQuestion = findNextQuestion(roomId, question);
            if (nextQuestion.isPresent()) {
                MatchQuestion next = nextQuestion.get();
                // 다음 문제 시작 이벤트 기록 (모든 참가자 공통)
                recordEvent(roomId, "QUESTION_STARTED", Map.of(
                        "questionId", next.getQuestionId(),
                        "roundNo", next.getRoundNo(),
                        "phase", next.getPhase().name(),
                        "startedAt", Instant.now().toString(),
                        "allParticipants", true // 모든 참가자 공통 시작
                ));
                log.info("골든벨 다음 문제 시작 이벤트 기록: roomId={}, questionId={}, roundNo={}",
                        roomId, next.getQuestionId(), next.getRoundNo());
            }

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
        return computeScoreboard(room, null, null);
    }

    /**
     * 스코어보드 계산 (토너먼트 모드에서 현재 라운드의 활성 참가자만 표시)
     * @param room 방 정보
     * @param currentQuestion 현재 문제 (토너먼트 모드에서 라운드 필터링용, null이면 전체 표시)
     * @param currentUserId 현재 사용자 ID (토너먼트 모드에서 상대방 필터링용, null이면 전체 표시)
     */
    public VersusDtos.ScoreBoardResp computeScoreboard(MatchRoom room, MatchQuestion currentQuestion, String currentUserId) {
        Long roomId = room.getId();
        List<MatchParticipant> participants = participantRepository.findByRoomId(roomId);
        Map<String, GoldenbellState> goldenState = goldenbellStateRepository.findByRoomId(roomId).stream()
                .collect(Collectors.toMap(GoldenbellState::getUserId, g -> g));

        Map<Long, MatchQuestion> questionMap = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId).stream()
                .collect(Collectors.toMap(
                    MatchQuestion::getQuestionId, 
                    q -> q,
                    (existing, replacement) -> existing  // 중복 키 발생 시 기존 값 유지
                ));

        // 답안 조회: flush() 후에도 영속성 컨텍스트에 이전 답안이 캐시되어 있을 수 있으므로
        // 명시적으로 DB에서 최신 답안을 조회하기 위해 entityManager를 사용하여 쿼리 힌트 추가
        // 또는 간단하게 answerRepository를 통해 조회 (JPA는 기본적으로 최신 데이터를 조회함)
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
                    .sorted((a, b) -> {
                        // alive 상태 우선: alive=true가 항상 alive=false보다 높은 순위
                        int aliveCompare = Boolean.compare(b.alive(), a.alive());
                        if (aliveCompare != 0) return aliveCompare;
                        // FINAL 라운드 점수 내림차순
                        // 점수는 정답 + 속도 보너스로 계산되므로, 정답 수가 같아도 속도가 빠른 사람이 더 높은 점수를 받음
                        int scoreCompare = Integer.compare(b.score(), a.score());
                        if (scoreCompare != 0) return scoreCompare;
                        // 점수가 같을 경우 (거의 발생하지 않지만) 전체 제출속도(합산) 빠른 사람이 우선
                        int timeCompare = Long.compare(a.totalTimeMs(), b.totalTimeMs());
                        if (timeCompare != 0) return timeCompare;
                        // userId 오름차순
                        return a.userId().compareTo(b.userId());
                    })
                    .toList();
        } else {
            // 일반 정렬 (점수 내림차순)
            // DUEL 모드: 동점일 경우 전체 제출속도(합산) 빠른 사람이 우승
            // TOURNAMENT/GOLDENBELL: 동일한 정렬 기준 적용
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
                        // GOLDENBELL 모드인 경우 alive 상태 우선
                        if (isGoldenbell) {
                            int aliveCompare = Boolean.compare(b.alive(), a.alive());
                            if (aliveCompare != 0) return aliveCompare;
                        }
                        // 점수 내림차순
                        // 점수는 정답 + 속도 보너스로 계산되므로, 정답 수가 같아도 속도가 빠른 사람이 더 높은 점수를 받음
                        // 따라서 점수 비교만으로도 정답 수와 속도를 모두 반영한 순위 결정 가능
                        int scoreCompare = Integer.compare(b.score(), a.score());
                        if (scoreCompare != 0) return scoreCompare;
                        // 점수가 같을 경우 (거의 발생하지 않지만) 전체 제출속도(합산) 빠른 사람이 우선
                        // totalTimeMs는 모든 문제의 제출 시간 합계이므로 이미 전체 제출속도 합산임
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
            
            // 현재 사용자와 같은 라운드의 활성 참가자만 표시
            // (토너먼트는 라운드별로 경쟁하므로, 같은 라운드의 모든 활성 참가자가 상대방)
            log.debug("토너먼트 라운드 {} 활성 참가자: {}", currentQuestion.getRoundNo(), activeUserIds);
        }

        int rank = 1;
        int previousScore = Integer.MIN_VALUE;
        int previousCorrect = Integer.MIN_VALUE;
        Long previousTime = null;

        List<VersusDtos.ScoreBoardItem> finalItems = new ArrayList<>();
        Map<String, MatchParticipant> participantMap = participants.stream()
                .collect(Collectors.toMap(MatchParticipant::getUserId, p -> p));

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

        // 현재 문제 정보 계산
        VersusDtos.CurrentQuestionInfo currentQuestionInfo = getCurrentQuestionInfo(roomId, room.getStatus());

        return new VersusDtos.ScoreBoardResp(roomId, room.getStatus(), finalItems, currentQuestionInfo);
    }

    private boolean hasNextRound(Long roomId, int currentRound) {
        return questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId).stream()
                .anyMatch(q -> q.getRoundNo() > currentRound);
    }
    
    /**
     * 현재 문제 다음 문제 찾기
     * 같은 라운드 내의 다음 문제(order 증가) 또는 다음 라운드의 첫 문제를 반환
     */
    private Optional<MatchQuestion> findNextQuestion(Long roomId, MatchQuestion currentQuestion) {
        List<MatchQuestion> allQuestions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId);
        
        // 같은 라운드 내의 다음 문제 찾기 (order 증가)
        Optional<MatchQuestion> sameRoundNext = allQuestions.stream()
                .filter(q -> q.getRoundNo().equals(currentQuestion.getRoundNo())
                        && q.getOrderNo() > currentQuestion.getOrderNo())
                .min(Comparator.comparing(MatchQuestion::getOrderNo));
        
        if (sameRoundNext.isPresent()) {
            return sameRoundNext;
        }
        
        // 같은 라운드에 다음 문제가 없으면 다음 라운드의 첫 문제 찾기
        return allQuestions.stream()
                .filter(q -> q.getRoundNo() > currentQuestion.getRoundNo())
                .min(Comparator.comparing(MatchQuestion::getRoundNo)
                        .thenComparing(MatchQuestion::getOrderNo));
    }

    private boolean checkAllQuestionsAnswered(Long roomId, long participants) {
        long totalAnswers = answerRepository.countByRoomId(roomId);
        long questionCount = questionRepository.countByRoomId(roomId);
        return participants > 0 && questionCount > 0 && totalAnswers >= questionCount * participants;
    }

    /**
     * 매치 완료 체크 및 완료 처리 (봇 플레이 완료 후 호출용)
     * @param roomId 방 ID
     * @return 매치가 완료되어 상태가 변경되었으면 true
     */
    @Transactional
    public boolean checkAndCompleteMatchIfNeeded(Long roomId) {
        MatchRoom room = findRoomOrThrow(roomId);
        
        if (room.getStatus() != MatchStatus.ONGOING) {
            return false; // 이미 완료되었거나 대기 중
        }
        
        long participants = participantRepository.countByRoomId(roomId);
        boolean matchCompleted = checkAllQuestionsAnswered(roomId, participants);
        
        if (matchCompleted) {
            log.info("봇 플레이 완료 후 매치 완료 체크: roomId={}, participants={}, status=DONE으로 변경", 
                    roomId, participants);
            
            // 스코어보드 계산
            VersusDtos.ScoreBoardResp scoreboard = computeScoreboard(room);
            
            // 매치 완료 처리
            room.setStatus(MatchStatus.DONE);
            roomRepository.save(room);
            
            // MATCH_FINISHED 이벤트 기록
            String winner = scoreboard.items().isEmpty() ? null : scoreboard.items().get(0).userId();
            recordEvent(room.getId(), "MATCH_FINISHED", Map.of(
                    "mode", room.getMode().name(),
                    "winner", winner != null ? winner : "N/A"
            ));
            
            // progress-service에 결과 통지 및 보상 지급
            notifyProgressService(room, scoreboard);
            
            log.info("매치 완료 처리 완료: roomId={}, status=DONE", roomId);
            return true;
        }
        
        return false;
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
        
        if (practical) {
            // 실기 모드: SHORT 10개 고정 (LONG 제거)
            long shortCount = questions.stream()
                    .filter(q -> Objects.equals(q.getTimeLimitSec(), DUEL_PRACTICAL_SHORT_LIMIT_SEC))
                    .count();
            long longCount = questions.stream()
                    .filter(q -> Objects.equals(q.getTimeLimitSec(), DUEL_PRACTICAL_LONG_LIMIT_SEC))
                    .count();
            
            if (shortCount != 10 || longCount != 0) {
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
                        "1:1 배틀(실기)은 SHORT 10개(" + DUEL_PRACTICAL_SHORT_LIMIT_SEC + "초)로 구성되어야 합니다. (현재: SHORT " + shortCount + "개, LONG " + longCount + "개)");
            }
        } else {
            // 필기 모드: OX 2개 + MCQ 8개
            long oxCount = questions.stream()
                    .filter(q -> Objects.equals(q.getTimeLimitSec(), DUEL_WRITTEN_OX_LIMIT_SEC))
                    .count();
            long mcqCount = questions.stream()
                    .filter(q -> Objects.equals(q.getTimeLimitSec(), DUEL_WRITTEN_MCQ_LIMIT_SEC))
                    .count();
            
            if (oxCount != 2 || mcqCount != 8) {
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
                        "1:1 배틀(필기)은 OX 2개(" + DUEL_WRITTEN_OX_LIMIT_SEC + "초)와 MCQ 8개(" 
                        + DUEL_WRITTEN_MCQ_LIMIT_SEC + "초)로 구성되어야 합니다. (현재: OX " + oxCount + "개, MCQ " + mcqCount + "개)");
            }
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
            
            if (practical && round == 3) {
                // 실기 모드 라운드 3: SHORT 1개 + LONG 2개 (총 3문제)
                if (roundQuestions.size() != TOURNAMENT_QUESTIONS_PER_ROUND) {
                    throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
                            "토너먼트 라운드 3(실기)는 3문제를 가져야 합니다. (현재: " + roundQuestions.size() + "개)");
                }
                long shortCount = roundQuestions.stream()
                        .filter(q -> Objects.equals(q.getTimeLimitSec(), DUEL_PRACTICAL_SHORT_LIMIT_SEC))
                        .count();
                long longCount = roundQuestions.stream()
                        .filter(q -> Objects.equals(q.getTimeLimitSec(), DUEL_PRACTICAL_LONG_LIMIT_SEC))
                        .count();
                
                if (shortCount != 1 || longCount != 2) {
                    throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
                            "토너먼트 라운드 3(실기)는 SHORT 1개(" + DUEL_PRACTICAL_SHORT_LIMIT_SEC + "초)와 LONG 2개(" 
                            + DUEL_PRACTICAL_LONG_LIMIT_SEC + "초)로 구성되어야 합니다. (현재: SHORT " + shortCount + "개, LONG " + longCount + "개)");
                }
            } else {
                // 필기 모드 또는 실기 모드 라운드 1, 2: 각 라운드 3문제
                if (roundQuestions.size() != TOURNAMENT_QUESTIONS_PER_ROUND) {
                    throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
                            "토너먼트 라운드 " + round + "는 3문제를 가져야 합니다.");
                }
                int expectedLimitSec = switch (round) {
                    case 1 -> practical ? DUEL_PRACTICAL_SHORT_LIMIT_SEC : DUEL_WRITTEN_OX_LIMIT_SEC;
                    case 2 -> practical ? DUEL_PRACTICAL_SHORT_LIMIT_SEC : DUEL_WRITTEN_MCQ_LIMIT_SEC;
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
    }

    private void validateGoldenbellQuestions(List<MatchQuestion> questions) {
        if (questions.size() < GOLDENBELL_REVIVE_CHECK_ROUND + 4) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "골든벨은 최소 8문제가 필요합니다.");
        }
        // 시간 제한 검증은 validateGoldenbellQuestionsWithRule에서 round_flow_json과 비교하여 수행
        // 여기서는 문제 수만 확인
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

    /**
     * 토너먼트 라운드 재경기: 1라운드에서 모두 0점일 때
     * 1. 해당 라운드의 모든 문제 삭제
     * 2. 해당 라운드의 모든 답안 삭제
     * 3. 같은 라운드 번호로 새 문제 3개 재출제
     */
    @Transactional
    private boolean retryTournamentRound(MatchRoom room, int roundNo) {
        Long roomId = room.getId();
        
        try {
            // 1. 해당 라운드의 모든 문제 삭제
            List<MatchQuestion> roundQuestions = questionRepository.findByRoomIdAndRoundNo(roomId, roundNo);
            if (roundQuestions.isEmpty()) {
                log.warn("토너먼트 재경기: 라운드 {}에 문제가 없음. roomId={}", roundNo, roomId);
                return false;
            }
            
            List<Long> questionIds = roundQuestions.stream()
                    .map(MatchQuestion::getQuestionId)
                    .toList();
            
            questionRepository.deleteAll(roundQuestions);
            log.info("토너먼트 라운드 {} 문제 삭제: roomId={}, questionIds={}", roundNo, roomId, questionIds);
            
            // 2. 해당 라운드의 모든 답안 삭제
            List<MatchAnswer> roundAnswers = answerRepository.findByRoomIdAndRoundNo(roomId, roundNo);
            if (!roundAnswers.isEmpty()) {
                answerRepository.deleteAll(roundAnswers);
                log.info("토너먼트 라운드 {} 답안 삭제: roomId={}, count={}", roundNo, roomId, roundAnswers.size());
            }
            
            // 3. scopeJson에서 examMode, topicId 추출
            Map<String, Object> scope = readScope(room.getScopeJson());
            String examMode = (String) scope.getOrDefault("examMode", "WRITTEN");
            Long topicId = scope.get("topicId") != null ? Long.valueOf(scope.get("topicId").toString()) : null;
            boolean isPractical = "PRACTICAL".equalsIgnoreCase(examMode);
            
            // 4. 라운드별 문제 타입 결정 및 새 문제 생성
            List<StudyServiceClient.QuestionDto> newQuestions;
            
            if (roundNo == 1) {
                // 1라운드: 필기면 OX 3문제, 실기면 SHORT 3문제
                String questionType = isPractical ? "SHORT" : "OX";
                StudyServiceClient.VersusQuestionRequest request = new StudyServiceClient.VersusQuestionRequest(
                        examMode,
                        topicId != null ? "SPECIFIC" : "ALL",
                        topicId,
                        "NORMAL",
                        3,
                        List.of(new StudyServiceClient.QuestionTypeSpec(questionType, 3))
                );
                newQuestions = studyServiceClient.generateVersusQuestions(request);
            } else if (roundNo == 2) {
                // 2라운드: 필기면 MCQ 3문제, 실기면 SHORT 3문제
                String questionType = isPractical ? "SHORT" : "MCQ";
                StudyServiceClient.VersusQuestionRequest request = new StudyServiceClient.VersusQuestionRequest(
                        examMode,
                        topicId != null ? "SPECIFIC" : "ALL",
                        topicId,
                        "NORMAL",
                        3,
                        List.of(new StudyServiceClient.QuestionTypeSpec(questionType, 3))
                );
                newQuestions = studyServiceClient.generateVersusQuestions(request);
            } else if (roundNo == 3) {
                // 3라운드: 필기면 MCQ 3문제 (HARD), 실기면 SHORT 1 + LONG 2
                if (isPractical) {
                    // 실기 3라운드: SHORT 1 + LONG 2
                    StudyServiceClient.VersusQuestionRequest requestShort = new StudyServiceClient.VersusQuestionRequest(
                            examMode,
                            topicId != null ? "SPECIFIC" : "ALL",
                            topicId,
                            "NORMAL",
                            1,
                            List.of(new StudyServiceClient.QuestionTypeSpec("SHORT", 1))
                    );
                    StudyServiceClient.VersusQuestionRequest requestLong = new StudyServiceClient.VersusQuestionRequest(
                            examMode,
                            topicId != null ? "SPECIFIC" : "ALL",
                            topicId,
                            "NORMAL",
                            2,
                            List.of(new StudyServiceClient.QuestionTypeSpec("LONG", 2))
                    );
                    List<StudyServiceClient.QuestionDto> shortQuestions = studyServiceClient.generateVersusQuestions(requestShort);
                    List<StudyServiceClient.QuestionDto> longQuestions = studyServiceClient.generateVersusQuestions(requestLong);
                    
                    newQuestions = new ArrayList<>();
                    newQuestions.addAll(shortQuestions);
                    newQuestions.addAll(longQuestions);
                } else {
                    // 필기 3라운드: MCQ 3문제 (HARD)
                    StudyServiceClient.VersusQuestionRequest request = new StudyServiceClient.VersusQuestionRequest(
                            examMode,
                            topicId != null ? "SPECIFIC" : "ALL",
                            topicId,
                            "HARD",
                            3,
                            List.of(new StudyServiceClient.QuestionTypeSpec("MCQ", 3))
                    );
                    newQuestions = studyServiceClient.generateVersusQuestions(request);
                }
            } else {
                log.error("토너먼트 재경기: 지원하지 않는 라운드 번호. roomId={}, roundNo={}", roomId, roundNo);
                return false;
            }
            
            if (newQuestions.size() < TOURNAMENT_QUESTIONS_PER_ROUND) {
                log.error("토너먼트 재경기: 문제 수 부족. roomId={}, roundNo={}, requested={}, actual={}", 
                        roomId, roundNo, TOURNAMENT_QUESTIONS_PER_ROUND, newQuestions.size());
                return false;
            }
            
            // 5. 새 문제 등록 (같은 라운드 번호, order 1, 2, 3)
            int orderNo = 1;
            for (StudyServiceClient.QuestionDto q : newQuestions.subList(0, TOURNAMENT_QUESTIONS_PER_ROUND)) {
                int timeLimit = determineTimeLimit(q.type(), q.mode(), MatchMode.TOURNAMENT);
                MatchQuestion newQuestion = MatchQuestion.builder()
                        .roomId(roomId)
                        .roundNo(roundNo)
                        .phase(MatchPhase.MAIN)
                        .orderNo(orderNo++)
                        .questionId(q.id())
                        .timeLimitSec(timeLimit)
                        .build();
                questionRepository.save(newQuestion);
            }
            
            log.info("토너먼트 라운드 {} 재경기 완료: roomId={}, 새 문제 수={}", roundNo, roomId, TOURNAMENT_QUESTIONS_PER_ROUND);
            
            // 6. 첫 번째 문제 시작 이벤트 기록
            Optional<MatchQuestion> firstQuestion = questionRepository.findByRoomIdAndRoundNo(roomId, roundNo).stream()
                    .min(Comparator.comparing(MatchQuestion::getOrderNo));
            if (firstQuestion.isPresent()) {
                MatchQuestion first = firstQuestion.get();
                recordEvent(roomId, "QUESTION_STARTED", Map.of(
                        "questionId", first.getQuestionId(),
                        "roundNo", first.getRoundNo(),
                        "phase", first.getPhase().name(),
                        "startedAt", Instant.now().toString(),
                        "allParticipants", true,
                        "retry", true
                ));
            }
            
            return true;
            
        } catch (Exception e) {
            log.error("토너먼트 라운드 재경기 중 오류 발생: roomId={}, roundNo={}", roomId, roundNo, e);
            return false;
        }
    }

    /**
     * 골든벨 전원 탈락 방지: aliveCount == 0일 때 문제 무효 처리 및 재출제
     * 1. 현재 문제 무효 처리 (삭제)
     * 2. 모든 참가자의 GoldenbellState를 alive=true로 롤백
     * 3. 해당 문제의 match_answer 삭제
     * 4. 같은 round/order/phase에서 새로운 문제 재출제
     */
    @Transactional
    private boolean retryQuestionOnFullElimination(MatchRoom room, MatchQuestion question) {
        Long roomId = room.getId();
        int roundNo = question.getRoundNo();
        int orderNo = question.getOrderNo();
        MatchPhase phase = question.getPhase();
        Long invalidQuestionId = question.getQuestionId();
        
        try {
            // 1. 현재 문제 무효 처리 (삭제)
            questionRepository.delete(question);
            log.info("골든벨 문제 무효 처리: roomId={}, questionId={}, round={}, order={}", 
                    roomId, invalidQuestionId, roundNo, orderNo);
            
            // 2. 해당 문제의 모든 답안 삭제
            List<MatchAnswer> answersToDelete = answerRepository.findByRoomIdAndQuestionId(roomId, invalidQuestionId);
            if (!answersToDelete.isEmpty()) {
                answerRepository.deleteAll(answersToDelete);
                log.info("골든벨 무효 문제의 답안 삭제: roomId={}, questionId={}, count={}", 
                        roomId, invalidQuestionId, answersToDelete.size());
            }
            
            // 3. 모든 참가자의 GoldenbellState를 alive=true로 롤백
            List<GoldenbellState> allStates = goldenbellStateRepository.findByRoomId(roomId);
            List<GoldenbellState> statesToRevive = allStates.stream()
                    .filter(state -> !state.isAlive())
                    .peek(state -> {
                        state.setAlive(true);
                        // 부활 플래그는 유지 (이미 부활한 사람은 그대로)
                    })
                    .toList();
            
            if (!statesToRevive.isEmpty()) {
                goldenbellStateRepository.saveAll(statesToRevive);
                log.info("골든벨 전원 부활: roomId={}, count={}", roomId, statesToRevive.size());
            }
            
            // 4. 같은 round/order/phase에서 새로운 문제 재출제
            // 문제 타입 결정 (phase, round, order에 따라)
            String questionType = determineQuestionTypeForRound(roundNo, phase, orderNo);
            if (questionType == null) {
                log.error("골든벨 문제 타입 결정 실패: roomId={}, round={}, phase={}, order={}", roomId, roundNo, phase, orderNo);
                return false;
            }
            
            // scopeJson에서 문제 생성 정보 가져오기
            Map<String, Object> scope = readScope(room.getScopeJson());
            String examMode = determineExamModeForQuestionType(questionType);
            String difficulty = (String) scope.getOrDefault("difficulty", "NORMAL");
            Long topicId = scope.get("topicId") != null ? Long.valueOf(scope.get("topicId").toString()) : null;
            
            // 새로운 문제 요청
            StudyServiceClient.VersusQuestionRequest request = new StudyServiceClient.VersusQuestionRequest(
                    examMode,
                    topicId != null ? "SPECIFIC" : "ALL",
                    topicId,
                    difficulty,
                    1, // 1개만 필요
                    List.of(new StudyServiceClient.QuestionTypeSpec(questionType, 1))
            );
            
            List<StudyServiceClient.QuestionDto> newQuestions = studyServiceClient.generateVersusQuestions(request);
            if (newQuestions.isEmpty()) {
                log.error("골든벨 문제 재출제 실패: 새로운 문제를 가져올 수 없음. roomId={}, type={}", roomId, questionType);
                return false;
            }
            
            StudyServiceClient.QuestionDto newQuestion = newQuestions.get(0);
            
            // 새로운 문제 등록
            MatchQuestion retryQuestion = MatchQuestion.builder()
                    .roomId(roomId)
                    .roundNo(roundNo)
                    .phase(phase)
                    .orderNo(orderNo)
                    .questionId(newQuestion.id())
                    .timeLimitSec(question.getTimeLimitSec()) // 기존 시간 제한 유지
                    .build();
            questionRepository.save(retryQuestion);
            
            log.info("골든벨 문제 재출제 완료: roomId={}, oldQuestionId={}, newQuestionId={}, round={}, order={}, phase={}", 
                    roomId, invalidQuestionId, newQuestion.id(), roundNo, orderNo, phase);
            
            // 이벤트 기록
            recordEvent(roomId, "QUESTION_RETRY_ON_FULL_ELIMINATION", Map.of(
                    "mode", "GOLDENBELL",
                    "oldQuestionId", invalidQuestionId,
                    "newQuestionId", newQuestion.id(),
                    "round", roundNo,
                    "order", orderNo,
                    "phase", phase.name(),
                    "reason", "ALL_PLAYERS_ELIMINATED"
            ));
            
            return true;
            
        } catch (Exception e) {
            log.error("골든벨 문제 재출제 중 오류 발생: roomId={}, questionId={}", roomId, invalidQuestionId, e);
            return false;
        }
    }
    
    /**
     * 라운드, 페이즈, order에 따라 문제 타입 결정
     */
    private String determineQuestionTypeForRound(int roundNo, MatchPhase phase, int orderNo) {
        // 골든벨 구성: 라운드 1(OX 2), 라운드 2(MCQ 2), 라운드 3(MCQ 1 REVIVAL), 라운드 4(SHORT 1 + LONG 1 FINAL)
        if (phase == MatchPhase.REVIVAL) {
            return "MCQ"; // 라운드 3 부활전
        } else if (phase == MatchPhase.FINAL) {
            // 라운드 4 FINAL: order 1은 SHORT, order 2는 LONG
            return orderNo == 1 ? "SHORT" : "LONG";
        } else {
            // MAIN 페이즈
            if (roundNo == 1) {
                return "OX";
            } else if (roundNo == 2) {
                return "MCQ";
            }
        }
        return null;
    }
    
    /**
     * 문제 타입에 따라 examMode 결정
     */
    private String determineExamModeForQuestionType(String questionType) {
        // SHORT, LONG은 PRACTICAL, 나머지는 WRITTEN
        if ("SHORT".equals(questionType) || "LONG".equals(questionType)) {
            return "PRACTICAL";
        }
        return "WRITTEN";
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
                  {"round":5,"type":"SHORT","limitSec":30,"phase":"MAIN"},
                  {"round":6,"type":"LONG","limitSec":30,"phase":"MAIN"},
                  {"round":7,"type":"SHORT","limitSec":30,"phase":"FINAL"},
                  {"round":8,"type":"LONG","limitSec":30,"phase":"FINAL"}
                ]
                """;
    }

    private ScoreOutcome evaluateScore(MatchQuestion question,
                                       boolean requestedCorrect,
                                       Integer requestedTimeMs) {
        int limitMs = Math.max(1, Optional.ofNullable(question.getTimeLimitSec()).orElse(GOLDENBELL_TIME_LIMIT_SEC)) * 1000;
        int timeMs = requestedTimeMs == null ? limitMs : Math.max(0, requestedTimeMs);
        // 시간 초과 시 정답도 오답 처리 (시간 초과 = 제출 불가 = 오답)
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
            log.debug("generateQuestionsFromScope 시작: roomId={}, scopeJson={}", room.getId(), scopeJson);
            
            Map<String, Object> scope = readScope(scopeJson);
            String examMode = (String) scope.getOrDefault("examMode", "WRITTEN");
            String difficulty = (String) scope.getOrDefault("difficulty", "NORMAL");
            String topicScope = (String) scope.getOrDefault("topicScope", "ALL");
            Long topicId = scope.get("topicId") != null ? Long.valueOf(scope.get("topicId").toString()) : null;

            log.info("Parsed scope: roomId={}, examMode={}, difficulty={}, topicScope={}, topicId={}", 
                    room.getId(), examMode, difficulty, topicScope, topicId);

            // topicScope가 "CATEGORY"인 경우 "SPECIFIC"로 변환 (둘 다 특정 topicId에서 문제를 가져옴)
            if ("CATEGORY".equalsIgnoreCase(topicScope)) {
                topicScope = "SPECIFIC";
                log.info("topicScope 'CATEGORY'를 'SPECIFIC'로 변환: roomId={}, topicId={}", room.getId(), topicId);
            }

            List<StudyServiceClient.QuestionDto> questions;
            // topicScope가 "ALL"이 아니고 topicId가 있으면 "SPECIFIC"로 설정
            String finalTopicScope = (topicId != null && !"ALL".equalsIgnoreCase(topicScope)) 
                    ? "SPECIFIC" 
                    : (topicId != null ? "SPECIFIC" : "ALL");
            
            if (room.getMode() == MatchMode.TOURNAMENT) {
                // 토너먼트는 라운드별로 다른 난이도/타입 요청 필요
                questions = generateTournamentQuestions(examMode, topicId);
            } else if (room.getMode() == MatchMode.GOLDENBELL) {
                // 골든벨은 WRITTEN(OX, MCQ)과 PRACTICAL(SHORT, LONG) 모두에서 가져오기
                questions = generateGoldenbellQuestions(examMode, topicId, difficulty);
            } else {
                // 모드별 문제 구성 결정
                List<StudyServiceClient.QuestionTypeSpec> questionTypes = determineQuestionTypes(room.getMode(), examMode);
                int totalCount = getTotalQuestionCount(room.getMode());
                
                StudyServiceClient.VersusQuestionRequest request = new StudyServiceClient.VersusQuestionRequest(
                        examMode,
                        finalTopicScope,
                        topicId,
                        difficulty,
                        totalCount,
                        questionTypes
                );

                log.info("Requesting {} questions from study-service for room {}: mode={}, examMode={}, topicScope={}, topicId={}, difficulty={}",
                        totalCount, room.getId(), room.getMode(), examMode, finalTopicScope, topicId, difficulty);

                try {
                    log.info("study-service 호출 시도: roomId={}, request={}", room.getId(), 
                            String.format("examMode=%s, topicScope=%s, topicId=%s, difficulty=%s, count=%d, questionTypes=%s",
                                    request.examMode(), request.topicScope(), request.topicId(), request.difficulty(), 
                                    request.count(), request.questionTypes()));
                    questions = studyServiceClient.generateVersusQuestions(request);
                    log.info("study-service 호출 성공: roomId={}, questionsCount={}", room.getId(), questions.size());
                } catch (Exception e) {
                    log.error("study-service 호출 실패: roomId={}, examMode={}, topicScope={}, topicId={}, difficulty={}, count={}, questionTypes={}, error={}, class={}", 
                            room.getId(), examMode, finalTopicScope, topicId, difficulty, request.count(), request.questionTypes(),
                            e.getMessage(), e.getClass().getName(), e);
                    throw e;
                }
            }

            if (questions.isEmpty()) {
                monitoringConfig.recordTimer(timer, "generateQuestionsFromScope", "status", "not_found");
                log.error("study-service에서 문제를 찾을 수 없음: roomId={}, examMode={}, topicScope={}, topicId={}, difficulty={}", 
                        room.getId(), examMode, finalTopicScope, topicId, difficulty);
                throw new ResponseStatusException(
                        HttpStatus.NOT_FOUND,
                        "No questions available for the given scope. Please check examMode, difficulty, and topicId."
                );
            }

            monitoringConfig.recordTimer(timer, "generateQuestionsFromScope", "status", "success");
            String questionTypes = questions.stream()
                    .map(q -> q.type())
                    .collect(java.util.stream.Collectors.joining(", "));
            log.info("Generated {} questions from study-service for room {}: types={}", 
                    questions.size(), room.getId(), questionTypes);

            // MatchQuestion 형태로 변환
            List<VersusDtos.QuestionInfo> result = convertToQuestionInfos(questions, room.getMode());
            
            // 골든벨의 경우 문제 수 확인 및 경고
            if (room.getMode() == MatchMode.GOLDENBELL && result.size() < 7) {
                log.warn("골든벨 문제 수 부족: roomId={}, 요구={}, 실제={}, study-service에서 받은 문제={}", 
                        room.getId(), 7, result.size(), questions.size());
            }
            
            return result;
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
                // 필기: OX 먼저, 그 다음 객관식 (순서 보장)
                return List.of(
                        new StudyServiceClient.QuestionTypeSpec("OX", 2),
                        new StudyServiceClient.QuestionTypeSpec("MCQ", 8)
                );
            } else {
                // 실기: SHORT 10개 고정 (LONG 제거)
                return List.of(
                        new StudyServiceClient.QuestionTypeSpec("SHORT", 10)
                );
            }
        } else if (matchMode == MatchMode.TOURNAMENT) {
            // 토너먼트는 generateTournamentQuestions에서 별도 처리
            return List.of(); // 사용 안 함
        } else { // GOLDENBELL
            // 골든벨은 generateGoldenbellQuestions에서 WRITTEN과 PRACTICAL을 분리해서 가져오므로
            // 이 메서드는 사용되지 않지만, 더미 문제 생성 시 참고용으로 유지
            return List.of(
                    new StudyServiceClient.QuestionTypeSpec("OX", 2),      // 라운드 1: OX 2문제
                    new StudyServiceClient.QuestionTypeSpec("MCQ", 3),    // 라운드 2: MCQ 2문제 + 라운드 3 부활전: MCQ 1문제
                    new StudyServiceClient.QuestionTypeSpec("SHORT", 1),  // 라운드 4 FINAL: SHORT 1문제
                    new StudyServiceClient.QuestionTypeSpec("LONG", 1)   // 라운드 4 FINAL: LONG 1문제
            );
        }
    }

    private int getTotalQuestionCount(MatchMode mode) {
        return switch (mode) {
            case DUEL -> DUEL_TOTAL_QUESTIONS;
            case TOURNAMENT -> TOURNAMENT_ROUNDS * TOURNAMENT_QUESTIONS_PER_ROUND;
            case GOLDENBELL -> 7; // 필기: OX2 + MCQ5(라운드2에2개+부활전1개+FINAL2개) = 7문제
                                  // 실기: SHORT7(라운드1에2개+라운드2에2개+부활전1개+FINAL2개) = 7문제
        };
    }

    private List<VersusDtos.QuestionInfo> convertToQuestionInfos(
            List<StudyServiceClient.QuestionDto> questions, MatchMode mode) {
        if (mode == MatchMode.TOURNAMENT) {
            return convertToQuestionInfosForTournament(questions);
        }
        if (mode == MatchMode.GOLDENBELL) {
            return convertToQuestionInfosForGoldenbell(questions);
        }
        
        List<VersusDtos.QuestionInfo> result = new ArrayList<>();
        int roundNo = 1;
        int orderNo = 1;

        // DUEL 모드 필기: OX 먼저, 그 다음 MCQ 순서 보장
        List<StudyServiceClient.QuestionDto> sortedQuestions = questions;
        if (mode == MatchMode.DUEL && !questions.isEmpty()) {
            // 첫 번째 문제의 examMode 확인
            String examMode = questions.get(0).mode();
            if ("WRITTEN".equals(examMode)) {
                // OX 타입을 먼저, 그 다음 MCQ 타입 순서로 정렬
                sortedQuestions = new ArrayList<>(questions);
                sortedQuestions.sort((a, b) -> {
                    boolean aIsOx = "OX".equals(a.type());
                    boolean bIsOx = "OX".equals(b.type());
                    if (aIsOx && !bIsOx) return -1; // OX가 먼저
                    if (!aIsOx && bIsOx) return 1;  // OX가 먼저
                    return 0; // 같은 타입이면 원래 순서 유지
                });
            }
        }

        for (StudyServiceClient.QuestionDto q : sortedQuestions) {
            MatchPhase phase = determinePhase(q.type(), mode);
            int timeLimit = determineTimeLimit(q.type(), q.mode(), mode);

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

    /**
     * 골든벨 모드 전용: 라운드별 문제 타입 구성
     * - 라운드 1: OX 2문제 (order 1, 2)
     * - 라운드 2: MCQ 2문제 (order 1, 2)
     * - 라운드 3: MCQ 1문제 (REVIVAL) - 패자부활전
     * - 라운드 4: SHORT 1문제 + LONG 1문제 (FINAL, order 1, 2)
     */
    private List<VersusDtos.QuestionInfo> convertToQuestionInfosForGoldenbell(
            List<StudyServiceClient.QuestionDto> questions) {
        List<VersusDtos.QuestionInfo> result = new ArrayList<>();
        
        // 문제를 타입별로 분류
        List<StudyServiceClient.QuestionDto> oxQuestions = new ArrayList<>();
        List<StudyServiceClient.QuestionDto> mcqQuestions = new ArrayList<>();
        List<StudyServiceClient.QuestionDto> shortQuestions = new ArrayList<>();
        
        // examMode 확인 (첫 번째 문제의 mode 사용)
        String examMode = questions.isEmpty() ? "WRITTEN" : questions.get(0).mode();
        boolean isPractical = "PRACTICAL".equals(examMode);
        
        for (StudyServiceClient.QuestionDto q : questions) {
            switch (q.type()) {
                case "OX" -> oxQuestions.add(q);
                case "MCQ" -> mcqQuestions.add(q);
                case "SHORT" -> shortQuestions.add(q);
                // LONG은 실기 골든벨에서 사용 안 함
            }
        }
        
        if (isPractical) {
            // 실기 골든벨: SHORT만 사용
            // 라운드 1: SHORT 2문제 (order 1, 2) - 25초
            if (shortQuestions.size() >= 2) {
                result.add(new VersusDtos.QuestionInfo(
                        shortQuestions.get(0).id(),
                        1,
                        MatchPhase.MAIN,
                        1,
                        25
                ));
                result.add(new VersusDtos.QuestionInfo(
                        shortQuestions.get(1).id(),
                        1,
                        MatchPhase.MAIN,
                        2,
                        25
                ));
            }
            
            // 라운드 2: SHORT 2문제 (order 1, 2, 난이도 ↑) - 25초
            if (shortQuestions.size() >= 4) {
                result.add(new VersusDtos.QuestionInfo(
                        shortQuestions.get(2).id(),
                        2,
                        MatchPhase.MAIN,
                        1,
                        25
                ));
                result.add(new VersusDtos.QuestionInfo(
                        shortQuestions.get(3).id(),
                        2,
                        MatchPhase.MAIN,
                        2,
                        25
                ));
            }
            
            // 라운드 3: SHORT 1문제 (REVIVAL) - 패자부활전 - 25초
            if (shortQuestions.size() >= 5) {
                result.add(new VersusDtos.QuestionInfo(
                        shortQuestions.get(4).id(),
                        3,
                        MatchPhase.REVIVAL,
                        1,
                        25
                ));
            } else {
                log.warn("골든벨 실기 라운드 3(부활전) SHORT 문제가 없습니다. SHORT 문제 수={}", 
                        shortQuestions.size());
            }
            
            // 라운드 4: SHORT 2문제 (FINAL, order 1, 2, 난이도 ↑) - 25초
            if (shortQuestions.size() >= 7) {
                result.add(new VersusDtos.QuestionInfo(
                        shortQuestions.get(5).id(),
                        4,
                        MatchPhase.FINAL,
                        1,
                        25
                ));
                result.add(new VersusDtos.QuestionInfo(
                        shortQuestions.get(6).id(),
                        4,
                        MatchPhase.FINAL,
                        2,
                        25
                ));
            } else {
                log.warn("골든벨 실기 라운드 4(FINAL) SHORT 문제가 부족합니다. SHORT 문제 수={}", 
                        shortQuestions.size());
            }
            
            log.info("골든벨 실기 문제 변환 완료: SHORT={}, 결과={}문제",
                    shortQuestions.size(), result.size());
        } else {
            // 필기 골든벨: OX, MCQ 사용
            // 라운드 1: OX 2문제 (order 1, 2) - 8초
            if (oxQuestions.size() >= 2) {
                result.add(new VersusDtos.QuestionInfo(
                        oxQuestions.get(0).id(),
                        1,
                        MatchPhase.MAIN,
                        1,
                        8
                ));
                result.add(new VersusDtos.QuestionInfo(
                        oxQuestions.get(1).id(),
                        1,
                        MatchPhase.MAIN,
                        2,
                        8
                ));
            }
            
            // 라운드 2: MCQ 2문제 (order 1, 2) - 12초
            if (mcqQuestions.size() >= 2) {
                result.add(new VersusDtos.QuestionInfo(
                        mcqQuestions.get(0).id(),
                        2,
                        MatchPhase.MAIN,
                        1,
                        12
                ));
                result.add(new VersusDtos.QuestionInfo(
                        mcqQuestions.get(1).id(),
                        2,
                        MatchPhase.MAIN,
                        2,
                        12
                ));
            }
            
            // 라운드 3: MCQ 1문제 (REVIVAL) - 패자부활전 - 15초
            if (mcqQuestions.size() >= 3) {
                result.add(new VersusDtos.QuestionInfo(
                        mcqQuestions.get(2).id(),
                        3,
                        MatchPhase.REVIVAL,
                        1,
                        15
                ));
            } else {
                log.warn("골든벨 필기 라운드 3(부활전) MCQ 문제가 없습니다. MCQ 문제 수={}", 
                        mcqQuestions.size());
            }
            
            // 라운드 4: MCQ 2문제 (HARD, FINAL, order 1, 2) - 12초
            if (mcqQuestions.size() >= 5) {
                result.add(new VersusDtos.QuestionInfo(
                        mcqQuestions.get(3).id(),
                        4,
                        MatchPhase.FINAL,
                        1,
                        12
                ));
                result.add(new VersusDtos.QuestionInfo(
                        mcqQuestions.get(4).id(),
                        4,
                        MatchPhase.FINAL,
                        2,
                        12
                ));
            } else {
                log.warn("골든벨 필기 라운드 4(FINAL) MCQ 문제가 부족합니다. MCQ 문제 수={}", 
                        mcqQuestions.size());
            }
            
            log.info("골든벨 필기 문제 변환 완료: OX={}, MCQ={}, 결과={}문제",
                    oxQuestions.size(), mcqQuestions.size(), result.size());
        }
        
        return result;
    }

    /**
     * 토너먼트 모드 전용: 라운드별 문제 타입 구성
     * 필기 모드:
     * - 1R: OX 3문제
     * - 2R: MCQ 3문제 (NORMAL)
     * - 3R: MCQ 3문제 (HARD)
     * 실기 모드:
     * - 1R: SHORT 3문제
     * - 2R: SHORT 3문제
     * - 3R: SHORT 1문제 + LONG 2문제
     */
    private List<VersusDtos.QuestionInfo> convertToQuestionInfosForTournament(
            List<StudyServiceClient.QuestionDto> questions) {
        List<VersusDtos.QuestionInfo> result = new ArrayList<>();
        
        if (questions.isEmpty()) {
            return result;
        }
        
        // 첫 번째 문제의 examMode로 필기/실기 판단
        String examMode = questions.get(0).mode();
        boolean isPractical = "PRACTICAL".equalsIgnoreCase(examMode);
        
        if (isPractical) {
            // 실기 모드
            List<StudyServiceClient.QuestionDto> shortQuestions = new ArrayList<>();
            List<StudyServiceClient.QuestionDto> longQuestions = new ArrayList<>();
            
            for (StudyServiceClient.QuestionDto q : questions) {
                if ("SHORT".equals(q.type())) {
                    shortQuestions.add(q);
                } else if ("LONG".equals(q.type())) {
                    longQuestions.add(q);
                }
            }
            
            // 1R: SHORT 3문제
            int roundNo = 1;
            int orderNo = 1;
            for (int i = 0; i < Math.min(3, shortQuestions.size()); i++) {
                StudyServiceClient.QuestionDto q = shortQuestions.get(i);
                int timeLimit = determineTimeLimit(q.type(), q.mode(), MatchMode.TOURNAMENT);
                result.add(new VersusDtos.QuestionInfo(
                        q.id(),
                        roundNo,
                        MatchPhase.MAIN,
                        orderNo++,
                        timeLimit
                ));
            }
            
            // 2R: SHORT 3문제
            roundNo = 2;
            orderNo = 1;
            for (int i = 3; i < Math.min(6, shortQuestions.size()); i++) {
                StudyServiceClient.QuestionDto q = shortQuestions.get(i);
                int timeLimit = determineTimeLimit(q.type(), q.mode(), MatchMode.TOURNAMENT);
                result.add(new VersusDtos.QuestionInfo(
                        q.id(),
                        roundNo,
                        MatchPhase.MAIN,
                        orderNo++,
                        timeLimit
                ));
            }
            
            // 3R: SHORT 1문제 + LONG 2문제
            roundNo = 3;
            orderNo = 1;
            // SHORT 1문제
            if (shortQuestions.size() >= 7) {
                StudyServiceClient.QuestionDto q = shortQuestions.get(6);
                int timeLimit = determineTimeLimit(q.type(), q.mode(), MatchMode.TOURNAMENT);
                result.add(new VersusDtos.QuestionInfo(
                        q.id(),
                        roundNo,
                        MatchPhase.MAIN,
                        orderNo++,
                        timeLimit
                ));
            }
            // LONG 2문제
            for (int i = 0; i < Math.min(2, longQuestions.size()); i++) {
                StudyServiceClient.QuestionDto q = longQuestions.get(i);
                int timeLimit = determineTimeLimit(q.type(), q.mode(), MatchMode.TOURNAMENT);
                result.add(new VersusDtos.QuestionInfo(
                        q.id(),
                        roundNo,
                        MatchPhase.MAIN,
                        orderNo++,
                        timeLimit
                ));
            }
        } else {
            // 필기 모드
            List<StudyServiceClient.QuestionDto> oxQuestions = new ArrayList<>();
            List<StudyServiceClient.QuestionDto> mcqNormalQuestions = new ArrayList<>();
            List<StudyServiceClient.QuestionDto> mcqHardQuestions = new ArrayList<>();
            
            for (StudyServiceClient.QuestionDto q : questions) {
                if ("OX".equals(q.type())) {
                    oxQuestions.add(q);
                } else if ("MCQ".equals(q.type())) {
                    // 난이도에 따라 분류
                    if ("HARD".equalsIgnoreCase(q.difficulty())) {
                        mcqHardQuestions.add(q);
                    } else {
                        mcqNormalQuestions.add(q);
                    }
                }
            }
            
            // 1R: OX 3문제
            int roundNo = 1;
            int orderNo = 1;
            for (int i = 0; i < Math.min(3, oxQuestions.size()); i++) {
                StudyServiceClient.QuestionDto q = oxQuestions.get(i);
                int timeLimit = determineTimeLimit(q.type(), q.mode(), MatchMode.TOURNAMENT);
                result.add(new VersusDtos.QuestionInfo(
                        q.id(),
                        roundNo,
                        MatchPhase.MAIN,
                        orderNo++,
                        timeLimit
                ));
            }
            
            // 2R: MCQ 3문제 (NORMAL)
            roundNo = 2;
            orderNo = 1;
            for (int i = 0; i < Math.min(3, mcqNormalQuestions.size()); i++) {
                StudyServiceClient.QuestionDto q = mcqNormalQuestions.get(i);
                int timeLimit = determineTimeLimit(q.type(), q.mode(), MatchMode.TOURNAMENT);
                result.add(new VersusDtos.QuestionInfo(
                        q.id(),
                        roundNo,
                        MatchPhase.MAIN,
                        orderNo++,
                        timeLimit
                ));
            }
            
            // 3R: MCQ 3문제 (HARD) - HARD가 부족하면 NORMAL로 보충
            roundNo = 3;
            orderNo = 1;
            int hardCount = 0;
            for (int i = 0; i < Math.min(3, mcqHardQuestions.size()); i++) {
                StudyServiceClient.QuestionDto q = mcqHardQuestions.get(i);
                int timeLimit = determineTimeLimit(q.type(), q.mode(), MatchMode.TOURNAMENT);
                result.add(new VersusDtos.QuestionInfo(
                        q.id(),
                        roundNo,
                        MatchPhase.MAIN,
                        orderNo++,
                        timeLimit
                ));
                hardCount++;
            }
            
            // HARD가 부족하면 NORMAL로 보충
            int normalIndex = 3; // 이미 2R에서 3개 사용
            while (hardCount < 3 && normalIndex < mcqNormalQuestions.size()) {
                StudyServiceClient.QuestionDto q = mcqNormalQuestions.get(normalIndex++);
                int timeLimit = determineTimeLimit(q.type(), q.mode(), MatchMode.TOURNAMENT);
                result.add(new VersusDtos.QuestionInfo(
                        q.id(),
                        roundNo,
                        MatchPhase.MAIN,
                        orderNo++,
                        timeLimit
                ));
                hardCount++;
            }
        }
        
        return result;
    }

    /**
     * 골든벨 모드 전용: WRITTEN과 PRACTICAL에서 문제 가져오기
     * - WRITTEN: OX 2문제, MCQ 3문제 (라운드 2에 2개 + 라운드 3 부활전에 1개)
     * - PRACTICAL: SHORT 1문제, LONG 1문제 (라운드 4 FINAL용)
     */
    private List<StudyServiceClient.QuestionDto> generateGoldenbellQuestions(
            String examMode, Long topicId, String difficulty) {
        List<StudyServiceClient.QuestionDto> allQuestions = new ArrayList<>();
        
        if ("PRACTICAL".equals(examMode)) {
            // 실기 골든벨: SHORT만 사용 (7문제)
            // 라운드 1: SHORT 2개
            // 라운드 2: SHORT 2개 (난이도 ↑)
            // 라운드 3: SHORT 1개 (REVIVAL)
            // 라운드 4: SHORT 2개 (FINAL, 난이도 ↑)
            try {
                // NORMAL 난이도로 먼저 시도
                StudyServiceClient.VersusQuestionRequest practicalRequest = new StudyServiceClient.VersusQuestionRequest(
                        "PRACTICAL",
                        topicId != null ? "SPECIFIC" : "ALL",
                        topicId,
                        "NORMAL",
                        7, // SHORT 7문제
                        List.of(
                                new StudyServiceClient.QuestionTypeSpec("SHORT", 7)
                        )
                );
                
                log.info("골든벨 실기 문제 요청: SHORT 7개 (NORMAL)");
                List<StudyServiceClient.QuestionDto> practicalQuestions = studyServiceClient.generateVersusQuestions(practicalRequest);
                allQuestions.addAll(practicalQuestions);
                log.info("골든벨 실기 문제 수신: {}개", practicalQuestions.size());
                
            } catch (FeignException.NotFound | FeignException.BadRequest e) {
                log.warn("골든벨 실기 문제 요청 실패 (NORMAL): {}", e.getMessage());
            } catch (Exception e) {
                log.error("골든벨 실기 문제 요청 중 오류: {}", e.getMessage(), e);
            }
        } else {
            // 필기 골든벨: OX, MCQ 사용 (7문제)
            // 라운드 1: OX 2개
            // 라운드 2: MCQ 2개
            // 라운드 3: MCQ 1개 (REVIVAL)
            // 라운드 4: MCQ 2개 (HARD, FINAL)
            try {
                // OX 2개 + MCQ 2개 (NORMAL) + MCQ 1개 (REVIVAL, NORMAL) + MCQ 2개 (HARD, FINAL)
                // 먼저 OX와 NORMAL MCQ 요청
                StudyServiceClient.VersusQuestionRequest writtenRequest1 = new StudyServiceClient.VersusQuestionRequest(
                        "WRITTEN",
                        topicId != null ? "SPECIFIC" : "ALL",
                        topicId,
                        "NORMAL",
                        4, // OX 2 + MCQ 2
                        List.of(
                                new StudyServiceClient.QuestionTypeSpec("OX", 2),
                                new StudyServiceClient.QuestionTypeSpec("MCQ", 2)
                        )
                );
                
                log.info("골든벨 필기 문제 요청 (1차): OX 2, MCQ 2 (NORMAL)");
                List<StudyServiceClient.QuestionDto> writtenQuestions1 = studyServiceClient.generateVersusQuestions(writtenRequest1);
                allQuestions.addAll(writtenQuestions1);
                log.info("골든벨 필기 문제 수신 (1차): {}개", writtenQuestions1.size());
                
                // REVIVAL용 MCQ 1개 (NORMAL)
                StudyServiceClient.VersusQuestionRequest writtenRequest2 = new StudyServiceClient.VersusQuestionRequest(
                        "WRITTEN",
                        topicId != null ? "SPECIFIC" : "ALL",
                        topicId,
                        "NORMAL",
                        1, // MCQ 1개 (REVIVAL)
                        List.of(
                                new StudyServiceClient.QuestionTypeSpec("MCQ", 1)
                        )
                );
                
                log.info("골든벨 필기 문제 요청 (2차): MCQ 1 (REVIVAL, NORMAL)");
                List<StudyServiceClient.QuestionDto> writtenQuestions2 = studyServiceClient.generateVersusQuestions(writtenRequest2);
                allQuestions.addAll(writtenQuestions2);
                log.info("골든벨 필기 문제 수신 (2차): {}개", writtenQuestions2.size());
                
                // FINAL용 MCQ 2개 (HARD) - 실패 시 NORMAL로 폴백
                try {
                    StudyServiceClient.VersusQuestionRequest writtenRequest3 = new StudyServiceClient.VersusQuestionRequest(
                            "WRITTEN",
                            topicId != null ? "SPECIFIC" : "ALL",
                            topicId,
                            "HARD",
                            2, // MCQ 2개 (FINAL)
                            List.of(
                                    new StudyServiceClient.QuestionTypeSpec("MCQ", 2)
                            )
                    );
                    
                    log.info("골든벨 필기 문제 요청 (3차): MCQ 2 (FINAL, HARD)");
                    List<StudyServiceClient.QuestionDto> writtenQuestions3 = studyServiceClient.generateVersusQuestions(writtenRequest3);
                    allQuestions.addAll(writtenQuestions3);
                    log.info("골든벨 필기 문제 수신 (3차): {}개", writtenQuestions3.size());
                } catch (FeignException.NotFound | FeignException.BadRequest e) {
                    log.warn("골든벨 필기 HARD 문제 요청 실패, NORMAL로 폴백: {}", e.getMessage());
                    StudyServiceClient.VersusQuestionRequest writtenRequest3Fallback = new StudyServiceClient.VersusQuestionRequest(
                            "WRITTEN",
                            topicId != null ? "SPECIFIC" : "ALL",
                            topicId,
                            "NORMAL",
                            2, // MCQ 2개 (FINAL, NORMAL)
                            List.of(
                                    new StudyServiceClient.QuestionTypeSpec("MCQ", 2)
                            )
                    );
                    List<StudyServiceClient.QuestionDto> writtenQuestions3Fallback = studyServiceClient.generateVersusQuestions(writtenRequest3Fallback);
                    allQuestions.addAll(writtenQuestions3Fallback);
                    log.info("골든벨 필기 문제 수신 (3차 폴백): {}개", writtenQuestions3Fallback.size());
                }
                
            } catch (FeignException.NotFound | FeignException.BadRequest e) {
                log.warn("골든벨 필기 문제 요청 실패: {}", e.getMessage());
            } catch (Exception e) {
                log.error("골든벨 필기 문제 요청 중 오류: {}", e.getMessage(), e);
            }
        }
        
        if (allQuestions.isEmpty()) {
            throw new ResponseStatusException(
                    HttpStatus.NOT_FOUND,
                    String.format("No questions available for Golden Bell (%s mode). Please check if questions exist.", examMode)
            );
        }
        
        log.info("골든벨 전체 문제 수집 완료: mode={}, 총 {}개", examMode, allQuestions.size());
        return allQuestions;
    }

    /**
     * 토너먼트 모드 전용: 라운드별로 문제 요청
     * - 1R: OX 3문제 (NORMAL)
     * - 2R: MCQ 3문제 (NORMAL)
     * - 3R: MCQ 3문제 (HARD)
     */
    /**
     * 토너먼트 모드 전용: 라운드별로 문제 요청
     * 필기 모드:
     * - 1R: OX 3문제 (NORMAL)
     * - 2R: MCQ 3문제 (NORMAL)
     * - 3R: MCQ 3문제 (HARD)
     * 실기 모드:
     * - 1R: SHORT 3문제 (NORMAL)
     * - 2R: SHORT 3문제 (NORMAL)
     * - 3R: SHORT 1문제 + LONG 2문제 (NORMAL)
     */
    private List<StudyServiceClient.QuestionDto> generateTournamentQuestions(String examMode, Long topicId) {
        List<StudyServiceClient.QuestionDto> allQuestions = new ArrayList<>();
        boolean isPractical = "PRACTICAL".equalsIgnoreCase(examMode);
        
        if (isPractical) {
            // 실기 모드
            // 1R: SHORT 3문제 (NORMAL)
            StudyServiceClient.VersusQuestionRequest request1R = new StudyServiceClient.VersusQuestionRequest(
                    examMode,
                    topicId != null ? "SPECIFIC" : "ALL",
                    topicId,
                    "NORMAL",
                    3,
                    List.of(new StudyServiceClient.QuestionTypeSpec("SHORT", 3))
            );
            List<StudyServiceClient.QuestionDto> round1Questions = studyServiceClient.generateVersusQuestions(request1R);
            allQuestions.addAll(round1Questions);
            
            // 2R: SHORT 3문제 (NORMAL)
            StudyServiceClient.VersusQuestionRequest request2R = new StudyServiceClient.VersusQuestionRequest(
                    examMode,
                    topicId != null ? "SPECIFIC" : "ALL",
                    topicId,
                    "NORMAL",
                    3,
                    List.of(new StudyServiceClient.QuestionTypeSpec("SHORT", 3))
            );
            List<StudyServiceClient.QuestionDto> round2Questions = studyServiceClient.generateVersusQuestions(request2R);
            allQuestions.addAll(round2Questions);
            
            // 3R: SHORT 1문제 + LONG 2문제 (NORMAL)
            StudyServiceClient.VersusQuestionRequest request3RShort = new StudyServiceClient.VersusQuestionRequest(
                    examMode,
                    topicId != null ? "SPECIFIC" : "ALL",
                    topicId,
                    "NORMAL",
                    1,
                    List.of(new StudyServiceClient.QuestionTypeSpec("SHORT", 1))
            );
            List<StudyServiceClient.QuestionDto> round3ShortQuestions = studyServiceClient.generateVersusQuestions(request3RShort);
            allQuestions.addAll(round3ShortQuestions);
            
            StudyServiceClient.VersusQuestionRequest request3RLong = new StudyServiceClient.VersusQuestionRequest(
                    examMode,
                    topicId != null ? "SPECIFIC" : "ALL",
                    topicId,
                    "NORMAL",
                    2,
                    List.of(new StudyServiceClient.QuestionTypeSpec("LONG", 2))
            );
            List<StudyServiceClient.QuestionDto> round3LongQuestions = studyServiceClient.generateVersusQuestions(request3RLong);
            allQuestions.addAll(round3LongQuestions);
        } else {
            // 필기 모드
            // 1R: OX 3문제 (NORMAL)
        StudyServiceClient.VersusQuestionRequest request1R = new StudyServiceClient.VersusQuestionRequest(
                examMode,
                topicId != null ? "SPECIFIC" : "ALL",
                topicId,
                "NORMAL",
                3,
                List.of(new StudyServiceClient.QuestionTypeSpec("OX", 3))
        );
        List<StudyServiceClient.QuestionDto> round1Questions = studyServiceClient.generateVersusQuestions(request1R);
        allQuestions.addAll(round1Questions);
        
        // 2R: MCQ 3문제 (NORMAL)
        StudyServiceClient.VersusQuestionRequest request2R = new StudyServiceClient.VersusQuestionRequest(
                examMode,
                topicId != null ? "SPECIFIC" : "ALL",
                topicId,
                "NORMAL",
                3,
                List.of(new StudyServiceClient.QuestionTypeSpec("MCQ", 3))
        );
        List<StudyServiceClient.QuestionDto> round2Questions = studyServiceClient.generateVersusQuestions(request2R);
        allQuestions.addAll(round2Questions);
        
        // 3R: MCQ 3문제 (HARD) - 실패 시 NORMAL로 폴백
        List<StudyServiceClient.QuestionDto> round3Questions = new ArrayList<>();
        try {
            StudyServiceClient.VersusQuestionRequest request3R = new StudyServiceClient.VersusQuestionRequest(
                    examMode,
                    topicId != null ? "SPECIFIC" : "ALL",
                    topicId,
                    "HARD",
                    3,
                    List.of(new StudyServiceClient.QuestionTypeSpec("MCQ", 3))
            );
            round3Questions = studyServiceClient.generateVersusQuestions(request3R);
            if (round3Questions.isEmpty()) {
                log.warn("HARD 난이도 문제가 없어서 NORMAL로 폴백합니다.");
                throw new ResponseStatusException(HttpStatus.NOT_FOUND, "No HARD questions available");
            }
        } catch (FeignException.NotFound e) {
            // 404 에러: HARD 문제가 없음
            log.warn("HARD 난이도 문제 요청 실패 (404), NORMAL로 폴백합니다: {}", e.getMessage());
            StudyServiceClient.VersusQuestionRequest request3RFallback = new StudyServiceClient.VersusQuestionRequest(
                    examMode,
                    topicId != null ? "SPECIFIC" : "ALL",
                    topicId,
                    "NORMAL",
                    3,
                    List.of(new StudyServiceClient.QuestionTypeSpec("MCQ", 3))
            );
            round3Questions = studyServiceClient.generateVersusQuestions(request3RFallback);
        } catch (FeignException.BadRequest e) {
            // 400 에러: 잘못된 요청
            log.warn("HARD 난이도 문제 요청 실패 (400), NORMAL로 폴백합니다: {}", e.getMessage());
            StudyServiceClient.VersusQuestionRequest request3RFallback = new StudyServiceClient.VersusQuestionRequest(
                    examMode,
                    topicId != null ? "SPECIFIC" : "ALL",
                    topicId,
                    "NORMAL",
                    3,
                    List.of(new StudyServiceClient.QuestionTypeSpec("MCQ", 3))
            );
            round3Questions = studyServiceClient.generateVersusQuestions(request3RFallback);
        } catch (ResponseStatusException e) {
            if (e.getStatusCode() == HttpStatus.NOT_FOUND || e.getStatusCode() == HttpStatus.BAD_REQUEST) {
                log.warn("HARD 난이도 문제 요청 실패 ({}), NORMAL로 폴백합니다: {}", e.getStatusCode(), e.getMessage());
                StudyServiceClient.VersusQuestionRequest request3RFallback = new StudyServiceClient.VersusQuestionRequest(
                        examMode,
                        topicId != null ? "SPECIFIC" : "ALL",
                        topicId,
                        "NORMAL",
                        3,
                        List.of(new StudyServiceClient.QuestionTypeSpec("MCQ", 3))
                );
                round3Questions = studyServiceClient.generateVersusQuestions(request3RFallback);
            } else {
                throw e; // 다른 에러는 그대로 전파
            }
        } catch (FeignException e) {
            // 기타 Feign 에러 (500 등)는 그대로 전파하지 않고 NORMAL로 폴백 시도
            int status = e.status();
            if (status == 404 || status == 400) {
                log.warn("HARD 난이도 문제 요청 실패 ({}), NORMAL로 폴백합니다: {}", status, e.getMessage());
                StudyServiceClient.VersusQuestionRequest request3RFallback = new StudyServiceClient.VersusQuestionRequest(
                        examMode,
                        topicId != null ? "SPECIFIC" : "ALL",
                        topicId,
                        "NORMAL",
                        3,
                        List.of(new StudyServiceClient.QuestionTypeSpec("MCQ", 3))
                );
                round3Questions = studyServiceClient.generateVersusQuestions(request3RFallback);
            } else {
                throw e; // 다른 에러는 그대로 전파
            }
        }
        allQuestions.addAll(round3Questions);
        }
        
        if (allQuestions.isEmpty()) {
            throw new ResponseStatusException(
                    HttpStatus.NOT_FOUND,
                    String.format("No questions available for Tournament (%s mode). Please check if questions exist.", examMode)
            );
        }
        
        log.info("토너먼트 전체 문제 수집 완료: mode={}, 총 {}개", examMode, allQuestions.size());
        return allQuestions;
    }

    private MatchPhase determinePhase(String questionType, MatchMode mode) {
        // GOLDENBELL의 경우 round_flow_json에서 결정해야 하지만, 일단 기본값
        if (mode == MatchMode.GOLDENBELL && ("SHORT".equals(questionType) || "LONG".equals(questionType))) {
            // 마지막 문제들은 FINAL로 처리
            return MatchPhase.FINAL;
        }
        return MatchPhase.MAIN;
    }

    private int determineTimeLimit(String questionType, String examMode, MatchMode matchMode) {
        // 모드별 시간 제한
        if (matchMode == MatchMode.GOLDENBELL) {
            // 골든벨 모드 시간 제한
            if ("WRITTEN".equals(examMode)) {
                return switch (questionType) {
                    case "OX" -> 8;   // 골든벨 필기: OX 8초
                    case "MCQ" -> 12; // 골든벨 필기: MCQ 12초
                    default -> 12;
                };
            } else {
                return switch (questionType) {
                    case "SHORT" -> 25; // 골든벨 실기: SHORT 25초
                    case "LONG" -> 25;  // LONG은 실기 골든벨에서 사용 안 함 (하위 호환성)
                    default -> 25;
                };
            }
        } else if (matchMode == MatchMode.DUEL) {
            // DUEL 모드 시간 제한
            if ("WRITTEN".equals(examMode)) {
                return switch (questionType) {
                    case "OX" -> DUEL_WRITTEN_OX_LIMIT_SEC;      // 5초
                    case "MCQ" -> DUEL_WRITTEN_MCQ_LIMIT_SEC;    // 10초
                    default -> DUEL_WRITTEN_MCQ_LIMIT_SEC;
                };
            } else {
                return switch (questionType) {
                    case "SHORT" -> DUEL_PRACTICAL_SHORT_LIMIT_SEC; // 15초
                    case "LONG" -> DUEL_PRACTICAL_LONG_LIMIT_SEC;   // 25초
                    default -> DUEL_PRACTICAL_SHORT_LIMIT_SEC;
                };
            }
        } else {
            // TOURNAMENT 모드 시간 제한 (기본값)
            if ("WRITTEN".equals(examMode)) {
                return switch (questionType) {
                    case "OX" -> 10;
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
                log.warn("User answer not provided for question {}, using client-provided correct value: {}", 
                        questionId, req.correct());
                monitoringConfig.recordTimer(timer, "validateAnswerOnServer", "status", "fallback_no_answer");
                return req.correct();
            }

            // study-service의 validateAnswer API 사용
            StudyServiceClient.UserAnswerDto userAnswerDto = new StudyServiceClient.UserAnswerDto(
                    req.userAnswer(),
                    null  // answerType은 자동 판단
            );

            log.debug("Calling study-service validateAnswer: questionId={}, userAnswer={}", 
                    questionId, req.userAnswer());
            
            StudyServiceClient.AnswerValidationResult result = studyServiceClient.validateAnswer(
                    questionId, userAnswerDto);

            monitoringConfig.recordTimer(timer, "validateAnswerOnServer", "status", "success");
            log.info("Answer validation SUCCESS for question {}: userAnswer=[{}], clientCorrect={}, serverCorrect={}, correctAnswer=[{}]",
                    questionId, req.userAnswer(), req.correct(), result.correct(), result.correctAnswer());
            
            // 클라이언트와 서버 결과가 다른 경우 경고
            if (req.correct() != result.correct()) {
                log.warn("Answer validation mismatch for question {}: clientCorrect={}, serverCorrect={}, userAnswer=[{}], correctAnswer=[{}]",
                        questionId, req.correct(), result.correct(), req.userAnswer(), result.correctAnswer());
            }

            return result.correct();
        } catch (Exception e) {
            monitoringConfig.recordTimer(timer, "validateAnswerOnServer", "status", "failure");
            monitoringConfig.recordFeignFailure("study-service", "validateAnswer", e);
            log.error("Failed to validate answer for question {}: userAnswer=[{}], clientCorrect={}, error={}", 
                    questionId, req.userAnswer(), req.correct(), e.getMessage(), e);
            // 검증 실패 시 클라이언트 값 사용 (fallback)
            log.warn("Using fallback: client-provided correct value={} for question {}", req.correct(), questionId);
            return req.correct();
        }
    }

    // ========== 개선 사항 4: 서버 기준 시간 계산 ==========

    /**
     * 문제 시작 시점 이벤트 기록 (처음 답안 제출 시)
     * 골든벨 모드에서는 startRoom 시 이미 기록되었을 수 있음
     */
    private void recordQuestionStartIfNeeded(Long roomId, Long questionId, String userId) {
        // 모든 모드에서 startRoom 또는 ROUND_COMPLETED 시 모든 참가자 공통으로 이미 기록되었을 수 있음
        // 해당 문제에 대한 QUESTION_STARTED 이벤트가 있는지 확인
        List<MatchEvent> existingEvents = eventRepository.findByRoomIdAndEventTypeContaining(
                roomId, "QUESTION_STARTED");
        boolean alreadyRecorded = existingEvents.stream()
                .anyMatch(e -> {
                    try {
                        if (e.getPayloadJson() == null) return false;
                        Map<String, Object> payload = objectMapper.readValue(
                                e.getPayloadJson(), new TypeReference<Map<String, Object>>() {
                                });
                        Object qId = payload.get("questionId");
                        // 모든 참가자 공통 시작 이벤트이거나, 해당 사용자의 시작 이벤트인지 확인
                        Boolean allParticipants = (Boolean) payload.get("allParticipants");
                        return qId != null && questionId.equals(Long.valueOf(qId.toString())) &&
                                (Boolean.TRUE.equals(allParticipants) || userId.equals(payload.get("userId")));
                    } catch (Exception ex) {
                        return false;
                    }
                });

        if (alreadyRecorded) {
            return; // 이미 기록됨 (모든 참가자 공통 또는 개별 사용자용)
        }

        // 문제 시작 시점 이벤트 기록 (개별 사용자용, 공통 이벤트가 없는 경우에만)
        // 주로 호환성을 위해 유지 (실제로는 공통 이벤트가 먼저 기록됨)
        recordEvent(roomId, "QUESTION_STARTED", Map.of(
                "questionId", questionId,
                "userId", userId,
                "startedAt", Instant.now().toString()
        ));

        log.debug("Recorded QUESTION_STARTED event for room={}, question={}, user={}",
                roomId, questionId, userId);
    }

    /**
     * 현재 진행 중인 문제 정보 조회
     * @param roomId 방 ID
     * @param status 방 상태
     * @return 현재 문제 정보. 문제가 진행 중이 아니거나 이벤트를 찾을 수 없으면 null
     */
    private VersusDtos.CurrentQuestionInfo getCurrentQuestionInfo(Long roomId, MatchStatus status) {
        // 방이 진행 중이 아니면 null 반환
        if (status != MatchStatus.ONGOING) {
            return null;
        }

        try {
            // 가장 최근 QUESTION_STARTED 이벤트 찾기
            List<MatchEvent> startEvents = eventRepository.findByRoomIdAndEventTypeContaining(
                    roomId, "QUESTION_STARTED");
            
            // createdAt 기준으로 최신순 정렬
            Optional<MatchEvent> latestEvent = startEvents.stream()
                    .max(Comparator.comparing(MatchEvent::getCreatedAt));

            if (latestEvent.isPresent()) {
                try {
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
                    
                    // MatchQuestion에서 orderNo와 timeLimitSec 가져오기
                    Optional<MatchQuestion> matchQuestion = questionRepository.findByRoomIdAndQuestionId(roomId, questionId);
                    if (matchQuestion.isPresent()) {
                        MatchQuestion q = matchQuestion.get();
                        // 종료 시간 계산: 시작 시간 + 시간 제한
                        Instant endTime = startTime.plusSeconds(q.getTimeLimitSec());
                        
                        return new VersusDtos.CurrentQuestionInfo(
                                questionId,
                                roundNo != null ? roundNo : q.getRoundNo(),
                                phase != null ? phase : q.getPhase(),
                                q.getOrderNo(),
                                q.getTimeLimitSec(),
                                endTime
                        );
                    } else {
                        // MatchQuestion을 찾을 수 없으면 이벤트 정보만 사용
                        if (roundNo != null && phase != null) {
                            // orderNo와 timeLimitSec은 기본값 사용, endTime은 계산 불가
                            return new VersusDtos.CurrentQuestionInfo(
                                    questionId,
                                    roundNo,
                                    phase,
                                    null, // orderNo를 알 수 없음
                                    null, // timeLimitSec을 알 수 없음
                                    null  // endTime을 계산할 수 없음
                            );
                        }
                    }
                } catch (Exception e) {
                    log.debug("Failed to parse current question info: {}", e.getMessage());
                    return null;
                }
            }
        } catch (Exception e) {
            log.debug("Failed to get current question info: {}", e.getMessage());
        }
        
        return null;
    }

    private Integer calculateServerTimeMs(Long roomId, Long questionId, String userId, Integer clientTimeMs, Instant answerSubmitTime) {
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
                            if (qId == null || !questionId.equals(Long.valueOf(qId.toString()))) {
                                return false;
                            }
                            // 골든벨 모드: 모든 참가자 공통 시작 이벤트 우선 사용
                            Boolean allParticipants = (Boolean) payload.get("allParticipants");
                            if (Boolean.TRUE.equals(allParticipants)) {
                                return true;
                            }
                            // 개별 사용자 시작 이벤트
                            Object uId = payload.get("userId");
                            return uId != null && userId.equals(uId.toString());
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
                        // answerSubmitTime을 사용하여 정확한 시간 계산
                        long serverTimeMs = Duration.between(startTime, answerSubmitTime).toMillis();
                        
                        // 비정상적으로 짧은 시간(100ms 미만)이면 클라이언트 시간 사용
                        // 단, 클라이언트 시간도 비정상적이면 최소값 적용
                        if (serverTimeMs < 100 && clientTimeMs != null && clientTimeMs >= 100) {
                            log.warn("서버 시간 계산이 비정상적으로 짧음: roomId={}, questionId={}, userId={}, serverTime={}ms, clientTime={}ms",
                                    roomId, questionId, userId, serverTimeMs, clientTimeMs);
                            return clientTimeMs;
                        }
                        
                        return (int) Math.max(0, serverTimeMs);
                    }
                } catch (Exception e) {
                    log.debug("Failed to parse question start time: {}", e.getMessage());
                }
            }

            // 문제 시작 이벤트가 없으면 문제 시간 제한을 사용 (타임아웃 처리)
            // 클라이언트 시간이 제공되지 않았거나 비정상적으로 짧으면 문제 시간 제한 사용
            MatchQuestion question = questionRepository.findByRoomIdAndQuestionId(roomId, questionId).orElse(null);
            int limitMs = question != null && question.getTimeLimitSec() != null 
                    ? question.getTimeLimitSec() * 1000 
                    : GOLDENBELL_TIME_LIMIT_SEC * 1000;
            
            if (clientTimeMs == null) {
                log.warn("문제 시작 이벤트와 클라이언트 시간 모두 없음: roomId={}, questionId={}, userId={}, 시간 제한 사용: {}ms",
                        roomId, questionId, userId, limitMs);
                return limitMs; // 타임아웃 처리
            }
            
            if (clientTimeMs < 100) {
                log.warn("클라이언트 시간이 비정상적으로 짧음: roomId={}, questionId={}, userId={}, clientTime={}ms, 시간 제한 사용: {}ms",
                        roomId, questionId, userId, clientTimeMs, limitMs);
                return limitMs; // 타임아웃 처리
            }
            
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
            // ExamMode 추출
            String examMode = extractExamMode(room);
            
            // 각 참가자의 개별 답안 정보 수집
            List<ProgressServiceClient.ParticipantResult> participants = scoreboard.items().stream()
                    .map(item -> {
                        List<ProgressServiceClient.AnswerDetail> answers = collectParticipantAnswers(
                                room.getId(), item.userId());
                        return new ProgressServiceClient.ParticipantResult(
                                item.userId(),
                                item.score(),
                                item.rank(),
                                item.correctCount(),
                                item.totalCount(),
                                item.totalTimeMs(),
                                answers
                        );
                    })
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
                    durationMs,
                    examMode
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
                String examMode = extractExamMode(room);
                
                List<ProgressServiceClient.ParticipantResult> participants = scoreboard.items().stream()
                        .map(item -> {
                            List<ProgressServiceClient.AnswerDetail> answers = collectParticipantAnswers(
                                    room.getId(), item.userId());
                            return new ProgressServiceClient.ParticipantResult(
                                    item.userId(),
                                    item.score(),
                                    item.rank(),
                                    item.correctCount(),
                                    item.totalCount(),
                                    item.totalTimeMs(),
                                    answers
                            );
                        })
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
                        durationMs,
                        examMode
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
    
    /**
     * 방의 scopeJson에서 examMode 추출
     */
    private String extractExamMode(MatchRoom room) {
        try {
            Map<String, Object> scope = readScope(room.getScopeJson());
            Object examModeObj = scope.get("examMode");
            if (examModeObj != null) {
                return examModeObj.toString();
            }
        } catch (Exception e) {
            log.debug("Failed to extract examMode from room {}: {}", room.getId(), e.getMessage());
        }
        return null;
    }
    
    /**
     * 참가자의 개별 답안 정보 수집
     */
    private List<ProgressServiceClient.AnswerDetail> collectParticipantAnswers(Long roomId, String userId) {
        List<MatchAnswer> answers = answerRepository.findByRoomId(roomId).stream()
                .filter(answer -> answer.getUserId().equals(userId))
                .toList();
        return answers.stream()
                .map(answer -> new ProgressServiceClient.AnswerDetail(
                        answer.getQuestionId(),
                        answer.getUserAnswer(),
                        answer.isCorrect(),
                        answer.getTimeMs(),
                        answer.getScoreDelta(),
                        answer.getRoundNo(),
                        answer.getPhase() != null ? answer.getPhase().name() : null
                ))
                .toList();
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
