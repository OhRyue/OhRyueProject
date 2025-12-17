package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.client.ProgressServiceClient;
import com.OhRyue.certpilot.versus.config.MonitoringConfig;
import com.OhRyue.certpilot.versus.domain.*;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.*;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.micrometer.core.instrument.Timer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

/**
 * DUEL 모드 매치 종료 후처리 서비스 (단일 진입점)
 * 
 * 정상 종료(10문제 완료)와 조기 종료(플레이어 이탈) 모두 처리
 * Redis 락으로 동시성 보장, 멱등성 플래그로 재시도 방어
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DuelMatchFinishService {

    private static final String EVENT_MATCH_FINISHED = "MATCH_FINISHED";
    private static final String EVENT_XP_REWARDED = "XP_REWARDED";
    private static final long LOCK_TTL_MS = 30000; // 30초

    private final MatchRoomRepository roomRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchAnswerRepository answerRepository;
    private final MatchQuestionRepository questionRepository;
    private final MatchEventRepository eventRepository;
    private final RedisLockService redisLockService;
    private final ScoreboardService scoreboardService;
    private final RealtimeEventService realtimeEventService;
    private final ProgressServiceClient progressServiceClient;
    private final MonitoringConfig monitoringConfig;
    private final RewardRetryService rewardRetryService;
    private final ObjectMapper objectMapper;
    
    private static final int DUEL_TOTAL_QUESTIONS = 10;

    /**
     * 매치 종료 후처리 (단일 진입점)
     * 
     * @param roomId 방 ID
     * @param reason 종료 사유
     * @return MatchFinishResult
     */
    @Transactional
    public MatchFinishResult finishMatch(Long roomId, FinishMatchReason reason) {
        String lockKey = String.format("versus:lock:duel:match-finish:%d", roomId);
        String requestId = UUID.randomUUID().toString();
        Duration ttl = Duration.ofMillis(LOCK_TTL_MS);

        // 1. 락 획득 시도
        boolean lockAcquired = redisLockService.tryLock(lockKey, requestId, ttl);
        if (!lockAcquired) {
            log.debug("MATCH_FINISH_LOCK_SKIPPED roomId={} reason={}", roomId, reason);
            return MatchFinishResult.skipped();
        }

        try {
            log.info("MATCH_FINISH_LOCK_ACQUIRED roomId={} reason={}", roomId, reason);

            // 2. 멱등성 방어: 이미 종료된 매치인지 확인
            MatchRoom room = roomRepository.findById(roomId)
                    .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));

            if (room.getMode() != MatchMode.DUEL) {
                log.warn("DuelMatchFinishService called for non-DUEL room: roomId={}, mode={}", 
                        roomId, room.getMode());
                return MatchFinishResult.skipped();
            }

            if (isMatchAlreadyFinished(room)) {
                log.info("MATCH_FINISH_ALREADY_DONE roomId={}", roomId);
                return MatchFinishResult.alreadyFinished();
            }

            // 3. 스코어보드 계산
            VersusDtos.ScoreBoardResp scoreboard = scoreboardService.computeScoreboard(room);
            if (scoreboard.items().isEmpty()) {
                log.warn("No participants in scoreboard for room {}, skipping match finish", roomId);
                return MatchFinishResult.skipped();
            }

            // 4. 승자 결정
            List<MatchParticipant> participantEntities = participantRepository.findByRoomId(roomId);
            String winner = resolveWinner(room, scoreboard, participantEntities);

            // 5. 매치 종료 이벤트 기록
            recordMatchFinished(roomId, reason, winner);

            // 6. 방 상태 변경
            room.setStatus(MatchStatus.DONE);
            roomRepository.save(room);

            // 7. XP 지급 (progress-service 호출)
            boolean xpGranted = grantXpReward(room, scoreboard, winner, participantEntities);

            log.info("MATCH_FINISH_DONE roomId={} winner={} xpGranted={}", roomId, winner, xpGranted);
            return MatchFinishResult.completed(winner, xpGranted);

        } catch (Exception e) {
            log.error("MATCH_FINISH_ERROR roomId={} reason={} ex={}", 
                    roomId, reason, e.getMessage(), e);
            throw e;
        } finally {
            // 8. 락 해제
            redisLockService.unlock(lockKey, requestId);
        }
    }

    /**
     * 멱등성 방어: 이미 종료된 매치인지 확인
     */
    private boolean isMatchAlreadyFinished(MatchRoom room) {
        // 1. 방 상태 확인
        if (room.getStatus() == MatchStatus.DONE) {
            return true;
        }

        // 2. resultReported 플래그 확인
        if (room.getResultReported() != null && room.getResultReported()) {
            return true;
        }

        // 3. MATCH_FINISHED 이벤트 확인
        List<MatchEvent> finishEvents = eventRepository.findByRoomIdAndEventType(room.getId(), EVENT_MATCH_FINISHED);
        if (!finishEvents.isEmpty()) {
            // DUEL 모드의 MATCH_FINISHED 이벤트가 있는지 확인
            boolean hasDuelFinishEvent = finishEvents.stream()
                    .anyMatch(e -> {
                        try {
                            if (e.getPayloadJson() == null) return false;
                            Map<String, Object> payload = objectMapper.readValue(
                                    e.getPayloadJson(), new TypeReference<Map<String, Object>>() {});
                            Object mode = payload.get("mode");
                            return "DUEL".equals(mode);
                        } catch (Exception ex) {
                            return false;
                        }
                    });
            if (hasDuelFinishEvent) {
                return true;
            }
        }

        return false;
    }

    /**
     * 승자 결정
     */
    private String resolveWinner(MatchRoom room, VersusDtos.ScoreBoardResp scoreboard, List<MatchParticipant> participants) {
        // 1. MATCH_FINISHED 이벤트에서 winner 확인
        try {
            Optional<MatchEvent> finishEvent = eventRepository.findByRoomIdAndEventType(room.getId(), EVENT_MATCH_FINISHED)
                    .stream()
                    .max(Comparator.comparing(MatchEvent::getCreatedAt));
            if (finishEvent.isPresent() && finishEvent.get().getPayloadJson() != null) {
                Map<String, Object> payload = objectMapper.readValue(
                        finishEvent.get().getPayloadJson(), new TypeReference<Map<String, Object>>() {});
                Object winnerObj = payload.get("winner");
                if (winnerObj != null && !"NONE".equalsIgnoreCase(winnerObj.toString())) {
                    return winnerObj.toString();
                }
            }
        } catch (Exception e) {
            log.debug("Failed to resolve winner from MATCH_FINISHED event: {}", e.getMessage());
        }

        // 2. 스코어보드 1위
        if (scoreboard != null && scoreboard.items() != null && !scoreboard.items().isEmpty()) {
            return scoreboard.items().get(0).userId();
        }

        // 3. 참가자 rank 확인
        return participants.stream()
                .filter(p -> p.getPlayerRank() != null && p.getPlayerRank() == 1)
                .findFirst()
                .map(MatchParticipant::getUserId)
                .orElse(null);
    }

    /**
     * 매치 종료 이벤트 기록
     */
    private void recordMatchFinished(Long roomId, FinishMatchReason reason, String winner) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("mode", "DUEL");
        payload.put("winner", winner != null ? winner : "NONE");
        payload.put("reason", reason.name());
        payload.put("finishedAt", Instant.now().toString());

        recordEvent(roomId, EVENT_MATCH_FINISHED, payload);
    }

    /**
     * XP 지급 (progress-service 호출)
     */
    private boolean grantXpReward(MatchRoom room, VersusDtos.ScoreBoardResp scoreboard, 
                                  String winner, List<MatchParticipant> participantEntities) {
        // 이미 지급되었는지 확인 (이중 방어)
        if (room.getResultReported() != null && room.getResultReported()) {
            log.info("XP already granted for room {}, skipping", room.getId());
            return false;
        }

        Timer.Sample timer = monitoringConfig.startTimer("notifyProgressService");

        try {
            Long roomId = room.getId();
            String examMode = extractExamMode(room);

            // 저장된 모든 문제 ID 조회 (DUEL은 반드시 10개)
            List<MatchQuestion> allQuestions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId);
            List<Long> questionIds = allQuestions.stream()
                    .map(MatchQuestion::getQuestionId)
                    .toList();
            
            log.info("DUEL_MATCH_FINISH_QUESTIONS roomId={} savedQuestionIds={} expectedCount={}",
                    roomId, questionIds, DUEL_TOTAL_QUESTIONS);
            
            // 참가자/집계 정보 조회
            Map<String, MatchAnswerRepository.AnswerAggregate> aggregateMap = answerRepository.aggregateByRoomId(roomId).stream()
                    .collect(Collectors.toMap(MatchAnswerRepository.AnswerAggregate::getUserId, a -> a));

            // 각 참가자의 개별 답안 정보 수집 (questionIds 기준으로 보정)
            List<ProgressServiceClient.ParticipantResult> participants = participantEntities.stream()
                    .map(p -> {
                        // questionIds 기준으로 답안 수집 (누락된 questionId는 자동 오답으로 보정)
                        List<ProgressServiceClient.AnswerDetail> answers = collectParticipantAnswersWithCorrection(
                                roomId, p.getUserId(), questionIds, allQuestions);
                        
                        // 누락된 questionId 계산 및 로깅
                        Set<Long> answeredQuestionIds = answers.stream()
                                .map(ProgressServiceClient.AnswerDetail::questionId)
                                .collect(Collectors.toSet());
                        List<Long> missingQuestionIds = questionIds.stream()
                                .filter(qid -> !answeredQuestionIds.contains(qid))
                                .toList();
                        
                        if (!missingQuestionIds.isEmpty()) {
                            log.warn("DUEL_MATCH_FINISH_MISSING_QUESTIONS roomId={} userId={} missingQuestionIds={} (자동 오답으로 보정됨)",
                                    roomId, p.getUserId(), missingQuestionIds);
                        }
                        
                        MatchAnswerRepository.AnswerAggregate agg = aggregateMap.get(p.getUserId());
                        VersusDtos.ScoreBoardItem boardItem = scoreboard.items().stream()
                                .filter(it -> Objects.equals(it.userId(), p.getUserId()))
                                .findFirst()
                                .orElse(null);
                        Integer score = Optional.ofNullable(p.getFinalScore())
                                .orElse(boardItem != null ? boardItem.score() : 0);
                        Integer rank = Optional.ofNullable(p.getPlayerRank())
                                .orElse(boardItem != null ? boardItem.rank() : null);
                        
                        // correctCount는 실제 정답 개수 (자동 오답 제외)
                        Integer correctCount = agg != null ? agg.getCorrectCount().intValue()
                                : boardItem != null ? boardItem.correctCount() : 0;
                        
                        // totalCount는 항상 questionIds.size(=10)로 설정 (보정 후)
                        Integer totalCount = questionIds.size();
                        
                        // totalTimeMs 계산 (자동 오답 포함)
                        Long totalTimeMs = answers.stream()
                                .mapToLong(ProgressServiceClient.AnswerDetail::timeMs)
                                .sum();

                        return new ProgressServiceClient.ParticipantResult(
                                p.getUserId(),
                                score,
                                rank,
                                correctCount,
                                totalCount,
                                totalTimeMs,
                                answers
                        );
                    })
                    .toList();

            // DUEL 문제 수 검증: 저장된 문제 수와 실제 진행된 문제 수 비교
            long savedQuestionCount = questionRepository.countByRoomId(roomId);
            int expectedCount = DUEL_TOTAL_QUESTIONS;
            
            // questionCount는 항상 questionIds.size(=10)로 설정 (보정 후)
            Integer questionCount = questionIds.size();
            
            // 보정 전 answeredCount 계산 (로깅용)
            Integer answeredCountBeforeCorrection = (int) answerRepository.countDistinctQuestionIdByRoomId(roomId);
            
            log.info("DUEL_MATCH_FINISH_QUESTION_COUNT roomId={} savedQuestionCount={} answeredCountBeforeCorrection={} questionCountAfterCorrection={}",
                    roomId, savedQuestionCount, answeredCountBeforeCorrection, questionCount);
            
            // DUEL은 반드시 10문제여야 함
            if (savedQuestionCount != expectedCount) {
                log.error("DUEL_MATCH_FINISH_QUESTION_COUNT_MISMATCH roomId={} expected={} savedCount={} answeredCountBeforeCorrection={}",
                        roomId, expectedCount, savedQuestionCount, answeredCountBeforeCorrection);
            } else if (answeredCountBeforeCorrection != expectedCount) {
                log.warn("DUEL_MATCH_FINISH_QUESTION_COUNT_ANSWERED_MISMATCH roomId={} expected={} savedCount={} answeredCountBeforeCorrection={} (자동 오답으로 보정됨)",
                        roomId, expectedCount, savedQuestionCount, answeredCountBeforeCorrection);
            } else {
                log.info("DUEL_MATCH_FINISH_QUESTION_COUNT_VALID roomId={} expected={} savedCount={} answeredCount={}",
                        roomId, expectedCount, savedQuestionCount, answeredCountBeforeCorrection);
            }
            Long durationMs = calculateMatchDuration(room);

            ProgressServiceClient.VersusResultRequest request = new ProgressServiceClient.VersusResultRequest(
                    room.getMode().name(),
                    room.getId(),
                    winner,
                    participants,
                    questionCount,
                    durationMs,
                    examMode,
                    room.getIsBotMatch()
            );

            log.info("RECORD_VERSUS_RESULT_CALL roomId={} mode=DUEL winner={} participants={} questions={} duration={}ms",
                    room.getId(), winner, participants.size(), questionCount, durationMs);

            ProgressServiceClient.VersusResultResponse xpResponse = progressServiceClient.recordVersusResult(request);
            monitoringConfig.recordTimer(timer, "notifyProgressService", "status", "success");
            log.info("Successfully notified progress-service for room {} completion", room.getId());

            // XP 결과 이벤트 저장 후 결과 보고 플래그 업데이트
            try {
                recordEvent(roomId, EVENT_XP_REWARDED, Map.of("xpResults", xpResponse.xpResults()));
                room.setResultReported(true);
                roomRepository.save(room);
                log.info("XP result stored as event for room {} and resultReported set", room.getId());
            } catch (Exception e) {
                log.warn("Failed to store XP_REWARDED event for room {}: {}", room.getId(), e.getMessage());
            }

            // 각 참가자별로 ProgressActivity 생성 (비동기)
            // Note: createProgressActivitiesForParticipants는 VersusService의 private 메서드이므로
            // 필요시 VersusService에 public 메서드로 노출하거나 여기서 직접 구현
            // 현재는 생략 (기존 로직과의 호환성을 위해)
            // TODO: VersusService.createProgressActivitiesForParticipants()를 public으로 변경하거나
            //       DuelMatchFinishService에서 직접 구현

            return true;

        } catch (Exception e) {
            monitoringConfig.recordTimer(timer, "notifyProgressService", "status", "failure");
            monitoringConfig.recordRewardFailure(room.getId().toString(), "all", e);
            log.error("Failed to notify progress-service for room {}: {}", room.getId(), e.getMessage(), e);

            // 비동기 재시도 큐에 추가
            try {
                String examMode = extractExamMode(room);
                
                // 저장된 모든 문제 ID 조회 (재시도 경로에서도 보정 필요)
                List<MatchQuestion> retryAllQuestions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(room.getId());
                List<Long> retryQuestionIds = retryAllQuestions.stream()
                        .map(MatchQuestion::getQuestionId)
                        .toList();
                
                List<ProgressServiceClient.ParticipantResult> participants = scoreboard.items().stream()
                        .map(item -> {
                            // questionIds 기준으로 답안 수집 (누락된 questionId는 자동 오답으로 보정)
                            List<ProgressServiceClient.AnswerDetail> answers = collectParticipantAnswersWithCorrection(
                                    room.getId(), item.userId(), retryQuestionIds, retryAllQuestions);
                            Long totalTimeMs = answers.stream()
                                    .mapToLong(ProgressServiceClient.AnswerDetail::timeMs)
                                    .sum();
                            
                            // totalCount는 항상 questionIds.size(=10)로 설정
                            Integer totalCount = retryQuestionIds.size();
                            
                            return new ProgressServiceClient.ParticipantResult(
                                    item.userId(),
                                    item.score(),
                                    item.rank(),
                                    item.correctCount(),
                                    totalCount,
                                    totalTimeMs,
                                    answers
                            );
                        })
                        .toList();

                String winnerForRetry = scoreboard.items().get(0).userId();
                // questionCount는 항상 questionIds.size(=10)로 설정 (보정 후)
                Integer questionCount = retryQuestionIds.size();
                Long durationMs = calculateMatchDuration(room);

                ProgressServiceClient.VersusResultRequest request = new ProgressServiceClient.VersusResultRequest(
                        room.getMode().name(),
                        room.getId(),
                        winnerForRetry,
                        participants,
                        questionCount,
                        durationMs,
                        examMode,
                        room.getIsBotMatch()
                );

                rewardRetryService.retryRewardPayment(room.getId(), request);
                log.info("보상 지급 재시도 큐에 추가: roomId={}", room.getId());
            } catch (Exception retryException) {
                log.error("보상 지급 재시도 큐 추가 실패: roomId={}", room.getId(), retryException);
            }

            return false;
        }
    }

    /**
     * 매치 지속 시간 계산
     */
    private Long calculateMatchDuration(MatchRoom room) {
        Optional<MatchEvent> startEvent = eventRepository.findByRoomIdAndEventType(room.getId(), "MATCH_STARTED")
                .stream()
                .findFirst();

        if (startEvent.isPresent()) {
            return Duration.between(startEvent.get().getCreatedAt(), Instant.now()).toMillis();
        }
        return 0L;
    }

    /**
     * 방의 scopeJson에서 examMode 추출
     */
    private String extractExamMode(MatchRoom room) {
        try {
            if (room.getScopeJson() == null || room.getScopeJson().isBlank()) {
                return null;
            }
            Map<String, Object> scope = objectMapper.readValue(
                    room.getScopeJson(), new TypeReference<Map<String, Object>>() {});
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
     * 참가자의 개별 답안 정보 수집 (questionIds 기준으로 보정)
     * 
     * **핵심 원칙**: 결과 집계는 "답안 테이블 기준"이 아니라 "questionIds 기준"으로 수행
     * 
     * **첫 문제 늦은 제출 실패 케이스 해결**:
     * - 첫 문제에서 두 유저 모두 늦은 제출로 실패하면 Answer 레코드가 생성되지 않음
     * - 기존: answeredCount=9 (첫 문제 누락) → mismatch 발생
     * - 수정: questionIds(10개) 기준으로 순회하여, 누락된 questionId는 자동 오답으로 보정
     * - 결과: answeredCount=10 (항상 10개 보장)
     * 
     * **보정 로직**:
     * 1. questionIds 기준으로 순회
     * 2. 기존 답안이 있으면 그대로 사용
     * 3. 누락된 questionId는 자동 오답 AnswerDetail 생성:
     *    - userAnswer = null (미제출)
     *    - isCorrect = false (오답)
     *    - timeMs = timeLimitMs
     *    - scoreDelta = 0
     * 4. DB에도 자동 오답 Answer 엔티티 저장 (멱등성 보장)
     * 
     * @param roomId 방 ID
     * @param userId 참가자 ID
     * @param questionIds 저장된 모든 문제 ID 목록 (DUEL은 10개)
     * @param allQuestions 저장된 모든 문제 엔티티 목록
     * @return 보정된 답안 목록 (항상 questionIds.size()개)
     */
    private List<ProgressServiceClient.AnswerDetail> collectParticipantAnswersWithCorrection(
            Long roomId, String userId, List<Long> questionIds, List<MatchQuestion> allQuestions) {
        
        // 기존 답안 조회
        Map<Long, MatchAnswer> existingAnswers = answerRepository.findByRoomId(roomId).stream()
                .filter(answer -> answer.getUserId().equals(userId))
                .collect(Collectors.toMap(MatchAnswer::getQuestionId, a -> a));
        
        // questionIds 기준으로 답안 수집 (누락된 questionId는 자동 오답으로 보정)
        List<ProgressServiceClient.AnswerDetail> result = new ArrayList<>();
        
        for (Long questionId : questionIds) {
            MatchAnswer existingAnswer = existingAnswers.get(questionId);
            
            if (existingAnswer != null) {
                // 기존 답안이 있으면 그대로 사용
                result.add(new ProgressServiceClient.AnswerDetail(
                        existingAnswer.getQuestionId(),
                        existingAnswer.getUserAnswer(),
                        existingAnswer.isCorrect(),
                        existingAnswer.getTimeMs(),
                        existingAnswer.getScoreDelta(),
                        existingAnswer.getRoundNo(),
                        existingAnswer.getPhase() != null ? existingAnswer.getPhase().name() : null
                ));
            } else {
                // 누락된 questionId: 자동 오답 생성
                MatchQuestion question = allQuestions.stream()
                        .filter(q -> q.getQuestionId().equals(questionId))
                        .findFirst()
                        .orElse(null);
                
                if (question != null) {
                    // DB에 자동 오답 저장 (멱등성 보장: 이미 있으면 skip)
                    ensureAutoTimeoutAnswer(roomId, userId, question);
                    
                    // 자동 오답 AnswerDetail 생성
                    result.add(new ProgressServiceClient.AnswerDetail(
                            questionId,
                            null, // userAnswer = null (미제출)
                            false, // isCorrect = false (오답)
                            question.getTimeLimitSec() * 1000, // timeMs = timeLimitMs
                            0, // scoreDelta = 0
                            question.getRoundNo(),
                            question.getPhase() != null ? question.getPhase().name() : null
                    ));
                    
                    log.info("DUEL_MATCH_FINISH_AUTO_ANSWER roomId={} userId={} questionId={} (미제출 자동 오답 생성)",
                            roomId, userId, questionId);
                } else {
                    log.warn("DUEL_MATCH_FINISH_AUTO_ANSWER_SKIP roomId={} userId={} questionId={} (문제 엔티티를 찾을 수 없음)",
                            roomId, userId, questionId);
                }
            }
        }
        
        return result;
    }
    
    /**
     * 자동 오답 Answer 엔티티 저장 (멱등성 보장)
     * 
     * 이미 답안이 있으면 저장하지 않음 (중복 방지)
     * 
     * @param roomId 방 ID
     * @param userId 참가자 ID
     * @param question 문제 엔티티
     */
    private void ensureAutoTimeoutAnswer(Long roomId, String userId, MatchQuestion question) {
        // 이미 답안이 있으면 skip (멱등성 보장)
        if (answerRepository.findByRoomIdAndQuestionIdAndUserId(roomId, question.getQuestionId(), userId).isPresent()) {
            return;
        }
        
        // 자동 오답 저장
        MatchAnswer autoAnswer = MatchAnswer.builder()
                .roomId(roomId)
                .questionId(question.getQuestionId())
                .userId(userId)
                .roundNo(question.getRoundNo())
                .phase(question.getPhase())
                .correct(false)
                .timeMs(question.getTimeLimitSec() * 1000)
                .scoreDelta(0)
                .userAnswer(null) // 미제출
                .build();
        
        answerRepository.save(autoAnswer);
        
        log.info("DUEL_MATCH_FINISH_AUTO_ANSWER_SAVED roomId={} userId={} questionId={} (DB에 자동 오답 저장됨)",
                roomId, userId, question.getQuestionId());
    }
    
    /**
     * 참가자의 개별 답안 정보 수집 (기존 메서드, 하위 호환성 유지)
     * 
     * @deprecated collectParticipantAnswersWithCorrection을 사용하세요
     */
    @Deprecated
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

    /**
     * 이벤트 기록 및 실시간 브로드캐스트
     */
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

            MatchEvent savedEvent = eventRepository.save(event);

            // 실시간 브로드캐스트
            realtimeEventService.broadcastEvent(savedEvent);
        } catch (com.fasterxml.jackson.core.JsonProcessingException e) {
            log.warn("Failed to serialize payload for event: roomId={}, type={}, error={}", 
                    roomId, type, e.getMessage());
            // payload 없이 이벤트 저장 시도
            try {
                MatchEvent event = MatchEvent.builder()
                        .roomId(roomId)
                        .eventType(type)
                        .payloadJson(null)
                        .build();
                MatchEvent savedEvent = eventRepository.save(event);
                realtimeEventService.broadcastEvent(savedEvent);
            } catch (Exception ex) {
                log.warn("Failed to record event without payload: roomId={}, type={}, error={}", 
                        roomId, type, ex.getMessage());
            }
        } catch (Exception e) {
            log.warn("Failed to record event: roomId={}, type={}, error={}", 
                    roomId, type, e.getMessage());
        }
    }

    /**
     * 종료 사유
     */
    public enum FinishMatchReason {
        LAST_QUESTION_DONE,  // 정상 종료: 10문제 완료
        PLAYER_LEFT,         // 조기 종료: 플레이어 이탈
        HEARTBEAT_TIMEOUT    // 조기 종료: 하트비트 타임아웃
    }

    /**
     * 종료 결과
     */
    public static class MatchFinishResult {
        private final boolean processed;
        private final boolean alreadyFinished;
        private final String winner;
        private final boolean xpGranted;

        private MatchFinishResult(boolean processed, boolean alreadyFinished, String winner, boolean xpGranted) {
            this.processed = processed;
            this.alreadyFinished = alreadyFinished;
            this.winner = winner;
            this.xpGranted = xpGranted;
        }

        public static MatchFinishResult skipped() {
            return new MatchFinishResult(false, false, null, false);
        }

        public static MatchFinishResult alreadyFinished() {
            return new MatchFinishResult(false, true, null, false);
        }

        public static MatchFinishResult completed(String winner, boolean xpGranted) {
            return new MatchFinishResult(true, false, winner, xpGranted);
        }

        public boolean isProcessed() {
            return processed;
        }

        public boolean isAlreadyFinished() {
            return alreadyFinished;
        }

        public String getWinner() {
            return winner;
        }

        public boolean isXpGranted() {
            return xpGranted;
        }
    }
}

