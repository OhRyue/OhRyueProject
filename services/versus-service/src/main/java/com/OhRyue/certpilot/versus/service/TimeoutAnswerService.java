package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.MatchAnswer;
import com.OhRyue.certpilot.versus.domain.MatchEvent;
import com.OhRyue.certpilot.versus.domain.MatchQuestion;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.MatchAnswerRepository;
import com.OhRyue.certpilot.versus.repository.MatchEventRepository;
import com.OhRyue.certpilot.versus.repository.MatchParticipantRepository;
import com.OhRyue.certpilot.versus.repository.MatchQuestionRepository;
import com.OhRyue.certpilot.versus.repository.MatchRoomRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 시간 초과 시 자동 오답 처리 서비스
 * - 시간 내 답하지 않은 사용자 자동 오답 처리
 * - 모든 답변이 오기까지 기다리지 않고 시간 제한 지나면 자동 진행
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class TimeoutAnswerService {

    private final MatchRoomRepository roomRepository;
    private final MatchQuestionRepository questionRepository;
    private final MatchAnswerRepository answerRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchEventRepository eventRepository;
    private final VersusService versusService;

    /**
     * 매 10초마다 실행: 시간 초과 답안 자동 처리
     */
    @Scheduled(fixedRate = 10000) // 10초마다
    @Transactional
    public void processTimeoutAnswers() {
        Instant now = Instant.now();
        
        // 진행 중인 방 찾기
        List<MatchRoom> ongoingRooms = roomRepository.findByStatus(MatchStatus.ONGOING);
        
        for (MatchRoom room : ongoingRooms) {
            try {
                processRoomTimeoutAnswers(room, now);
            } catch (Exception e) {
                log.error("Failed to process timeout answers for room {}: {}", 
                    room.getId(), e.getMessage(), e);
            }
        }
    }

    /**
     * 특정 방의 시간 초과 답안 처리
     */
    private void processRoomTimeoutAnswers(MatchRoom room, Instant now) {
        // 현재 라운드의 문제들 찾기
        List<MatchQuestion> currentQuestions = questionRepository.findByRoomIdAndRoundNo(
            room.getId(), getCurrentRound(room));
        
        if (currentQuestions.isEmpty()) {
            return;
        }

        // 모든 참가자 목록
        Set<String> allParticipants = participantRepository.findByRoomId(room.getId())
            .stream()
            .map(p -> p.getUserId())
            .collect(Collectors.toSet());

        for (MatchQuestion question : currentQuestions) {
            // 이미 답안을 제출한 사용자들
            Set<String> answeredUsers = answerRepository
                .findByRoomId(room.getId())
                .stream()
                .filter(a -> a.getQuestionId().equals(question.getQuestionId()))
                .map(MatchAnswer::getUserId)
                .collect(Collectors.toSet());

            // 답안을 제출하지 않은 사용자들
            Set<String> unansweredUsers = allParticipants.stream()
                .filter(userId -> !answeredUsers.contains(userId))
                .collect(Collectors.toSet());

            if (unansweredUsers.isEmpty()) {
                continue; // 모든 사용자가 답안 제출
            }

            // 문제 시작 시점 확인 (QUESTION_STARTED 이벤트)
            Instant questionStartTime = getQuestionStartTime(room.getId(), question.getQuestionId());
            if (questionStartTime == null) {
                continue; // 문제 시작 이벤트가 없으면 스킵
            }

            // 시간 제한 확인
            Duration elapsed = Duration.between(questionStartTime, now);
            int timeLimitSec = question.getTimeLimitSec();
            
            if (elapsed.getSeconds() >= timeLimitSec) {
                // 시간 초과: 자동 오답 처리
                log.info("Processing timeout answers for room {}, question {}: {} users", 
                    room.getId(), question.getQuestionId(), unansweredUsers.size());
                
                for (String userId : unansweredUsers) {
                    processTimeoutAnswer(room, question, userId, timeLimitSec);
                }
            }
        }
    }

    /**
     * 시간 초과 답안 처리 (자동 오답)
     */
    private void processTimeoutAnswer(MatchRoom room, MatchQuestion question, 
                                      String userId, int timeLimitSec) {
        // 이미 처리되었는지 확인
        boolean alreadyAnswered = answerRepository
            .findByRoomIdAndQuestionIdAndUserId(room.getId(), question.getQuestionId(), userId)
            .isPresent();
        
        if (alreadyAnswered) {
            return; // 이미 답안 제출됨
        }

        // 자동 오답 생성 (시간 제한 내 제출 못함)
        MatchAnswer timeoutAnswer = MatchAnswer.builder()
            .roomId(room.getId())
            .questionId(question.getQuestionId())
            .userId(userId)
            .roundNo(question.getRoundNo())
            .phase(question.getPhase())
            .correct(false) // 자동 오답
            .timeMs(timeLimitSec * 1000) // 시간 제한을 시간으로 설정
            .scoreDelta(0) // 오답이므로 점수 변화 없음
            .userAnswer("") // 공백으로 처리
            .build();

        answerRepository.save(timeoutAnswer);

        // 이벤트 기록
        MatchEvent timeoutEvent = MatchEvent.builder()
            .roomId(room.getId())
            .eventType("ANSWER_TIMEOUT")
            .payloadJson(String.format(
                "{\"userId\":\"%s\",\"questionId\":%d,\"round\":%d,\"phase\":\"%s\",\"timeLimitSec\":%d}",
                userId, question.getQuestionId(), question.getRoundNo(), 
                question.getPhase().name(), timeLimitSec))
            .build();
        eventRepository.save(timeoutEvent);

        log.debug("Auto-processed timeout answer for room {}, question {}, user {}", 
            room.getId(), question.getQuestionId(), userId);

        // 매치 진행 상태 확인 (모든 답변이 오지 않아도 시간 제한 지나면 진행)
        try {
            VersusDtos.ScoreBoardResp scoreboard = versusService.computeScoreboard(room);
            versusService.handleModeAfterAnswer(room, question, scoreboard);
        } catch (Exception e) {
            log.error("Failed to process match progress after timeout answer: {}", e.getMessage());
        }
    }

    /**
     * 문제 시작 시점 조회
     */
    private Instant getQuestionStartTime(Long roomId, Long questionId) {
        return eventRepository.findByRoomIdAndEventTypeContaining(roomId, "QUESTION_STARTED")
            .stream()
            .filter(e -> {
                if (e.getPayloadJson() == null) return false;
                return e.getPayloadJson().contains("\"questionId\":" + questionId);
            })
            .map(e -> e.getCreatedAt())
            .findFirst()
            .orElse(null);
    }

    /**
     * 현재 라운드 번호 조회 (간단한 구현)
     */
    private int getCurrentRound(MatchRoom room) {
        // 가장 최근에 제출된 답안의 라운드 번호 사용
        return answerRepository.findByRoomId(room.getId())
            .stream()
            .mapToInt(a -> a.getRoundNo() != null ? a.getRoundNo() : 1)
            .max()
            .orElse(1);
    }
}

