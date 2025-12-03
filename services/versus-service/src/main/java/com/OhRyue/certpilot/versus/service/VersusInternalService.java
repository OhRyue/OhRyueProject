package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.client.StudyServiceClient;
import com.OhRyue.certpilot.versus.domain.MatchAnswer;
import com.OhRyue.certpilot.versus.domain.MatchQuestion;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.dto.InternalDtos;
import com.OhRyue.certpilot.versus.repository.MatchAnswerRepository;
import com.OhRyue.certpilot.versus.repository.MatchParticipantRepository;
import com.OhRyue.certpilot.versus.repository.MatchQuestionRepository;
import com.OhRyue.certpilot.versus.repository.MatchRoomRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class VersusInternalService {

    private final MatchRoomRepository roomRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchQuestionRepository questionRepository;
    private final MatchAnswerRepository answerRepository;
    private final StudyServiceClient studyServiceClient;

    /**
     * 매치 상세 조회 (내부 API용)
     * progress-service에서 호출하여 매치의 문제/답안/정답 정보를 가져옴
     */
    public InternalDtos.MatchDetailDto getMatchDetail(Long matchId, String userId) {
        MatchRoom room = roomRepository.findById(matchId)
                .orElseThrow(() -> new IllegalArgumentException("Match not found: " + matchId));

        // 참가자 검증 (해당 사용자가 참가했는지 확인)
        boolean isParticipant = participantRepository.findByRoomIdAndUserId(matchId, userId).isPresent();
        
        if (!isParticipant) {
            throw new IllegalArgumentException("User is not a participant of this match: " + userId);
        }

        // 매치의 문제 목록 조회 (순서대로)
        List<MatchQuestion> matchQuestions = questionRepository
                .findByRoomIdOrderByRoundNoAscOrderNoAsc(matchId);
        
        // 해당 사용자의 답안 조회
        List<MatchAnswer> userAnswers = answerRepository.findByRoomId(matchId).stream()
                .filter(answer -> answer.getUserId().equals(userId))
                .collect(Collectors.toList());
        
        Map<Long, MatchAnswer> answerMap = userAnswers.stream()
                .collect(Collectors.toMap(MatchAnswer::getQuestionId, a -> a, (a1, a2) -> a1));

        // 문제 상세 리스트 구성
        List<InternalDtos.QuestionDetailDto> questionDetails = new ArrayList<>();
        int order = 1;
        
        for (MatchQuestion matchQuestion : matchQuestions) {
            Long questionId = matchQuestion.getQuestionId();
            MatchAnswer answer = answerMap.get(questionId);
            
            // study-service에서 문제 상세 정보 가져오기
            StudyServiceClient.QuestionDto questionDto;
            try {
                questionDto = studyServiceClient.getQuestion(questionId);
            } catch (Exception e) {
                log.warn("Failed to get question detail from study-service: questionId={}, error={}", 
                        questionId, e.getMessage());
                continue;
            }

            // 내 답 추출
            String myAnswer = answer != null ? Optional.ofNullable(answer.getUserAnswer()).orElse("") : "";
            
            // 정답 추출
            String correctAnswer = Optional.ofNullable(questionDto.answerKey()).orElse("").trim();
            
            // 시간 계산
            Long timeTakenMs = answer != null && answer.getTimeMs() != null 
                    ? Long.valueOf(answer.getTimeMs()) 
                    : null;

            questionDetails.add(new InternalDtos.QuestionDetailDto(
                    order++,
                    matchQuestion.getRoundNo(),
                    questionId,
                    questionDto.type(),
                    Optional.ofNullable(questionDto.stem()).orElse(""),
                    myAnswer,
                    correctAnswer,
                    answer != null && answer.isCorrect(),
                    answer != null && answer.getSubmittedAt() != null
                            ? answer.getSubmittedAt().atZone(ZoneId.of("Asia/Seoul")).toLocalDateTime()
                            : null,
                    timeTakenMs,
                    answer != null ? answer.getScoreDelta() : null
            ));
        }

        return new InternalDtos.MatchDetailDto(
                matchId,
                userId,
                questionDetails
        );
    }
}

