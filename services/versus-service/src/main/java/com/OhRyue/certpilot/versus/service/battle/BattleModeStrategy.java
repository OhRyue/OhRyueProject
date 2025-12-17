package com.OhRyue.certpilot.versus.service.battle;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchQuestion;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.dto.VersusDtos;

/**
 * 배틀 모드별 전략 인터페이스
 * 
 * 각 모드(DUEL/TOURNAMENT/GOLDENBELL)의 비즈니스 로직을 정의합니다.
 * 공통 엔진은 이 인터페이스를 통해 모드별 정책을 적용합니다.
 */
public interface BattleModeStrategy {

    /**
     * 모드 식별
     */
    MatchMode mode();

    /**
     * 필수 참가자 수
     * 
     * @return 최소 참가자 수 (DUEL=2, TOURNAMENT=8, GOLDENBELL=1)
     */
    int requiredPlayers();

    /**
     * 질문 계획 생성
     * 
     * @param room 방 정보
     * @return 질문 계획 (문제 목록, 라운드 구성 등)
     */
    QuestionPlan buildQuestionPlan(MatchRoom room);

    /**
     * 점수 계산
     * 
     * @param question 문제 정보
     * @param answer 답안 정보
     * @param policy 점수 정책
     * @return 점수 결과
     */
    ScoreResult score(MatchQuestion question, MatchAnswer answer, ScorePolicy policy);

    /**
     * 다음 진행 판단
     * 
     * @param room 방 정보
     * @param currentStepKey 현재 stepKey
     * @param scoreboard 스코어보드
     * @return 진행 결정 (다음 문제, 다음 라운드, 종료 등)
     */
    ProgressDecision decideNext(MatchRoom room, String currentStepKey, VersusDtos.ScoreBoardResp scoreboard);

    /**
     * 탈락 판단 (TOURNAMENT/GOLDENBELL 전용)
     * 
     * @param room 방 정보
     * @param stepKey 현재 stepKey
     * @param scoreboard 스코어보드
     * @return 탈락 결정 (탈락 대상, 탈락 사유 등)
     */
    EliminationDecision eliminate(MatchRoom room, String stepKey, VersusDtos.ScoreBoardResp scoreboard);

    /**
     * stepKey 파싱
     * 
     * @param stepKey stepKey 문자열
     * @return 파싱된 StepKey 객체
     */
    StepKey parseStepKey(String stepKey);

    /**
     * stepKey 생성
     * 
     * @param question 문제 정보
     * @return stepKey 문자열
     */
    String buildStepKey(MatchQuestion question);

    /**
     * 현재 질문 조회
     * 
     * @param room 방 정보
     * @param stepKey stepKey
     * @return 현재 질문 정보
     */
    MatchQuestion getCurrentQuestion(MatchRoom room, String stepKey);

    /**
     * 질문 종료 조건 확인
     * 
     * @param room 방 정보
     * @param stepKey stepKey
     * @return 질문 종료 가능 여부
     */
    boolean canFinishQuestion(MatchRoom room, String stepKey);

    /**
     * 질문 종료 처리
     * 
     * @param room 방 정보
     * @param stepKey stepKey
     * @param reason 종료 사유
     * @param triggeredByUserId 트리거한 사용자 ID
     * @return 질문 종료 결과
     */
    QuestionFinishResult finishQuestion(MatchRoom room, String stepKey, 
                                        com.OhRyue.certpilot.versus.service.battle.BattleEngineService.FinishReason reason, 
                                        String triggeredByUserId);

    /**
     * 매치 종료 처리
     * 
     * @param room 방 정보
     * @param reason 종료 사유
     * @return 매치 종료 결과
     */
    MatchFinishResult finishMatch(MatchRoom room, 
                                 com.OhRyue.certpilot.versus.service.battle.BattleEngineService.FinishMatchReason reason);
}

