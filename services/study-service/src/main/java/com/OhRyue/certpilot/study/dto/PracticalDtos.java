package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;

public class PracticalDtos {

    // 실기용 단답/서술 문제 1개
    @Schema(description = "실기용 문제 1개(단답/서술)")
    public record PracticalQuestion(
            Long questionId, // 문제 ID
            String type,     // SHORT | LONG
            String text,     // 문제 본문
            String imageUrl  // 문제 이미지(선택)
    ) {}

    // 실기 세트(여러 문제)
    @Schema(description = "실기 문제 세트")
    public record PracticalSet(
            List<PracticalQuestion> items // 문제 배열
    ) {}

    /* ---------- 제출 ---------- */

    // 사용자 답안 1개(실기)
    @Schema(description = "실기 제출: 사용자 답안 1개")
    public record PracticalAnswer(
            Long questionId, // 문제 ID
            String userText  // 사용자의 주관식 답변 텍스트
    ) {}

    // 실기 제출 요청(배치)
    @Schema(description = "실기 제출 요청")
    public record PracticalSubmitReq(
            Long topicId,                  // 토픽 ID
            List<PracticalAnswer> answers  // 제출 답안 목록
    ) {}

    // 실기 제출 결과 1개
    @Schema(description = "실기 제출 결과 아이템")
    public record PracticalSubmitItem(
            Long questionId,       // 문제 ID
            Integer score,         // 0~100 (AI 채점)
            String baseExplanation,// DB 기본 해설
            String aiExplanation   // LLM 맞춤 해설
    ) {}

    // 실기 제출 응답(배치)
    @Schema(description = "실기 제출 응답")
    public record PracticalSubmitResp(
            int total,                          // 채점된 문항 수
            int avgScore,                       // 평균 점수
            List<PracticalSubmitItem> items,    // 결과 아이템 목록
            List<Long> wrongQuestionIds         // 방금 세트의 오답 문제 ID들(score<60)
    ) {}

    @Schema(description = "실기 리뷰 제출 요청")
    public record PracticalReviewSubmitReq(
            Long rootTopicId,
            List<PracticalAnswer> answers
    ) {}

    @Schema(description = "실기 리뷰 제출 응답")
    public record PracticalReviewSubmitResp(
            int total,
            int avgScore,
            List<PracticalSubmitItem> items,
            List<Long> wrongQuestionIds
    ) {}

    /* ---------- 즉시 채점 ---------- */

    // 실기 즉시 채점 요청
    @Schema(description = "실기 단건 즉시 채점 요청")
    public record PracticalGradeOneReq(
            Long topicId,    // 토픽 ID
            Long questionId, // 문제 ID
            String userText  // 사용자의 주관식 답변
    ) {}

    // 실기 즉시 채점 응답
    @Schema(description = "실기 단건 즉시 채점 응답")
    public record PracticalGradeOneResp(
            Integer score,         // 0~100 (AI 채점)
            String baseExplanation,// DB 기본 해설
            String aiExplanation   // LLM 맞춤 해설
    ) {}
}
