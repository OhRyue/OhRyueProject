package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;

public class WrittenDtos {

  /* ===================== 개념(리치 컨텐츠) ===================== */

  // 개념 페이지 응답(토픽의 리치 블록)
  @Schema(name = "ConceptResp")
  public record ConceptResp(
      Long topicId,            // 토픽 ID
      String title,            // 토픽명
      List<Section> sections   // 섹션 배열
  ) {
    // 섹션(소제목/중요도/블록들)
    @Schema(name = "ConceptSection")
    public record Section(
        Integer orderNo,     // 순서
        String subCode,      // 소코드(선택)
        String title,        // 섹션 제목
        Integer importance,  // 중요도(별 0~3)
        List<Block> blocks   // 블록 배열
    ) {}

    // 블록(문단/리스트/이미지/표 등)
    @Schema(name = "ConceptBlock")
    public record Block(
        String type,               // heading | paragraph | list | image | table
        String text,               // 본문/제목
        List<String> items,        // 리스트 항목
        String url,                // 이미지 URL
        String alt,                // 이미지 대체 텍스트
        String caption,            // 캡션(이미지/표)
        List<String> headers,      // 표 헤더
        List<List<String>> rows    // 표 행들
    ) {}
  }

  /* ===================== 미니체크(OX) ===================== */

  // OX 세트
  @Schema(name = "MiniSet")
  public record MiniSet(
      List<MiniQuestion> items // OX 문제 배열
  ) {}

  // OX 문제 1개
  @Schema(name = "MiniQuestion")
  public record MiniQuestion(
      Long questionId, // 문제 ID
      String text      // 문제 본문
  ) {}

  // OX 제출 요청(배치)
  @Schema(name = "MiniSubmitReq")
  public record MiniSubmitReq(
      Long topicId,              // 토픽 ID
      List<MiniAnswer> answers   // 답안 목록
  ) {}

  // OX 답안 1개
  @Schema(name = "MiniAnswer")
  public record MiniAnswer(
      Long questionId, // 문제 ID
      Boolean answer   // true/false
  ) {}

  // OX 제출 응답(배치)
  @Schema(name = "MiniSubmitResp")
  public record MiniSubmitResp(
      int total,                          // 제출 문항 수
      int correct,                        // 정답 개수
      boolean passed,                     // 전부 정답 여부
      List<MiniSubmitItem> items,         // 결과 아이템 목록
      List<Long> wrongQuestionIds         // (NEW) 방금 세트의 오답 문제 ID들
  ) {}

  // OX 제출 결과 1개
  @Schema(name = "MiniSubmitItem")
  public record MiniSubmitItem(
      Long questionId,     // 문제 ID
      boolean correct,     // 정오
      String explanation,  // DB 기본 해설
      String aiExplanation // (미사용) OX는 빈 문자열 유지
  ) {}

  /* ===================== MCQ ===================== */

  // MCQ 세트
  @Schema(name = "McqSet")
  public record McqSet(
      List<McqQuestion> items // 객관식 문제 배열
  ) {}

  // MCQ 문제 1개
  @Schema(name = "McqQuestion")
  public record McqQuestion(
      Long questionId,        // 문제 ID
      String text,            // 문제 본문
      List<McqChoice> choices,// 보기 목록
      String imageUrl         // 문제 이미지(선택)
  ) {}

  // 보기 1개
  @Schema(name = "McqChoice")
  public record McqChoice(
      String label, // 보기 라벨(A/B/C/D)
      String text   // 보기 텍스트
  ) {}

  // MCQ 제출 요청(배치)
  @Schema(name = "McqSubmitReq")
  public record McqSubmitReq(
      Long topicId,             // 토픽 ID
      List<McqAnswer> answers   // 답안 목록
  ) {}

  // MCQ 답안 1개
  @Schema(name = "McqAnswer")
  public record McqAnswer(
      Long questionId, // 문제 ID
      String label     // 사용자가 선택한 보기 라벨
  ) {}

  // MCQ 제출 응답(배치)
  @Schema(name = "McqSubmitResp")
  public record McqSubmitResp(
      int total,                         // 제출 문항 수
      int correct,                       // 정답 개수
      List<McqSubmitItem> items,         // 결과 아이템 목록
      List<Long> wrongQuestionIds        // 방금 세트의 오답 문제 ID들
  ) {}

  // MCQ 제출 결과 1개
  @Schema(name = "McqSubmitItem")
  public record McqSubmitItem(
      Long questionId,     // 문제 ID
      boolean correct,     // 정오
      String correctLabel, // 정답 보기 라벨
      String explanation,  // DB 기본 해설
      String aiExplanation // 오답 시 AI 해설
  ) {}

  /* ===================== 요약 ===================== */

  // 학습 요약 카드
  @Schema(name = "SummaryResp")
  public record SummaryResp(
      int miniTotal,     // 미니체크 총 풀이 수
      int miniCorrect,   // 미니체크 정답 수
      boolean miniPassed,// 미니체크 전부 정답 여부
      int mcqTotal,      // MCQ 총 풀이 수
      int mcqCorrect,    // MCQ 정답 수
      String aiSummary,  // AI 학습 요약(폴백 포함)
      boolean completed  // 완료 여부(최소 시도)
  ) {}

  /* ===================== 즉시 채점 - OX ===================== */

  // OX 단건 즉시 채점 요청
  @Schema(name = "MiniGradeOneReq")
  public record MiniGradeOneReq(
      Long topicId,    // 토픽 ID
      Long questionId, // 문제 ID
      Boolean answer   // 사용자 답
  ) {}

  // OX 단건 즉시 채점 응답
  @Schema(name = "MiniGradeOneResp")
  public record MiniGradeOneResp(
      boolean correct,        // 정오
      String explanation,     // DB 기본 해설
      Long learningSessionId // LearningSession ID (오답 조회용)
  ) {}

  /* ===================== 즉시 채점 - MCQ ===================== */

  // MCQ 즉시 채점 요청
  @Schema(name = "McqGradeOneReq")
  public record McqGradeOneReq(
      Long topicId,    // 토픽 ID
      Long questionId, // 문제 ID
      String label     // 사용자 선택 라벨
  ) {}

  // MCQ 즉시 채점 응답
  @Schema(name = "McqGradeOneResp")
  public record McqGradeOneResp(
      boolean correct,     // 정오
      String correctLabel, // 정답 라벨
      String explanation,  // DB 기본 해설
      String aiExplanation // 오답 시 AI 해설
  ) {}

  /* ===================== 문제 상세 조회 ===================== */

  // 필기 문제 상세 조회 응답
  @Schema(name = "QuestionDetailResp")
  public record QuestionDetailResp(
      Long questionId,        // 문제 ID
      String type,            // 문제 타입 (OX, MCQ)
      String stem,            // 문제 본문
      List<McqChoice> choices,// 선택지 (MCQ만, OX는 빈 배열)
      String correctAnswer,   // 정답 (OX: "O"/"X", MCQ: "A"/"B"/"C"/"D")
      String explanation      // 해설 (DB 기본 해설)
  ) {}

  // 여러 필기 문제 상세 조회 응답
  @Schema(name = "QuestionDetailListResp")
  public record QuestionDetailListResp(
      List<QuestionDetailResp> questions  // 문제 상세 정보 목록
  ) {}
}
