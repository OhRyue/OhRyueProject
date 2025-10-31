package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.util.List;

public class MicroDtos {

  /* ===================== 개념(리치 컨텐츠) ===================== */

  @Schema(name = "ConceptResp")
  public record ConceptResp(
      Long topicId,
      String title,                  // 토픽명
      List<Section> sections         // 섹션 목록(중요도/코드/블록 포함)
  ) {
    @Schema(name = "ConceptSection")
    public record Section(
        Integer orderNo,
        String subCode,
        String title,
        Integer importance,          // 별 개수(0~3)
        List<Block> blocks
    ) {}

    @Schema(name = "ConceptBlock")
    public record Block(
        String type,                 // heading | paragraph | list | image | table
        String text,                 // heading/paragraph
        List<String> items,          // list
        String url,                  // image
        String alt,                  // image
        String caption,              // image/table
        List<String> headers,        // table
        List<List<String>> rows      // table
    ) {}
  }

  /* ===================== 미니체크(OX) ===================== */

  @Schema(name = "MiniSet")
  public record MiniSet(List<MiniQuestion> items) {}

  @Schema(name = "MiniQuestion")
  public record MiniQuestion(Long questionId, String text) {}

  @Schema(name = "MiniSubmitReq")
  public record MiniSubmitReq(String userId, Long topicId, List<MiniAnswer> answers) {}

  @Schema(name = "MiniAnswer")
  public record MiniAnswer(Long questionId, Boolean answer) {}

  @Schema(name = "MiniSubmitResp")
  public record MiniSubmitResp(int total, int correct, boolean passed, List<MiniSubmitItem> items) {}

  @Schema(name = "MiniSubmitItem")
  public record MiniSubmitItem(Long questionId, boolean correct, String explanation, String aiExplanation) {}

  /* ===================== MCQ ===================== */

  @Schema(name = "McqSet")
  public record McqSet(List<McqQuestion> items) {}

  @Schema(name = "McqQuestion")
  public record McqQuestion(Long questionId, String text, List<McqChoice> choices, String imageUrl) {}

  @Schema(name = "McqChoice")
  public record McqChoice(String label, String text) {}

  @Schema(name = "McqSubmitReq")
  public record McqSubmitReq(String userId, Long topicId, List<McqAnswer> answers) {}

  @Schema(name = "McqAnswer")
  public record McqAnswer(Long questionId, String label) {}

  @Schema(name = "McqSubmitResp")
  public record McqSubmitResp(int total, int correct, List<McqSubmitItem> items) {}

  @Schema(name = "McqSubmitItem")
  public record McqSubmitItem(Long questionId, boolean correct, String correctLabel, String explanation, String aiExplanation) {}

  /* ===================== 요약 ===================== */

  @Schema(name = "SummaryResp")
  public record SummaryResp(int miniTotal, int miniCorrect, boolean miniPassed, int mcqTotal, int mcqCorrect) {}
}
