package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.VersusDtos;
import com.OhRyue.certpilot.study.service.VersusQuestionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Versus Question", description = "Versus 모드용 문제 생성 및 검증 API")
@RestController
@RequestMapping("/api/study/versus")
@RequiredArgsConstructor
public class VersusQuestionController {

    private final VersusQuestionService versusQuestionService;

    @Operation(
        summary = "Versus 모드용 문제 세트 생성",
        description = "Versus 모드(대전/토너먼트/골든벨)에서 사용할 문제 세트를 자동으로 생성합니다.\n\n" +
            "**문제 생성 방식:**\n" +
            "- examMode, difficulty, topicScope에 따라 문제를 필터링합니다.\n" +
            "- questionTypes에 지정된 유형별 개수만큼 문제를 수집합니다.\n" +
            "- 랜덤 셔플 후 요청한 count만큼 반환합니다.\n\n" +
            "**examMode:**\n" +
            "- WRITTEN: 필기 시험 (OX, MCQ)\n" +
            "- PRACTICAL: 실기 시험 (SHORT, LONG)\n\n" +
            "**difficulty:**\n" +
            "- EASY: 쉬움\n" +
            "- NORMAL: 보통\n" +
            "- HARD: 어려움\n\n" +
            "**topicScope:**\n" +
            "- ALL: 전체 범위\n" +
            "- SPECIFIC: topicId로 지정된 특정 토픽"
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "문제 생성 성공"),
        @ApiResponse(responseCode = "400", description = "잘못된 요청 (questionTypes가 비어있음 등)"),
        @ApiResponse(responseCode = "404", description = "조건에 맞는 문제를 찾을 수 없음")
    })
    @PostMapping("/questions")
    public List<VersusDtos.QuestionDto> generateVersusQuestions(
        @io.swagger.v3.oas.annotations.parameters.RequestBody(
            description = "문제 생성 요청",
            required = true,
            content = @Content(
                examples = {
                    @ExampleObject(
                        name = "WRITTEN 모드 (OX + MCQ)",
                        value = """
                            {
                              "examMode": "WRITTEN",
                              "topicScope": "ALL",
                              "topicId": null,
                              "difficulty": "NORMAL",
                              "count": 10,
                              "questionTypes": [
                                {"type": "OX", "count": 2},
                                {"type": "MCQ", "count": 8}
                              ]
                            }
                            """
                    ),
                    @ExampleObject(
                        name = "PRACTICAL 모드 (SHORT + LONG)",
                        value = """
                            {
                              "examMode": "PRACTICAL",
                              "topicScope": "SPECIFIC",
                              "topicId": 100,
                              "difficulty": "HARD",
                              "count": 5,
                              "questionTypes": [
                                {"type": "SHORT", "count": 3},
                                {"type": "LONG", "count": 2}
                              ]
                            }
                            """
                    )
                }
            )
        )
        @Valid @RequestBody VersusDtos.VersusQuestionRequest request
    ) {
        return versusQuestionService.generateVersusQuestions(request);
    }

    @Operation(
        summary = "문제 정보 조회",
        description = "특정 문제의 상세 정보를 조회합니다. 정답 검증 전에 문제 정보를 확인할 때 사용합니다.\n\n" +
            "**반환 정보:**\n" +
            "- 문제 ID, 모드, 유형, 난이도\n" +
            "- 문제 내용 (stem)\n" +
            "- 정답 키 (answerKey) - **주의: 실제 정답이 포함됩니다!**\n" +
            "- 해설 (solutionText)\n" +
            "- 추가 정보 (payloadJson)"
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "조회 성공"),
        @ApiResponse(responseCode = "404", description = "문제를 찾을 수 없음")
    })
    @GetMapping("/questions/{questionId}")
    public VersusDtos.QuestionDto getQuestion(
        @Parameter(description = "문제 ID", example = "1001", required = true)
        @PathVariable Long questionId) {
        return versusQuestionService.getQuestion(questionId);
    }

    @Operation(
        summary = "정답 검증",
        description = "사용자가 제출한 답안을 서버에서 검증합니다.\n\n" +
            "**검증 방식:**\n" +
            "- **OX/MCQ**: label 비교 (대소문자 무시)\n" +
            "  - 예: 정답이 \"A\"이고 사용자 답안이 \"a\"면 정답으로 처리\n" +
            "- **SHORT/LONG**: 텍스트 비교 (대소문자 무시, 공백 정규화)\n" +
            "  - 예: 정답이 \"정규화\"이고 사용자 답안이 \"정규화 \"(공백 포함)면 정답으로 처리\n\n" +
            "**반환 정보:**\n" +
            "- correct: 정답 여부\n" +
            "- correctAnswer: 정답 (참고용)\n" +
            "- explanation: 해설"
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "검증 성공"),
        @ApiResponse(responseCode = "404", description = "문제를 찾을 수 없음")
    })
    @PostMapping("/questions/{questionId}/validate")
    public VersusDtos.AnswerValidationResult validateAnswer(
        @Parameter(description = "문제 ID", example = "1001", required = true)
        @PathVariable Long questionId,
        @io.swagger.v3.oas.annotations.parameters.RequestBody(
            description = "사용자 답안",
            required = true,
            content = @Content(
                examples = {
                    @ExampleObject(
                        name = "MCQ 답안 검증",
                        value = """
                            {
                              "answer": "A",
                              "answerType": "label"
                            }
                            """
                    ),
                    @ExampleObject(
                        name = "SHORT 답안 검증",
                        value = """
                            {
                              "answer": "정규화",
                              "answerType": "text"
                            }
                            """
                    )
                }
            )
        )
        @Valid @RequestBody VersusDtos.UserAnswerDto userAnswer
    ) {
        return versusQuestionService.validateAnswer(questionId, userAnswer);
    }
}

