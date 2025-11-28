package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.dto.VersusDtos;
import com.OhRyue.certpilot.progress.service.VersusResultService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Versus Result", description = "Versus 매치 결과 기록 및 보상 지급 API")
@RestController
@RequestMapping("/api/progress/versus")
@RequiredArgsConstructor
public class VersusResultController {

    private final VersusResultService versusResultService;

    @Operation(
        summary = "Versus 매치 결과 기록 및 보상 지급",
        description = "매치 완료 시 호출됩니다. 참가자별 XP 지급, 뱃지 평가, 랭킹 반영을 수행합니다.\n\n" +
            "**자동 처리 사항:**\n" +
            "1. 각 참가자에게 모드별 XP 지급\n" +
            "   - DUEL 승리: 30 XP, 참가: 5 XP\n" +
            "   - TOURNAMENT 우승: 100 XP, 참가: 10 XP\n" +
            "   - GOLDENBELL 우승: 200 XP, 참가: 20 XP\n" +
            "2. 배틀 기록 저장 (battle_record, battle_answer 테이블)\n" +
            "3. 뱃지 평가 (배틀 관련 뱃지 체크)\n" +
            "4. 랭킹 재계산\n\n" +
            "**주의사항:**\n" +
            "- winner는 1등 참가자의 userId입니다.\n" +
            "- participants는 점수 순으로 정렬된 리스트여야 합니다.\n" +
            "- 개별 참가자 처리 실패 시에도 다른 참가자 처리는 계속됩니다."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "204", description = "처리 완료 (No Content)"),
        @ApiResponse(responseCode = "400", description = "잘못된 요청 (participants가 비어있음 등)")
    })
    @PostMapping("/result")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void recordVersusResult(
        @io.swagger.v3.oas.annotations.parameters.RequestBody(
            description = "매치 결과 요청",
            required = true,
            content = @Content(
                examples = {
                    @ExampleObject(
                        name = "DUEL 모드 결과",
                        value = """
                            {
                              "mode": "DUEL",
                              "roomId": 123,
                              "winner": "user1",
                              "participants": [
                                {
                                  "userId": "user1",
                                  "score": 8500,
                                  "rank": 1,
                                  "correctCount": 8,
                                  "totalCount": 10,
                                  "totalTimeMs": 45000
                                },
                                {
                                  "userId": "user2",
                                  "score": 7200,
                                  "rank": 2,
                                  "correctCount": 7,
                                  "totalCount": 10,
                                  "totalTimeMs": 50000
                                }
                              ],
                              "questionCount": 10,
                              "durationMs": 120000
                            }
                            """
                    ),
                    @ExampleObject(
                        name = "TOURNAMENT 모드 결과",
                        value = """
                            {
                              "mode": "TOURNAMENT",
                              "roomId": 124,
                              "winner": "user1",
                              "participants": [
                                {
                                  "userId": "user1",
                                  "score": 15000,
                                  "rank": 1,
                                  "correctCount": 9,
                                  "totalCount": 9,
                                  "totalTimeMs": 60000
                                },
                                {
                                  "userId": "user2",
                                  "score": 12000,
                                  "rank": 2,
                                  "correctCount": 7,
                                  "totalCount": 9,
                                  "totalTimeMs": 70000
                                }
                              ],
                              "questionCount": 9,
                              "durationMs": 180000
                            }
                            """
                    )
                }
            )
        )
        @Valid @RequestBody VersusDtos.VersusResultRequest request) {
        versusResultService.recordVersusResult(request);
    }
}

