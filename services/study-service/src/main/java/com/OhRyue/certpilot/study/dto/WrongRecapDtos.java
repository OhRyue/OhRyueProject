package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;

public class WrongRecapDtos {

    @Schema(name = "WrongRecapSet", description = "틀린문제 요약 보기 세트")
    public record WrongRecapSet(List<Item> items) {

        @Schema(name = "WrongRecapItem", description = "문항/내답/정답/DB 해설 요약")
        public record Item(
                Long questionId,
                String type,            // OX | MCQ | SHORT | LONG
                String text,
                String myAnswer,        // 사용자가 제출한 답(라벨/boolean/텍스트)
                String correctAnswer,   // 정답 라벨/boolean/NULL(실기는 정답 개념이 없는 경우 빈값)
                String baseExplanation, // DB 해설
                String imageUrl,        // 선택
                String aiExplanation,   // AI 해설 (실기에서 사용, 필기에서는 null)
                Boolean aiExplanationFailed // AI 해설 생성 실패 여부 (실기에서만 사용, 필기에서는 null)
        ){}
    }
}
